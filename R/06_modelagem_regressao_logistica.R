library(rsample)
library(caret)
library(car)
library(detectseparation)

dados_modelo <- readRDS(
  "data/processed/breast_cancer_processed.rds"
)

# --------------------------------------------
# Separação Treino/Teste
# --------------------------------------------
set.seed(123)

divisao <- initial_split(
  dados_modelo,
  prop = 0.8,
  strata = diagnosis
)

dados_treino <- training(divisao)
dados_teste <- testing(divisao)

# -------------------------------------------------
# Modelo com todas as caractéristicas não convergiu
# -------------------------------------------------

modelo_completo <- glm(
  diagnosis ~ .,
  data = dados_treino,
  family = binomial()
)

summary(modelo_completo) # dados da regressão

# ---------------------------------------------------------------
# Preciso reduzir a redundância para tentar convergir a regressão
# ---------------------------------------------------------------

preditores_treino <- dados_treino |>
  dplyr::select(-diagnosis)

correlacao_treino <- cor(
  preditores_treino,
  method = "spearman"
)

indices_correlacao_forte_treino <- which(
  abs(correlacao_treino) >= 0.8 &
    upper.tri(correlacao_treino),
  arr.ind = TRUE
)

tabela_correlacoes_fortes_treino <- data.frame(
  variavel_1 = rownames(correlacao_treino)[
    indices_correlacao_forte_treino[,"row"]
  ],
  variavel_2 = colnames(correlacao_treino)[
    indices_correlacao_forte_treino[,"col"]
  ],
  rho_spearman = correlacao_treino[
    indices_correlacao_forte_treino
  ]
) |> 
  dplyr::mutate(
    correlacao_absoluta = abs(rho_spearman)
  ) |> 
  dplyr::arrange(
    dplyr::desc(correlacao_absoluta)
  )


frequencia_correlacoes_treino <- dplyr::bind_rows(
  tabela_correlacoes_fortes_treino |>
    dplyr::select(variavel=variavel_1),
  
  tabela_correlacoes_fortes_treino |>
    dplyr::select(variavel=variavel_2)
) |>
  dplyr::count(
    variavel,
    sort = TRUE,
    name = "numero_correlacoes_fortes"
  )

# -----------------------------------------------------
# Encontrar as variáveis mais reduntantes para remover
# -----------------------------------------------------

variaveis_remover_correlacao <- findCorrelation(
  correlacao_treino,
  cutoff = 0.8,
  names = TRUE,
  exact = TRUE
)

variaveis_remover_correlacao # variáveis que foram redundantes

dados_treino_reduzido <- dados_treino |>
  dplyr::select(
    -dplyr::all_of(variaveis_remover_correlacao)
  )
  
# -----------------------------------
# Segunda tentativa da regressão
# -----------------------------------

modelo_reduzido <- glm( # CONVERGIU!!!!!!
  diagnosis ~ .,
  data = dados_treino_reduzido,
  family = binomial()
)
  
vif_modelo <- vif(modelo_reduzido)

vif_modelo

# -----------------------------------------------------
# Baseado no VIF, removerei caractéristicas aos poucos
# -----------------------------------------------------

dados_treino_vif <- dados_treino_reduzido |>
  dplyr::select(-fractal_dimension_worst)

modelo_vif <- glm(
  diagnosis ~ .,
  data = dados_treino_vif,
  family = binomial()
)

car::vif(modelo_vif) # verifica multicolinearidade agora

dados_treino_vif_2 <- dados_treino_vif |> 
  dplyr::select(-compactness_se)

modelo_vif_2 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_2,
  family = binomial()
)

vif_modelo_2 <- car::vif(modelo_vif_2)

sort(
  vif_modelo_2,
  decreasing = TRUE
)

dados_treino_vif_3 <- dados_treino_vif_2 |>
  dplyr::select(-smoothness_mean)

modelo_vif_3 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_3,
  family = binomial()
)  

vif_modelo_3 <- car::vif(modelo_vif_3)
sort(
  vif_modelo_3,
  decreasing = TRUE
)

dados_treino_vif_4 <- dados_treino_vif_3 |>
  dplyr::select(-fractal_dimension_se)

modelo_vif_4 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_4,
  family = binomial()
)  

vif_modelo_4 <- car::vif(modelo_vif_4)
sort(
  vif_modelo_4,
  decreasing = TRUE
)

# -----------------------------------------------------------
# A partir daqui, multicolinearidade não é mais um problema
# Temos VIF < 5 para todas as características restantes
# -----------------------------------------------------------

range(
  fitted(modelo_vif_4)
)

summary(modelo_vif_4)

dados_treino_padronizado <- dados_treino_vif_4

dados_treino_padronizado <- dados_treino_padronizado |>
  dplyr::mutate(
    dplyr::across(
      -diagnosis,
      scale
    )
  )

modelo_padronizado <- glm(
  diagnosis ~ .,
  data = dados_treino_padronizado,
  family = binomial()
)


# ----------------------------------------------------
# Estou com um warning sobre probabilidades extremas
# ----------------------------------------------------

diagnostico_separacao <- glm(
  diagnosis ~ .,
  data = dados_treino_padronizado,
  family = binomial(),
  method = detect_separation
)

diagnostico_separacao # Não estão divergindo para infinito, então tudo ok


# ---------------------------------------
# Análise da regressão logística
# ---------------------------------------

summary(modelo_vif_4) # significante: p-value < 0.05

resultado_drop_1 <- drop1(
  modelo_vif_4,
  test = "Chisq"
)

resultado_drop_1 
# Observar o p-value.
# H0: retirar essa variável não piora significativamente o modelo (pode retirar)
# H1: retirar essa varíavel piora significativamente o modelo (deve manter)

# Vou remover "smoothness_se" pois não impactará

dados_treino_vif_5 <- dados_treino_vif_4 |>
  dplyr::select(-smoothness_se)

modelo_vif_5 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_5,
  family = binomial()
)

car::vif(modelo_vif_5)


resultado_drop1_5 <- drop1(
  modelo_vif_5,
  test = "Chisq"
)

resultado_drop1_5 # p-value > 0.05 e menor AIC -> remover

dados_treino_vif_6 <- dados_treino_vif_5 |>
  dplyr::select(-fractal_dimension_mean)

modelo_vif_6 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_6,
  family = binomial()
)

car::vif(modelo_vif_6)


resultado_drop1_6 <- drop1(
  modelo_vif_6,
  test = "Chisq"
)

resultado_drop1_6

dados_treino_vif_7 <- dados_treino_vif_6 |>
  dplyr::select(-symmetry_mean)

modelo_vif_7 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_7,
  family = binomial()
)

car::vif(modelo_vif_7)


resultado_drop1_7 <- drop1(
  modelo_vif_7,
  test = "Chisq"
)

resultado_drop1_7

dados_treino_vif_8 <- dados_treino_vif_7 |>
  dplyr::select(-compactness_worst)

modelo_vif_8 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_8,
  family = binomial()
)

car::vif(modelo_vif_8)


resultado_drop1_8 <- drop1(
  modelo_vif_8,
  test = "Chisq"
)

resultado_drop1_8

dados_treino_vif_9 <- dados_treino_vif_8 |>
  dplyr::select(-symmetry_se)

modelo_vif_9 <- glm(
  diagnosis ~ .,
  data = dados_treino_vif_9,
  family = binomial()
)

car::vif(modelo_vif_9)


resultado_drop1_9 <- drop1(
  modelo_vif_9,
  test = "Chisq"
)

resultado_drop1_9

saveRDS(
  modelo_vif_9,
  "results/modelo_logistico_final.rds"
)
