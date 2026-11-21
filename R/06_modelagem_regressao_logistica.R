library(rsample)
library(caret)

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
  




  
  