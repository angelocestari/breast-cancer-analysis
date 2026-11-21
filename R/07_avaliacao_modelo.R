library(dplyr)
library(pROC)

modelo_final <- readRDS(
  "results/modelo_logistico_final.rds"
)

dados_testes <- readRDS(
  "data/processed/dados_teste.rds"
)

variaveis_finais <- readRDS(
  "results/variaveis_finais.rds"
)


dados_testes_final <- dados_testes |>
  dplyr::select(
    diagnosis,
    dplyr::all_of(variaveis_finais)
  )

probabilidades <- predict(
  modelo_final,
  newdata = dados_testes_final,
  type = "response"
)

resultados_teste <- dados_testes_final |>
  dplyr::mutate(
    probabilidade_maligno = probabilidades
  )

View(resultados_teste)

resultados_teste <- resultados_teste |>
  dplyr::mutate(
    diagnostico_previsto = dplyr::if_else(
      probabilidade_maligno >= 0.5,
      "M",
      "B"
    )
  )

resultados_teste <- resultados_teste |>
  dplyr::mutate(
    diagnostico_previsto = factor(
      diagnostico_previsto,
      levels = c("B", "M")
    )
  )


table(
  Real = resultados_teste$diagnosis,
  Previsto = resultados_teste$diagnostico_previsto
) #matriz de confusão


calcular_metricas <- function(real, previsto, classe_positiva="M"){
  
  classes <- unique(c(
    as.character(real),
    as.character(previsto)
  ))
  
  classe_negativa <- setdiff(
    classes, 
    classe_positiva
  )
  
  TP <- sum(
    real == classe_positiva &
      previsto == classe_positiva
  )
  
  TN <- sum(
    real == classe_negativa &
      previsto == classe_negativa
  )
  
  FP <- sum(
    real == classe_negativa &
      previsto == classe_positiva
  )
  
  FN <- sum(
    real == classe_positiva &
      previsto == classe_negativa
  )
  
  acuracia <- (TP + TN) / (TP + TN + FP + FN)
  
  sensibilidade <- TP / (TP + FN)
  
  especificidade <- TN / (TN + FP)
  
  precisao <- TP / (TP + FP)
  
  matriz_confusao <- table(
    Real = real,
    Previsto = previsto
  )
  
  metricas <- data.frame(
    metrica = c(
      "Acurácia",
      "Sensibilidade",
      "Especificidade",
      "Precisão"
    ),
    valor = c(
      acuracia,
      sensibilidade,
      especificidade,
      precisao
    )
  )
  
  return(
    list(
      matriz_confusao = matriz_confusao,
      TP = TP,
      TN = TN,
      FP = FP,
      FN = FN,
      metricas = metricas
    )
  )
  
}

avaliacao <- calcular_metricas(
  real = resultados_teste$diagnosis,
  previsto = resultados_teste$diagnostico_previsto
)

avaliacao$matriz_confusao
avaliacao$TP
avaliacao$TN
avaliacao$FP
avaliacao$FN
avaliacao$metricas

roc_modelo <- roc(
  response = resultados_teste$diagnosis,
  predictor = resultados_teste$probabilidade_maligno,
  levels = c("B", "M"),
  direction = "<"
)

auc_modelo <- auc(roc_modelo)

auc_modelo

plot(
  roc_modelo,
  main="Curva ROC - Modelo Logístico Final",
  print.auc = TRUE
)

pasta_avaliacao <- file.path(
  "results",
  "figures",
  "avaliacao_modelo"
)

if(!dir.exists(pasta_avaliacao)){
  dir.create(
    pasta_avaliacao,
    recursive = TRUE
  )
}

png(
  filename = file.path(
    pasta_avaliacao,
    "curva_roc_modelo_final.png"
  ),
  width = 2400,
  height = 1800,
  res = 300
)

plot(
  roc_modelo,
  main = "Curva ROC - Modelo Logístico Final",
  print.auc = TRUE
)

dev.off()

auc_modelo

