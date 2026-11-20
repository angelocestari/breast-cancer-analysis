library(ggplot2)
library(dplyr)
library(tidyr)

#1. Carregar dados

dados_modelo <- readRDS(
  "data/processed/breast_cancer_processed.rds"
)

#2. Distribuição da variável resposta


table(dados_modelo$diagnosis) #qtde maligno/benigno

prop.table(table(dados_modelo$diagnosis)) # % maligno/benigno


tabela_diagnostico <- dados_modelo |> 
  dplyr::count(diagnosis) |>
  dplyr::mutate(
    percentual = n / sum(n) * 100
  )

tabela_diagnostico

ggplot(tabela_diagnostico, aes(x=diagnosis, y=n, fill=diagnosis)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "B" = "#4DAF4A",
      "M" = "#E41A1C"
    ),
    labels = c(
      "B" = "Benigno",
      "M" = "Maligno"
    )
  ) +
  labs(
    x="Diagnóstico",
    y="Número de casos",
    title="Distribuição dos diagnósticos",
    fill= "Diagnóstico"
  )

ggsave(
  "results/figures/distribuicao_diagnostico.png",
  width = 7,
  height = 5
)

#3. Estatísticas descritivas das variáveis numéricas

summary(dados_modelo)

dados_longos <- dados_modelo |>
  pivot_longer(
    cols = -diagnosis,
    names_to = c("caracteristica", "tipo"),
    names_pattern = "(.+)_(mean|se|worst)",
    values_to = "valor"
  )

head(dados_longos)
unique(dados_longos$caracteristica)
unique(dados_longos$tipo)
table(dados_longos$caracteristica)

estatisticas_descritivas <- dados_longos |>
  dplyr::group_by(diagnosis, caracteristica, tipo) |>
  dplyr::summarise(
    n = dplyr::n(),
    media = mean(valor),
    mediana = median(valor),
    desvio_padrao = sd(valor),
    q1 = quantile(valor, 0.25),
    q3 = quantile(valor, 0.75),
    minimo = min(valor),
    maximo = max(valor),
    .groups = "drop"
  )

#4. Comparação visual entre os diagnósticos

#Função para criar o boxplot e salvar

grafico_caracteristica <- function(nome_caracteristica, salvar = FALSE) {
  
  grafico <- dados_longos |>
    dplyr::filter(caracteristica == nome_caracteristica) |>
    ggplot(
      aes(
        x = diagnosis,
        y = valor,
        fill = diagnosis
      )
    ) +
    geom_boxplot() +
    facet_wrap(
      ~ tipo,
      scales = "free_y"
    ) +
    scale_fill_manual(
      values = c(
        "B" = "#4DAF4A",
        "M" = "#E41A1C"
      ),
      labels = c(
        "B" = "Benigno",
        "M" = "Maligno"
      )
    ) +
    labs(
      x = "Diagnóstico",
      y = "Valor",
      title = paste(
        "Distribuição de",
        nome_caracteristica,
        "por diagnóstico"
      ),
      fill = "Diagnóstico"
    )
  
  if (salvar) {
    ggsave(
      filename = paste0(
        "results/figures/distribuicao_",
        nome_caracteristica,
        "_diagnostico.png"
      ),
      plot = grafico,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
  
  return(grafico)
}

unique(dados_longos$caracteristica)

grafico_caracteristica("radius", salvar = TRUE)
grafico_caracteristica("texture", salvar = TRUE)
grafico_caracteristica("perimeter", salvar = TRUE)
grafico_caracteristica("area", salvar = TRUE)
grafico_caracteristica("smoothness", salvar = TRUE)
grafico_caracteristica("compactness", salvar = TRUE)
grafico_caracteristica("concavity", salvar = TRUE)
grafico_caracteristica("concave_points", salvar = TRUE)
grafico_caracteristica("symmetry", salvar = TRUE)
grafico_caracteristica("fractal_dimension", salvar = TRUE)
