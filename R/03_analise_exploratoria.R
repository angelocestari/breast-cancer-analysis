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

grafico_radius <- dados_longos |> 
  dplyr::filter(caracteristica=="radius") |>
  ggplot(
    aes(
      x=diagnosis,
      y=valor,
      fill=diagnosis
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
    x="Diagnóstico",
    y="Valor",
    title = "Distribuição do raio por diagnóstico",
    fill = "Diagnóstico"
  )

ggsave(
  filename = "results/figures/distribuicao_radius_diagnostico.png",
  plot = grafico_radius,
  width = 8,
  height = 5,
  dpi = 300
)




