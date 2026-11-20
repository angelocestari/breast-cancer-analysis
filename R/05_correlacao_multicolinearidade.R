library(ggcorrplot)

dados_modelo <- readRDS(
  "data/processed/breast_cancer_processed.rds"
)

dados_preditores <- dados_modelo |>
  dplyr::select(-diagnosis)

matriz_correlacao <- cor(
  dados_preditores,
  method = "spearman"
)

pasta_saida <- file.path(
  "results",
  "figures",
  "correlacao"
)

if(!dir.exists(pasta_saida)){
  dir.create(
    pasta_saida,
    recursive = TRUE
  )
}

arquivo_heatmap <- file.path(
  pasta_saida,
  "heatmap_correlacao_spearman.png"
)

ordem_variaveis <- c(
  "radius_mean", "radius_se", "radius_worst",
  "texture_mean", "texture_se", "texture_worst",
  "perimeter_mean", "perimeter_se", "perimeter_worst",
  "area_mean", "area_se", "area_worst",
  "smoothness_mean", "smoothness_se", "smoothness_worst",
  "compactness_mean", "compactness_se", "compactness_worst",
  "concavity_mean", "concavity_se", "concavity_worst",
  "concave_points_mean", "concave_points_se", "concave_points_worst",
  "symmetry_mean", "symmetry_se", "symmetry_worst",
  "fractal_dimension_mean", "fractal_dimension_se", "fractal_dimension_worst"
)

matriz_correlacao_ordenada <- matriz_correlacao[
  ordem_variaveis,
  ordem_variaveis
]

grafico_correlacao <- ggcorrplot(
  matriz_correlacao_ordenada,
  type = "lower",
  lab = FALSE,
  hc.order = FALSE
) + 
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 7
    ),
    axis.text.y = element_text(
      size = 7
    )
  )

ggsave(
  filename = arquivo_heatmap,
  plot = grafico_correlacao,
  width = 13,
  height = 11,
  dpi = 300
)



