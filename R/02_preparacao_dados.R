dados_modelo <- dados |> #elimina coluna id e transforma diagnosis em variável categórica
  dplyr::select(-id) |>
  dplyr::mutate(
    diagnosis = factor(diagnosis, levels = c("B", "M"))
  )


str(dados_modelo$diagnosis)

readr::write_csv(
  dados_modelo,
  "data/processed/breast_cancer_processed.csv"
)

saveRDS(
  dados_modelo,
  "data/processed/breast_cancer_processed.rds"
)
