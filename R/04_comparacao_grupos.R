analisar_normalidade <- function(nome_variavel){
  
  caracteristica <- sub(
    "_(mean|se|worst)$",
    "",
    nome_variavel
  )
  
  pasta_saida <- file.path(
    "results",
    "figures",
    caracteristica
  )
  
  if (!dir.exists(pasta_saida)) {
    dir.create(
      pasta_saida,
      recursive = TRUE
    )
  }
  
  resultado_shapiro <- dados_modelo |>
    dplyr::group_by(diagnosis) |> 
    dplyr::summarise(
      p_shapiro = shapiro.test(
        .data[[nome_variavel]]
      )$p.value,
      .groups = "drop"
    )
  
  grafico_histograma <- ggplot(
    dados_modelo,
    aes(
      x = .data[[nome_variavel]],
      fill = diagnosis
    )
  ) + 
    geom_histogram(
      bins = 30,
      alpha = 0.7
    ) + 
    facet_wrap(
      ~ diagnosis,
      scales = "free_y"
    ) + 
    scale_fill_manual(
      values = c(
        "B" = "#4DAF4A",
        "M" = "#E41A1C"
      )
    ) + 
    labs(
      x = nome_variavel,
      y = "Frequência",
      title = paste(
        "Distribuição de",
        nome_variavel,
        "por diagnóstico"
      )
    ) + 
    theme(
      legend.position = "none"
    )
  
  grafico_qqplot <- ggplot(
    dados_modelo,
    aes(
      sample = .data[[nome_variavel]]
    )
  ) + 
    stat_qq() + 
    stat_qq_line() + 
    facet_wrap(
      ~ diagnosis
    ) + 
    labs(
      title = paste(
        "QQ-plot de",
        nome_variavel,
        "por diagnóstico"
      )
    )
  
  arquivo_histograma <- file.path(
    pasta_saida,
    paste0(
      nome_variavel,
      "_histograma.png"
    )
  )
  
  arquivo_qqplot <- file.path(
    pasta_saida, paste0(
      nome_variavel,
      "_qqplot.png"
    )
  )
  
  if(!file.exists(arquivo_histograma)){
    
    ggsave(
      filename = arquivo_histograma,
      plot = grafico_histograma,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
  
  if (!file.exists(arquivo_qqplot)){
    
    ggsave(
      filename = arquivo_qqplot,
      plot = grafico_qqplot,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
  
  return(resultado_shapiro)
  
}

variaveis <- names(dados_modelo)[
  names(dados_modelo) != "diagnosis"
]

resultados_normalidade <- lapply(
  variaveis,
  function(variavel) {
    resultado <- analisar_normalidade(variavel)
    
    resultado$variavel <- variavel
    
    return(resultado)
  }
)

tabela_normalidade <- dplyr::bind_rows(
  resultados_normalidade
) |> 
  dplyr::mutate(
    decisao_normalidade = dplyr::if_else(
      p_shapiro > 0.05,
      "Não rejeita normalidade",
      "Rejeita normalidade"
    )
  )

tabela_testes <- tabela_normalidade |>
  dplyr::group_by(variavel) |>
  dplyr::summarise(
    normalidade_b = dplyr::if_else(
      p_shapiro[diagnosis == "B"] > 0.05,
      "Não rejeita",
      "Rejeita"
    ),
    
    normalidade_m = dplyr::if_else(
      p_shapiro[diagnosis == "M"] > 0.05,
      "Não rejeita",
      "Rejeita"
    ),
    
    teste_sugerido = dplyr::if_else(
      all(p_shapiro > 0.05),
      "Welch",
      "Mann-Whitney"
    ),
    
    .groups = "drop"
  )


resultados_mann_whitney <- lapply(
  variaveis,
  function(variavel){
    
    teste <- wilcox.test(
      dados_modelo[[variavel]] ~ dados_modelo$diagnosis,
      exact = FALSE
    )
    
    data.frame(
      variavel = variavel,
      p_value = teste$p.value
    )
  }
)

tabela_mann_whitney <- dplyr::bind_rows(
  resultados_mann_whitney
)

tabela_mann_whitney <- tabela_mann_whitney |>
  dplyr::mutate(
    p_bonferroni = p.adjust(
      p_value,
      method = "bonferroni"
    ),
    significativo = dplyr::if_else(
      p_bonferroni < 0.05,
      "Sim",
      "Não"
    )
  )








