library(readr)

list.files("data/raw")

nomes_colunas <- c(
  "id",
  "diagnosis",
  
  "radius_mean",
  "texture_mean",
  "perimeter_mean",
  "area_mean",
  "smoothness_mean",
  "compactness_mean",
  "concavity_mean",
  "concave_points_mean",
  "symmetry_mean",
  "fractal_dimension_mean",
  
  "radius_se",
  "texture_se",
  "perimeter_se",
  "area_se",
  "smoothness_se",
  "compactness_se",
  "concavity_se",
  "concave_points_se",
  "symmetry_se",
  "fractal_dimension_se",
  
  "radius_worst",
  "texture_worst",
  "perimeter_worst",
  "area_worst",
  "smoothness_worst",
  "compactness_worst",
  "concavity_worst",
  "concave_points_worst",
  "symmetry_worst",
  "fractal_dimension_worst"
)

dados <- read.csv(
  "data/raw/wdbc.data",
  col.names = nomes_colunas,
  header = FALSE
  )


dim(dados)
names(dados)
str(dados)
table(dados$diagnosis)
summary(dados$diagnosis)
sum(is.na(dados)) #dados ausentes
sum(duplicated(dados)) #dados duplicados
