# pacotes nescessarios
pacman::p_load(plotly,pscl,dplyr)

# Carregar o conjunto de dados
dados <- pscl::bioChemists

# Visualizar os primeiros registros do conjunto de dados
head(dados)

# exploratoria dos dados por sexo

eda_G <- function(variavel) {
dados |> 
  dplyr::mutate(sexo = dplyr::case_when(fem == "Men" ~"Masculino" ,
                                        fem == "Women" ~"Feminino")) |> 
  dplyr::group_by(sexo) |> 
  dplyr::select(variavel) |> 
  dplyr::summarise_all(c(
    "Média" = mean,
    "Mínimo" = min,
    "Q1" = ~(quantile(.x, 0.25)),
    "Q2" = median,
    "Máximo" = max,
    "Q3" = ~(quantile(.x, 0.75)),
    "Desvio Padrão" = sd,
    "Amplitude" = ~(max(.x)-min(.x)),
    "IQR" = IQR,
    "Curtose" = moments::kurtosis,
    "Assimtria" = moments::skewness,
    "CV" = ~(sd(.x)/mean(.x)),
    "nobs" = length  )) |> 
    knitr::kable(digits = round(2)) }

purrr::map(c("art",
             "kid5",
             "phd",
             "ment"),~eda_G(.x))


# exploratoria dos dados geral
eda <- function(variavel) {
  dados |> 
    dplyr::mutate(sexo = dplyr::case_when(fem == "Men" ~"Masculino" ,
                                          fem == "Women" ~"Feminino")) |> 
    dplyr::select(variavel) |> 
    dplyr::summarise_all(c(
      "Média" = mean,
      "Mínimo" = min,
      "Q1" = ~(quantile(.x, 0.25)),
      "Q2" = median,
      "Máximo" = max,
      "Q3" = ~(quantile(.x, 0.75)),
      "Desvio Padrão" = sd,
      "Amplitude" = ~(max(.x)-min(.x)),
      "IQR" = IQR,
      "Curtose" = moments::kurtosis,
      "Assimtria" = moments::skewness,
      "CV" = ~(sd(.x)/mean(.x)),
      "nobs" = length )) |> knitr::kable(digits = round(2)) }

purrr::map(c("art",
             "kid5",
             "phd",
             "ment"),~eda(.x))

# graficos exploratorios 

#########Boxplot do N de artigo vs sex#####

boxp <- function(x,Y){
dados |>  dplyr::mutate(sexo = dplyr::case_when(fem == "Men" ~"Masculino" ,
                                            fem == "Women" ~"Feminino")) |>
  plotly::plot_ly(
  x = ~sexo,
  y = ~x,
  split = ~sexo,
  type = 'violin',
  box = list(
    visible = T
  ),
  meanline = list(
    visible = T
  ),
  points = 'all',
  jitter = 0,
  scalemode = 'count',
  meanline = list(
    visible = T
  ),
  marker = list(
    line = list(
      width = 2
    ),
    symbol = 'line-ns'
  )
  ) |>  plotly::layout(title = 'Gráficos Boxplot',
                      yaxis = list(title = Y),
                     colorway = c( '#330066','#003366')) }

boxp(dados$art,"Número de artigos publicados")
boxp(dados$kid5,"Número de crianças até 5 anos")
boxp(dados$phd,"Nota do phd")
boxp(dados$ment,"Número de artigos publicados por orientados")

############# Outros grafico 

dados |>  dplyr::mutate(sexo = dplyr::case_when(fem == "Men" ~"Masculino" ,
                                                fem == "Women" ~"Feminino")) |>
plotly::plot_ly(x = ~kid5, y = ~art , colors = cc ,type = 'scatter') 




# Ajustar o modelo sem inflação
#  o ponto (.) é a soma de todos as outras colunas do dataset
mod_neg <- pscl::zeroinfl(art ~ ., data = dados,
                   dist = "negbin", link = "logit")

mod_poi <- pscl::zeroinfl(art ~ ., data = dados,
                   dist = "poisson", link = "logit")

# Ajustando com uma inflação simples (sem regressores para os zeros)

mod_1_neg <- pscl::zeroinfl(art ~ .|1, data = dados,
                          dist = "negbin", link = "logit")

mod_1_poi <- pscl::zeroinfl(art ~ .|1, data = dados,
                          dist = "poisson", link = "logit")


# Ajustando com os valores inflacionados 
# ("art ~ . | ." is "art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment")

zero_neg <- pscl::zeroinfl(art ~ .|., data = dados,
                            dist = "negbin", link = "logit")

zero_poi <- pscl::zeroinfl(art ~ .|., data = dados,
                            dist = "poisson", link = "logit")

# comparação dos modelos
performance::compare_performance(mod_neg,mod_poi)
performance::compare_performance(mod_1_neg,mod_1_poi)
performance::compare_performance(zero_neg,zero_poi)


# Analise de residuos
