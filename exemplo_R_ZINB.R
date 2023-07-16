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



# Ajustar o modelo ZINB
modelo <- zeroinfl(art ~ fem + kid5, data = bioChemists,
                   dist = "negbin", link = "logit")

# Sum?rio do modelo
summary(modelo)
require(hnp)
hnp(modelo)

residuos <- residuals(modelo)
plot(predict(modelo), residuos)
abline(h = 0, col = "red")


modelo2 <- zeroinfl(art ~ fem + kid5+ment, data = bioChemists,
                   dist = "negbin", link = "logit")
summary(modelo2)
hnp(modelo2)
residuos <- residuals(modelo2)
plot(predict(modelo2), residuos)
abline(h = 0, col = "red")
