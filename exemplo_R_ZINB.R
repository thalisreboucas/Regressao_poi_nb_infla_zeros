# pacotes nescessarios
pacman::p_load(plotly,pscl,tidyverse,easystats,glmtoolbox,tidymodels)

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
      "nobs" = length )) |> 
      knitr::kable(digits = round(2)) }

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

cc <- c( '#330066','#003366')

dados |>  dplyr::mutate(sexo = dplyr::case_when(fem == "Men" ~"Masculino" ,
                                                fem == "Women" ~"Feminino")) |>
plotly::plot_ly(x = ~kid5, y = ~art , colors = cc ,type = 'scatter') 



# Ajustar o modelo de inflação

# Ajustando com uma inflação simples (sem regressores para os zeros)

mod_1_neg <- pscl::zeroinfl(art ~ .|1, data = dados,
                            dist = "negbin", link = "logit")

mod_1_poi <- pscl::zeroinfl(art ~ .|1, data = dados,
                            dist = "poisson", link = "logit")
# 
#  o ponto (.) é a soma de todos as outras colunas do dataset
#   art ~ fem + mar + kid5 + phd + ment
mod_neg <- pscl::zeroinfl(art ~ ., data = dados,
                   dist = "negbin", link = "logit")

mod_poi <- pscl::zeroinfl(art ~ ., data = dados,
                   dist = "poisson", link = "logit")


performance::compare_performance(mod_neg,mod_1_neg,mod_poi,mod_1_poi)
 
#Analise de resíduos

analises_res_zi <- function(modelo){
g1 <- hnp::hnp(modelo)

df_g1  <- data.frame(
  x = g1$x,
  lower = g1$lower,
  median = g1$median,
  upper = g1$upper,
  residuals = g1$residuals
)

grafico_hnp <- ggplotly(
ggplot(df_g1, aes(x)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2) +
  geom_point(aes(y = residuals),colour = "gray10",size = 2) +
  geom_line(aes(y = median) , colour = "#FF4500")  +
  labs(x = "Quantis Teóricos ",title = "QQNorm dos Resíduos para distribuição normal" , y = "Quantis da Amostra")+theme_minimal())


grafico_box <- plot_ly(
  y = ~residuals(modelo),
  type = 'violin',
  box = list(
    visible = T
  ),
  meanline = list(
    visible = T
  ),
  color = I("gray10"),
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
) |>  plotly::layout(title = 'Gráficos Boxplot dos resíduos',
                     xaxis = list(title = ''), 
                     yaxis = list(title = 'Resíduos') )



G_AJUSTE <- plot_ly(
  y= ~fitted(modelo),type = 'scatter',
  marker = list(size = 5 , color = "#1A1A1A")) |> 
  plotly::layout(title = 'Gráficos de disperssão do ajuste do modelo',
                 xaxis = list(title = ''),
                 yaxis = list(title = 'Valores ajustados') )
G_LINEAR <- plot_ly(
  y = ~residuals(modelo),
  x= ~fitted(modelo),
  type = 'scatter',
  marker = list(size = 5 , color = "#1A1A1A")) |> 
  plotly::layout(title = 'Gráficos de linearidade',
                 xaxis = list(title = 'Valores Ajustados'),
                 yaxis = list(title = 'Resíduos') )

G_HOMO <- plot_ly(
  y = ~sqrt(residuals(modelo)),
  x= ~fitted(modelo),
  type = 'scatter',
  marker = list(size = 5 , color = "#1A1A1A")) |> 
  plotly::layout(title = 'Gráficos de homogeneidade de variância',
                 xaxis = list(title = 'Valores Ajustados'),
                 yaxis = list(title = 'Desvio padrão dos resíduos') )




## Anova
anova <- car::Anova(modelo) 

vcov <- vcov(modelo)

summa <- summary(modelo)


lista <- list(grafico_hnp,grafico_box,anova,vcov,summa,G_AJUSTE,G_HOMO,G_LINEAR)
names(lista) <-c("HNP","BOXPLOT","ANOVA","VCOV","SUMMARY","AJUSTE","HOMOGVAR","LINEAR")

return(lista)

}

res_zn <- analises_res_zi(mod_neg)
#res_zn <- analises_res_zi(mod_poi)
res_zn$SUMMARY
res_zn$HNP
res_zn$ANOVA
res_zn$BOXPLOT
#res_zn$VCOV



# comparação
test_performance(mod_1_neg,mod_1_poi)
test_performance(mod_neg,mod_poi)
