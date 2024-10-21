library(plumber)
library(jsonlite)
library(ggplot2)

#* @apiTitle Trabalho 3 Grupo B muito louco yipee
#* @apiDescription Regressão para um modelo em que há uma variável preditora numérica e uma categórica com 3 fatores. Interação com banco de dados, regressão linear e predições.  Inclusãode novos, e rotas relacionadas a extração de inferências sobre o conjunto de dados em seu estado atual e predição para novos dados.


######### Parte 1 ######### 

if (!file.exists("dados_regressao.csv")){
  ra <- 194340
    set.seed(ra)
  b0 <- runif(1, -2, 2); b1 <- runif(1, -2, 2)
  bB <- 2; bC <- 3
  n <- 25
  x <- rpois(n, lambda = 4) + runif(n, -3, 3)
  grupo <- sample(LETTERS[1:3], size = n, replace = TRUE)
  y <- rnorm(n, mean = b0 + b1*x + bB*(grupo=="B") + bC*(grupo=="C"), sd = 2)
  df <- data.frame(x = x, grupo = grupo, y = y,
                  momento_registro = lubridate::now())
  readr::write_csv(df, file = "dados_regressao.csv")
}

######### Parte 2 ######### 

#* Adiciona uma nova observação nos dados
#* @param x Nova variável preditora numérica
#* @param grupo Nova variável preditora categórica
#* @param y Nova resposta numérica (variável dependente)
#* @post /new_obs
function(x, grupo, y) {
  if (grupo != "A" & grupo != "B" & grupo != "C"){
    stop("Variável categórica 'grupo' precisa ser A, B ou C")
  }
  x <- as.numeric(x)
  if (is.na(x)){
    stop("Variável numérica 'x' inválida")
  }
  y <- as.numeric(y)
  if (is.na(y)){
    stop("Resposta numérica 'y' inválida")
  }
  tabela <- readr::read_csv("dados_regressao.csv")
  tabela <- rbind(tabela, data.frame(x = x,
                                    grupo = grupo,
                                    y = y,
                                    momento_registro = lubridate::now()))
  readr::write_csv(tabela, file = "dados_regressao.csv")
  print(paste("As novas observações ", x, grupo, y, " foram incluídas na data", lubridate::now()))
}

######### Parte 3 ######### (adicionar parametros de filtro?)

#* Cria um Gráfico com os valores registrados
#* @serializer png
#* @get /grafico
function(){
  tabela <- readr::read_csv("dados_regressao.csv")
  grafico <- ggplot(tabela, aes(x = x, y = y, color = grupo))+
              geom_point()+
              geom_smooth(method = "lm", se = FALSE)
  print(grafico)
}

#* Estima os parâmetros de regressão linear dos dados
#* @serializer json
#* @get /parametros
function(){
  tabela <- readr::read_csv("dados_regressao.csv")
  reg <- lm(y ~ x + grupo, data = tabela)
  data.frame(estimativa = coef(reg))
}



######### Parte 4 #########

#* Realiza uma predição baseada no seu modelo ajustado com os dados presentes e novos dados
#* @serializer unboxedJSON
#* @param new_x Nova variável preditora numérica para estimar
#* @param new_grupo Nova variável preditora categórica para estimar
#* @get /predicao_unica
function(new_x, new_grupo){
  tabela <- readr::read_csv("dados_regressao.csv")
  reg <- lm(y ~ x + grupo, data = tabela)
  new_data <- data.frame(x = as.numeric(new_x), grupo = new_grupo)
  data.frame(novo_x = new_data$x, novo_grupo = new_data$grupo, y_predito = predict(reg, new_data))
}

######### Parte 4.2 #########

#* Realiza múltiplas predições baseadas no modelo ajustado a partir de novos dados
#* @parser json
#* @serializer json
#* @post /predicao_multipla
function(req){
  tabela <- readr::read_csv("dados_regressao.csv")
  reg <- lm(y ~ x + grupo, data = tabela)
  new_data <- req$body
  data.frame(novo_x = new_data$x, novo_grupo = new_data$grupo, y_predito = predict(reg, new_data))
}

