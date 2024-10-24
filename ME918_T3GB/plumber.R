library(plumber)
library(jsonlite)
library(ggplot2)
library(readr)
library(lubridate)

#* @apiTitle Trabalho 3 Grupo B muito louco yipee
#* @apiDescription Regressão para um modelo em que há uma variável preditora numérica e uma categórica com 3 fatores. Interação com banco de dados, regressão linear e predições. Inclusão de novos, e rotas relacionadas a extração de inferências sobre o conjunto de dados em seu estado atual e predição para novos dados.











###########################
###########################
######### Parte 1 #########
###########################
###########################
dados_regressao <- if (file.exists("dados_regressao.csv")) {
  read_csv("dados_regressao.csv")
} else {
    ra <- 194340
    set.seed(ra)
    b0 <- runif(1, -2, 2); b1 <- runif(1, -2, 2)
    bB <- 2; bC <- 3
    n <- 25
    x <- rpois(n, lambda = 4) + runif(n, -3, 3)
    grupo <- sample(LETTERS[1:3], size = n, replace = TRUE)
    y <- rnorm(n, mean = b0 + b1 * x + bB * (grupo == "B") + bC * (grupo == "C"), sd = 2)
    df <- data.frame(x = x, grupo = grupo, y = y, momento_registro = now())
    write_csv(df, file = "dados_regressao.csv")
    df
}

atualizar_csv <- function() {
  write_csv(dados_regressao, file = "dados_regressao.csv")
}









###########################
###########################
######### Parte 2 #########
###########################
###########################

#* Adiciona uma nova observação nos dados
#* @param x Nova variável preditora numérica
#* @param grupo Nova variável preditora categórica
#* @param y Nova resposta numérica (variável dependente)
#* @post /new_obs
function(x, grupo, y) {
  grupo <- toupper(grupo)
  if (grupo != "A" & grupo != "B" & grupo != "C") {
    stop("Variável categórica 'grupo' precisa ser A, B ou C")
  }
  
  x <- as.numeric(x)
  if (is.na(x)) {
    stop("Variável numérica 'x' inválida")
  }
  
  y <- as.numeric(y)
  if (is.na(y)) {
    stop("Resposta numérica 'y' inválida")
  }
  
  nova_obs <- data.frame(x = x, grupo = grupo, y = y, momento_registro = now())
  dados_regressao <<- rbind(dados_regressao, nova_obs)
  atualizar_csv()
  print(paste0("A nova observação (", x, ", ", grupo, ", ", y, ") foi incluída na data ", now(), " e na posição ", nrow(dados_regressao), "."))
}

#* Deleta uma observação específica do banco de dados
#* @param observacao Observação a ser removida, de acordo com a posição da linha.
#* @post /delete_obs
function(observacao) {
  observacao <- as.numeric(observacao)
  
  if (observacao != as.integer(observacao) | observacao < 1 | observacao > nrow(dados_regressao) | is.na(observacao)) {
    stop(paste0("Posicionamento da observação inválido ou fora da quantidade total de dados. Sua entrada deve ser um inteiro entre 1 e ", nrow(dados_regressao), "."))
  }
  
  dados_regressao <<- dados_regressao[-observacao, ]
  atualizar_csv()
  print(paste0("A observação ", observacao, " foi removida na data ", now(), "."))
}

#* Muda uma observação específica do banco de dados
#* @param observacao Observação a ser mudada, de acordo com a posição da linha.
#* @param x Nova variável preditora numérica
#* @param grupo Nova variável preditora categórica
#* @param y Nova resposta numérica (variável dependente)
#* @post /switch_obs
function(observacao, x, grupo, y) {
  observacao <- as.numeric(observacao)
  grupo <- toupper(grupo)
  
  if (grupo != "A" & grupo != "B" & grupo != "C") {
    stop("Variável categórica 'grupo' precisa ser A, B ou C")
  }
  
  x <- as.numeric(x)
  if (is.na(x)) {
    stop("Variável numérica 'x' inválida")
  }
  
  y <- as.numeric(y)
  if (is.na(y)) {
    stop("Resposta numérica 'y' inválida")
  }
  
  if (observacao != as.integer(observacao) | observacao < 1 | observacao > nrow(dados_regressao) | is.na(observacao)) {
    stop(paste0("Posicionamento da observação inválido ou fora da quantidade total de dados. Sua entrada deve ser um inteiro entre 1 e ", nrow(dados_regressao), "."))
  }
  
  anterior <- dados_regressao[observacao, ]
  dados_regressao[observacao, ] <<- list(x, grupo, y, now())
  atualizar_csv()
  print(paste0("A observação ", observacao, " foi alterada de (", anterior[1], ", ", anterior[2], ", ", anterior[3], ") para (", x, ", ", grupo, ", ", y, ") na data ", now(), "."))
}









###########################
###########################
######### Parte 3 #########
###########################
###########################

#* Cria um gráfico com os valores registrados
#* @serializer png
#* @get /grafico
function() {
  grafico <- ggplot(dados_regressao,
                    aes(x = x,
                        y = y,
                        color = grupo)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE) +
    labs(title = "Gráfico dos dados com reta de regressão (desconsiderando o grupo)",
         x = "Variável preditora (x)",
         y = "Variável resposta (y)") +
    theme_classic()
  print(grafico)
}

#* Estima os parâmetros de regressão linear dos dados
#* @serializer json
#* @get /parametros
function() {
  reg <- lm(y ~ x + grupo, data = dados_regressao)
  reg_ <- reg$coefficients
  variancia <- anova(reg)["Residuals", "Mean Sq"]
  names(reg_) <- NULL
  parametros <- data.frame(parametro = c(names(reg$coefficients), "Variância_do_erro"),
                           estimativa = c(reg_, variancia))
  return(parametros)
}

#* Retorna os resíduos do modelo de regressão linear
#* @serializer json
#* @get /residuos
function() {
  reg <- lm(y ~ x + grupo, data = dados_regressao)
  data.frame(observacao = 1:nrow(dados_regressao), residuo = reg$residuals)
}

#* Cria um gráfico com os resíduos do modelo de regressão
#* @serializer png
#* @get /grafico_residuos
function() {
  reg <- lm(y ~ x + grupo, data = dados_regressao)
  residuos <- data.frame(residuo = reg$residuals)
  grafico <- ggplot(residuos) +
    stat_qq(aes(sample = residuo)) +
    labs(title = "Comparação de quantis com a distribuição normal padrão\n(QQ-Norm)",
         x = "Quantis teóricos",
         y = "Resíduos") +
    stat_qq_line(aes(sample = residuo)) +
    theme_classic()
  print(grafico)
}

#* Retorna o nível de significância dos parâmetros da regressão
#* @serializer json
#* @get /significancia
function() {
  reg <- summary(lm(y ~ x + grupo, data = dados_regressao))$coefficients[, "Pr(>|t|)"]
  reg_ <- reg
  names(reg_) <- NULL
  data.frame(parametro = names(reg), significancia = reg_)
}









###########################
###########################
######### Parte 4 #########
###########################
###########################

#* Realiza uma predição baseada no seu modelo ajustado com os dados presentes e novos dados
#* @serializer unboxedJSON
#* @param new_x Nova variável preditora numérica para estimar
#* @param new_grupo Nova variável preditora categórica para estimar
#* @get /predicao_unica
function(new_x, new_grupo) {
  new_grupo <- toupper(new_grupo)
  if (new_grupo != "A" & new_grupo != "B" & new_grupo != "C") {
    stop("Variável categórica 'grupo' precisa ser A, B ou C")
  }
  
  new_x <- as.numeric(new_x)
  if (is.na(new_x)) {
    stop("Variável numérica 'x' inválida")
  }
  
  reg <- lm(y ~ x + grupo, data = dados_regressao)
  new_data <- data.frame(x = as.numeric(new_x), grupo = new_grupo)
  data.frame(novo_x = new_data$x, novo_grupo = new_data$grupo, y_predito = predict(reg, new_data))
}

#* Realiza múltiplas predições baseadas no modelo ajustado a partir de novos dados
#* @parser json
#* @serializer json
#* @post /predicao_multipla
function(req) {
  reg <- lm(y ~ x + grupo, data = dados_regressao)
  new_data <- req$body
  valores_permitidos <- c("A", "B", "C")
  if (!all(new_data$grupo %in% valores_permitidos)) {
    stop("Variável categórica 'grupo' precisa ser A, B ou C")
  }
  
  new_data$x <- as.numeric(new_data$x)
  if (anyNA(new_data$x)) {
    stop("Variável numérica 'x' inválida")
  }
  
  data.frame(novo_x = new_data$x, novo_grupo = new_data$grupo, y_predito = predict(reg, new_data))
}
