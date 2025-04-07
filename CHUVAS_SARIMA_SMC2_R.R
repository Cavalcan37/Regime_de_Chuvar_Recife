# Previsão de Chuvas em Recife - Próximos 12 meses

# Modelagem SARIMA - Chuvas em Recife
# Pacotes
if(!require(forecast))install.packages("forecast") 
library(forecast)
if(!require(lmtest))install.packages("lmtest")
library(lmtest)
if(!require(readxl))install.packages("readxl")
library(readxl)
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)

# 1. Dados
ip.recife <- read_excel("ip.pernambuco.xlsx", sheet = "recife") 
ip.recife

# 2. Convertendo em ST
ip.recife <- ts(ip.recife,frequency = 12, start = c(2000,7))
ip.recife
str(ip.recife)

ts.plot(ip.recife)

decomp.ip.recife<-decompose(ip.recife)
plot(decomp.ip.recife)

ggtsdisplay(ip.recife)

# 3. Aferição Gráfica de Sazonalidade
seasonplot(ip.recife, col = rainbow(12), year.labels = T, type = "o", pch = 16)

# 4. Transformação Box-Cox
L <- BoxCox.lambda(ip.recife)
L

ip.recife_BC <- BoxCox(ip.recife, lambda = L)
ip.recife_BC

ts.plot(ip.recife_BC)

decomp.ip.recife_BC<-decompose(ip.recife_BC)
plot(decomp.ip.recife_BC)

ggtsdisplay(ip.recife_BC)

# 5. Nº de Integrações (Diferenciações)
ndiffs(ip.recife_BC)

ip.recife_DIFF <- diff(ip.recife_BC,1)
ts.plot(ip.recife_DIFF)

decomp.ip.recife_DIFF<-decompose(ip.recife_DIFF)
plot(decomp.ip.recife_DIFF)

ggtsdisplay(ip.recife_DIFF) # "contar" "p" e "q"

# 6. Nº de Diferenciações Sazonais # usando a série original
nsdiffs(ip.recife) # Acho que tenho que fazer isso com a série original!!!

ip.recife_DIFF2 <- diff(ip.recife,12)
seasonplot(ip.recife_DIFF2, col = rainbow(12), year.labels = T, type = "o", pch = 16)

ggtsdisplay(ip.recife_DIFF2) # "contar" "P" e "Q"

# 7. Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
ip.recife_train <- ts(ip.recife[1:203], frequency = 12)
ip.recife_train

ip.recife_test <- ts(ip.recife[204:289], frequency = 12)
ip.recife_test

FIT <- Arima(y = ip.recife_train, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# 8. Plotar a Série Treino e Acurácia
plot(ip.recife_train)
lines(FIT$fitted, col = "blue")

accuracy(ip.recife_train, FIT$fitted)

# 9. Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((ip.recife_train - mean(ip.recife_train))^2)
ss_residual <- sum((ip.recife_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# 10. Realizando Agora Forecast para 86 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 86)
predi

plot(predi)

# 11. Plotar as Previsões com a Base de Teste
plot(as.numeric(ip.recife_test), type = "l")
lines(as.numeric(predi$mean), col = "red")

accuracy(as.numeric(ip.recife_test), as.numeric(predi$mean))

# 12. Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)
qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# 13. Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = ip.recife, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full

plot(forecast_full)
lines(forecast_full$mean, col = "red", type = "o", pch = 16)

# Definir a nova janela da série histórica a partir de janeiro de 2012
ip.recife_2012 <- window(ip.recife, start = c(2020, 1))

# Plotar as previsões com a série histórica iniciando em janeiro de 2012
plot(forecast_2012, main = "Previsões a partir de janeiro de 2020", xlab = "Ano", ylab = "Chuvas (mm)")
lines(forecast_2012$mean, col = "red", type = "o", pch = 16)

# HISTOGRAMAS DE PROBABILIDADE (Chuvas)
# 13. Probabilidades Mensais de Chuvas
# 13.1. Prev. Chuvas de Ago/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 37.67, sd = 53))
dados

# Valor de referência para destaque
valor_referencia <- 100
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 37.67, sd = 53)
probabilidade

# Gerar o gráfico ajustado
CH_ago <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 15, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 37.67, sd = 53),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 37.67, sd = 53),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$chuvas))
  ) +
  labs(
    x = "Chuvas - Ago/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 140, y = 0.005,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = -30, y = 0.007,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_ago

# 13.2. Prev. Chuvas de Set/24 (Chuvas)
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 14.97, sd = 45))
dados

# Valor de referência para destaque
valor_referencia <- 60
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 14.97, sd = 45)
probabilidade

# Gerar o gráfico ajustado
CH_set <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 15, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 14.97, sd = 45),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 14.97, sd = 45),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$chuvas))
  ) +
  labs(
    x = "Chuvas - Set/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 120, y = 0.005,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = -50, y = 0.007,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_set

# 13.3. Prev. Chuvas de Out/24 (Chuvas)
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 7.24, sd = 13.65))
dados

# Valor de referência para destaque
valor_referencia <- 17
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 7.24, sd = 13.65)
probabilidade

# Gerar o gráfico ajustado
CH_out <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 6, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 7.24, sd = 13.65),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 7.24, sd = 13.65),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$chuvas))
  ) +
  labs(
    x = "Chuvas - Out/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 40, y = 0.015,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = -22, y = 0.02,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_out

# 13.4. Prev. Chuvas de Nov/24 (Chuvas)
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 5.35, sd = 11.58))
dados

# Valor de referência para destaque
valor_referencia <- 4
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 5.35, sd = 11.58)
probabilidade

# Gerar o gráfico ajustado
CH_nov <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 6, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 5.35, sd = 11.58),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 5.35, sd = 11.58),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$chuvas))
  ) +
  labs(
    x = "Chuvas - Nov/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 34, y = 0.015,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = -18, y = 0.02,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_nov

library(patchwork)
library(magrittr)

(CH_ago + CH_set)/(CH_out + CH_nov)

##
## AGORA VAMOS CALCULAR A PROB. DE NÃO CHOVER (PROB(CHUVAS) < 0 = ?)

# 13.1. Prev. Chuvas de Ago/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 37.67, sd = 53))
dados

# Valor de referência para destaque
valor_referencia <- 0
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- pnorm(valor_referencia, mean = 37.67, sd = 53)
probabilidade

# Gerar o gráfico ajustado
CH_ago <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 15, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 37.67, sd = 53),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 37.67, sd = 53),
    fill = "red", alpha = 0.5,
    xlim = c(min(dados$chuvas), valor_referencia)
  ) +
  labs(
    x = "Chuvas - Ago/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = -80, y = 0.005,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 120, y = 0.007,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_ago

# 13.2. Prev. Chuvas de Set/24 (Chuvas)
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 14.97, sd = 45))
dados

# Valor de referência para destaque
valor_referencia <- 0
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- pnorm(valor_referencia, mean = 14.97, sd = 45)
probabilidade

# Gerar o gráfico ajustado
CH_set <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 15, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 14.97, sd = 45),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 14.97, sd = 45),
    fill = "red", alpha = 0.5,
    xlim = c(min(dados$chuvas), valor_referencia)
  ) +
  labs(
    x = "Chuvas - Set/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = -80, y = 0.005,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 85, y = 0.0075,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_set

# 13.3. Prev. Chuvas de Out/24 (Chuvas)
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 7.24, sd = 13.65))
dados

# Valor de referência para destaque
valor_referencia <- 0
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- pnorm(valor_referencia, mean = 7.24, sd = 13.65)
probabilidade

# Gerar o gráfico ajustado
CH_out <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 6, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 7.24, sd = 13.65),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 7.24, sd = 13.65),
    fill = "red", alpha = 0.5,
    xlim = c(min(dados$chuvas), valor_referencia)
  ) +
  labs(
    x = "Chuvas - Out/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = -20, y = 0.015,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 28, y = 0.025,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_out

# 13.4. Prev. Chuvas de Nov/24 (Chuvas)
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(chuvas = rnorm(5000, mean = 5.35, sd = 11.58))
dados

# Valor de referência para destaque
valor_referencia <- 0
media_valor <- mean(dados$chuvas) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- pnorm(valor_referencia, mean = 5.35, sd = 11.58)
probabilidade

# Gerar o gráfico ajustado
CH_nov <- ggplot(dados, aes(x = chuvas)) +
  geom_histogram(aes(y = ..density..), binwidth = 6, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 5.35, sd = 11.58),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 5.35, sd = 11.58),
    fill = "red", alpha = 0.5,
    xlim = c(min(dados$chuvas), valor_referencia)
  ) +
  labs(
    x = "Chuvas - Nov/24 (mmHg)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = -20, y = 0.015,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 20, y = 0.032,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CH_nov

library(patchwork)
library(magrittr)

(CH_ago + CH_set)/(CH_out + CH_nov)






















