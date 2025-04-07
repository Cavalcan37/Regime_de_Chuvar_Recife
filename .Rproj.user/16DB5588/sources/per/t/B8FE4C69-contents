# Previsão de Chuvas em Recife - Próximos 12 meses

# Modelagem SARIMA - Chuvas em Recife
# Pacotes
if(!require(forecast))install.packages("forecast") 
library(forecast)
if(!require(lmtest))install.packages("lmtest")
library(lmtest)
if(!require(readxl))install.packages("readxl")
library(readxl)

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
plot(forecast_2012, main = "Previsões a partir de janeiro de 2012", xlab = "Ano", ylab = "Chuvas (mm)")
lines(forecast_2012$mean, col = "red", type = "o", pch = 16)




# Histograma das 12 Próximas Previsões
# Resíduos do modelo ajustado
residuals <- residuals(FIT_full)
residuals

# Distribuição dos resíduos
residual_mean <- mean(residuals)
residual_sd <- sd(residuals)

# Função para simular erros de previsão
simulate_errors <- function(n, mean, sd) {
  rnorm(n, mean = mean, sd = sd)
}

# Simular 1.000 erros para cada um dos 3 períodos de previsão
set.seed(123)
num_simulations <- 500000
errors_simulated <- matrix(nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  errors_simulated[, i] <- simulate_errors(num_simulations, residual_mean, residual_sd)
}

# Gerar cenários de previsão com erros simulados
scenarios <- matrix(NA, nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  scenarios[, i] <- forecast_full$mean[i] + errors_simulated[, i]
}

# Plotar histogramas para cada uma das 3 previsões com curvas de densidade e linhas tracejadas indicando a média
par(mfrow = c(3,4)) # Configurar a janela gráfica para 3 colunas
for (i in 1:12) {
  hist(scenarios[, i], main = paste("Mês", i),
       xlab = "Previsão Pluviométrica", col = "blue", border = "black", breaks = 20,
       prob = TRUE) # Ajustar probabilidade a TRUE para a densidade
  dens <- density(scenarios[, i])
  lines(dens, col = "red") # Adicionar a curva de densidade
  media <- mean(scenarios[, i])
  abline(v = media, col = "red", lty = 2) # Adicionar a linha vermelha tracejada da média
  text(x = media + 0.3 * diff(range(scenarios[, i])), y = max(dens$y) * 0.9,
       labels = paste("Média:", round(media, 2)), col = "black") # Adicionar o valor da média em preto
}

# Retornar ao gráfico único na tela
par(mfrow = c(1,1))

# Histograma das 12 Próximas Previsões com IC 95%
# Resíduos do modelo ajustado
residuals <- residuals(FIT_full)
residuals

# Distribuição dos resíduos
residual_mean <- mean(residuals)
residual_sd <- sd(residuals)

# Função para simular erros de previsão
simulate_errors <- function(n, mean, sd) {
  rnorm(n, mean = mean, sd = sd)
}

# Simular 1.000 erros para cada um dos 12 períodos de previsão
set.seed(123)
num_simulations <- 1000
errors_simulated <- matrix(nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  errors_simulated[, i] <- simulate_errors(num_simulations, residual_mean, residual_sd)
}

# Gerar cenários de previsão com erros simulados
scenarios <- matrix(NA, nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  scenarios[, i] <- forecast_full$mean[i] + errors_simulated[, i]
}

# Plotar histogramas para cada uma das 12 previsões com curvas de densidade e linhas tracejadas indicando a média
par(mfrow = c(3, 4)) # Configurar a janela gráfica para 3 linhas e 4 colunas
for (i in 1:12) {
  hist(scenarios[, i], main = paste("Mês", i),
       xlab = "Previsão Pluviométrica", col = "blue", border = "black", breaks = 20,
       prob = TRUE) # Ajustar probabilidade a TRUE para a densidade
  dens <- density(scenarios[, i])
  lines(dens, col = "red") # Adicionar a curva de densidade
  media <- mean(scenarios[, i])
  abline(v = media, col = "red", lty = 2) # Adicionar a linha vermelha tracejada da média
  
  # Calcular os intervalos de confiança de 95%
  ci_lower <- quantile(scenarios[, i], probs = 0.025)
  ci_upper <- quantile(scenarios[, i], probs = 0.975)
  abline(v = ci_lower, col = "red", lty = 2) # Adicionar a linha verde tracejada para o limite inferior do IC
  abline(v = ci_upper, col = "red", lty = 2) # Adicionar a linha verde tracejada para o limite superior do IC
  
  # Adicionar os valores dos intervalos de confiança no histograma
  text(x = ci_lower, y = max(dens$y) * 0.6, labels = paste("IC 95% Inf:", round(ci_lower, 2)), col = "black", pos = 4)
  text(x = ci_upper, y = max(dens$y) * 0.8, labels = paste("IC 95% Sup:", round(ci_upper, 2)), col = "black", pos = 2)
  
  # Adicionar o valor da média em preto
  text(x = media, y = max(dens$y) * 0.9,
       labels = paste("Média:", round(media, 2)), col = "black", pos = 3)
}

# Retornar ao gráfico único na tela
par(mfrow = c(1,1))
