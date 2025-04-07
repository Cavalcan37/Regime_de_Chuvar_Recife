# Correlação entre as Movimentações e o Regime de Chuvas

# Pacotes
if(!require(corrplot))install.packages("corrplot")
library(corrplot)
if(!require(PerformanceAnalytics))install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Dados
dados <- read_excel("ip.pernambuco.xlsx", sheet = "correlacao") 
dados

# Correlação

m<-dados[,2:5]
m
chart.Correlation(m,histogram=T)

