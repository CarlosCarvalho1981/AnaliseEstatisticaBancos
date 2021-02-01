#Análise das ações dos bancos
# BBAS3.SA - BANCO DO BRASIL
# BCSA34.SA - SANTANDER
# BBDC4.SA - BRADESCO
# ITUB4.SA - ITAU UNIBANCO
# BRSR6.SA - BANRISUL
# 1 - CAPTAR A COTAÇÃO DAS AÇÕES DE TODOS OS BANCOS EM 2020
# 2 - EXTRAIR AS COLUNAS DE FECHAMENTO E VOLUME PARA CADA BANCO
# 3 - VERIFICAR AS MEDIDAS DE TENDÊNCIA CENTRAR DE CADA BANCO (PREÇO E VOLUME)
# 4 - GERAR HISTOGRAMA E BOX PLOT PARA O PREÇO DE CADA BANCO
# 5 - VERIFICAR AS MEDIDAS DE DISPERSÃO DE CADA BANCO
# 6 - RELAÇÃO ENTRE CADA BANCO
# Carlos E. Carvalho
# carlos.e.carvalho@gmail.com
# LinkedIn: https://www.linkedin.com/in/carlos-carvalho-93204b13/
# Github: https://github.com/CarlosCarvalho1981/AnaliseEstatisticaBancos

setwd("D:/CIENTISTA_DADOS/BANCOS")
getwd()

#http://www.quantmod.com/

#Carrega todos os pacotes

library(quantmod)
library(xts)
library(moments)
library(readr)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(psych)


#Selecao do periodo de analise - Aqui tem que colocar o dia que você quer 
startDate <- as.Date("2020-01-02")
endDate <- as.Date("2022-12-30")

# Vetor com o nome dos tickers de cada banco
bancos <- c("BBAS3.SA", "BCSA34.SA", "BBDC4.SA", "ITUB4.SA", "BRSR6.SA")

#Criando um dataframe vazio
#dfBancos <- data.frame()

#Criando o dataframe com as informações do primeiro banco, para manter a coluna de data
ticker <- bancos[1]
dfBancos <- getSymbols(ticker, src = "yahoo", from = startDate, to = endDate, auto.assign = F)
extensao <- ".png"

Completo <- str_replace(ticker, "\\.","")
Completo

Completo <- str_replace(Completo, "SA","")
Completo
Completo <- paste(Completo, extensao)

Completo

Completo <- str_replace(Completo, "\\s", "")
Completo


png(filename = Completo, width = 1200, height = 600, res = 80)
candleChart(dfBancos, name = ticker)
dev.off()

#Extraindo apenas a coluna do fechamento
dfBancos <- dfBancos[,4]

# Captando a coluna com o preço de fechamento dos outros bancos
for (i in 2:length(bancos)){
  ticker <- bancos[i]
  obj <- getSymbols(ticker, src = "yahoo", from = startDate, to = endDate, auto.assign = F)
  #Retira o ponto (.) do ticker
  Completo <- str_replace(ticker, "\\.","")
  Completo
  #Retira o SA do nome
  Completo <- str_replace(Completo, "SA","")
  Completo
  #Junta o nome com a extensão .png
  Completo <- paste(Completo, extensao)
  Completo
  #Retira o espaço do nome
  Completo <- str_replace(Completo, "\\s", "")
  Completo
  
  
  #Gera a figura
  png(filename = Completo, width = 1200, height = 600, res = 80)
  candleChart(obj, name = ticker)
  dev.off()
  
  
  obj1 <- as.vector(obj[,4])
  dfBancos <- cbind(dfBancos,obj1)
  Sys.sleep(0.3) #É importante esperar um tempo para cada requisição, senão o site pode bloquear
}

#Mudando os nomes das colunas
colnames(dfBancos) <- c("BancoBrasil", "Santander", "Bradesco", "Itau", "Banrisul")

# Retirando os valores NA
dfBancos <- dfBancos[complete.cases(dfBancos), ]

View(dfBancos)

#Medidas de tendência central
summary(dfBancos)

#Desvio padrão
apply(dfBancos,2, sd)

#Variância
apply(dfBancos,2, var)

#Coeficiente de Varição
CV <- function(x){
  return ((sd(x)/mean(x))*100)
}

apply(dfBancos,2,CV)

#Coeficiente de Assimetria
apply(dfBancos, 2, skewness)

#Coeficiente de curtose
apply(dfBancos, 2, kurtosis)


dfBancos <- as.data.frame(dfBancos)
dfBancos <- dfBancos %>% mutate(id = row_number())
rownames(dfBancos) <- dfBancos$id
dfBancos$id <- NULL
View(dfBancos)
cor(as.data.frame(dfBancos[c("BancoBrasil", "Santander", "Bradesco", "Itau", "Banrisul")]))

#Visualizando a relação entre as variáveis
pairs(dfBancos[c("BancoBrasil", "Santander", "Bradesco", "Itau", "Banrisul")])


#Mais informações sobre os relacionamentos entre as variáveis
pairs.panels(dfBancos[c("BancoBrasil", "Santander", "Bradesco", "Itau", "Banrisul")])

ggplot(dfBancos, aes(x = BancoBrasil), binwidth = 30) + 
  geom_histogram(aes(y = ..density..), fill = "yellow1", alpha = 0.5)+
  geom_density(colour = "blue") + xlab(expression(bold("Preço de Fechamento"))) + 
  ylab(expression(bold("Densidade")))

ggplot(dfBancos, aes(y = BancoBrasil)) + geom_boxplot() 

ggplot(dfBancos, aes(x = Santander), binwidth = 30) + 
  geom_histogram(aes(y = ..density..), fill = "tomato", alpha = 0.5)+
  geom_density(colour = "blue") + xlab(expression(bold("Preço de Fechamento"))) + 
  ylab(expression(bold("Densidade")))

ggplot(dfBancos, aes(y = Santander)) + geom_boxplot() 

ggplot(dfBancos, aes(x = Bradesco), binwidth = 30) + 
  geom_histogram(aes(y = ..density..), fill = "red", alpha = 0.5)+
  geom_density(colour = "blue") + xlab(expression(bold("Preço de Fechamento"))) + 
  ylab(expression(bold("Densidade")))

ggplot(dfBancos, aes(y = Bradesco)) + geom_boxplot() 

ggplot(dfBancos, aes(x = Itau), binwidth = 30) + 
  geom_histogram(aes(y = ..density..), fill = "blue4", alpha = 0.5)+
  geom_density(colour = "blue") + xlab(expression(bold("Preço de Fechamento"))) + 
  ylab(expression(bold("Densidade")))

ggplot(dfBancos, aes(y = Itau)) + geom_boxplot() 

ggplot(dfBancos, aes(x = Banrisul), binwidth = 30) + 
  geom_histogram(aes(y = ..density..), fill = "skyblue2", alpha = 0.5)+
  geom_density(colour = "blue") + xlab(expression(bold("Preço de Fechamento"))) + 
  ylab(expression(bold("Densidade")))

ggplot(dfBancos, aes(y = Banrisul)) + geom_boxplot() 










