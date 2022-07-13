library("dplyr")
library("data.table")
library("ggplot2")
library("stringr")
library("stringi")
library("jsonlite")
library("lubridate")
source("/Users/pdealcan/Documents/github/doc_suomi/code/utils.R")

dt = fread("./financas.csv")

colnames(dt) <- c("Mes", "Descricao", "Categoria", "Parcelas", "Credito", "Debito", "FOP", "Situacao")

dt$Credito = str_replace(dt$Credito, "-", "")
dt$Credito = str_replace(dt$Credito, ",", ".")
dt$Credito = as.numeric(dt$Credito)

dt$Debito = str_replace(dt$Debito, "-", "")
dt$Debito = str_replace(dt$Debito, ",", ".")
dt$Debito = as.numeric(dt$Debito)

dt$Categoria = tolower(dt$Categoria)
dt$Categoria = stri_trans_general(str = dt$Categoria, id = "Latin-ASCII")

dt$Descricao = tolower(dt$Descricao)
dt$Descricao = stri_trans_general(str = dt$Descricao, id = "Latin-ASCII")

dt$Mes = str_replace_all(dt$Mes, "/", "-")
dt$Mes = as.Date(dt$Mes, format = "%d-%m-%Y")

dt$Mes = month(dt$Mes, label=TRUE)

firstReport = dt %>% 
  group_by(Categoria) %>%
  summarise(Debito = sum(Debito),
            Credito = sum(Credito)
  ) %>%
  melt()

firstReport$Mes = "media"

secondReport = dt %>% 
  group_by(Categoria, Mes) %>%
  summarise(Debito = sum(Debito),
            Credito = sum(Credito)
  ) %>%
  melt()

#Select seed (e.g. salario)
#select targets (air bnb, alimentacao),
#sum within target values and create from seed to each target,
#sum seed value and summ of all targets
#difference is dropp of
thirdReport = dt %>% dvisu()
  group_by(Categoria, Descricao) %>%
  summarise(Debito = sum(Debito)) %>%
  melt()

salarioTotal = dt %>% filter(Categoria == 'salario') %>% select(Credito) %>% sum()
airbnbTotal = dt %>% filter(Categoria == 'air bnb') %>% select(Debito) %>% sum(na.rm = TRUE)
alimentacaoTotal = dt %>% filter(Categoria == 'alimentacao') %>% select(Debito) %>% sum(na.rm = TRUE)

thirdReport = rbind(thirdReport, 
                    c(Categoria = 'salario', Descricao = 'air bnb', Variable = 'Debito', weight = airbnbTotal),
                    c(Categoria = 'salario', Descricao = 'alimentacao', Variable = 'Debito', weight = alimentacaoTotal))

colnames(firstReport) <- c("Categoria", "Tipo", "Valor", "Mes")
firstReport[is.na(firstReport)] <- 0

colnames(secondReport) <- c("Categoria", "Mes", "Tipo", "Valor")
secondReport[is.na(secondReport)] <- 0

colnames(thirdReport) <- c("from", "to", "Tipo", "weight")
thirdReport[is.na(thirdReport)] <- 0

firstReport <- toJSON(firstReport)
secondReport <- toJSON(secondReport)
thirdReport <- toJSON(thirdReport)

firstReport = paste("var data = ", firstReport, sep="")
secondReport = paste("var data1 = ", secondReport, sep="")
thirdReport = paste("var data2 = ", thirdReport, sep="")

allReports = paste(firstReport, secondReport, thirdReport, sep = "\n")
write(allReports, "./financas.js")
