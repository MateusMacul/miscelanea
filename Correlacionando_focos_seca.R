library(rgdal)
library(dplyr)
library(mapview)
library(PerformanceAnalytics)
library(corrplot)
# estudando as correlações ------------------------------------------------

setwd("C:/Users/Mateus/OneDrive - inpe.br/Disciplinas/TOPICOS_FLORESTAS")


# abrindo os dados --------------------------------------------------------

dados <- readOGR("grade_fim_organizada.shp")
View(dados@data)
str(dados@data[,4:13])
# criando a variavel somatoria de todos os focos no ano
for (i in 1:length(dados$Id)) {
  valor <- sum(dados@data[i,4:13])
  dados$Ftotal[i] <- valor
}
dados.nozero <- dados[dados$Ftotal != 0,]
dados.nozero$id <- 1:length(dados.nozero$Id)
# amostragem --------------------------------------------------------------
# agrupar para estratificar
agrupada <- group_by(dados@data, ntile(dados$Ftotal, 100)) # estratificando por quantis (tiles)
agrupada0 <- group_by(dados.nozero@data, ntile(dados.nozero$Ftotal, 100)) # estratificando por quantis (tiles)
# a variavel 'agurpada' agora a um DataFrame
group_size(agrupada) #tamanho das amostras
set.seed(20191127) # para manter as mesmas depois da amostragem aleatoria
amostrada <- sample_frac(agrupada, size = 0.1) # amostrando 30% (0.3) de cada quantil (tiles)
amostrada0 <- sample_frac(agrupada0, size = 0.1) # amostrando 30% (0.3) de cada quantil (tiles)
group_size(amostrada)
group_size(amostrada0)
# selecionando do conjunto amostral (dados.), as amostras (amostrada)
length(amostrada$Id)
length(amostrada0$Id)

dados.amostrado <- dados[c(amostrada$Id),]

dados.nozero.amostrado <- dados.nozero[c(amostrada0$id),]

mapview(dados, col.regions = "grey") + mapview(dados.amostrado, col.regions = "red")
mapview(dados, col.regions = "grey") +mapview(dados.nozero, col.regions = "yellow")+ mapview(dados.nozero.amostrado, col.regions = "red")
str(dados.amostrado@data)
# somente jan
procurar <-paste(c('Id', '1'), collapse = "|")
colunas <- grep(procurar, colnames(dados.amostrado@data))
dados.amostrado.jan <- dados.amostrado[, colunas]
str(dados.amostrado.jan@data)
retirar <-paste(c('10'), collapse = "|")
colunas.retirar <- grep(retirar, colnames(dados.amostrado.jan@data))
dados.amostrado.jan <- dados.amostrado.jan[, -colunas.retirar]
str(dados.amostrado.jan@data)

procurar <-paste(c('Id', '1'), collapse = "|")
colunas <- grep(procurar, colnames(dados.amostrado@data))
dados.nozero.amostrado.jan <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.jan@data)
retirar <-paste(c('10'), collapse = "|")
colunas.retirar <- grep(retirar, colnames(dados.nozero.amostrado.jan@data))
dados.nozero.amostrado.jan <- dados.nozero.amostrado.jan[, -colunas.retirar]
str(dados.amostrado.jan@data)

# fev
procurar <-paste(c('Id', '2'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.fev <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.fev@data)

# mar
procurar <-paste(c('Id', '3'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.mar <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.mar@data)
# abr
procurar <-paste(c('Id', '4'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.abr <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.abr@data)
# mai
procurar <-paste(c('Id', '5'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.mai <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.mai@data)
# jun
procurar <-paste(c('Id', '6'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.jun <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.jun@data)
# jul
procurar <-paste(c('Id', '7'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.jul <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.jul@data)
# ago
procurar <-paste(c('Id', '8'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.ago <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.ago@data)
# set
procurar <-paste(c('Id', '9'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.set <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.set@data)
# out
procurar <-paste(c('Id', '10'), collapse = "|")
colunas <- grep(procurar, colnames(dados.nozero.amostrado@data))
dados.nozero.amostrado.out <- dados.nozero.amostrado[, colunas]
str(dados.nozero.amostrado.out@data)

# gráfico de correlacao ---------------------------------------------------

chart.Correlation(dados.amostrado.jan@data[, -1], histogram=TRUE, pch=19, 
                  font.labels = 4, cex.labels = 10)
png("./figuras/correlacao1_jan.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.jan@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10, cex = 20)
dev.off()
png("./figuras/correlacao1_fev.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.fev@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_mar.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.mar@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_abr.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.abr@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_mai.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.mai@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_jun.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.jun@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_jul.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.jul@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_ago.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.ago@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_set.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.set@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()
png("./figuras/correlacao1_out.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
chart.Correlation(dados.nozero.amostrado.out@data[, -1], histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dev.off()



# jan
png("./figuras/correlacao2_jan.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.jan@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3, tl.pos = "d",)
dev.off()
corrplot(m,method = "number", type = "upper")

# fev
png("./figuras/correlacao2_fev.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.fev@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# mar
png("./figuras/correlacao2_mar.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.mar@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# abr
png("./figuras/correlacao2_abr.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.abr@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# mai
png("./figuras/correlacao2_mai.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.mai@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# jun
png("./figuras/correlacao2_jun.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.jun@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# jul
png("./figuras/correlacao2_jul.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.jul@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# ago
png("./figuras/correlacao2_ago.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.ago@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# set
png("./figuras/correlacao2_set.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.set@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")

# out
png("./figuras/correlacao2_out.png", bg = "transparent",width = 20, height = 20, units = "cm", res = 110)
m<-cor(dados.nozero.amostrado.out@data[,-1])
corrplot.mixed(m, lower.col = "black", number.cex = 3)
dev.off()
corrplot(m,method = "number", type = "upper")








