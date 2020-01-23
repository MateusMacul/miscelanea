################################################################################
############            SCRIPT PARA PLOTAR OS MAPAS            #################
############               usando cartogrpahy                  #################
################################################################################

##### Pacotes #######
require(foreign)
require(rgdal)
require(spdep)
require(maptools)
require(dplyr)

library("corrplot")
library("Hmisc")
library("PerformanceAnalytics")
library("nortest")
library("betas")
library("mapview")
library("cartography")
####### pegando os dados #####
setwd("C:/Users/Padrao/Documents/fillcell")
dados <- readOGR("celulas_extraida.shp")
#### pegar a paleta de cores ##
display.carto.all(n=10)
cartography::carto.pal.info()
##### plotando ######
# Parâmentros gerais --------------------------------------------------------------
metodo = "fisher-jenks"
classes = 10
pal = carto.pal("blue.pal",5,"orange.pal",5) #lembrar de mudar os numero de cores de acordo com o tamanho da classe
tema = "blue.pal"
f_dim <- getFigDim(x = dados, height = 600, mar= c(0,0,0,0))
# Amostras ---------------------
titulo = "Amostras utilizadas"
f_dim <- getFigDim(x = dados, width = 700, mar= c(0,0,0,0))
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/amostras2_2.png", width = 700, height = f_dim[2])
par(mar = c(1,1,5,1))
plot(dados, col="#729fcf")
plot(dados.amost2, col = "red", add = T)
layoutLayer(title = titulo, scale = 10,north = TRUE, frame = T, theme = tema)
dev.off()
# AHP ####
unidade = "Ãndice de ValorizaÃ§Ã£o da Terra"
titulo = "ValorizaÃ§Ã£o potencial da terra"
variavel = "ahp"
v = dados$ahp
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_ahp_3v3.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,1,5,1))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
plot(incremento, add = T)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = carto.pal("red.pal",10), x,xlab = unidade, ylim = c(0,2000),ylab = "Frequência", main = titulo, freq = T)
## ahp mais
unidade = "Ãndice de ValorizaÃ§Ã£o da Terra"
titulo = "AHP mais"
variavel = "ahpmais"
v = dados$ahp
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_ahpmais.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,1,5,1))
choroLayer(spdf = dados, var = variavel, breaks = bks1, col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
## ahp menos
unidade = "Ãndice de ValorizaÃ§Ã£o da Terra"
titulo = "AHP menos"
variavel = "ahpmenos"
v = dados$ahp
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_ahpmenos.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,1,5,1))
choroLayer(spdf = dados, var = variavel, breaks = bks1, col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
# sensibilidade 25####
unidade = "q3 -q1"
titulo = "Sensibilidade do mapa de terras 25"
variavel = "sense"
v = dados$sense
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = rev(carto.pal("red.pal",10)), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 8)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = carto.pal("red.pal",10), x,xlab = unidade, ylim = c(0,2000),ylab = "Frequência", main = titulo, freq = T)

plot(incremento, add = T)
# sensibilidade 50 ###
unidade = "q3 -q1"
titulo = "Sensibilidade do mapa de terras 50"
variavel = "sense"
v = dados2$sense
choroLayer(spdf = dados2, var = variavel, method = metodo, nclass = classes, col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 8)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = carto.pal("red.pal",10), x,xlab = unidade, ylim = c(0,2000),ylab = "Frequência", main = titulo, freq = T)


# variÃ¡vel dependente ####
unidade = "incremento/floresta"
titulo = "VariÃ¡vel dependente"
variavel = "y1"
v = dados$y1
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, frame = T, theme = tema)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

plot(incremento, add = T)
# variÃ¡vel dependente alterada####
unidade = "incremento/floresta"
titulo = "VariÃ¡vel dependente alterada"
variavel = "i_y1"
v = dados$i_y1
choroLayer(spdf = dados, var = variavel, breaks = c(0,0.0001, 0.03, 0.12, 0.21, 0.33, 0.5, 0.74, 1.04, 1.42, 1.82, 2), col = carto.pal("blue.pal",1,"orange.pal",10), border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, frame = T, theme = tema)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

plot(v,dados$y1, xlab = titulo, ylab = "VariÃ¡vel dependente (incremente/floresta)")

# Area Aberta ####
unidade = "PorporÃ§Ã£o da celula"
titulo = "ProporÃ§Ã£o de Ãrea Aberta"
variavel = "i_aberta"
v = dados$i_aberta
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", ylim = c(0,length(v[v<=bks1])), main = titulo, freq = T)

# VegetaÃ§Ã£o SecundÃ¡ria --------------
unidade = "Porporção na célula"
titulo = "Proporção de Vegetação Secundária"
variavel = "i_vegse"
v = dados$i_vegse
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
dev.off
# Dist BR 163 ####
unidade = "metros"
titulo = "DistÃ¢ncia das Celulas a BR-163"
variavel = "dis_mn_br"
v = dados$dis_mn_br
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

br163 = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/estradas_22765_2014_BR163.shp")
plot(br163, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# Dist BR 163 Transformada ####
unidade = "Ãndice"
titulo = "DistÃ¢ncia das Celulas a BR-163 Transformada para Ãndice"
variavel = "i_br"
v = dados$i_br
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

plot(br163, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_br, xlab = titulo, ylab = "DistÃ¢ncia das Celulas a BR-163 (m)")

# Estradas vicinais ####
unidade = "Metros"
titulo = "DistÃ¢ncia das Celulas a Estradas Vicinais"
variavel = "dis_mn_est"
v = dados$dis_mn_est
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

estradas = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/estradas_22765_2014.shp")
vicinais = estradas[c(6:length(estradas)),]
plot(vicinais, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Estradas vicinais Transformada####
unidade = "Ãndice"
titulo = "DistÃ¢ncia das Celulas a Estradas Vicinais Transformada para Ãndice"
variavel = "i_est"
v = dados$i_est
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

estradas = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/estradas_22765_2014.shp")
vicinais = estradas[c(6:length(estradas)),]
plot(vicinais, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_est, xlab = titulo, ylab = "DistÃ¢ncia das Celulas a Estradas Vicinais (m)")


# Area Urbana ####
unidade = "Metros"
titulo = "DistÃ¢ncia das Celulas a Ãrea Urbana"
variavel = "dis_mn_au"
v = dados$dis_mn_au
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# Area Urbana Transformada####
unidade = "Ãndice"
titulo = "DistÃ¢ncia das Celulas a Ãrea Urbana Transformada para Ãndice"
variavel = "i_au"
v = dados$i_au
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_au, xlab = titulo, ylab = "DistÃ¢ncia das Celulas a Ãrea Urbana (m)")





# Ãrea dos imoveis ####
unidade = "kmÂ²"
titulo = "MÃ©dia da Ãrea dos Imóveis nas Celulas"
variavel = "areakm_mea"
v = dados$areakm_mea
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Ãrea dos Imóveis Transformada####
unidade = "Ãndice"
titulo = "MÃ©dia da Ãrea dos Imóveis nas Celulas Transformada para Ãndice"
variavel = "i_frag"
v = dados$i_frag
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$areakm_mea, xlab = titulo, ylab = "DistÃ¢ncia das Celulas a Ãrea Urbana (m)")

# Imoveis Certificados####
unidade = "ProporÃ§Ã£o"
titulo = "ProporÃ§Ã£o de Imóveis Certificados nas Celulas"
variavel = "i_cert"
v = dados$i_cert
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Declividade####
unidade = "%"
titulo = "Mediana da Declividade nas Celulas"
variavel = "med_decliv"
v = dados$med_decliv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Declividade Transformada####
unidade = "Ãndice"
titulo = "Mediana da Declividade nas Celulas Transformada para Ãndice"
variavel = "i_decliv"
v = dados$i_decliv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$med_decliv, xlab = titulo, ylab = "Mediana da Declividade nas Celulas (%)")


####
# RIOS ####
unidade = "Metros"
titulo = "Mediana da DistÃ¢ncia das Celulas a Rios"
variavel = "dis_mn_r5"
v = dados$dis_mn_r5
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Rios Transformada####
unidade = "Ãndice"
titulo = "DistÃ¢ncia das Celulas a Rios Transformada para Ãndice"
variavel = "i_rios5"
v = dados$i_rios5
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_r5, xlab = titulo, ylab = "Mediana da DistÃ¢ncia das Celulas a Rios (m)")
# Assentamento ####
unidade = "ProporÃ§Ã£o"
titulo = "ProporÃ§Ã£o de Assentamento nas Celulas"
variavel = "area_ass"
v = dados$area_ass
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Assentamentos Transformada####
unidade = "Ãndice"
titulo = "ProporÃ§Ã£o de Assentamentos nas Celulas Transformada para Ãndice"
variavel = "i_ass_p"
v = dados$i_ass_p
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Embargo ####
unidade = "Porcentagem"
titulo = "Porcentagem de Embargo nas Celulas"
variavel = "area_embar"
v = dados$area_embar
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
# Embargo Transformada####
unidade = "Ãndice"
titulo = "Porcentagem de Embargo nas Celulas Transformada para Ãndice"
variavel = "i_emb"
v = dados$i_emb
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_embar, xlab = titulo, ylab = "Porcentagem de Embargo nas Celulas")

# Floresta ####
unidade = "ProporÃ§Ã£o"
titulo = "ProporÃ§Ã£o de Floresta nas Celulas"
variavel = "area_flore"
v = dados$area_flore
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# Floresta Transformada####
unidade = "Ãndice"
titulo = "ProporÃ§Ã£o de Floresta nas Celulas Transformada para Ãndice"
variavel = "i_flor"
v = dados$i_flor
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_flore, xlab = titulo, ylab = "ProporÃ§Ã£o de Floresta nas Celulas")
# UC ####
unidade = "Proporção"
titulo = "Proporção de Unidades de Conservação nas Células"
variavel = "area_uc"
v = dados$area_uc
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# UC Transformada####
unidade = "Ãndice"
titulo = "ProporÃ§Ã£o de Unidades de ConservaÃ§Ã£o nas Celulas Transformada para Ãndice"
variavel = "i_uc"
v = dados$i_uc
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_uc, xlab = titulo, ylab = "ProporÃ§Ã£o de Unidades de ConservaÃ§Ã£o nas Células")

# TI ####
unidade = "ProporÃ§Ã£o"
titulo = "ProporÃ§Ã£o de Terra IndÃ�gena nas Celulas"
variavel = "area_ti"
v = dados$area_ti
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# TI Transformada####
unidade = "Ãndice"
titulo = "ProporÃ§Ã£o de Terra IndÃ�gena nas Células Transformada para Ãndice"
variavel = "i_ti"
v = dados$i_ti
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_ti, xlab = titulo, ylab = "ProporÃ§Ã£o de Terra IndÃ�gena nas Células")

# MÃ³dulos Fiscais ####
unidade = "MÃ³dulos Fiscais"
titulo = "NÂº MÃ³dulos Fiscais nas Células"
variavel = "modulos_imv"
v = dados$modulos_imv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = T)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Densidade", main = titulo)

# MÃ³dulos Fiscais Transformada ####
unidade = "Ãndice"
titulo = "Ãndice de MÃ³dulos Fiscais nas Células"
variavel = "class_imv"
v = dados$class_imv
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = T)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Densidade", main = titulo)

plot(v,dados$modulos_imv, xlab = titulo, ylab = "NÂº MÃ³dulos Fiscais nas Células")
# Assentamento Distancia ####
unidade = "Metros"
titulo = "Distância das Células a Assentamentos"
variavel = "dis_mn_ass"
v = dados$dis_mn_ass
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "FrequÃªcia", main = titulo, freq = T)

# Assentameto Distancia Transformada ####
unidade = "Ídice"
titulo = "Índice da Distância das Células a Assentamentos"
variavel = "i_ass_d"
v = dados$i_ass_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE,
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "FrequÃªcia", main = titulo, freq = T)

plot(v,dados$ass_dist, xlab = titulo, ylab = "DistÃ¢ncia das Células a Assentamentos")

# TI Distancia ####
unidade = "Metros"
titulo = "Distância das Células a Terra Indígena"
variavel = "dis_mn_ti"
v = dados$dis_mn_ti
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2,  legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "FrequÃªcia", main = titulo, freq = T)

# TI Transformada ####
unidade = "Ídice"
titulo = "Ídice da Distância das Células a Terra Indígena"
variavel = "i_ti_d"
v = dados$i_ti_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = T, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "FrequÃªcia", main = titulo, freq = T)

plot(v,dados$ti_dist, xlab = titulo, ylab = "DistÃ¢ncia das Células a Terra IndÃ�gena")


# UC Dist ####
unidade = "Metros"
titulo = "Distância das Células a Unidades de Conservação"
variavel = "dis_mn_uc"
v = dados$dis_mn_uc
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2,  legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# UC Dist Transformada####
unidade = "Índice"
titulo = "Distância das Células a Unidades de Conservação Transformada para Índice"
variavel = "i_uc_d"
v = dados$i_uc_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$i_uc_d, xlab = titulo, ylab = "PDistÃ¢ncia das Células a Unidades de ConservaÃ§Ã£o")

# DegradaÃ§Ã£o Dist ####
unidade = "Metros"
titulo = "Distância das Células a Indícios de Degradação"
variavel = "dis_mn_deg"
v = nozero$dis_mn_deg
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
abline(v = quantile(v, probs  = 0.75))
# DegradaÃ§Ã£o Dist Transformada####
unidade = "Índice"
titulo = "Distância das Células a Indícios de Degradação Transformada para Índice"
variavel = "i_deg_d"
v = dados$i_deg_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$deg_dist, xlab = titulo, ylab = "DistÃ¢ncia das Células a Indícios de DegradaÃ§Ã£o")

# DegradaÃ§Ã£o ####
unidade = "ProporÃ§Ã£o"
titulo = "ProporÃ§Ã£o de Indícios de DegradaÃ§Ã£o nas Celulas"
variavel = "area_deg"
v = dados$area_deg
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# DegradaÃ§Ã£o Transformada####
unidade = "Índice"
titulo = "Proporção de Indícios de Degradação nas Células Transformada para Índice"
variavel = "i_deg_p"
v = dados$i_deg_p
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_deg, xlab = titulo, ylab = "ProporÃ§Ã£o de Indícios de DegradaÃ§Ã£o nas Celulas")



# Imoveis Certificados Dist ####
unidade = "Metros"
titulo = "Distância das Células a Imóveis Certificados"
variavel = "dis_mn_crt"
v = dados$dis_mn_crt
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
abline(v = quantile(nozero$dis_mn_crt, probs= 0.75))
# Imoveis Certificados Dist Transformada####
unidade = "Índice"
titulo = "Distância das Células a Imóveis Certificados Transformada para Índice"
variavel = "i_cert_d"
v = dados$i_cert_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$tl_sgf_dis, xlab = titulo, ylab = "DistÃ¢ncia das Células a Imóveis Certificados")

# Imoveis Conflituosos Dist ####
unidade = "Metros"
titulo = "Distância das Células a Imóveis Conflituosos"
variavel = "dis_mn_cnf"
v = dados$dis_mn_cnf
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
abline(v = 20000)
# Imoveis Conflituosos Dist Transformada####
unidade = "Índice"
titulo = "Distância das Células a Imóveis  Conflituosos Transformada para Índice"
variavel = "i_conf_d"
v = dados$i_conf_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$confl_dis, xlab = titulo, ylab = "DistÃ¢ncia das Células a Imóveis Conflituosos (m)")

# Imoveis Conflituosos ####
unidade = "Proporção"
titulo = "Proporção de Imóveis Conflituosos nas Células"
variavel = "area_confl"
v = dados$area_confl
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
hist(nozero$area_confl, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# DegradaÃ§Ã£o Transformada####
unidade = "Índice"
titulo = "Proporção de Imóveis Conflituosos nas Células Transformada para Índice"
variavel = "i_conf_p"
v = dados$i_conf_p
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_confl, xlab = titulo, ylab = "ProporÃ§Ã£o de Imóveis Conflituosos nas Celulas")
hist(nozero$i_conf_p, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# nÂº de imoveis ####
unidade = "nº de imóveis"
titulo = "Número de imóveis nas Células"
variavel = "num_imov"
v = dados$num_imov
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade,  ylab = "Frequência", main = titulo, freq = T)

