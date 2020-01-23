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
library("mapproj")
####### pegando os dados #####
setwd("C:/Users/Padrao/Documents/fillcell")
dados <- readOGR("Células_extraida.shp")
#### pegar a paleta de cores ##
display.carto.all(n=10)
cartography::carto.pal.info()
##### plotando ######
# Parâmentros gerais --------------------------------------------------------------
metodo = "fisher-jenks"
classes = 10
pal = carto.pal("blue.pal",5,"orange.pal",5) #lembrar de mudar os numero de cores de acordo com o tamanho da classe
pal =  carto.pal("red.pal",10)
tema = "grey.pal"
f_dim <- getFigDim(x = dados, height = 600, mar= c(0,0,0,0))
# Tentativa de criar o grid ####
m <- map(dados@polygons)
map.grid(m, lim = c(585366.7,787366.7, 9108384.8, 9294384.8), nx = 5000,ny = 50000, labels= T, pretty = T)

# Amostras ---------------------
titulo = "Amostras utilizadas"
f_dim <- getFigDim(x = dados, width = 700, mar= c(0,0,0,0))
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/amostras_6d_cinza.png", width = 700, height = f_dim[2])
par(mar = c(1,1,5,1))
plot(dados, col="#DFDFDF")
plot(dados.amost6d, col = "red", add = T)
layoutLayer(title = NULL, scale = F,north = TRUE, frame = F, theme = NULL)
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/espaco_amostral.png", width = 700, height = f_dim[2])
par(mar = c(1,1,5,1))
plot(dados, col="#DFDFDF")
plot(nozero.cort, col="#729fcf", add = T)
layoutLayer(title = NULL, scale = F,north = TRUE, frame = F, theme = NULL)
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = "Espaço amostral")
dev.off()
# AHP ####
unidade = "Índice de Valor da Terra (IVT)"
titulo = "IVT"
variavel = "ahp"
v = dados$ahp
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_ahp_final.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,0,5,1))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = "IVT", legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
         style = "pretty")
title(main = unidade)
#plot(incremento, add = T)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
bks1[11]<- 0.9
bks1[11]
png("C:/Users/Padrao/OneDrive - inpe.br/graficos/histograma_ivt.png", width = f_dim[1], height = f_dim[2])
h <- hist(v, breaks = c(0,bks1), plot = F)
h$counts=h$counts/sum(h$counts)
plot(h,col = carto.pal("red.pal",11),xlab = unidade,ylab = "Frequência relativa",
     main = titulo, freq = T,  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xaxt="n")
axis(side = 1 , at = round(c(0,bks1),2), cex.axis=1.1, las = 1)
dev.off()
parametros <- par()  
parametros$font


# sensibilidade 50 --------------------------------------------------------
unidade = "DIQ"
titulo = "Sensibilidade do IVT"
variavel = "sense"
v = dados.ahp$sense
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_DIQ_ahp_final.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,0,5,1))
choroLayer(spdf = dados.ahp, var = variavel, method = metodo, nclass = classes, 
           col = carto.pal("red.pal",10), border = FALSE, legend.title.txt = unidade, 
           legend.pos = "topleft",legend.values.rnd = 4, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL, scale = F,north = T, theme = NULL, frame = F, tabtitle = F)
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/graficos/hist_DIQ.png", width = f_dim[1], height = f_dim[2])
hist(v, breaks = c(0,bks1), col = carto.pal("red.pal",11),xlab = unidade,ylim = c(0,2000),
     ylab = "Frequência", main = titulo, freq = T, xaxt="n",cex.lab=1.5,cex.axis=1.5, cex.main=1.5 )
axis(side = 1 , at = round(c(0,bks1),4), cex.axis=1.5)
dev.off()
summary(v)

# Area Aberta ####
unidade = "PorporÃ§Ã£o da celula"
titulo = "Proporção de Área Aberta"
variavel = "i_aberta"
v = dados$i_aberta
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", ylim = c(0,length(v[v<=bks1])), main = titulo, freq = T)

# Vegetação Secundária --------------
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

# Estradas vicinais Transformada####
unidade = "Índice"
titulo = "Distância das Células a Estradas Vicinais Transformada para Índice"
variavel = "i_est"
v = dados$i_est
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

estradas = readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014/estradas_22765_2014.shp")
vicinais = estradas[c(6:length(estradas)),]
plot(vicinais, add = T)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_est, xlab = titulo, ylab = "Distância das Células a Estradas Vicinais (m)")



# Area Urbana Transformada####
unidade = "Índice"
titulo = "Distância das Células a Área Urbana Transformada para Índice"
variavel = "i_au"
v = dados$i_au
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_au, xlab = titulo, ylab = "Distância das Células a Área Urbana (m)")






# Área dos Imóveis Transformada####
unidade = "Índice"
titulo = "MÃ©dia da Área dos Imóveis nas Células Transformada para Índice"
variavel = "i_frag"
v = dados$i_frag
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$areakm_mea, xlab = titulo, ylab = "Distância das Células a Área Urbana (m)")

# Imoveis Certificados####
unidade = "Proporção"
titulo = "Proporção de Imóveis Certificados nas Células"
variavel = "i_cert"
v = dados$i_cert
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)


# i_decliv ----------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de declividade nas células"
variavel = "i_decliv"
v = dados$i_decliv

choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$med_decliv, xlab = titulo, ylab = "Mediana da Declividade nas Células (%)")

png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_decliv_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,0,5,1))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_decliv_hist.png", width = f_dim[1], height = f_dim[2])
hist(v, breaks = bks1, col = carto.pal("red.pal",10),xlab = unidade,ylab = "Frequência", 
     main = titulo, freq = T,  cex.lab=1.5, cex.axis=1.5, cex.main=1.5,  xaxt="n")
axis(side = 1 , at = round(c(0,bks1),2), cex.axis=1.1, las = 1)
dev.off()


####

# Rios Transformada####
unidade = "Índice"
titulo = "Distância das Células a Rios Transformada para Índice"
variavel = "i_rios5"
v = dados$i_rios5
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$dis_mn_r5, xlab = titulo, ylab = "Mediana da Distância das Células a Rios (m)")

# Assentamentos Transformada####

# Embargo Transformada####
unidade = "Índice"
titulo = "Porcentagem de Embargo nas Células Transformada para Índice"
variavel = "i_emb"
v = dados$i_emb
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_embar, xlab = titulo, ylab = "Porcentagem de Embargo nas Células")


# Floresta Transformada####
unidade = "Índice"
titulo = "Proporção de Floresta nas Células Transformada para Índice"
variavel = "i_flor"
v = dados$i_flor
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_flore, xlab = titulo, ylab = "Proporção de Floresta nas Células")

# UC Transformada####
unidade = "Índice"
titulo = "Proporção de Unidades de Conservação nas Células Transformada para Índice"
variavel = "i_uc"
v = dados$i_uc
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_uc, xlab = titulo, ylab = "Proporção de Unidades de Conservação nas Células")


# TI Transformada####
unidade = "Índice"
titulo = "Proporção de Terra Indígena nas Células Transformada para Índice"
variavel = "i_ti"
v = dados$i_ti
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = TRUE)

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
plot(v,dados$area_ti, xlab = titulo, ylab = "Proporção de Terra Indígena nas Células")



# i_ass_p -----------------------------------------------------------------


unidade = "Índice"
titulo = "Índice da Propoção de Assentamentos"
variavel = "i_ass_p"
v = dados$i_ass_p
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_ass_p_mapa.png", width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE,
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()
# i_ass_d -----------------------------------------------------------------

unidade = "Índice"
titulo = "Índice da Distância de Assentamentos"
variavel = "i_ass_d"
v = dados$i_ass_d
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_ass_d_mapa.png", width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE,
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()

# i_ti_d ------------------------------------------------------------

unidade = "Índice"
titulo = "Índice de distância das Células a Terra Indígena"
variavel = "i_ti_d"
v = dados$i_ti_d
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_ti_d_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col =  carto.pal("red.pal",10), border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2,  
           legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL, scale = F,north = TRUE, theme = tema, frame = F, tabtitle = F)
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()

bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_ti_d_hist.png", width = f_dim[1], height = f_dim[2])
hist(v, breaks = bks1, col =  carto.pal("red.pal",10), xlab = unidade, ylab = "Frequêcia",
     main = titulo, freq = T, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,  xaxt="n")
axis(side = 1 , at = round(c(0,bks1),2), cex.axis=1.1, las = 2)
dev.off()


# i_ti_p ------------------------------------------------------------

unidade = "Índice"
titulo = "Índice de proporção de Terra Indígena"
variavel = "i_ti"
v = dados$i_ti
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_ti_mapa.png", width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col =  carto.pal("red.pal",10), border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2,  
           legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL, scale = F,north = TRUE, theme = tema, frame = F, tabtitle = F)
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()
# i_uc_d ------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de distância de Unidades de Conservação"
variavel = "i_uc_d"
v = dados$i_uc_d
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_uc_d_mapa.png", width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col =  carto.pal("red.pal",10), border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2,  
           legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL, scale = F,north = TRUE, theme = tema, frame = F, tabtitle = F)
barscale(lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()

# i_uc ------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de Proporção de Unidades de Conservação"
variavel = "i_uc"
v = dados$i_uc
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_uc_mapa.png", width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col =  carto.pal("red.pal",10), border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2,  
           legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL, scale = F,north = TRUE, theme = tema, frame = F, tabtitle = F)
barscale(lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
dev.off()

# Dist BR 163 Transformada ####

unidade = "Índice"
titulo = "Índice de distância da BR-163"
variavel = "i_br"
v = dados$i_br
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_br_mapa.png", width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col =  carto.pal("red.pal",10), border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft", legend.values.rnd = 2,  
           legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = NULL, scale = F,north = TRUE, theme = tema, frame = F, tabtitle = F)
barscale(lwd = 1.5, cex = 1.3, pos = "bottomright",
         style = "pretty")
title(main = titulo)
dev.off()

# Degradação Dist Transformada ####
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
plot(v,dados$deg_dist, xlab = titulo, ylab = "Distância das Células a Indícios de Degradação")


# Degradação Transformada####
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
plot(v,dados$area_deg, xlab = titulo, ylab = "Proporção de Indícios de Degradação nas Células")




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
plot(v,dados$tl_sgf_dis, xlab = titulo, ylab = "Distância das Células a Imóveis Certificados")


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
plot(v,dados$confl_dis, xlab = titulo, ylab = "Distância das Células a Imóveis Conflituosos (m)")


# Imoveis Conflituosos Transformada ----------
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
plot(v,dados$area_confl, xlab = titulo, ylab = "Proporção de Imóveis Conflituosos nas Células")
hist(nozero$i_conf_p, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)

# Distancia de área desmatameta até 2013 Transformada-----
unidade = "Índice"
titulo = "Distância das Células a Área Desmatada até 2013 Transformada para Índice"
variavel = "i_d13_d"
v = dados$i_d13_d
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapa_",paste0(variavel),".jpeg"),width = f_dim[1], height = f_dim[2])
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, 
           legend.title.txt = unidade, legend.pos = "topleft",legend.values.rnd = 2, legend.title.cex = 1, legend.values.cex = 1)
layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
hist(v, breaks = bks1, col = pal, xlab = unidade, ylab = "Frequência", main = titulo, freq = T)
abline(v = 10000)

# nº de imoveis ####
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



# TAOBIA ------------------------------------------------------------------
unidade = "ha"
titulo = "TAOBIA"
variavel = "TAOBIA_0"
v = dados$TAOBIA_0
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/TAOBIA_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,0,5,1))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 0, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/TAOBIA_hist.png", width = f_dim[1], height = f_dim[2])
hist(v, breaks = bks1, col = carto.pal("red.pal",10),xlab = unidade, ylim = c(0,2000),ylab = "Frequência", 
     main = titulo, freq = T,  cex.lab=1.5, cex.axis=1.5, cex.main=1.5,  xaxt="n")
axis(side = 1 , at = round(c(0,bks1),0), cex.axis=1.1, las = 2)
dev.off()
# BIA ------------------------------------------------------------------
unidade = "ha"
titulo = "BIA"
variavel = "BIA_0"
v = dados$BIA_0
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/BIA_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,0,5,1))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 0, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/BIA_hist.png", width = f_dim[1], height = f_dim[2])
hist(v, breaks = bks1, col = carto.pal("red.pal",10),xlab = unidade,ylab = "Frequência", 
     main = titulo, freq = T,  cex.lab=1.5, cex.axis=1.5, cex.main=1.5,  xaxt="n")
axis(side = 1 , at = round(c(0,bks1),0), cex.axis=1.1, las = 1)
dev.off()

# MPAR ------------------------------------------------------------------
unidade = "m-¹"
titulo = "MPAR"
variavel = "MPAR_0"
v = dados$MPAR_0
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/MPAR_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(1,0,5,1))
mapa_ahp <- choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
                      border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
                      legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()
bks1 <- getBreaks(v = v, nclass = classes, method = metodo)
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/MPAR_hist.png", width = f_dim[1], height = f_dim[2])
hist(v, breaks = bks1, col = carto.pal("red.pal",10),xlab = unidade,ylab = "Frequência", 
     main = titulo, freq = T,  cex.lab=1.5, cex.axis=1.5, cex.main=1.5,  xaxt="n")
axis(side = 1 , at = round(c(0,bks1),2), cex.axis=1.1, las = 1)
dev.off()


# AWMPFD_0 ------------------------------------------------------------------
unidade = ""
titulo = "AWMPFD"
variavel = "AWMPFD_0"
v = dados$AWMPFD_0
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/AWMPFD_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()

# SIDI ------------------------------------------------------------------
unidade = ""
titulo = "SIDI"
variavel = "SIDI"
v = dados$SIDI
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/SIDI_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()

# i_emb ------------------------------------------------------------------
unidade = "Índice"
titulo = "i_emb"
variavel = "i_emb"
v = dados$i_emb
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_emb_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()

# i_conf_d ------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de distância de Imóveis Rurais inconsitentes"
variavel = "i_conf_d"
v = dados$i_conf_d
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_conf_d_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()

# i_d13_d ------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de distância de área desmatada até 2013"
variavel = "i_d13_d"
v = dados$i_d13_d
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_d13_d_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()

# i_cert_d------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de distância de imóveis rurais certificados"
variavel = "i_cert_d"
v = dados$i_cert_d
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_cert_d_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()


# i_rios5------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de distância de rios"
variavel = "i_rios5"
v = dados$i_rios5
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_rios5_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()


# i_vegse------------------------------------------------------------------
unidade = "Índice"
titulo = "Índice de distância de Vegetação Secundária"
variavel = "i_vegse"
v = dados$i_vegse
png("C:/Users/Padrao/OneDrive - inpe.br/Mapas_dissertacao/mapas_graficos_vars/i_vegse_mapa.png", width = f_dim[1], height = f_dim[2])
par(mar = c(0,0,2,0))
choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes,col = carto.pal("red.pal",10),
           border = FALSE, legend.title.txt = unidade, legend.pos = "topleft",
           legend.values.rnd = 2, legend.title.cex = 1.3, legend.values.cex = 1.3)
layoutLayer(title = NULL,scale = F, north = TRUE, theme = NULL, frame =F, tabtitle = F, coltitle = "white")
barscale( lwd = 1.5, cex = 1.3, pos = "bottomright",
          style = "pretty")
title(main = titulo)
#plot(incremento, add = T)
dev.off()

