################################################################################
############               ANALISE DOS DADOS                   #################
############                    Script 2                       #################
################################################################################
install.packages("sp")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("PerformanceAnalytics")
install.packages("nortest")
install.packages("betas")
install.packages("spdep")
install.packages("maptools")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("mapview")
install.packages("car")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("spdep")

####################### DEFINIR PASTA DE TRABALHO ##############################
setwd("C:/Users/Padrao/Documents/fillcell")

################################# DEPENDENCIAS ################################

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


# Importanado os dados ----------------------------------------------------

# dados <- readOGR("celulas_extraida.shp")
# dados <- readOGR("celulas_trabalhando.shp")
# dados <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/celulas/celulas_trabalhando_landmetric.shp")
# dados <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/celulas/celulas_trabalhando_landmetric2.shp")
dados <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/celulas/celulas_arrumadas.shp")
dados2 <- dados
# rownames(dados)<-dados[,1]
View(as.data.frame(dados@data))
summary(dados)

# Deletar colunas q não serão usadas --------------------------------------

drop = c("y1")
# drop = c(names(dados@data[,c(61:length(names(dados)))]))
dados <-dados[,!(names(dados) %in% drop)]
str(dados@data)

##### renomeando colunas
names(dados)[names(dados)=="incre_14a1"] <- "incre_1417"

# criando o atributos que faltam  -----------------------------------------

dados$incre_1417 <- dados$incre_14 + dados$incre_15 + dados$incre_16 + dados$incre_17 #variavel de incremnto acumulado
dados$flor_13_p <- dados$incre_1417+dados$flor_17_p # variavel de floresta de 2013
for (i in 1: length(dados$flor_13_p)){
  if(dados$flor_13_p[i] > 1){
    dados$flor_13_p[i] <- 1}
  else { dados$flor_13_p[i] <- dados$flor_13_p[i]}
}

# transformando as variáveis em índices de 0 a 1  -------------------------
# Variáveis AHP -----------------------------------------------------------

dados$i_aberta = dados$area_abert # minha variável já está entre 0 e 1 por definição
dados$i_vegse = 1 - dados$area_vegse # invertendo entre 0 e 1 (minha variável já está entre 0 e 1 por definição)
for (i in 1:length(dados$dis_mn_br)){ # 
  if (dados$dis_mn_br[i] <= 50000){
    dados$i_br[i] = 1 - (dados$dis_mn_br[i]/50000) # invertido
  }
  else {
    dados$i_br[i] = 0 # vai para zero pq ta invertido. se não, seria 1
  }
}
for (i in 1:length(dados$dis_mn_est)){
  if (dados$dis_mn_est[i] <= 6000){
    dados$i_est[i] = 1 - (dados$dis_mn_est[i]/6000)
  }
  else {
    dados$i_est[i] = 0
  }
}
for (i in 1:length(dados$dis_mn_au)){
  if (dados$dis_mn_au[i] <= 60000){
    dados$i_au[i] = 1 - (dados$dis_mn_au[i]/60000)
  }
  else {
    dados$i_au[i] = 0
  }
}
for (i in 1:length(dados$areakm_mea)){
  if (dados$areakm_mea[i] <= 25){
    dados$i_tamimov[i] = 1 - (dados$areakm_mea[i]/25)
  }
  else {
    dados$i_tamimov[i] = 0
  }
}
dados$i_cert = dados$area_tlsgf
for (i in 1:length(dados$med_decliv)){
  if (dados$med_decliv[i] <= 13){
    dados$i_decliv[i] = 1 - (dados$med_decliv[i]/13)
  }
  else {
    dados$i_decliv[i] = 0
  }
}
for (i in 1:length(dados$dis_mn_r5)){
  if (dados$dis_mn_r5[i] <= 10000){
    dados$i_rios5[i] =  1 - (dados$dis_mn_r5[i]/10000)
  }
  else {
    dados$i_rios5[i] = 0
  }
}
dados$i_ass_p = 1 - dados$area_ass
for (i in 1:length(dados$area_embar)){
  if (dados$area_embar[i] <= 2){
    dados$i_emb[i] = 1 - (dados$area_embar[i]/2)
  }
  else {
    dados$i_emb[i] = 0
  }
}
dados$i_flor13 = 1 - dados$flor_13_p
dados$i_uc = 1 - dados$area_uc
dados$i_ti = 1 - dados$area_ti
# Não vai mais usar : dados$modulos_imv = dados$areakm_mea/0.75 #for (i in 1:length(dados$modulos_imv)){#####
 # if (dados$modulos_imv[i] <= 15){
 #   dados$class_imv[i] = dados$modulos_imv[i]/15}
#  else {
 #   dados$class_imv[i] = 1}
#}



# outras variáveis --------------------------------------------------------

for (i in 1:length(dados$num_imov)){
  if (dados$num_imov[i] <= 14){
    dados$i_n_imov[i] = dados$num_imov[i]/14
  }
  else {
    dados$i_n_imov[i] = 1
  }
}
for(i in 1:length(dados$dis_mn_ass)){
  if (dados$dis_mn_ass[i] <= 20000){
    dados$i_ass_d[i] = dados$dis_mn_ass[i]/20000}
  else {
    dados$i_ass_d[i] = 1}
  }
for(i in 1:length(dados$dis_mn_ti)){
  if (dados$dis_mn_ti[i] <= 40000){
    dados$i_ti_d[i] = dados$dis_mn_ti[i]/40000}
  else {
    dados$i_ti_d[i] = 1}
}
for (i in 1:length(dados$dis_mn_uc)){
  if (dados$dis_mn_uc[i] <= 80000){
    dados$i_uc_d[i] = dados$dis_mn_uc[i]/80000}
  else {
    dados$i_uc_d[i] = 1}
}
for (i in 1:length(dados$dis_mn_deg)){
  if (dados$dis_mn_deg[i] <= 10000){
    dados$i_deg_d[i] = dados$dis_mn_deg[i]/10000}
  else {
    dados$i_deg_d[i] = 1}
}
for (i in 1:length(dados$area_deg)){
  if (dados$area_deg[i] <= 0.2){
    dados$i_deg_p[i] = dados$area_deg[i]/0.2}
  else {
    dados$i_deg_p[i] = 1}
}
for (i in 1:length(dados$dis_mn_crt)){
  if (dados$dis_mn_crt[i] <= 25000 ){
    dados$i_cert_d[i] = dados$dis_mn_crt[i]/25000}
  else {
    dados$i_cert_d[i] = 1}
}
for (i in 1:length(dados$dis_mn_cnf)){
  if (dados$dis_mn_cnf[i] <= 20000){
    dados$i_conf_d[i] = dados$dis_mn_cnf[i]/20000}
  else {
    dados$i_conf_d[i] = 1}
}
for (i in 1:length(dados$area_confl)){
  if (dados$area_confl[i] <= 1 ){
    dados$i_conf_p[i] = dados$area_confl[i]}
  else {
    dados$i_conf_p[i] = 1}
}
for (i in 1:length(dados$dis_mn_d13)){
  if (dados$dis_mn_d13[i] <= 1000 ){
    dados$i_d13_d[i] = dados$dis_mn_d13[i]/1000}
  else {
    dados$i_d13_d[i] = 1}
}

# outra Variável dependente y1 -------------------------------------------------

for (i in 1:length(dados$incre_1417)){
  if (dados$flor_13_p[i] == 0){
    dados$y1[i] = 0}
  else{
    dados$y1[i] = dados$incre_1417[i]/dados$flor_13_p[i]}
}

#### criando a proporção de classes de uso multiplicada pelos pesos da ahp ###
p1 = 0.2019 #Area Aberta
p2 = 0.1464 #Vegetação Secundária
p3 = 0.1464 #BR 163
p4 = 0.0984 #Proximidade a área urbana
p5 = 0.0859 #Estradas vicinais
p6 = 0.0745 #Tamanho dos imóveis
p7 = 0.0662 #Imóveis certificados
p8 = 0.0454 #Declividade
p9 = 0.0436 #Rios
p10 = 0.0313 #Assentamento
p11 = 0.0250 #Embargo
p12 = 0.0151 #Floresta
p13 = 0.0110 #Unidade de Conservação
p14 = 0.0089 #Terra Indígena

dados$ahp = (dados$i_aberta * p1)+(dados$i_vegse * p2)+
  (dados$i_br * p3)+(dados$i_au * p4)+(dados$i_est * p5)+
  (dados$i_tamimov * p6)+(dados$i_cert * p7)+(dados$i_decliv * p8)+
  (dados$i_rios5 * p9)+(dados$i_ass_p* p10) +(dados$i_emb* p11) +
  (dados$i_flor13* p12)+(dados$i_uc* p13)+(dados$i_ti* p14)

# obeservando os dados ------------------------------------------------------

str(dados@data)
dados.df = as.data.frame(dados)
View(dados.df)

# Separando só os dados com desmatamento ----------------------------------
prodes.ha = 6.25
prodes.km2 = prodes.ha/100
cell.pencent = prodes.km2/4

nozero <- dados[dados$incre_1417 != 0,]
length(nozero)
nozero.cort <- nozero[which(nozero$incre_1417>0.02 & nozero$flor_13_p<1 & nozero$PLAND_0 != 0),]
length(nozero.cort)
mapview(incremento, col.regions = "black") + mapview(nozero, col.regions = "grey")+ mapview(nozero.cort, col.regions = "red")

# nozerob <- dados[dados$incre_1417 != 0,] #criado com o celulas_trabalhando_landmetric.shp
str(nozero@data) # verificando o conteúdo
summary(nozero) # verificando o resumo de cada variável
View(nozero.df) # olhando a tabela dos dados

###### retirando amostras dos dados com desmatamento --------------------------------------
nozero$ID_NUM <- c(1:nrow(nozero)) # criando uma coluna com número para identificar os dados 
nozero.df = as.data.frame(nozero) # transformando para data frame
agrupada<- group_by(as.data.frame(nozero.cort), ntile(nozero.cort$incre_1417, 10)) #agrupando meus dados (tiles) para estratificar a amostragem
intervalos <- getBreaks(nozero.cort$incre_1417, method = "fisher-jenks", nclass = 200)
agrupada<- group_by(as.data.frame(nozero.cort), cut(nozero.cort$incre_1417,breaks = intervalos )) #agrupando meus dados (tiles) para estratificar a amostragem
agrupadak<- group_by(as.data.frame(nozero.cort), kmeans(nozero.cort@data[,c(3:ncol(nozero.cort@data))],centers = 200,iter.max = 100 )$cluster) #agrupando meus dados (tiles) para estratificar a amostragem
group_size(agrupadak)
set.seed(10) # isso mantem a seleção das mesmas amostrar sempre que eu rodar esse código numa mesma sessão
amostradakp <- sample_frac(agrupada, size = 0.1) # pegando 30% (0.3) de cada grupo (tiles) aleatoriamente
amostradakn <- sample_n(agrupadak, size =1) # pegando 30% (0.3) de cada grupo (tiles) aleatoriamente
group_size(amostradakn) # vendo o tamanho de cada grupo amostrado
group_size(amostradakp) # vendo o tamanho de cada grupo amostrado
boxplot(amost6$incre_1417,amostradakn$incre_1417,amostradakp$incre_1417,amost6d$incre_1417)
nrow(amostrada)
dados.amost.alter2 = nozero[c(amostrada$ID_NUM),] # pegando a amostra pela primeira vez
hist(dados.amost.alter$incre_1417)
# se amostra ficar boa, sempre que rodar de novo e quiser pegar as mesmas amostras fazer o q estar aseguir e não o código a cima
dados.amost4 = nozero[c(nozero$id == dados.amost$id),] # pegando dos meus dados em SpatialPolygonDataFrame só os dados amostrados identificados pelos ID_NUM no dado amostrado. Neste caso como eu já tenho essa amostra retirada anteriormente eu uso ela como referencia para sempre pegar as mesmas amostras.
dados.amost5 = nozero[grep(c(paste(dados.amost@data$id,collapse = "|")),nozero@data$id, ignore.case = T),]
dados.amost6 = nozero.cort[grep(c(paste(amostrada$id,collapse = "|")),nozero.cort@data$id, ignore.case = T),]
dados.amost6b = nozero.cort[grep(c(paste(amostrada$id,collapse = "|")),nozero.cort@data$id, ignore.case = T),]
dados.amost6c = nozero.cort[grep(c(paste(amostrada$id,collapse = "|")),nozero.cort@data$id, ignore.case = T),]
dados.amost6d = nozero.cort[grep(c(paste(amostrada$id,collapse = "|")),nozero.cort@data$id, ignore.case = T),]
teste_rio = dados[grep(c(paste(dados.amost6d$id,collapse = "|")),dados@data$id, ignore.case = T),]
boxplot(nozero.cort$incre_1417, dados.amost6c[which(dados.amost6c$PLAND_0!=0),]$incre_1417, dados.amost6d$incre_1417)
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/box_nozerocort_amostras6d.jpg"))
boxplot(nozero.cort$incre_1417, dados.amost6d$incre_1417, col = c("#729fcf", "red"), 
        ylab = "Proporção",
        cex.lab=1.5,cex.axis=1.5)
axis(side =1 , at =c(1,2), labels = c("Espaço amostral", "Amostrado") , cex.axis=1.5)
dev.off()
length(dados.amost6d)
hist(dados.amost6c$incre_1417)
#dados.amost = nozero[c(amostrada$ID_NUM),] Esse jeito não mantém a mesma amostra sempre, mesmo colocando o set.seed(10), pois quando se reiniciar a sessão, ele perde o set.seed

mapview(incremento, col.regions = "black") + mapview(nozero, col.regions = "grey")+ mapview(nozero.cort, col.regions = "red") + mapview(dados.amost6c, col.regions = "yellow") + mapview(dados.amost6c[which(dados.amost6c$PLAND_0!=0),], col.regions = "yellow")+ mapview(dados.amost6d, col.regions = "blue")
mapview(dados.amost6d, col.regions = "blue") +  mapview(teste_rio, col.regions = "red")
# olhando as amostras para ver se estão bem distribuidas no espaço. -------

mapview(dados, col.regions = "grey")+ mapview(dados.amost, col.regions = "black")+ mapview(dados.amost2) + mapview(dados.amost3, col.regions = "red")+ mapview(dados.amost4, col.regions = "yellow")+ mapview(dados.amost5, col.regions = "brown")+mapview(dados.amost.alter, col.regions = "orange")
mapview(incremento, col.regions = "black", layer.name =" Incremento de desmatamento acumulado 2014 a 2017") + mapview(nozero.cort, col.regions = "grey", layer.name = "Espaço amostral") + mapview(dados.amost6c, col.regions = "yellow", layer.name = "Amostras")

str(dados.amost5@data)




writeOGR(obj = dados, dsn ="C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014"  ,layer = "dados", driver = "ESRI Shapefile")
