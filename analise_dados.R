################################################################################
############               ANALISE DOS DADOS                   #################
############                    Script 1                       #################
################################################################################

install.packages("corrplot")
install.packages("Hmisc")
install.packages("PerformanceAnalytics")
install.packages("nortest")
install.packages("betas")
install.packages("spdep")
install.packages("maptools")
install.packages("Hmisc")
install.packages("dplyr")

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

######################### IMPORTAR TABELA DO  ESPAÇO CELULAR ###########################

#dados <- readOGR("celulas_extraida.shp")
dados <- readOGR("celulas_trabalhando.shp")
dados <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/celulas/celulas_trabalhando_landmetric.shp")
dados <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/celulas/celulas_trabalhando_landmetric2.shp")
rownames(dados)<-dados[,1]
View(as.data.frame(dados@data))
summary(dados)
##### Deletar colunas q não serão usadas 
drop = "dif_incre"
drop = c(names(dados@data[,c(61:length(names(dados)))]))
dados <-dados[,!(names(dados) %in% drop)]
str(dados@data)

##### renomeando colunas
names(dados)[names(dados)=="incre_14a1"] <- "incre_1417"

######## criando o atributos que faltam ####

dados$incre_1417 <- dados$incre_14 + dados$incre_15 + dados$incre_16 + dados$incre_17 #variavel de incremnto acumulado
dados$flor_13_p <- dados$incre_1417+dados$flor_17_p # variavel de floresta de 2013

### transformando as variáveis em índices de 0 a 1 ##
# Vars AHP
dados$i_aberta = (dados$area_abert-min(dados$area_abert))/diff(range(dados$area_abert))
dados$i_vegse = (dados$area_vegse-max(dados$area_vegse))/-(diff(range(dados$area_vegse)))
for (i in 1:length(dados$dis_mn_br)){
  if (dados$dis_mn_br[i] <= 50000){
    dados$i_br[i] = (dados$dis_mn_br[i] - 50000)/-(50000-min(dados$dis_mn_br))
  }
  else {
    dados$i_br[i] = 0
  }
}
for (i in 1:length(dados$dis_mn_est)){
  if (dados$dis_mn_est[i] <= 4000){
    dados$i_est[i] = (dados$dis_mn_est[i] - 4000)/-(4000-min(dados$dis_mn_est))
  }
  else {
    dados$i_est[i] = 0
  }
}
for (i in 1:length(dados$dis_mn_au)){
  if (dados$dis_mn_au[i] <= 60000){
    dados$i_au[i] = (dados$dis_mn_au[i] - 60000)/-(60000-min(dados$dis_mn_au))
  }
  else {
    dados$i_au[i] = 0
  }
}
for (i in 1:length(dados$areakm_mea)){
  if (dados$areakm_mea[i] <= 25 & dados$areakm_mea[i] > 0){
    dados$i_tamimov[i] = (dados$areakm_mea[i] - 25)/-(25-min(dados$dis_mn_au))
  }
  else {
    dados$i_tamimov[i] = 0
  }
}
dados$i_cert = (dados$area_tlsgf-min(dados$area_tlsgf))/diff(range(dados$area_tlsgf))
for (i in 1:length(dados$dis_mn_r5)){
  if (dados$med_decliv[i] <= 13){
    dados$i_decliv[i] = (dados$med_decliv[i] - 13)/-(13-min(dados$med_decliv))
  }
  else {
    dados$i_decliv[i] = 0
  }
}
dados$i_rios5 = (dados$dis_mn_r5-max(dados$dis_mn_r5))/-(diff(range(dados$dis_mn_r5)))
dados$i_ass_p = (dados$area_ass-max(dados$area_ass))/-(diff(range(dados$area_ass)))
for (i in 1:length(dados$area_embar)){
  if (dados$area_embar[i] <= 2){
    dados$i_emb[i] = (dados$area_embar[i] - 2)/-(2-min(dados$area_embar))
  }
  else {
    dados$i_emb[i] = 0
  }
}
dados$i_flor13 = (dados$flor_13_p-max(dados$flor_13_p))/-(diff(range(dados$flor_13_p)))
dados$i_uc = (dados$area_uc-max(dados$area_uc))/-(diff(range(dados$area_uc)))
dados$i_ti = (dados$area_ti-max(dados$area_ti))/-(diff(range(dados$area_ti)))
# Não vai mais usar : dados$modulos_imv = dados$areakm_mea/0.75 #for (i in 1:length(dados$modulos_imv)){#####
 # if (dados$modulos_imv[i] <= 15){
 #   dados$class_imv[i] = dados$modulos_imv[i]/15}
#  else {
 #   dados$class_imv[i] = 1}
#}

##outras variáveis
for(i in 1:length(dados$dis_mn_ass)){
  if (dados$dis_mn_ass[i] <= 20000){
    dados$i_ass_d[i] = (dados$dis_mn_ass[i]-min(dados$dis_mn_ass))/100796-min(dados$dis_mn_ass)}
  else {
    dados$i_ass_d[i] = 1}
  }
for(i in 1:length(dados$dis_mn_ti)){
  if (dados$dis_mn_ti[i] <= 40000){
    dados$i_ti_d[i] = (dados$dis_mn_ti[i]-min(dados$dis_mn_ti))/40000-min(dados$dis_mn_ti)}
  else {
    dados$i_ti_d[i] = 1}
}
for (i in 1:length(dados$dis_mn_uc)){
  if (dados$dis_mn_uc[i] <= 40000){
    dados$i_uc_d[i] = (dados$dis_mn_uc[i]-min(dados$dis_mn_uc))/40000-min(dados$dis_mn_uc)}
  else {
    dados$i_uc_d[i] = 1}
}
for (i in 1:length(dados$dis_mn_deg)){
  if (dados$dis_mn_deg[i] <= 10000){
    dados$i_deg_d[i] = (dados$dis_mn_deg[i]-min(dados$dis_mn_deg))/(10000-min(dados$dis_mn_deg))}
  else {
    dados$i_deg_d[i] = 1}
}
for (i in 1:length(dados$area_deg)){
  if (dados$area_deg[i] <= 0.185681){
    dados$i_deg_p[i] = (dados$area_deg[i]-min(dados$area_deg))/(0.185681-min(dados$area_deg))}
  else {
    dados$i_deg_p[i] = 1}
}
for (i in 1:length(dados$dis_mn_crt)){
  if (dados$dis_mn_crt[i] <= 16000 ){
    dados$i_cert_d[i] = (dados$dis_mn_crt[i]-min(dados$dis_mn_crt))/(16000-min(dados$dis_mn_crt))}
  else {
    dados$i_cert_d[i] = 1}
}
for (i in 1:length(dados$dis_mn_cnf)){
  if (dados$dis_mn_cnf[i] <= 20000){
    dados$i_conf_d[i] = (dados$dis_mn_cnf[i]-min(dados$dis_mn_cnf))/(20000-min(dados$dis_mn_cnf))}
  else {
    dados$i_conf_d[i] = 1}
}
for (i in 1:length(dados$area_confl)){
  if (dados$area_confl[i] <= 1 ){
    dados$i_conf_p[i] = (dados$area_confl[i]-min(dados$area_confl))/(1-min(dados$area_confl))}
  else {
    dados$i_conf_p[i] = 1}
}
for (i in 1:length(dados$dis_mn_d13)){
  if (dados$dis_mn_d13[i] <= 1000 ){
    dados$i_d13_d[i] = (dados$dis_mn_d13[i]-min(dados$dis_mn_d13))/(1000-min(dados$dis_mn_d13))}
  else {
    dados$i_d13_d[i] = 1}
}

#Var dependente
for (i in 1:length(dados$incre_1417)){
  if (dados$flor_13_p[i] == 0){
    dados$y1[i] = 0}
  else{
    dados$y1[i] = dados$incre_1417[i]/(dados$flor_17_p[i] + dados$incre_14[i] + dados$incre_15[i]+dados$incre_16[i]+dados$incre_17[i])}
}
summary(dados$y1)
str(dados@data)

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
p11 = 0.0250 #Embargo2
p12 = 0.0151 #Floresta
p13 = 0.0110 #Unidade de Conservação
p14 = 0.0089 #Terra Indígena

dados$ahp = (dados$i_aberta * p1)+(dados$i_vegse * p2)+
  (dados$i_br * p3)+(dados$i_au * p4)+(dados$i_est * p5)+
  (dados$i_tamimov * p6)+(dados$i_cert * p7)+(dados$i_decliv * p8)+
  (dados$i_rios5 * p9)+(dados$i_ass_p* p10) +(dados$i_emb* p11) +
  (dados$i_flor13* p12)+(dados$i_uc* p13)+(dados$i_ti* p14)
### transformando em dataframe para poder obeservar os valores

dados.df = as.data.frame(dados)
View(dados.df)

### exportando a tabela para a vizualização dos dados ###

write.dbf(dados,file="celulas_visual")

###### Separando só os dados com desmatamento -----------------------------------------
nozero <- dados[dados$incre_1417 != 0,]
# nozerob <- dados[dados$incre_1417 != 0,] #criado com o celulas_trabalhando_landmetric.shp
summary(nozero)
View(nozero.df)
###### retirando amostras dos dados com desmatamento --------------------------------------
nozero$ID_NUM <- c(1:nrow(nozero)) #criando uma coluna com número para identificar os dados 
nozero.df = as.data.frame(nozero) #transformando para data frame
agrupada<- group_by(nozero.df, ntile(nozero.df$y1, 6)) #agrupando meus dados (tiles) para estratificar a amostragem
set.seed(10) # isso mantem a seleção das mesmas amostrar sempre que eu rodar esse código numa mesma sessão
amostrada <- sample_frac(agrupada, size = 0.3) #pegando 30% (0.3) de cada grupo (tiles) aleatoriamente
group_size(amostrada) #vendo o tamanho de cada grupo amostrado
dados.amost4 = nozero[c(nozero$id == dados.amost$id),] # pegando dos meus dados em SpatialPolygonDataFrame só os dados amostrados identificados pelos ID_NUM no dado amostrado. Neste caso como eu já tenho essa amostra retirada anteriormente eu uso ela como referencia para sempre pegar as mesmas amostras.
dados.amost4 = nozero[grep(c(paste(dados.amost@data$id,collapse = "|")),nozero@data$id, ignore.case = T),]

# dados.amost5 = nozero[c(amostrada$ID_NUM),] Esse jeito não mantém a mesma amostra sempre, mesmo colocando o set.seed(10), pois quando se reiniciar a sessão, ele perde o set.seed

#olhando as amostras para ver se estão bem distribuidas no espaço.
mapview(dados, col.regions = "grey")+ mapview(dados.amost, col.regions = "black")+ mapview(dados.amost2) + mapview(dados.amost3, col.regions = "red")+ mapview(dados.amost4, col.regions = "yellow")+ mapview(dados.amost5, col.regions = "brown")






