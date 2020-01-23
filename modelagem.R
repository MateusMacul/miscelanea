################################################################################
############                   LABORATÓRIO                      ################
############            ANÁLISE DE REGRESSÃO NO R               ################
################################################################################

# Limpar o ambiente -------------------------------------------------------


rm(list=ls())


# Instalando os pacotes necessários ---------------------------------------

install.packages("sp") 
install.packages("corrplot") 
install.packages("PerformanceAnalytics") 
install.packages("nortest") 
install.packages("betas") 
install.packages("spdep") 
install.packages("maptools") 
install.packages("Hmisc") 
install.packages("mapview") 
install.packages("car") 
install.packages("ggplot2") 
install.packages("dplyr") 
install.packages("spdep") 
install.packages("rgdal") 
install.packages("cartography") 
install.packages("lmtest") 
install.packages("MASS") 

# carregando os pacotes ---------------------------------------------------
# forma 1
require(foreign)
require(rgdal)
require(spdep)
require(sp)
require(maptools)
require(dplyr)
# fomra 2
library("corrplot")
library("Hmisc")
library("PerformanceAnalytics")
library("nortest")
library("betas")
library("mapview")
library("cartography")
library("ggplot2")
library("car")
library("lmtest")
library("tmap")
library("MASS")

# definindo o diretorio de trabalho (working directory) -----------------------

setwd("C:/Users/Queimadas/Dropbox/Projeto_tapajos/tap_fire_data_codes/4_grids_new")

# abrindo os dados --------------------------------------------------------

dados <- readOGR("cell_10km_allvar_cropped.shp")

# analisar o formato de dados

dados

# olhando os dados --------------------------------------------------------
# existem diversas formas:
str(dados@data)
head(dados@data,10)
summary(dados)
View(dados@data)

length(dados)
# olhando no espaço
mapview(dados)

# contruindo novas variáveis ----------------------------------------------

# criando a variável y = incrmento acumulado de 2014 a 2017
for (i in 1:length(dados$id)) {
  valor <- sum(dados@data[i,2:13])
  if (valor > 1) {
    dados$bs_total_p[i] <- 1
  } else { 
    dados$bs_total_p[i] <- valor } 
}

summary(dados$bs_total_p)

mapview(dados,zcol = "bs_total_p", col.regions = colorRampPalette(colors = c("yellow","white", "red2")))
# aplicando a tranformação logit na variável y

# Não fazer logit, pois aparece dados -inf a +inf 
# dados$ylgit <- log(dados$bs_total_p/(1-dados$bs_total_p)) 

summary(dados$ylgit)

summary(dados)

# Mexendo nas outroas variáveis
for (i in 1:length(dados$id)) {
  dados$pr_total_s[i] <- sum(dados@data[i,14:27])
}
summary(dados$pr_total_s)

mapview(dados,zcol = "pr_total_s", col.regions = colorRampPalette(colors = c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858')))


for (i in 1:length(dados$id)) {
    dados$pr_total_a[i] <- sum(dados@data[i,14:27])/length(c(14:27))
}

summary(dados$pr_total_a)

mapview(dados,zcol = "pr_total_a", col.regions = colorRampPalette(colors = c("#f6eff7","#bdc9e1", "#67a9cf", "#02818a")))

cores = c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858')
tmap_mode("view")
tm_shape(shp = dados)+
  tm_polygons(c("pr_total_s", "pr_total_a"), border.alpha = 0, palette = cores, aes.palette ="seq", n = 9, style = "jenks") +
  tm_facets(sync = TRUE, ncol = 2)

for (i in 1:length(dados$id)) {
  dados$af_total_s[i] <- sum(dados@data[i,35:48])
}

summary(dados$bs_total_p)

cores = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
tm_shape(shp = dados)+
  tm_polygons(c("bs_total_p", "af_total_s"), border.alpha = 0, palette = cores, aes.palette ="seq", n = 9, style = "jenks") +
  tm_facets(sync = TRUE, ncol = 2)


# exportar os dados caso necessário

writeOGR(obj = dados, dsn ="dados"  ,layer = "dados_exemplo", driver = "ESRI Shapefile")

# selecionando os dados --------------------------------------------------

# Selecionando todos os dados que não tem y = 0 e as colunas que interessam
# para selecionar usamos a indexação 'dados[linhas, colunas]'

dados.sub <- dados[, c('id', 'bs_total_p', 'pr_total_s', 'pr_total_a',
                                      'af_total_s', 'sf_edg_l', 'pf_edg_l', 'riv_d_m',
                                      'roa_d_m', 'lu_p', 'pf_p', 'sf_p', 'nf_p', 'wt_p')]

# analisando o resultado dos dados selecionados
length(dados.sub)
str(dados.sub@data)
View(dados.sub@data)
summary(dados.sub)

mapview(dados, col.regions = "grey") + mapview(dados.nozero, col.regions = "red")

# plotando ----------------------------------------------------------------

hist(dados.nozero$y) # histograma
# histograma com frequencia relativa
h_inc_d <- hist(dados.nozero$y, plot = F)
h_inc_d$counts <- h_inc_d$counts/sum(h_inc_d$counts)
plot(h_inc_d,xlab = "y",ylab = "Frequência relativa",
     main = "Histograma do beta para y", freq = T)

boxplot(dados.nozero@data[, -1])# boxplot de todos as colunas menos a coluna 'id'
plot(dados.nozero$y, dados.nozero$br) # dispersao


# amostragem --------------------------------------------------------------
# agrupar para estratificar
agrupada <- group_by(dados.sub@data, 
                     ntile(dados.sub$bs_total_p, 10)) # estratificando por quantis (tiles)
# a variavel 'agurpada' agora é um DataFrame
group_size(agrupada) #tamanho das amostras

amostrada <- sample_frac(agrupada, size = 0.3) # amostrando 30% (0.3) de cada quantil (tiles)

# selecionando do conjunto amostral (dados.nozero), as amostras (amostrada)

dados.amostrado <- dados.sub[grep(c(paste(amostrada$id,collapse = "|")), 
                                     dados.sub@data$id, ignore.case = T), ]

# criando uma variável dependente q seja 0 ou 1
for (i in 1:length(dados.amostrado$id)){
  if (dados.amostrado$bs_total_p[i] <= 0.3) {
    dados.amostrado$y01[i] <- 0
  } else {
    dados.amostrado$y01[i] <- 1  
    } 
}
# tentando adiconando 0,0001 nos valores 0 e tirando 0,0001 nos valores 1
i=1
for (i in 1:length(dados.amostrado$id)) {
  #print(i)
  if (dados.amostrado$bs_total_p[i] >= 1) {
    #print(rownames(dados.amostrado@data[i,]))
    dados.amostrado$y01[i] <- dados.amostrado$bs_total_p[i] - 0.00001
    } else if (dados.amostrado$bs_total_p[i] <= 0) {
      dados.amostrado$y01[i] <- dados.amostrado$bs_total_p[i] + 0.00001
      } else {dados.amostrado$y01[i] <- dados.amostrado$bs_total_p[i]
}}
dados.amostrado$y01[248]<-dados.amostrado$y01[248]- 0.00001
View(dados.amostrado@data)
summary(dados.amostrado$y01) 
hist(dados.amostrado$y01) 

# olhando o resultado
mapview(dados, col.regions = "grey") + 
  mapview(dados.amostrado, col.regions = "blue")

# análise de correlação ---------------------------------------------------
# duas formas para a análise de correlação

chart.Correlation(dados.amostrado@data[, -1], histogram=TRUE, pch=19, 
                  font.labels = 4, cex.labels = 10)


m<-cor(dados.amostrado@data[,-1])
corrplot(m,method = "number", type = "upper")

# contruindo o modelo -----------------------------------------------------
'
operadores para a modelagem:

operador |           exemplo      | função
---------------------------------------------------------------------------
   +     |            +x          | inclui variável
   -     |            -X          | exclui a variável
   :     |           X1:X2        | inclui a interação entre as variáveis
   *     |X1*X2 = X1 + X2 + X1:X2 | inclui as variáveis e a interação entre elas
   ^     |      (X1 + X2 +X3)^3   | inclui as variáveis e todas as interações possíveus até 3º grau
  I()    |         I(X1*X2)       | usado para realizar operações entre variáveis da forma como é escrita (X1 vezes X2)
   1     |           -1           | inclui (+1) ou exclui (-1) o intercepto 
   .     |            .           | inclui todas as variáveis presente nos dados
---------------------------------------------------------------------------
'
# contruindo um modelo
form <- formula(bs_total_p ~ . -id -pr_total_s -wt_p)
modelobeta <- betareg(bs_total_p ~ .-bs_total_p -id -pr_total_s -wt_p, data = dados.amostrado)
modelolm <- lm(I(dados.amostrado$y01/(1-dados.amostrado$y01)) ~ . -id -pr_total_s -wt_p, data = dados.amostrado)
modelo <- glm(bs_total_p ~ . -id -pr_total_s -wt_p -af_total_s -nf_p -sf_edg_l -sf_p -lu_p, data = dados.amostrado, family = quasibinomial(link="logit"))
modelo <- glmmTMB(form, data = dados.amostrado, family=list(family="beta",link="logit"))
# visualisando o modelo

summary(modelo)


# uma fomra automática para encontrar as melhores variáveis para o modelo

modelo.step <- step(modelo, scale = 0 , direction = "both", trace = 0)
modelo.step <- step(modelolm, scale = 0 , direction = "both", trace = 0)
# visualizando o resultado
summary(modelo.step)

vif(modelo) # evitando a multcolinearidade
vif(modelo.step) # evitando a multcolinearidade
plot(modelo.step)

# plotando resultados -----------------------------------------------------

# Gráfico de dispersão do predito x Observado
ggplot()+
  geom_point(aes(y = dados.amostrado$bs_total_p, x = modelo$fitted.values))+
  geom_smooth()+
  xlab("y Predito")+
  ylab("y Observado")+
  geom_abline()+
  geom_smooth(method = "lm")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

# Gráfico de dispersão dos resíduos do modelo x y predito
ggplot()+
  geom_point(aes(y = (modelo$residuals/sd(modelo$residuals)), x = modelo$fitted.values))+
  geom_hline(yintercept = 0)+
  ylab("Resíduos padronizados")+
  xlab("y Predito")+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

# Analisando os erros
modelo.step$residuals<- modelo.step$fitted.values-dados.amostrado$bs_total_p
dados.amostrado$erros <- modelo.step$residuals
dados.amostrado$predito <- modelo.step$fitted.values
View(dados.amostrado@data)
mapview(dados.amostrado,zcol = "erros", col.regions = colorRampPalette(colors = c("red2","white", "yellow")))

# teste para os residuos --------------------------------------------------

# homocedasticidade

bptest(modelo, data=dados.amostrado, studentize=F)

# normalidade
residuos <- modelo$residuals 
shapiro.test(residuos) 

# modelo predito ----------------------------------------------------------

dados.sub$predicao <- predict.glm(modelo, dados.sub,type = "response")
tmap_mode('plot')
cores = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
tm_shape(shp = dados.sub)+
  tm_polygons(c("bs_total_p", "predicao"), border.alpha = 0, palette = cores, aes.palette ="seq", n = 9, style = "jenks") +
  tm_facets(sync = TRUE, ncol = 2)


# ROC ---------------------------------------------------------------------
index <- dados.sub$predicao
boolean <- dados.sub$bs_total_p


thresholds <- seq(min(unique(index)), max(unique(index)) + 1, 
                  by = ceiling(max(unique(index))/10))
rocd <- ROC(index, boolean)
plot(rocd)
# removendo outliers ------------------------------------------------------
dados.out <- dados.amostrado[!(dados.amostrado$id == "C32L08"), ]
#dados.out <- dados.amostrado[!(dados.amostrado$erros < -2), ]
#dados.out <- dados.out[!(dados.out$erros > 2), ]
length(dados.amostrado)
length(dados.out)

# rodando o modelo novamente sem os outliers

modelo <- lm(ylgit ~ . -y -erros -predito, data = dados.out@data[, -1])
summary(modelo)

# rodando o stepwise para o modelo

modelo.step =  step(modelo, scale = 0 , direction = "both", trace = 0)
summary(modelo.step)

vif(modelo.step) # evitando a multcolinearidade


# plotando resultados 2 

ggplot()+
  geom_point(aes(y = dados.out$ylgit, x = modelo.step$fitted.values))+
  geom_smooth()+
  xlab("y' Predito")+
  ylab("y' Observado")+
  geom_abline()+
  geom_smooth(method = "lm")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

ggplot()+
  geom_point(aes(y = (modelo.step$residuals/sd(modelo.step$residuals)), x = modelo.step$fitted.values))+
  geom_hline(yintercept = 0)+
  ylab("Resíduos padronizados")+
  xlab("y Predito")+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

# analisando os resíduos novamente sem os outliers

dados.out$erros <- modelo.step$residuals
dados.out$predito <- modelo.step$fitted.values
View(dados.out@data)
mapview(dados.out,zcol = "erros", col.regions = colorRampPalette(colors = c("red2","white", "yellow")))
mapview(dados,zcol = "y", col.regions = colorRampPalette(colors = c("red2","white", "yellow")))

# teste para os residuos 

# homocedasticidade

bptest(modelo.step, data=dados.amostrado, studentize=F)

# normalidade
residuos <- modelo.step$residuals 
shapiro.test(residuos) 



