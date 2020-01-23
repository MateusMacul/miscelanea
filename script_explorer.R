################################################################################
############           ANALISE EXPLORATORIA DOS DADOS           ################
################################################################################


####################### DEFINIR PASTA DE TRABALHO ##############################

setwd("C:/Users/Padrao/Documents/fillcell")



################################## DEPENDENCIAS ################################

install.packages("corrplot")
install.packages("Hmisc")
install.packages("PerformanceAnalytics")
install.packages("nortest")
install.packages("betas")
install.packages("spdep")
install.packages("maptools")
install.packages("Hmisc")
require(foreign)
require(rgdal)
require(spdep)
require(maptools)

library("corrplot")
library("Hmisc")
library("PerformanceAnalytics")
library("nortest")
library("betas")



############################ IMPORTAR ESPACO CELULAR ###########################

dados <- read.dbf("cell_2km_prodes_4326.dbf")
View(dados)
summary(dados)


##################### ORGANIZAR ESPACO CELULAR PARA ANALISES ####################

cs <- dados[,-c(1:3,15)]
rownames(cs)<-dados[,1]
View(cs)
cs
#nome e número das colunas#

num_colunas.cs <- index(names(cs))
names(num_colunas.cs) <- names(cs)
num_colunas.cs
#selecionando os atributos para analise

#subCells.cs = subset(cs, O = 0, select = c(4, 1,2,9,10,11,8,12,13,6,7,5,19))
#View(subCells.cs)
######################### ANALISE DESCRITIVA DOS DADOS ##########################

summary(cs$desm14_are)

sd (cs$desm14_are)

# Histograma em relacao a frequencia
hist (cs, col="grey") 
hist (cs$desm14_are, col="grey")


# Histograma em relacao a densidade
hist (cs, freq = FALSE, col="grey")
hist (cs$dist_dsmt, freq = FALSE, col="grey")


# Desenhar a curva de distribuicao normal
x= cs
mean = mean(cs)
sd = sd(cs)

hist (cs, freq = FALSE, col="grey")
curve(dnorm(x, mean, sd), col = 2, add = TRUE)

boxplot(subCells.cs[,-1], outline = F, names = c("Dist. Prob.", "Dist. Assent.", 
                                                 "Dist. Grand.","Dist. Med.", "Dist. Peq", 
                                                 "Dist. Mini", "Dist. TI", "Dist. UC", 
                                                 "Dist. Estr.", "Dist. Hidro.", "Dist. Dsmt. Ant.", 
                                                 "Dist. Degrad."))
boxplot(subCells.cs[,1], outline = F , xlab = "Ocor. Dmst.")

############################# TESTES DE NORMALIDADE #############################

# Realiza o teste de Anderson-Darling para normalidade
ad.test(x) 

# Realiza o teste de Kolmogorov-Smirnov para normalidade
ks.test(x, "pnorm", mean, sd)


# Realiza o teste Qui-quadrado de Pearson para normalidade
pearson.test(x)


################ TRANSFORMACAO DE DADOS (log10, sqrt, 1/x etc) ##################


# Adiciona uma nova coluna com o log10 da variavel escolhida
## Adicionou o 0.0001 para nao ter log de zero
##cs$logArea_desmt<- c(log10(cs$Area_DESMT+0.0001))

cs$log.dist_prob <- c(log(cs$dist_Probl+0.0001))
cs$log.dist_AST <- c(log(cs$dist_AST+0.0001))
cs$log.dist_grand <- c(log(cs$dist_grand+0.0001))
cs$log.dist_media <- c(log(cs$dist_media+0.0001))
cs$log.dist_peq <- c(log(cs$dist_peque+0.0001))
cs$log.dist_mini <- c(log(cs$dist_mini+0.0001))
cs$log.dist_ti <- c(log(cs$dist_ti+0.0001))
cs$log.dist_uc <- c(log(cs$dist_uc+0.0001))
cs$log.dist_estrd <- c(log(cs$dist_estrd+0.0001))
cs$log.dist_hidro <- c(log(cs$dist_hidro+0.0001))
cs$log.dist_dsm13 <- c(log(cs$dist_Dsm13+0.0001))
cs$log.dist_dgd2 <- c(log(cs$dist_DGD_2+0.0001))



View(cs)
# Histograma da variavel transformada (log10)
hist (cs$logArea_desmt, freq = FALSE, col="grey")
boxplot(cs$logArea_desmt)
# Teste de normalidade

ad.test(cs$log.dist_prob)
ad.test(cs$log.dist_AST)
ad.test(cs$log.dist_grand)
ad.test(cs$log.dist_media)
ad.test(cs$log.dist_peq)
ad.test(cs$log.dist_mini)
ad.test(cs$log.dist_ti)
ad.test(cs$log.dist_uc)
ad.test(cs$log.dist_estrd)
ad.test(cs$log.dist_hidro)
ad.test(cs$log.dist_dsm13)
ad.test(cs$log.dist_dgd2)

shapiro.test(sample(subCells.cs$kernel_med, size = 5000))
shapiro.test(sample(subCells.cs$dist_prob, size = 5000))
shapiro.test(sample(subCells.cs$dist_AST, size = 5000))
shapiro.test(sample(subCells.cs$dist_grand,size = 5000))
shapiro.test(sample(subCells.cs$dist_media, size = 5000))
shapiro.test(sample(subCells.cs$dist_peq, size = 5000))
shapiro.test(sample(subCells.cs$dist_mini, size = 5000))
shapiro.test(sample(subCells.cs$dist_ti, size = 5000))
shapiro.test(sample(subCells.cs$dist_uc, size = 5000))
shapiro.test(sample(subCells.cs$dist_estrd, size = 5000))
shapiro.test(sample(subCells.cs$dist_hidro, size = 5000))
shapiro.test(sample(subCells.cs$dist_dsm13, size = 5000))
shapiro.test(sample(subCells.cs$dist_dgd2, size = 5000))



# Adiciona uma nova coluna com o sqrt da variavael escolhida
#cs$sqrt_area_desmt<- c(sqrt(cs$Area_DESMT))
cs$sqrt_AST<- c(sqrt(cs$dist_AST))
View(cs)
# Histograma da variÃ¡vel transformada (sqrt)
hist (cs$sqrt_area_desmt, freq = FALSE, col="grey")
boxplot(cs$sqrt_area_desmt)
# Teste de normalidade
ad.test(cs$sqrt_area_desmt)

cs$Area_DESMT_norm <- c((cs$Area_DESMT - mean(cs$Area_DESMT))/(diff(range(cs$Area_DESMT))))
a <- c(index(cs$Area_DESMT_norm, cs$Area_DESMT_norm > 0))
hist (cs$Area_DESMT_norm[a], freq = FALSE, col="grey")
boxplot(cs$Area_DESMT_norm)
summary(cs$Area_DESMT_norm[a])


################################################################################
############          SCRIP PARA ANALISE DE CORRELACAO          ################
################################################################################



######################  SELECIONANDO ATRIBUTOS PARA ANALISE ####################
# (caso nao seja necessario analisar todos) #

subCells = subset(cs, O = 0, select = c(21, 1,2,5,6,9,10,13,19))

View (subCells)
subCells.log = subset(cs, O = 0, select = c(21, 24:35))

View (subCells.log)


######################## CORRELACAO ENTRE DUAS VARIAVEIS #######################

#Executar analise de correlacao
cor(cs$d, cs$fertilidad, method = "pearson")

#Executar teste de significancia
cor.test(cs$d, cs$fertilidad)


###################### CORRELACAO COM TODAS AS VARIAVEIS #######################

#Executar analise de correlacao
cor <- (round(cor(cs), 2))
View(cor)
cor

################# MATRIZ DE CORRELAO COM OS VALORES DE "P" #####################

corp <- rcorr(as.matrix(cs))
corp


#################### PLOTAR RELACAO ENTRE DUAS VARIAVEIS #######################

plot(cs$d, cs$fertilidad)



#################### PLOTAR RELACAO ENTRE DUAS VARIAVEIS #######################

#Opcao 1
pairs(cs[, c(1,2,3,4)], labels = c("Ocor. Dsmt.","Dist. Prob.", "Dist. Assent.", "Dist. Grand."), font.labels = 2, cex.labels = 3)

#Opcao 2
#exemplo:
cs_plot <- cs[, -c(3, 4,8, 11, 14, 15, 17, 20, 22)]
chart.Correlation(cs, histogram=TRUE, pch=19)

chart.Correlation(cs, histogram=TRUE, pch=19, labels = c("Dsmt.","Dist. Ass.", "Dist. UC.", "Dist. TI.","Dist.Hidro.", "Dist. AU", "Dist. Deg.", "Dist. Estr.", "Dist. CAR", "Dist. CARxUC","Dist. TL e Sigef", " Decl. Med."), font.labels = 3, cex.labels = NULL)
#tirando variáveis#
#--variaveis tiradas: media, pequena, ti
chart.Correlation(subCells.cs[,-c(5,6,8)], histogram=TRUE, pch=19, labels = c("Dsmt.","Dist. Prob.", "Dist. Assent.", "Dist. Grand.", "Dist. Mini", "Dist. UC", "Dist. Estr."
                                                                              ,"Dist. Hidro.", "Dist. Dsmt. Ant.", "Dist. Degrad."), font.labels = 2, cex.labels = NULL)

################################# CORRELOGRAMA ##################################

corrplot::corrplot(cor(cs))



######################### EXPORTAR MATRIZ DE CORRELACAO  ########################

write.dbf(subCells.log,file="celulas_2000_cortada_kernel3_log.dbf")

write.csv2(cor,file="correlacao_2.csv")




################################################################################
####################### SCRIP PARA ANALISE REGRESSAO ###########################
################################################################################


#################### REGRESSAO LINEAR SIMPLES/MULTIPLA  ########################
#####primeira regressaão (dados não normalizados) ####
reg <- lm(Area_DESMT ~ dist_Probl + dist_AST + dist_grand + dist_peque + 
            dist_uc + dist_estrd + dist_hidro + dist_Dsm13 +     
            dist_DGD_2, data = subCells.cs)
summary (reg)
stepRegression = step (reg, scale = 1, direction= "both")
stepRegression = step (reg, scale = 1, direction= "forward")
summary(stepRegression)

anova(reg)

plot(reg)

## regressão escolhida: sem dis_med, dist_peq, dist_TI, dist_hidro #

reg <- lm(kernel_med ~ dist_Probl + dist_AST + dist_grand + dist_mini + 
            dist_uc + dist_estrd + dist_Dsm13 +     
            dist_DGD_2, data = subCells.cs)

reg <- lm(Area_DESMT ~ dist_Probl + dist_AST + 
            dist_uc + dist_estrd + dist_Dsm13 +     
            dist_DGD_2, data = subCells.cs)
summary (reg)

stepRegression = step (reg, scale = 1, direction= "forward")
summary(stepRegression)

anova(reg)
plot(reg)
###### segunda regressão dados transformados para log #####
reg2 <- lm(subCells.log$kernel_med ~ subCells.log$log.dist_prob + subCells.log$log.dist_AST
           + subCells.log$log.dist_grand + subCells.log$log.dist_media + subCells.log$log.dist_peq 
           + subCells.log$log.dist_mini + subCells.log$log.dist_ti + subCells.log$log.dist_uc
           + subCells.log$log.dist_estrd + subCells.log$log.dist_hidro + subCells.log$log.dist_dsm13
           + subCells.log$log.dist_dgd2, data = subCells.log)
summary (reg2)
stepRegression = step (reg2, scale = 1, direction= "both")
stepRegression = step (reg2, scale = 1, direction= "forward")
summary(stepRegression)

anova(reg2)

plot(reg2)



######## eliminando algumas variávies do modelo ##########
#-----eliminado "log.dist_grand" e "log.dist_hidro"-------

reg3 <- lm(subCells$kernel_med ~ subCells$dist_Probl + subCells$dist_AST
           + subCells$dist_media 
           + subCells$dist_uc
           + subCells$dist_estrd + subCells$dist_Dsm13
           + subCells$dist_DGD_2, data = subCells)
summary (reg3)
stepRegression = step (reg3, scale = 1, direction= "both")
stepRegression = step (reg3, scale = 1, direction= "forward")
summary(stepRegression)

anova(reg3)

plot(reg3)
#########################################################

betas<- betas.lm(reg)
betas

residuos <- resid(reg)
preditos <- predict(reg)


plot(subCells.cs$kernel_med, residuos, main="analise dos residuos", ylab="residuos", xlab="Desmatamento")
abline(0,0, col="red", lwd=4)

plot(preditos, residuos, main="analise dos residuos", ylab="residuos", xlab="Desmatamento (predito)")
abline(0,0, col="red", lwd=4)

ad.test(residuos)

help(resid)

############################ REGRESSAO LOGISTICA ###############################

lreg <- glm(subCells$dist_dsmt ~ subCells$dist_Probl + subCells$dist_grand +subCells$dist_media + 
              subCells$dist_AST + subCells$dist_DEGRD + subCells$dist_Dsm13 + subCells$dist_ti + 
              subCells$dist_uc + subCells$dist_estrd + subCells$dist_hidro)
summary (lreg)
stepRegression = step (lreg, scale = 1, direction= "both")
summary(stepRegression)


anova(lreg)

plot(reg)
############################### matriz de viz ###############################
celulas <- readOGR("C:/Users/MateusMacul/OneDrive/Documentos/AnaliseEspacial/Shapes_Final/celulas_2000_cortada_kernel3.shp","celulas_2000_cortada_kernel3")

em.teste = amostra
viz <- poly2nb(pl = em.teste, queen = TRUE)
viz <- nb2listw(viz, style = "B",zero.policy = T )
############################## Teste de Autocorrelação #######################

tmoran <- lm.morantest(modstep, viz, alternative = "two.sided", zero.policy = T)
tmoran$estimate
tmoran$p.value
z.tmoran <-(modstepvai$residuals - mean(modstepvai$residuals))/sd(modstepvai$residuals)
w.z.tmoran <-lag.listw(viz, z.tmoran, zero.policy = T)
moran_lm_varo2004 <-lm(z.tmoran ~ w.z.tmoran)
plot(z.tmoran, w.z.tmoran, type="n",xlab="Erros normalizados",ylab="Médias dos vizinhos")
points(z.tmoran,w.z.tmoran,col="blue")
abline(moran_lm_varo2004, col="red", lwd=3)
abline(h=0,lty=2) 
abline(v=0,lty=2) 
summary(moran_lm_varo2004)
############################ Criando um objeto "Spatial Polygon"###############

spols<-polygons(celulas)



brks <- round(fivenum(reg$residuals), digits=2)
brks
cols <- rev(heat.colors(4))
plot(spols,col = cols[findInterval(reg$residuals,brks)])
legend(x=c(2,4), y=c(7,9), legend = leglabs(brks,"<", ">="), fill = cols, bty = "n", cex = 5, y.intersp = 5)
title(main="Mapa dos resíduos do modelo Múltiplo")
par(mar=c(1,1,1,1))

map.axes()
########################### Teste de lagrange ##################################

modelo8.lagrange <- lm.LMtests(modstepvai, viz, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"), zero.policy = T)
modelo8.lagrange
help("lm.LMtests")


# Spatial Lag model (SAR) -------------------------------------------------

modelo8.lag<- lagsarlm(formula = incre_1417 ~ AWMPFD_0 + SHDI - i_aberta - i_au - 
                         i_decliv + i_emb + i_cert_d + i_d13_d - ahp, data = amost6c, listw = viz, zero.policy = T)
modelo8.lag2<-  stsls(formula = incre_1417 ~ AWMPFD_0 + SHDI + i_aberta + i_au + 
                         i_decliv + i_emb + i_cert_d + i_d13_d + ahp, data = amost6c, listw = viz, zero.policy = T)
summary(modelo8.lag)
summary(modelo8.lag2)
plot(summary(modelo8.lag)$residuals, amost6c$incre_1417)
plot(modelo8.lag)

# spatial error model -----------------------------------------------------
celulas.err <- errorsarlm (Area_DESMT ~ dist_Probl + dist_AST + 
                             dist_uc + dist_estrd + dist_Dsm13 +     
                             dist_DGD_2, data = subCells.cs, listw = viz)
summary(celulas.err)
names(celulas.err)
class(celulas.err)

# SIMULTANEOUS AUTOREGRESSIVE MODEL OU Spatial AutoRegressive- SAR -----------------------------------------------------
celulas.err.sar <- spautolm (kernel_med ~ dist_Probl + dist_AST + dist_grand + dist_mini + 
                               dist_uc + dist_estrd + dist_Dsm13 +     
                               dist_DGD_2, data = subCells.cs, listw = viz)
summary(celulas.err.sar)
names(celulas.err.sar)
class(celulas.err.sar)


# Plots -------------------------------------------------------------------

par(mfrow=c(1,2))
par(mar=c(5,5,5,5))
plot(DGRAD_2004~AREADESMAT, cex=2)
points(DGRAD_2004~AREADESMAT,col="blue", cex=2, lwd=1)
abline(var2004.lm, col = "red", cex=5, lwd=3)
abline(var2004.err, col = "black", cex=5, lwd=3)

plot(DGRAD_2004~DENS_BORDA, cex=2)
points(DGRAD_2004~DENS_BORDA,col="blue", cex=2, lwd=1)
abline(var2004.lm, col = "red", cex=5, lwd=3)
abline(var2004.err, col = "black", cex=5, lwd=3)

#----------------plotando o mapa de resíduos

brks <- round(fivenum(celulas.err$residuals), digits=2)
brks
cols <- rev(heat.colors(4))
plot(spols,col = cols[findInterval(celulas.err$residuals,brks)])
title(main="Mapa dos resíduos do modelo CAR : DGRAD_2004~AREADESMAT+DENS_BORDA")

legend(c(6, 8), c(13,15), legend = leglabs(brks,"<", ">="), fill = cols, bty = "n", cex = 0.9, y.intersp = 0.9)

#--------------------moran I spatial error

tmoran <- lm.morantest(celulas.err$residuals, viz, alternative = "two.sided")
tmoran <- lm.LMtests(viz, model = "all")
z.tmoran <-(var2004.err$residuals - mean(var2004.err$residuals))/sd(var2004.err$residuals)
w.z.tmoran <-lag.listw(viz, z.tmoran)
moran_err_varo2004 <-lm(z.tmoran ~ w.z.tmoran)
plot(z.tmoran, w.z.tmoran, type="n",xlab="var2004",ylab="Spatial Lag of var2004")
points(z.tmoran,w.z.tmoran,col="blue")
abline(moran_err_varo2004, col="red", lwd=3)
abline(h=0,lty=2) 
abline(v=0,lty=2)

