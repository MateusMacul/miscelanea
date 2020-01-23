library("PerformanceAnalytics")
library("car")
library(ggplot2)
library(MASS)
# pegando os nomes e números de cada coluna no dado amostrado -------------

data.sample = dados.amost6d #dado amostrado em análise
indices <- matrix(c(index(names(data.sample))),length(names(data.sample)),1)
rownames(indices) <- names(data.sample)
indices
View(indices)
View(dados.amost5@data)
# tirando celulas outliers ------------------------------------------------

dados.amost5 <- dados.amost5[!(dados.amost5$id == "C15L53"), ]
C15L53 = which(dados.amost5$id== "C15L53")
C22L88 = which(dados.amost5$id== "C22L88")
C53L70 = which(dados.amost5$id== "C53L70")
C28L68 = which(dados.amost5$id== "C28L68")

dados.amost5 <- dados.amost5[-c(C15L53, C22L88,C53L70,C28L68), ]
# estabelecendo quais colunas retirar -----------------------------------------------------------------

colunas_retirar <- c(indices[grep(c(paste(c("id", "col", "row","med_decliv","area_confl", "area_deg", "area_tlsgf", "area_ass", 
                                            "area_uc", "area_ti", "area_embar", "area_abert","area_vegse", "areakm_mea", "num_imov",
                                            "dis_mn_au", "dis_mn_br", "dis_mn_est", "dis_mn_r5", "dis_mn_ass", "dis_mn_cnf", "dis_mn_ti",
                                            "dis_mn_deg", "dis_mn_crt", "dis_mn_uc", "dis_mn_d13", "CA_0", "PD_0", "TE_0", "ED_0",
                                            "ass_dist","uc_dist","ti_dist", "au_dist", "au_dist", "deg_dist", "estr_dist", "tl_sgf_dis",
                                            "confl_dis", "area_retir", "flor_14_p", "areakm_med", "\\bincre_14\\b", "incre_15", "incre_16",
                                            "\\bincre_17\\b", "flor_17_p", "y1","ID_NUM","IJI_0", "SHEI", "SIEI"),collapse = "|")),
                                  rownames(indices), ignore.case = F)])

amost6d <- dados.amost6d[,-colunas_retirar]
str(amost6c@data)
View(amost6@data)
# criando a fórmula -----------------------------------------------------------------------------------
fmla <- as.formula(paste("incre_1417 ~ ", paste(c(names(data.sample[,-colunas_retirar])), collapse= "+")))
fmla

# transformação da variavel dependente ------------------------------------
amostra <- amost6d
amostra$incre_1417 <- log(amostra$incre_1417/(1-amostra$incre_1417)) # transformação logit
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/transformacao_y.jpg"))
plot(amost6d$incre_1417, amostra$incre_1417,cex.lab=1.5,cex.axis=1.5, ylab = "'y", xlab = "y = Proporção de desmatamento 2014 a 2017 na célula")
dev.off()
# modelo agroa vai amost6 --------------------------------------------------------
modelovai <- lm(incre_1417 ~ . +TABO_0 - TAOBIA_0 - SIDI, data = amost6c[,c(1,3:18)])
modelovai <- lm(incre_1417 ~ . -PLAND_0-NP_0-MPS_0-PSSD_0+LSI_0-MSI_0-AWMSI_0-MPFD_0+AWMPFD_0+MPAR_0+PSCOV_0-TABO_0-BIA_0-TAOBIA_0+SHDI-SIDI
                -i_br - i_ass_p -i_d13_d -i_decliv , data = amost6c)

modelovai <- lm(incre_1417 ~. -ahp -i_est -i_ass_p -i_ass_d 
                -PLAND_0-NP_0-MPS_0-PSSD_0-LSI_0-MSI_0-AWMSI_0-MPFD_0-AWMPFD_0-MPAR_0-PSCOV_0-TABO_0-BIA_0-TAOBIA_0-SHDI-SIDI , data = amost6c)
modelovai <- lm(incre_1417 ~. -ahp -i_br -i_est +flor_13_p -i_flor13 +i_decliv +i_ass_p +i_uc -i_cert_d
                +PLAND_0+NP_0+MPS_0+PSSD_0+LSI_0+MSI_0+AWMSI_0+MPFD_0+AWMPFD_0+MPAR_0+PSCOV_0+TABO_0+BIA_0+TAOBIA_0+SHDI-SIDI , data = amost6c)
summary(modelovai)
modelovai <- lm(incre_1417 ~. -flor_13_p -i_uc_d -i_ti_d
                +i_aberta + i_vegse -i_br + i_au + i_est +i_tamimov +i_cert -i_decliv + i_rios5 -i_ass_p +i_emb -i_flor13 -i_uc +i_ti
                -PLAND_0-NP_0+MPS_0+PSSD_0+LSI_0-MSI_0-AWMSI_0+MPFD_0+AWMPFD_0+MPAR_0+PSCOV_0+TABO_0+BIA_0+TAOBIA_0+SHDI +SIDI , data = amost6c)



# # só variveis da AHP ----------------------------------------------------

modelovai <- lm(incre_1417 ~ +i_aberta + i_vegse +i_br + i_au - i_est +i_tamimov +i_cert +i_decliv + i_rios5 +i_ass_p +i_emb +i_flor13 +i_uc -i_ti
                , data = amostra)

# # todas veriáveis -ahp --------------------------------------------------

modelovai <- lm(incre_1417 ~ .+i_aberta + i_vegse +i_br -i_au -i_est -i_tamimov +i_cert +i_decliv -i_rios5 +i_ass_p +i_emb -i_flor13 +i_uc -i_ti
                +PLAND_0+NP_0+MPS_0+PSSD_0+LSI_0+MSI_0+AWMSI_0+MPFD_0+AWMPFD_0+MPAR_0-PSCOV_0+TABO_0-BIA_0-TAOBIA_0+SHDI+SIDI
                -i_conf_p -i_ass_p +i_uc_d -i_cert
                -ahp -residuo -fitted
                , data = amostra)
modstepvai =  step(modelovai, scale = 0 , direction = "both", trace = 0)
summary(modstepvai)
vif(modstepvai)

# # todas variávei +ahp -vars da ahp --------------------------------------

modelovai <- lm(incre_1417 ~ .-i_aberta -i_vegse -i_br -i_au -i_est -i_tamimov -i_cert -i_decliv -i_rios5 -i_ass_p -i_emb -i_flor13 -i_uc -i_ti
                +PLAND_0+NP_0+MPS_0+PSSD_0+LSI_0+MSI_0+AWMSI_0+MPFD_0-AWMPFD_0+MPAR_0+PSCOV_0-TABO_0-BIA_0-TAOBIA_0-SHDI-SIDI
                +i_conf_p -i_ass_p +i_uc_d +i_deg_d +i_ass_d +i_cert_d +flor_13_p -i_d13_d
                -ahp -residuo -fitted
                , data = amostra)
summary(modelovai)
modstepvai =  stepAIC(modelovai, scale = 0 , direction = "both", trace = 0)
summary(modstepvai)
vif(modstepvai)
# # só métricas -----------------------------------------------------------

modelovai <- lm(incre_1417 ~ .-i_aberta -i_vegse -i_br -i_au -i_est -i_tamimov -i_cert -i_decliv -i_rios5 -i_ass_p -i_emb -i_flor13 -i_uc -i_ti
                +PLAND_0-NP_0+MPS_0+PSSD_0+LSI_0+MSI_0+AWMSI_0+MPFD_0+AWMPFD_0-MPAR_0+PSCOV_0+TABO_0-BIA_0+TAOBIA_0-SHDI+SIDI
                -i_conf_p -i_ass_p -i_uc_d -i_deg_d -i_ass_d -i_conf_d -i_d13_d -i_n_imov -i_ti_d -i_cert_d -flor_13_p -i_deg_p
                -ahp -residuo -fitted
                , data = amostra)
modelovai <- lm(ahp ~ .-i_aberta -i_vegse -i_br -i_au -i_est -i_tamimov -i_cert -i_decliv -i_rios5 -i_ass_p -i_emb -i_flor13 -i_uc -i_ti
                +PLAND_0+NP_0-MPS_0+PSSD_0+LSI_0+MSI_0+AWMSI_0+MPFD_0+AWMPFD_0+MPAR_0+PSCOV_0-TABO_0-BIA_0-TAOBIA_0+SHDI+SIDI
                -i_conf_p -i_ass_p -i_uc_d -i_deg_d -i_ass_d -i_conf_d -i_d13_d -i_n_imov -i_ti_d -i_cert_d -flor_13_p -i_deg_p
                -ahp -residuo -fitted -incre_1417
                , data = amostra)
summary(modelovai)
modstepvai =  stepAIC(modelovai, scale = 0 , direction = "both", trace = 0)
summary(modstepvai)
vif(modstepvai)
# # tudo, sem métricas ------------------------------------------------------------

modelovai <- lm(incre_1417 ~ .+i_aberta +i_vegse -i_br +i_au +i_est -i_tamimov +i_cert -i_decliv +i_rios5 +i_ass_p +i_emb -i_flor13 +i_uc -i_ti
                -PLAND_0-NP_0-MPS_0-PSSD_0-LSI_0-MSI_0-AWMSI_0-MPFD_0-AWMPFD_0-MPAR_0-PSCOV_0-TABO_0-BIA_0-TAOBIA_0-SHDI-SIDI
                -i_conf_p +i_ass_p +i_uc_d +i_deg_d +i_ass_d +i_conf_d -i_d13_d +i_n_imov +i_ti_d +i_cert_d +i_cert
                +ahp -residuo -fitted +flor_13_p
                , data = amostra)
summary(modelovai)
modstepvai =  stepAIC(modelovai, scale = 0 , direction = "both", trace = 0)
summary(modstepvai)
vif(modstepvai)
# # tudo ------------------------------------------------------------

modelovai <- lm(incre_1417 ~ .+i_aberta +i_vegse -i_br +i_au +i_est -i_tamimov +i_cert -i_decliv +i_rios5 +i_ass_p -i_emb -i_flor13 +i_uc +i_ti
                +PLAND_0+NP_0+MPS_0-PSSD_0+LSI_0-MSI_0+AWMSI_0-MPFD_0+AWMPFD_0+MPAR_0+PSCOV_0+TABO_0-BIA_0-TAOBIA_0-SHDI-SIDI
                +i_conf_p +i_ass_p +i_uc_d +i_deg_d +i_ass_d +i_conf_d -i_d13_d +i_n_imov +i_ti_d +i_cert_d +i_cert
                +ahp -residuo -fitted +flor_13_p
                , data = amostra)
summary(modelovai)
modstepvai =  stepAIC(modelovai, scale = 0 , direction = "both", trace = 0)
summary(modstepvai)
vif(modstepvai)


# #  ----------------------------------------------------------------------


modstepvai =  step(modelovai, scale = 0 , direction = "both", trace = 0)
summary(modstepvai)
vif(modstepvai)

amostra$residuo<- modstepvai$residuals
plot(amostra$MPAR_0, amostra$residuo)
plot(amostra$PLAND_0, amostra$residuo)
plot(amostra$i_aberta, amostra$residuo)
plot(amostra$i_au, amostra$residuo)
plot(amostra$i_ass_d, amostra$residuo)
plot(amostra$i_deg_d, amostra$residuo)
plot(amostra$i_cert_d, amostra$residuo)
plot(amostra$i_conf_d, amostra$residuo)
plot(amostra$ahp, amostra$residuo)
#  descartado a não linearidade da relação
multiplot(
  ggplot()+
    geom_point(aes(y = amostra$incre_1417, x = modstepvai$fitted.values)),
  ggplot()+
    geom_point(aes(y = modstepvai$residuals, x = modstepvai$fitted.values)) +
    geom_hline(yintercept = 0),
  cols = 1)
bptest(modstepvai, data=amostra, studentize=F) #primeiro elemento é a fórmula da regressão
residuos <- modstepvai$residuals 
shapiro.test(residuos) #residuos é uma variável que recebeu os resíduos da regressão


# modelo com interações amost6d -------------------------------------------

modstep <- lm(formula = incre_1417 ~ MPAR_0 + i_aberta + i_au + ahp + i_d13_d + 
                i_conf_d:i_emb + i_d13_d:SHDI + i_d13_d:i_cert_d + i_d13_d:i_rios5 + 
                i_d13_d:i_br + i_deg_d:i_uc_d + i_ti_d:i_conf_p + i_uc_d:i_ti_d, 
              data = amost6d)
# achador de modelo Camilo ------------------------------------------------


vnames <- colnames(amostra@data)
isel <- which(amost5$flor_13_p < 1) # usado para tirar células
# isel <- isel[-c(310,228,241)] # usado para tirar células
modstepsel <- NULL
r2sel <- -1
system.time(for (i in 2:(ncol(amostra)-2)) {
  # if(i==12) next # elimnando interações com MPAR
  for (j in i:(ncol(amostra)-2)) {
    # if(j==12) next # elimnando interações com MPAR
    amostrado <- amostra #[isel,] #usado para tirar células
    for(g in 1:nrow(amostrado@data)){
      amostrado$iter[g] <- amostrado@data[g,i]* amostrado@data[g,j]}
    modcompleto <- lm(formula = incre_1417 ~ .+PLAND_0 - MPS_0 - BIA_0 +TAOBIA_0 -SHDI -MPAR_0 -NP_0 -PSCOV_0 -MSI_0 -MPFD_0 -PSSD_0 -AWMPFD_0 -LSI_0 -AWMSI_0
                      -i_br -i_au -i_tamimov - i_cert -i_decliv -i_rios5 -i_emb -i_ti_d  -i_ti -i_est 
                      -i_uc_d -i_deg_d - i_cert_d - i_conf_d - i_conf_p -i_ass_d -i_aberta -i_vegse -i_uc
                      + MPS_0:i_ti_d +i_deg_d:i_d13_d -i_uc:i_n_imov + NP_0:TABO_0 +i_br:i_tamimov +ahp +i_rios5:i_d13_d +i_emb:i_conf_d + MPFD_0:i_ti +i_cert_d:i_d13_d +i_aberta:i_rios5 +i_ti_d:i_uc_d + i_emb:i_uc  -LSI_0:TABO_0
                      -fitted -residuo -iter, data = amostrado)
    modstep =  step(modcompleto, scale = 0 , direction = "both", trace = 0)
    if (summary(modstep)$adj.r.squared > r2sel) {
      r2sel <- summary(modstep)$adj.r.squared
      modstepsel <- modstep
      cat('i j r2:',i,j,r2sel,vnames[i],":",vnames[j],"\n")
    }
    
  }
})


# refinando o modelo ------------------------------------------------------

modcompleto <- lm(formula = incre_1417 ~ . +PLAND_0 +MPAR_0 +i_aberta -i_au +i_ass_d +i_deg_d +i_conf_d 
                  +ahp +i_emb*i_conf_d +BIA_0*i_ti_d +i_cert_d*i_d13_d + i_rios5*i_d13_d +TAOBIA_0*i_d13_d 
                  +MPAR_0*i_decliv -AWMSI_0*i_deg_d +i_vegse*i_conf_d -i_ti_d*i_conf_p -i_tamimov*i_uc 
                  -i_ti_d -BIA_0 -i_cert_d -i_rios5 -TAOBIA_0 -i_decliv -AWMSI_0 -i_vegse -i_conf_p +i_est -i_tamimov 
                  -i_ti +i_uc -i_uc_d +i_br +MPAR_0 -MSI_0 -SHDI +AWMPFD_0 -MPFD_0 -fitted -residuo, data = amostra)
modstep =  step(modcompleto, scale = 0 , direction = "both", trace = 0)

summary(modcompleto)
summary(modstep)
vif(modstep)
summary(modstepsel)
vif(modstepsel)
amostrab = amostra
amostrab$i_emb = 1 - amostrab$i_emb
amostrab$i_decliv = 1 - amostrab$i_decliv
amostrab$i_rios5 = 1 - amostrab$i_rios5
amostrab$i_vegse = 1 - amostrab$i_vegse

modelo_final = lm(formula = incre_1417 ~ AWMPFD_0 + MPAR_0 + SIDI + i_emb + 
                    i_conf_d + i_d13_d + ahp + i_emb:i_conf_d + BIA_0:i_ti_d + 
                    i_d13_d:i_cert_d + i_d13_d:i_rios5 + i_d13_d:TAOBIA_0 + MPAR_0:i_decliv + 
                    i_conf_d:i_vegse, data = amostra)
summary(modelo_final)
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/mode_fim_PedxObs.jpg"))
ggplot()+
  geom_point(aes(y = amostra$incre_1417, x = modelo_final$fitted.values))+
  geom_smooth()+
  xlab("y' Predito")+
  ylab("y' Observado")+
  xlim(-4,4)+
  ylim(-4,4)+ 
  geom_abline()+
  geom_smooth(method = "lm")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))
dev.off()
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/mod_fim_res.jpg"))
ggplot()+
  geom_point(aes(y = (modelo_final$residuals/sd(modelo_final$residuals)), x = modelo_final$fitted.values))+
  geom_hline(yintercept = 0)+
  ylab("Resíduos padronizados")+
  xlab("y' Predito")+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))
dev.off()


amostra$residuo<- modstep$residuals
amostra$fitted<- modstep$fitted.values
plot(amostra$MPAR_0, amostra$residuo)
plot(amostra$i_aberta, amostra$residuo)
plot(amostra$i_au, amostra$residuo)
plot(amostra$ahp, amostra$residuo)
plot(amostra$i_d13_d, amostra$residuo)
plot((amostra$i_conf_d*amostra$i_emb), amostra$residuo)
plot((amostra$SHDI*amostra$i_d13_d), amostra$residuo)
plot((amostra$i_cert_d*amostra$i_d13_d), amostra$residuo)
plot((amostra$i_rios5*amostra$i_d13_d), amostra$residuo)
plot((amostra$i_br*amostra$i_d13_d), amostra$residuo)
plot((amostra$i_uc_d*amostra$i_deg_d), amostra$residuo)
plot((amostra$i_ti_d*amostra$i_conf_p), amostra$residuo)
plot((amostra$i_ti_d*amostra$i_uc_d), amostra$residuo)

plot(modstep$fitted.values, modstep$residuals)
abline(0,0)
plot(modstep)
ggplot()+
  geom_point(aes(y = modstep$residuals, x = modstep$fitted.values))
residuos <- modstep$residuals 
shapiro.test(residuos)
bptest(modstep, data=amostrado, studentize=TRUE)

mapview(desmatamento13, col.regions = "grey") + mapview(incremento, col.regions = "black", map.types = "OpenTopoMap") + mapview(amost6c,zcol = "residuo", 
                                                                                col.regions = colorRampPalette(
                                                                                  colors = c("red2","white", "yellow")))
# testes da regressão -----------------------------------------------------

# TESTE BREUSCH-PAGAN PARA VERIFICAR A HOMOCEDASTICIDADE DOS RESÍDUOS
install.packages("lmtest") # Pacote com o teste de homocedasticidade Breusch-Pagan
library("lmtest")
bptest(modstep, data=amostrado, studentize=TRUE) #primeiro elemento é a fórmula da regressão

#TESTE SHAPIRO WILK PARA VERIFICAR A NORMALIDADE DOS RESÍDUOS
install.packages("dplyr")
library("dplyr")
residuos <- modstep$residuals 
shapiro.test(residuos) #residuos é uma variável que recebeu os resíduos da regressão

# Meu modelo mágico com todas as celulas ----------------------------------

modmagtudo <- lm(formula = incre_1417 ~ . - flor_13_p + NP_0 + MPS_0 + LSI_0 + 
                   MSI_0 - AWMSI_0 - MPFD_0 - AWMPFD_0 - MPAR_0 + TABO_0 + BIA_0 + 
                   TAOBIA_0 + SHDI + SIDI + i_aberta - i_br - i_est + i_au + 
                   i_tamimov + i_cert + i_rios5 - i_ass_p + i_emb + i_uc - i_ti + 
                   i_ass_d + i_ti_d + i_uc_d + i_deg_d + i_deg_p + i_cert_d + 
                   i_conf_d + i_conf_p + i_d13_d + ahp + I(i_emb^2) + I(MPAR_0^2) + 
                   I(i_est^2) + AWMPFD_0:i_au + flor_13_p:MPAR_0 + NP_0:LSI_0 + 
                   TABO_0:i_uc + i_tamimov:i_cert_d + i_br:i_au + AWMPFD_0:SHDI + 
                   i_est:i_d13_d + MSI_0:i_ass_p + i_ass_d:i_ti_d + MPAR_0:ahp + 
                   AWMPFD_0:i_cert + i_emb:i_conf_d + TAOBIA_0:i_emb + TABO_0:i_aberta + 
                   i_aberta:i_conf_p + i_cert:i_uc + i_ass_d:i_d13_d + i_est:i_rios5 + 
                   i_deg_p:i_cert_d + i_ti:i_conf_d + i_ass_d:i_uc_d + i_uc:i_conf_d + 
                   i_aberta:i_uc + MSI_0:i_au + i_tamimov:ahp + i_emb:i_ti_d + 
                   LSI_0:i_emb + AWMPFD_0:i_ass_d + AWMSI_0:i_deg_d + MPS_0:i_deg_p + 
                   MPFD_0:MPAR_0 + i_est:i_uc_d + MPAR_0:SHDI + i_est:i_ass_p + 
                   i_au:i_emb + i_br:i_uc_d + MPAR_0:i_br + MPFD_0:i_emb + i_est:i_conf_p, 
                 data = amost5)
stepmodmagtudo <- step(modmagtudo, scale = 0 , direction = "both", trace = 0)

summary(stepmodmagtudo)

multiplot(
  ggplot()+
    geom_point(aes(y = stepmodmagtudo$residuals, x = stepmodmagtudo$fitted.values)),
  ggplot()+
    geom_point(aes(y = amost5$incre_1417, x = stepmodmagtudo$fitted.values)),
  ggplot()+
    geom_point(aes(y = stepmodmagtudo$residuals, x = amost5$incre_1417)),
  cols = 1)
bptest(stepmodmagtudo, data=amost5, studentize=TRUE) #primeiro elemento é a fórmula da regressão
residuos <- stepmodmagtudo$residuals 
shapiro.test(residuos) #residuos é uma variável que recebeu os resíduos da regressão
hist(residuos, density = F, main = "Histograma dos Resíduos")

# Modelo mágico completo flor_13_p <1 -------------------------------------
modmagflor13 <- lm(incre_1417 ~ .-i_flor13 - iter - i_aberta - i_emb - NP_0 - i_br -TABO_0 -i_au - i_cert + MSI_0:MPAR_0 + NP_0:i_cert_d + TAOBIA_0:i_emb + i_decliv:i_ti + i_au:i_tamimov +MSI_0:i_au + TAOBIA_0:i_d13_d + i_au:i_ass_d  +  i_est:i_tamimov +i_rios5:i_conf_d  + MPAR_0:i_br + SHDI:i_ass_d  + PLAND_0:i_emb + i_emb:i_cert_d + MPAR_0:SHDI + PSCOV_0:i_conf_d + TABO_0:i_ass_d + i_n_imov:i_d13_d + I(i_emb^2) + MPAR_0:i_est + I(MPAR_0^2),data=amostrado)

stepmodmagfrlo13 <- step(modmagflor13, scale = 0 , direction = "both", trace = 0)
summary(stepmodmagfrlo13)
modmagflor13 <- lm(formula = incre_1417 ~ PLAND_0 + MPS_0 + PSSD_0 + MSI_0 + 
                     MPAR_0 + PSCOV_0 + BIA_0 + TAOBIA_0 + SHDI + SIDI + i_est + 
                     i_tamimov + i_decliv + i_rios5 + i_uc + i_ti + i_n_imov + 
                     i_ass_d + i_cert_d + i_conf_d + i_conf_p + i_d13_d + I(i_emb^2) + 
                     I(MPAR_0^2) + MSI_0:MPAR_0 + i_cert_d:NP_0 + TAOBIA_0:i_emb + 
                     i_decliv:i_ti + i_tamimov:i_au + MSI_0:i_au + TAOBIA_0:i_d13_d + 
                     i_ass_d:i_au + i_est:i_tamimov + i_rios5:i_conf_d + MPAR_0:i_br + 
                     SHDI:i_ass_d + PLAND_0:i_emb + i_cert_d:i_emb + MPAR_0:SHDI + 
                     i_ass_d:TABO_0 + i_n_imov:i_d13_d + MPAR_0:i_est, data = amostrado)

summary(modmagflor13)

multiplot(
  ggplot()+
    geom_point(aes(y = modmagflor13$residuals, x = modmagflor13$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = modmagflor13$fitted.values)),
  ggplot()+
    geom_point(aes(y = modmagflor13$residuals, x = amostrado$incre_1417)),
  cols = 1)
bptest(modmagflor13, data=amost5, studentize=TRUE) #primeiro elemento é a fórmula da regressão
residuos <- modmagflor13$residuals 
shapiro.test(residuos) #residuos é uma variável que recebeu os resíduos da regressão
hist(residuos, density = F, main = "Histograma dos Resíduos")

# modelo simples tudo amost5  ---------------------------------------------
modisimpltudo <- lm(incre_1417 ~ .+ ahp - i_aberta - i_cert_d - i_flor13 - i_n_imov 
                    + MPAR_0 - MPFD_0 +SHDI + LSI_0+TABO_0 +PSSD_0 + i_decliv, data = amost5)
summary(modisimpltudo)
stepmodisimpltudo<- step(modisimpltudo, scale = 0 , direction = "both", trace = 0)
summary(stepmodisimpltudo)
vif(stepmodisimpltudo)
modisimples7<- lm(formula = incre_1417 ~ +ahp-i_est - i_deg_p - i_tamimov - areakm_mea  + AWMPFD_0 + SHDI + SIDI + i_emb + i_uc + 
                    i_ti - i_n_imov - i_deg_p + i_cert_d, data = dados.amost5)

summary(modisimples7)


# modelo simples flor_13_p < 1: amostrado  ---------------------------------------------
modisimplflor13 <- lm(incre_1417 ~ .-BIA_0 -iter - PSCOV_0 -PSSD_0 + i_d13_d - LSI_0 + MPFD_0 + MPS_0 -i_flor13 
                      +flor_13_p -i_conf_d -i_aberta - SIDI, data = amostrado)
stepmodisimplflor13<- step(modisimplflor13, scale = 0 , direction = "both", trace = 0)

summary(stepmodisimplflor13)
vif(stepmodisimplflor13)

multiplot(
  ggplot()+
    geom_point(aes(y = stepmodisimplflor13$residuals, x = stepmodisimplflor13$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = stepmodisimplflor13$fitted.values)),
  ggplot()+
    geom_point(aes(y = stepmodisimplflor13$residuals, x = amostrado$incre_1417)),
  cols = 1)
bptest(stepmodisimplflor13, data=amostrado, studentize=TRUE) #primeiro elemento é a fórmula da regressão
residuos <- stepmodisimplflor13$residuals 
shapiro.test(residuos) #residuos é uma variável que recebeu os resíduos da regressão
hist(residuos, density = F, main = "Histograma dos Resíduos")


# modelo com os índices -------------------------------------------------------------------------------
modi <- lm(incre_1417 ~  - flor_13_p - i_aberta + (i_vegse - i_br + i_est + i_au + i_tamimov + i_cert + i_decliv
           + i_rios5 + i_ass_p + i_emb + i_uc + i_ti + i_ass_d + i_ti_d + i_uc_d + i_deg_d 
           + i_deg_p + i_cert_d + i_conf_d + i_conf_p + i_d13_d*i_flor13 - i_d13_d + AWMPFD_0)*ahp, data = dados.amost5)

summary(modi)
vif(modi)
vif(stepwise)
stepwise =  step(modi, scale = 0, direction= "both")
summary(stepwise)

dados.amost5$erros <- stepwise$residuals
dados.amost5$fitted <- stepwise$fitted.values
View(dados.amost5@data)


multiplot(
  ggplot()+
    geom_point(aes(y = stepwise$residuals, x = stepwise$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost.alter2$incre_1417, x = stepwise$fitted.values)),
  ggplot()+
    geom_point(aes(y = stepwise$residuals, x = dados.amost.alter2$incre_1417)),
  cols = 1)

# ?  -----------------------------------------------------------------------


modi2 <- lm(incre_1417 ~  + AWMPFD_0 + MPAR_0 + i_d13_d + LSI_0 + i_flor13 + PLAND_0 + BIA_0 
            + SHDI + MPS_0  + SIDI - AWMSI_0 + PD_0 + ahp - dis_mn_est + TABO_0 
            + i_emb + TAOBIA_0 + i_est - num_imov + dis_mn_deg + areakm_mea + i_deg_d 
            + i_conf_d + PSSD_0 - i_decliv - i_deg_p + PSCOV_0 + dis_mn_crt - i_uc  + i_ass_d 
            + i_au + i_tamimov + i_cert_d + area_confl + i_ass_p + i_conf_p + i_rios5 + dis_mn_uc 
            + i_uc_d + i_ti + i_ti_d + i_cert + i_aberta + i_vegse + 1, data = dados.amost5)

summary(modi2)
vif(modi2)

step_modi2 <- step(modi2, scale = 0.022, direction= "both")
summary(step_modi2)
vif(step_modi2)

multiplot(
  ggplot()+
    geom_point(aes(y = step_modi2$residuals, x = step_modi2$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = step_modi2$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modi2$residuals, x = dados.amost5$incre_1417)),
  cols = 1)

modi3 <- lm(formula = incre_1417 ~ (AWMPFD_0 + MPAR_0 + LSI_0 + i_flor13 + 
              ahp + i_emb + i_est + areakm_mea + dis_mn_crt + i_au + i_tamimov + 
              dis_mn_uc + i_uc_d + i_ti + i_aberta)*ahp, data = dados.amost5)

summary(modi3)
vif(modi3)

step_modi3 <- step(modi3, scale = 0.03, direction= "both")
summary(step_modi3)
vif(step_modi3)

multiplot(
  ggplot()+
    geom_point(aes(y = step_modi3$residuals, x = step_modi3$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = step_modi3$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modi3$residuals, x = dados.amost5$incre_1417)),
  cols = 1)

modi4 <-lm(incre_1417 ~ (AWMPFD_0 + i_flor13 + i_emb + i_est + areakm_mea 
                         + dis_mn_crt + i_tamimov + dis_mn_uc + i_uc_d + i_ti)*ahp, data = dados.amost5)

summary(modi4)
vif(modi4)

step_modi4 <- step(modi4, scale = 0.03, direction= "both")
summary(step_modi4)
vif(step_modi4)

multiplot(
  ggplot()+
    geom_point(aes(y = step_modi4$residuals, x = step_modi4$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = step_modi4$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modi4$residuals, x = dados.amost5$incre_1417)),
  cols = 1)

# modelos da busca exaustiva ----------------------------------------------

modi5 <- lm(incre_1417 ~ AWMPFD_0 + MPAR_0 + LSI_0 + i_emb + i_est + areakm_mea + i_tamimov + i_ti, data = dados.amost5)
summary(modi5)
vif(modi5)

multiplot(
  ggplot()+
    geom_point(aes(y = modi5$residuals, x = modi5$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = modi5$fitted.values)),
  ggplot()+
    geom_point(aes(y = modi5$residuals, x = dados.amost5$incre_1417)),
  cols = 1)


modi6 <- lm(incre_1417 ~ AWMPFD_0 +i_flor13 + i_emb + i_est + areakm_mea + dis_mn_crt + i_uc + i_tamimov ,data = dados.amost5)
summary(modi6)
vif(modi6)
step_modi6 <- step(modi6, scale = 0.03, direction= "both")
summary(step_modi6)
vif(step_modi6)

multiplot(
  ggplot()+
    geom_point(aes(y = modi6$residuals, x = modi6$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = modi6$fitted.values)),
  ggplot()+
    geom_point(aes(y = modi6$residuals, x = dados.amost5$incre_1417)),
  cols = 1)

modi7 <- lm(formula = incre_1417 ~ AWMPFD_0 + i_flor13 + ahp + TABO_0 + 
              i_emb + i_n_imov + areakm_mea + i_deg_p + i_uc + i_ass_d + 
              i_tamimov2 + i_uc_d2 + i_ti + i_vegse, data = dados.amost5)
summary(modi7)
vif(modi7)
step_modi7 = step(modi7, scale = 0.0, direction= "both", trace = 0)
summary(step_modi7)
vif(step_modi7)

multiplot(
  ggplot()+
    geom_point(aes(y = step_modi7$residuals, x = step_modi7$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = step_modi7$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modi7$residuals, x = dados.amost5$incre_1417)),
  cols = 1)


modi8 <- lm(formula = incre_1417 ~ AWMPFD_0 + i_flor13 + ahp + TABO_0 +
              i_emb + i_n_imov + areakm_mea + i_deg_p + i_uc + i_ass_d + 
              i_tamimov2 + i_uc_d2 + i_ti + i_vegse + AWMPFD_0:i_emb + 
              AWMPFD_0:i_deg_p + AWMPFD_0:i_uc + AWMPFD_0:i_ass_d + 
              AWMPFD_0:i_uc_d2 + i_flor13:LSI_0 + ahp:LSI_0 + TABO_0:LSI_0 +
              i_vegse:LSI_0 + i_flor13:ahp - i_vegse + ahp:areakm_mea + 
              ahp:i_tamimov2 + ahp:i_uc_d2 -i_tamimov2, data = dados.amost5[-C60L76,])
summary(modi8)
step_modi8 = step(modi8, scale = 0.0, direction= "both", trace = 0)
summary(step_modi8)
vif(step_modi8)



multiplot(
  ggplot()+
    geom_point(aes(y = step_modi8$residuals, x = step_modi8$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5[-C60L76,]$incre_1417, x = step_modi8$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modi8$residuals, x = dados.amost5[-C60L76,]$incre_1417)),
  cols = 1)

dados.amost5$erros <- step_modi8$residuals
dados.amost5$fitted <- step_modi8$fitted.values
View(dados.amost5@data)
C60L76 = which(dados.amost5$id== "C60L76")

# modelo com outras amostras  ---------------------------------------------


modi.alter <- lm(formula = incre_1417 ~ i_vegse + i_tamimov + i_ass_p + i_emb + 
                   i_uc + i_ti + i_ass_d + i_ti_d + i_deg_p + i_conf_p + i_flor13 + 
                   AWMPFD_0 + ahp + i_tamimov:ahp + i_ass_p:ahp + i_ass_d:ahp + 
                   i_ti_d:ahp + i_conf_p:ahp + i_flor13:ahp, data = dados.amost.alter)

summary(modi.alter)

multiplot(
  ggplot()+
    geom_point(aes(y = modi.alter$residuals, x = modi.alter$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost.alter$incre_1417, x = modi.alter$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost.alter$incre_1417, x = modi.alter$residuals)),
  cols = 1)


# modelo com os erros -----------------------------------------------------


stepwise$call
moderros <- lm(formula = erros ~  (-flor_13_p + i_aberta + i_vegse + i_br + i_est + i_au - i_tamimov + i_cert - i_decliv 
               + i_rios5 - i_ass_p - i_emb - i_uc - i_ti - i_ass_d - i_ti_d - i_uc_d + i_deg_d 
               - i_deg_p + i_cert_d + i_conf_d - i_conf_p + i_d13_d - i_flor13 + ahp)^2- i_est - i_au - i_vegse - i_br - i_rios5, data = dados.amost5)
summary(moderros)
steperros = step(moderros, scale = 0.023, direction= "both") # checar o valor de scale
summary(steperros)

moderros <- lm(formula = erros ~  - CA_0 + PLAND_0 + PD_0 - NP_0 + MPS_0 + PSSD_0 - LSI_0 + MSI_0 + AWMSI_0
               + MPFD_0 + AWMPFD_0 + TE_0 - ED_0 + MPAR_0 + PSCOV_0 + TABO_0 + BIA_0 + TAOBIA_0 + SHDI + SIDI, data = dados.amost5)


summary(moderros)
steperros = step(moderros, scale = 0, direction= "both")
summary(steperros)

multiplot(
  ggplot()+
    geom_point(aes(y = steperros$residuals, x = steperros$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$erros, x = steperros$fitted.values)),
  cols = 1)
# Tentativas antteriores --------------------------------------------------


# eliminando os dados menores 
dados.amost4.maior = subset(dados.amost4, dados.amost4$incre_1417>= 0.11205)
dados.amost4.maior %>% length()
# original 
modi.mais.o <- lm(incre_1417 ~ -(i_aberta + i_vegse + i_br + i_est + i_au + i_tamimov + i_cert + i_decliv + i_rios5 +
                  i_ass_p + i_emb + i_flor13 + i_uc + i_ti)+ i_ass_d + i_ti_d - i_uc_d + i_deg_d + i_deg_p +
                    i_cert_d + i_conf_d + i_conf_p + i_d13_d + ( i_ass_d + i_ti_d - i_uc_d + i_deg_d + i_deg_p +
                  i_cert_d + i_conf_d + i_conf_p + i_d13_d) %in% ahp + SIDI, data = dados.amost4)
summary(modi.mais.o)
vif(modi.mais.o)
modi.mais.step.o =  step(modi.mais.o, scale = 0, direction= "both")
summary(modi.mais.step.o)

# removido devido ao vif: i_br, i_flor13, i_uc_d
# repetindo com os dados cortados modelo com os índices 
modi.mais <- lm(incre_1417 ~ i_uc_d + i_aberta + i_vegse + i_est - i_au + i_tamimov + i_cert + i_decliv + i_rios5 +
                 i_ass_p + i_emb + i_uc + i_ti + i_ass_d + i_ti_d + i_deg_d + i_deg_p -
                 i_cert_d + i_conf_d + i_conf_p + i_d13_d + SIDI, data = dados.amost4)
summary(modi.mais)
vif(modi.mais)
modi.mais.step =  step(modi.mais, scale = 0, direction= "both")
summary(modi.mais.step)
# gráfico
multiplot(
  ggplot()+
    geom_point(aes(y = modi.mais.step$residuals, x = modi.mais.step$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost4$incre_1417, x = modi.mais.step$fitted.values)),
  cols = 1)
dados.amost4$erros <- modi.mais.step$residuals
dados.amost4$abs_erros <- abs(modi.mais.step$residuals)

###

reg000 <- lm(incre_1417 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
               area_ass + area_uc + area_ti + area_embar + area_abert + 
               area_vegse  + areakm_mea + num_imov + dis_mn_au + 
               dis_mn_est + dis_mn_r5  + dis_mn_ass + 
               dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
               PD_0 + MPS_0 + PSSD_0 + 
               MSI_0 + AWMSI_0 + MPFD_0 + 
               PSCOV_0 + PR + TABO_0 + TAOBIA_0 + 
               SHDI + dis_mn_d13, data = dados.amost4)
summary(reg000)
vif(reg000)
stepwise =  step(reg000, scale = 1, direction= "both")
summary(stepwise)

# TENTATIVA MODELO SEM AS MÉTRICAS, MAS COM AS DISTANCIAS


reg001 <- lm(incre_1417 ~  med_decliv + area_confl + area_deg + area_tlsgf + area_ass + 
  area_uc + area_ti + area_embar + area_abert + area_vegse + 
  areakm_mea + num_imov + dis_mn_au + dis_mn_br + dis_mn_est + dis_mn_r5 + 
  dis_mn_ass + dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
  flor_13_p + dis_mn_d13 + AWMPFD_0, data = dados.amost4)

summary(reg001)
vif(reg001)
 
# tirando a dis_mn_br pelo vif
reg001a <- lm(incre_1417 ~  med_decliv + area_confl + area_deg + area_tlsgf + area_ass + 
                area_uc + area_ti + area_embar + area_abert + area_vegse + 
                areakm_mea + num_imov + dis_mn_au + dis_mn_est + dis_mn_r5 + 
                dis_mn_ass + dis_mn_cnf + min_uc_ti + dis_mn_deg + dis_mn_crt +  
                flor_13_p + dis_mn_d13, data = dados.amost4)


summary(reg001a)
vif(reg001a)
ggplot()+
  geom_point(aes(y = reg001a$residuals, x = reg001a$fitted.values))
stepwise1a =  step(reg001a, scale = 0, direction= "both")
summary(stepwise1a)
stepwise1a$call
reg001a_step <- lm(formula = incre_1417 ~ med_decliv + area_deg + area_uc + area_ti + 
                     area_embar + dis_mn_crt + flor_13_p + dis_mn_d13, data = dados.amost4)
summary(reg001a_step)
multiplot(
  ggplot()+
    geom_point(aes(y = reg001a_step$residuals, x = reg001a_step$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost4$incre_1417, x = reg001a_step$fitted.values)),
  cols = 1)

# colocando Métrica
reg001b <- lm(incre_1417 ~  med_decliv + area_confl + area_deg + area_tlsgf + area_ass + 
               area_uc + area_ti + area_embar + area_abert + area_vegse + 
               areakm_mea + num_imov + dis_mn_au + dis_mn_est + dis_mn_r5 + 
               dis_mn_ass + dis_mn_cnf + min_uc_ti + dis_mn_deg + dis_mn_crt +  
               flor_13_p + dis_mn_d13 + AWMPFD_0, data = dados.amost4)


summary(reg001b)
vif(reg001b)
stepwise1b =  step(reg001b, scale = 0, direction= "both")
summary(stepwise1b)

# trabalhando com o modleo setpwised a partir do reg001a
reg002 = lm(formula = incre_1417 ~ area_deg + area_uc + area_ti + 
               area_embar + dis_mn_crt + dis_mn_uc + 
              flor_13_p + SIDI, data = dados.amost4)
summary(reg002)
vif(reg002)

multiplot(
  ggplot()+
    geom_point(aes(y = reg002$residuals, x = reg002$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost4$incre_1417, x = reg002$fitted.values)),
  cols = 1)
# Modelo só Métrica
regmetri = lm(formula = incre_1417 ~ PLAND_0 + PD_0 + MPS_0 + PSSD_0 + 
                LSI_0 + MSI_0 + AWMSI_0 + MPFD_0 + AWMPFD_0 + 
                PSCOV_0 + TABO_0 + BIA_0 + TAOBIA_0 + 
                SHDI, data = dados.amost4)
summary(regmetri)
vif(regmetri)
stepwisemetri =  step(regmetri, scale = 0, direction= "both")
summary(stepwisemetri)

# trocando variaveis pela ahp
reg002a = lm(formula = incre_1417 ~  ahp + area_deg + area_uc + area_ti + 
              area_embar + dis_mn_crt + dis_mn_uc + 
              flor_13_p + AWMPFD_0, data = dados.amost4)
summary(reg002a)
vif(reg002a)

regi = lm(incre_1417 ~ i_aberta + i_vegse + i_est + i_au + 
            i_tamimov + i_cert + i_decliv + i_rios5 + i_ass_p + i_emb + 
            i_uc + i_ti + i_ass_d + i_ti_d + i_uc_d + i_deg_d + 
            i_deg_p + i_cert_d + i_conf_d + i_conf_p + 
            num_imov  + flor_13_p + dis_mn_d13 + AWMPFD_0, data = dados.amost4)

summary(regi)
vif(regi)
ggplot()+
  geom_point(aes(y = regi$residuals, x = regi$fitted.values))

## construindo um modelo 
CA_0 + PLAND_0 + PD_0 + NP_0 + MPS_0 + PSSD_0 + 
  LSI_0 + MSI_0 + AWMSI_0 + MPFD_0 + AWMPFD_0 + TE_0 + ED_0 + 
  MPAR_0 + PSCOV_0 + PR + IJI_0 + TABO_0 + BIA_0 + TAOBIA_0 + 
  PRD + SHDI + SIDI + SHEI + SIEI
reg000 <- lm(incre_1417 ~ med_decliv + "area_confl" + area_deg + "area_tlsgf" + 
               area_ass + area_uc + area_ti + area_embar + "area_abert" + 
               area_vegse  + areakm_mea + num_imov + dis_mn_au + 
               dis_mn_est + dis_mn_r5  + dis_mn_ass + 
               dis_mn_cnf + dis_mn_ti + "dis_mn_deg" + dis_mn_crt + dis_mn_uc + 
               dis_mn_d13, data = dados.amost4)

regcc <- lm(incre_1417 ~ dis_mn_d13 + PLAND_0 +  med_decliv + area_deg + area_embar + dis_mn_crt + area_vegse, data = dados.amost4)
summary(regcc)
vif(regcc)

multiplot(
  ggplot()+
  geom_point(aes(y = regcc$residuals, x = regcc$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost4$incre_1417, x = regcc$fitted.values)),
  cols = 1)
    
  



