setwd("C:/_alunos/Mateus/r")
dados.amost4<- read.csv("C:/Users/Padrao/OneDrive - inpe.br/Tabelas/dados_amostrados_arrumados.csv", header=TRUE, sep=",", dec=".")
View(dados.amost4)
#colocando todos os atributos e eliminando células com f2013 = 100%
amostra <- dados.amost4[which(dados.amost4$flor_13_p < 1),] # sem excluir nenhuma coluna
amostra <- dados.amost4[which(dados.amost4$flor_13_p < 1),c(13,35:78)]
modcompleto <- lm(incre_1417 ~ .,data=amostra)
modstep =  step(modcompleto, scale = 0, direction= "both")
summary(modstep)
vif(modstep)

#retirando atributos com vif alto r2=0,4327
amostra <- dados.amost4[which(dados.amost4$flor_13_p < 1),c(13,37:47,49:78)]
modcompleto <- lm(incre_1417 ~ .,data=amostra)
modstep =  step(modcompleto, scale = 0, direction= "both")
summary(modstep)
vif(modstep)

#retirando atributos nao significativos r2=0,4485
amostra <- dados.amost4[which(dados.amost4$flor_13_p < 1),c(13,35,36,40,41,43:49,51,57:73,75,76,78)]
modcompleto <- lm(incre_1417 ~ .,data=amostra)
modstep =  step(modcompleto, scale = 0, direction= "both")
summary(modstep)
vif(modstep)

#retirando atributos com vif alto e nao significativos r2=0,4025
amostra <- dados.amost4[which(dados.amost4$flor_13_p < 1),c(13,37:47,49,51,53,56,58:65,67:69,74:78)]
modcompleto <- lm(incre_1417 ~ .,data=amostra)
modstep =  step(modcompleto, scale = 0, direction= "both")
summary(modstep)
vif(modstep)
plot(modstep$fitted.values,amostra$incre_1417)

#eliminando outliers r2=0,5368
amostra <- dados.amost4[which(dados.amost4$flor_13_p < 1),c(13,37:41,45:47,49,51,56,58,62:68,70,71,74,75,77,78,53,60)]
modcompleto <- lm(incre_1417 ~ . - SHDI - i_tamimov + SHDI:i_tamimov,data=amostra[c(-310,-228,-241),])
modstep =  step(modcompleto, scale = 0, direction= "both", trace=0)
summary(modstep)
vif(modstep)
plot(modstep$fitted.values,amostra[c(-310,-228,-241),]$incre_1417)

vnames <- colnames(dados.amost4)
isel <- which(dados.amost4$flor_13_p < 1)
isel <- isel[-c(310,228,241)]
modstepsel <- NULL
r2sel <- -1
for (i in 35:77) {
  for (j in i:78) {
    amostra <- dados.amost4[isel,c(13,36:41,43,45:55,56,58,59,60:70,71,73:78)]
    amostra$iter <- dados.amost4[isel,i]*dados.amost4[isel,j]
    modcompleto <- lm(incre_1417 ~ .-i_conf_p -i_est - MPS_0 -MPAR_0  -i_decliv - i_tamimov -TABO_0 - i_aberta -SIDI -PLAND_0  -i_cert -i_rios5 -TAOBIA_0 -TE_0 + i_cert_d:i_emb + PLAND_0:i_emb + MPAR_0:i_est + TAOBIA_0:i_d13_d - i_conf_p:i_d13_d + I(i_emb^2) + I(MPAR_0^2) + MPAR_0:TE_0 + i_ass_d:i_d13_d + AWMPFD_0:i_ass_d + TAOBIA_0:i_emb - MPAR_0:i_vegse + i_vegse:i_est + SHDI:i_ass_d + i_uc:i_cert_d - AWMPFD_0:ahp + PSCOV_0:i_flor13 + LSI_0:i_d13_d + AWMSI_0:i_est + i_cert_d:i_ass_p + i_au:i_ass_d + i_au:i_emb + TABO_0:TAOBIA_0 + i_deg_p:i_conf_d + MPS_0:i_deg_p + i_deg_p:i_cert_d + i_cert:i_deg_p + AWMSI_0:SHDI + i_rios5:i_conf_d + PD_0:i_ass_p + AWMPFD_0:i_conf_p + LSI_0:i_conf_p + i_flor13:i_uc + I(i_cert_d^2) + TE_0:i_rios5 - i_est:i_conf_p + i_rios5:i_ass_p + i_rios5:ahp + LSI_0:PSCOV_0 + i_deg_p:i_conf_p + TABO_0:i_aberta + SHDI:i_au + BIA_0:i_ass_d - i_emb:i_ti + SIDI:i_tamimov - I(i_decliv^2),data=amostra)
    modstep =  step(modcompleto, scale = 0, direction = "both", trace = 0)
    if (summary(modstep)$adj.r.squared > r2sel) {
      r2sel <- summary(modstep)$adj.r.squared
      modstepsel <- modstep
      cat('i j r2:',i,j,r2sel,vnames[i],":",vnames[j],"\n")
    }
  }
}
summary(modstepsel)
summary(modstep)
summary(modcompleto)
vif(modstepsel)
plot(modstepsel$fitted.values,amostra$incre_1417)

vif(modstep)
summary(modstep)
tail(sort(vif(modstep)),5)
multiplot(
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostra$incre_1417, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = amostra$incre_1417)),
  cols = 1)
plot(modstep)
which(amostra$id == "C61L76")
