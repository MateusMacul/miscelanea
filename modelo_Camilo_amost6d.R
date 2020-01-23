library("PerformanceAnalytics")
library("car")

setwd("C:/_alunos/Mateus/r")
amostra<- read.csv("C:/Users/Padrao/OneDrive - inpe.br/Tabelas/amost6d.csv", header=TRUE, sep=",", dec=".")

amostra$incre_1417 <- log(amostra$incre_1417/(1-amostra$incre_1417)) # transformação logit
modcompleto <- lm(incre_1417 ~ . -MPFD_0 -AWMPFD_0 -i_ti -PSCOV_0 -i_conf_p -PLAND_0 -MPAR_0 -TAOBIA_0 -i_vegse -i_est -i_aberta -i_rios5 -i_ass_p -i_decliv -BIA_0,data=amostra)
modstep =  step(modcompleto, scale = 0, direction= "both", trace=0)
summary(modstep)
vif(modstep)
plot(modstep$fitted.values,amostra$incre_1417)

vnames <- colnames(amostra)
modstepsel <- NULL
r2sel <- -1
for (i in 3:43) {
  for (j in (i+1):44) {
    #    cat('i j:',i,j,"\n")
    amostra2 <- amostra
    amostra2$iter <- amostra[,i]*amostra[,j]
    modcompleto <- lm(incre_1417 ~ iter + flor_13_p*TAOBIA_0 + i_deg_d:i_conf_d + i_est:i_uc_d + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + flor_13_p:i_tamimov + NP_0:i_conf_p + i_cert_d:i_d13_d + i_emb:i_conf_d + i_d13_d + i_rios5 + i_emb + flor_13_p -X,data=amostra2)
    modstep =  step(modcompleto, scale = 0, direction = "both", trace = 0)
    if (summary(modstep)$adj.r.squared > r2sel) {
      r2sel <- summary(modstep)$adj.r.squared
      modstepsel <- modstep
      cat('i j r2:',i,j,r2sel,vnames[i],":",vnames[j],"  VIF max:",max(vif(modstepsel)),"\n")
    }
  }
}
summary(modstepsel)
vif(modstepsel)
plot(modstepsel$fitted.values,amostra2$incre_1417)
plot(modstepsel$fitted.values,modstepsel$residuals)
shapiro.test(modstepsel$residuals)

#modelo final
#modcompleto <- lm(incre_1417 ~ + i_deg_d:i_conf_d + NP_0:AWMPFD_0 + i_est:i_uc_d + i_tamimov:i_ti_d + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + i_uc:i_conf_d + flor_13_p:i_tamimov + i_vegse:i_conf_d + NP_0:i_est + NP_0:i_conf_p + i_uc:i_uc_d + TAOBIA_0:i_d13_d + i_tamimov:i_uc_d + i_cert_d:i_d13_d + i_d13_d:i_rios5 + i_emb:i_conf_d + i_emb + SHDI + i_d13_d + i_uc_d + NP_0 + i_tamimov + i_ti_d + MPS_0 + PSSD_0 + AWMSI_0,data=amostra) #Adjusted R-squared:  0.588
modcompleto <- lm(incre_1417 ~ i_deg_d:i_conf_d + i_est:i_uc_d + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + flor_13_p:i_tamimov + NP_0:i_conf_p + i_cert_d:i_d13_d + i_emb:i_conf_d + i_d13_d + i_rios5 + i_emb + flor_13_p,data=amostra)
modstep =  step(modcompleto, scale = 0, direction = "both", trace = 0)
summary(modstep)
summary(modcompleto)
vif(modcompleto)
plot(modcompleto$fitted.values,amostra$incre_1417)
plot(modcompleto$fitted.values,modcompleto$residuals)
shapiro.test(modcompleto$residuals)
bptest(modcompleto, data=amostra, studentize=TRUE)

modstepsel <- NULL
r2sel <- -1
for (i in 3:44) {
    #    cat('i j:',i,j,"\n")
    amostra2 <- amostra
    amostra2$v <- amostra[,i]
    modcompleto <- lm(incre_1417 ~ v + i_deg_d:i_conf_d + i_est:i_uc_d + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + flor_13_p:i_tamimov + NP_0:i_conf_p + i_cert_d:i_d13_d + i_emb:i_conf_d + i_d13_d + i_rios5 + i_emb + flor_13_p -X,data=amostra2)
    modstep =  step(modcompleto, scale = 0, direction = "both", trace = 0)
    if (summary(modstep)$adj.r.squared > r2sel) {
      r2sel <- summary(modstep)$adj.r.squared
      modstepsel <- modstep
      cat('i j r2:',i,r2sel,vnames[i],"  VIF max:",max(vif(modstepsel)),"\n")
    }
}
summary(modstepsel)
vif(modstepsel)

#modelo com ahp
modahp <- lm(ahp ~ i_deg_d:i_conf_d + i_est:i_uc_d + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + flor_13_p:i_tamimov + NP_0:i_conf_p + i_cert_d:i_d13_d + i_emb:i_conf_d + i_d13_d + i_rios5 + i_emb + flor_13_p,data=amostra)
modcompleto <- lm(incre_1417 ~ ahp + NP_0:AWMPFD_0 + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + NP_0:i_est + NP_0:i_conf_p + i_uc:i_uc_d + TAOBIA_0:i_d13_d + i_cert_d:i_d13_d + i_d13_d:i_rios5 + i_d13_d + i_uc_d,data=amostra) #Adjusted R-squared:  0.4037
modstepsel <- NULL
r2sel <- -1
for (i in 3:44) {
  #    cat('i j:',i,j,"\n")
  amostra2 <- amostra
  amostra2$iter <- amostra[,i]
  modcompleto <- lm(incre_1417 ~ iter + ahp + NP_0:AWMPFD_0 + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + NP_0:i_est + NP_0:i_conf_p + i_uc:i_uc_d + TAOBIA_0:i_d13_d + i_cert_d:i_d13_d + i_d13_d:i_rios5 + i_d13_d + i_uc_d -X,data=amostra2)
  modstep =  step(modcompleto, scale = 0, direction = "both", trace = 0)
  if (summary(modstep)$adj.r.squared > r2sel) {
    r2sel <- summary(modstep)$adj.r.squared
    modstepsel <- modstep
    cat('i j r2:',i,r2sel,vnames[i],"  VIF max:",max(vif(modstepsel)),"\n")
  }
}
summary(modstepsel)
vif(modstepsel)
summary(modcompleto)

vnames <- colnames(amostra)
modstepsel <- NULL
r2sel <- -1
for (i in 3:43) {
  for (j in (i+1):44) {
    #    cat('i j:',i,j,"\n")
    amostra2 <- amostra
    amostra2$iter <- amostra[,i]*amostra[,j]
    modcompleto <- lm(incre_1417 ~ iter + ahp + NP_0:AWMPFD_0 + i_ti_d:i_d13_d + PSCOV_0:i_uc_d + NP_0:i_est + NP_0:i_conf_p + i_uc:i_uc_d + TAOBIA_0:i_d13_d + i_cert_d:i_d13_d + i_d13_d:i_rios5 + i_d13_d + i_uc_d -X,data=amostra2)
    modstep =  step(modcompleto, scale = 0, direction = "both", trace = 0)
    if (summary(modstep)$adj.r.squared > r2sel) {
      r2sel <- summary(modstep)$adj.r.squared
      modstepsel <- modstep
      cat('i j r2:',i,j,r2sel,vnames[i],":",vnames[j],"  VIF max:",max(vif(modstepsel)),"\n")
    }
  }
}
summary(modstepsel)
vif(modstepsel)
plot(modstepsel$fitted.values,amostra2$incre_1417)
plot(modstepsel$fitted.values,modstepsel$residuals)
shapiro.test(modstepsel$residuals)

