amostrado <- amost5[which(amost5$flor_13_p<1),]

# Busca exaustiva ---------------------------------------------------------

library(leaps)
leaps<-regsubsets(incre_1417 ~ . -flor_13_p - i_aberta , data = amostrado,nbest = 40, really.big=T)

plot(leaps,scale="adjr2", ylim = c(0.4,0.5))

# construindo modelo simples ----------------------------------------------
library(car)
library(ggplot2)
modi9 <- lm(formula = incre_1417 ~ AWMPFD_0 + AWMSI_0 - MPFD_0 - MPAR_0 + i_tamimov - i_emb + i_uc + i_cert_d  - I(MPAR_0^2)+ MPAR_0:i_est + I(i_emb^2) + TABO_0:i_ass_d + MPAR_0:SHDI , data = amostrado)
modi9 <- lm(formula = incre_1417 ~ . -i_flor13 - i_aberta - SIDI - PLAND_0 - MPAR_0 - i_br - i_au - i_emb - i_est - i_vegse - i_tamimov - BIA_0 -i_ass_p  - NP_0 - LSI_0 - i_cert_d - SHDI - I(MPAR_0^2) + MPAR_0:SHDI + i_emb:i_cert_d - MPAR_0:i_br + I(i_emb^2) + MPAR_0:i_est + SHDI:i_ass_d + MSI_0:MPAR_0 + i_est:i_tamimov + PLAND_0:i_emb, data = amostrado)
modi9 <- lm(formula = incre_1417 ~ . -i_flor13 - i_aberta - SIDI - PLAND_0 - MPFD_0 - MPS_0 - flor_13_p - i_conf_d - LSI_0 - PSSD_0,data = amostrado)
summary(modi9)
vif(modi9)

step_modi9 = step(modi9, scale = 0, direction= "both", trace = 0)
summary(step_modi9)
vif(step_modi9)

multiplot(
  ggplot()+
    geom_point(aes(y = step_modi9$residuals, x = step_modi9$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = step_modi9$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modi9$residuals, x = amostrado$incre_1417)),
  cols = 1)
reiduos<- step_modi9$residuals
hist(residuos, density = F, main = "Histograma dos Resíduos")

vnames <- colnames(amost5@data)
#isel <- which(amost5$flor_13_p < 1) # usado para tirar células
# isel <- isel[-c(310,228,241)] # usado para tirar células
modstepsel <- NULL
r2sel <- -1
system.time(for (i in (3:ncol(amost5))) {
  for (j in i:ncol(amost5)) {
    #amostrado <- amost5[isel,] #usado para tirar células
    for(g in 1:nrow(amostrado@data)){
      amostrado$iter[g] <- amostrado@data[g,i]* amostrado@data[g,j]}
    modcompleto <- lm(formula = incre_1417 ~ MSI_0 + AWMSI_0 + AWMPFD_0 + MPAR_0 + 
                        TABO_0 + SHDI + i_cert + i_emb + i_uc + i_n_imov + i_ass_d + 
                        i_uc_d + i_deg_p + i_cert_d + i_conf_p + ahp + . - I(MPAR_0 ^2) + MPFD_0:i_est + I(i_emb^2) + TAOBIA_0:i_d13_d + LSI_0:i_ass_d - i_emb:i_cert_d - i_emb:i_flor13 + MPAR_0:SIDI - AWMPFD_0:ahp + MPAR_0:i_est 
                      - LSI_0 - PSSD_0 -i_flor13 - i_aberta - SIDI - PLAND_0 - MPFD_0 +MSI_0 +NP_0 - MPS_0 - BIA_0 - PSCOV_0 + SHDI + AWMSI_0 - i_d13_d - i_au - i_est - i_br - TAOBIA_0 - MPAR_0 + i_n_imov - i_emb - i_ass_p - iter - flor_13_p, data = amostrado)
    modstep =  step(modcompleto, scale = 0 , direction = "both", trace = 0)
    if (summary(modstep)$adj.r.squared > r2sel) {
      r2sel <- summary(modstep)$adj.r.squared
      modstepsel <- modstep
      cat('i j r2:',i,j,r2sel,vnames[i],":",vnames[j],"\n")
    }
    
  }
})
summary(modstepsel)
vif(modstepsel)
summary(modstep)
vif(modstep)

multiplot(
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = amostrado$incre_1417)),
  cols = 1)


library("lmtest")
bptest(modstep, data=amostrado, studentize=TRUE) #regs é a fórmula da regressão

#TESTE SHAPIRO WILK PARA VERIFICAR A NORMALIDADE DOS RESÍDUOS
install.packages("dplyr")
library("dplyr")
residuos <- modstep$residuals 
shapiro.test(residuos)


# Modelo Controlado VIF muito ALTO ----------------------------------------

modcompleto <- lm(formula = incre_1417 ~ MSI_0 + AWMSI_0 + AWMPFD_0 + MPAR_0 + 
                      TABO_0 + SHDI + i_cert + i_emb + i_uc + i_n_imov + i_ass_d + 
                      i_uc_d + i_deg_p + i_cert_d + i_conf_p + ahp + . + I(MPAR_0 ^2) + MPFD_0:i_est + I(i_emb^2) + TAOBIA_0:i_d13_d + LSI_0:i_ass_d + i_emb:i_cert_d + i_emb:i_flor13 + MPAR_0:SIDI - AWMPFD_0:ahp + MPAR_0:i_est 
                    - LSI_0 - PSSD_0 -i_flor13 - i_aberta - SIDI - PLAND_0 - I(MPFD_0^2) +MSI_0 -NP_0 - MPS_0 - BIA_0 - PSCOV_0 + SHDI + AWMSI_0 - i_d13_d - i_au - i_est - i_br - TAOBIA_0 - MPAR_0 + i_n_imov - i_emb - i_ass_p - iter, data = amostrado)
modstep =  step(modcompleto, scale = 0 , direction = "both", trace = 0)
summary(modstep)

multiplot(
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = amostrado$incre_1417)),
  cols = 1)
residuos <- modstep$residuals 
shapiro.test(residuos)
hist(residuos, density = F, main = "Histograma dos Resíduos")



# Modelo Controlado alguns VIF alto ---------------------------------------

modcompleto <- lm(formula = incre_1417 ~ MSI_0 + AWMSI_0 + AWMPFD_0 + MPAR_0 + 
                    TABO_0 + SHDI + i_cert + i_emb + i_uc + i_n_imov + i_ass_d + 
                    i_uc_d + i_deg_p + i_cert_d + i_conf_p + ahp + . - I(MPAR_0 ^2) 
                  + MPFD_0:i_est + I(i_emb^2) + TAOBIA_0:i_d13_d + LSI_0:i_ass_d 
                  + i_emb:i_cert_d + i_emb:i_flor13 + MPAR_0:SIDI - AWMPFD_0:ahp + MPAR_0:i_est 
                  - LSI_0 - PSSD_0 -i_flor13 - i_aberta - SIDI - PLAND_0 - MPFD_0 
                  +MSI_0 +NP_0 - MPS_0 - BIA_0 - PSCOV_0 + SHDI + AWMSI_0 - i_d13_d 
                  - i_au - i_est - i_br - TAOBIA_0 - MPAR_0 + i_n_imov - i_emb - i_ass_p - iter, data = amostrado)
modstep =  step(modcompleto, scale = 0 , direction = "both", trace = 0)

summary(modstep)

multiplot(
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = amostrado$incre_1417)),
  cols = 1)
residuos <- modstep$residuals 
shapiro.test(residuos)
hist(residuos, density = F, main = "Histograma dos Resíduos")



# Modelo controlado NENHUM VIF alto ---------------------------------------
modcompleto <- lm(formula = incre_1417 ~ MSI_0 + AWMSI_0 + AWMPFD_0 + MPAR_0 + 
                    TABO_0 + SHDI + i_cert + i_emb + i_uc + i_n_imov + i_ass_d + 
                    i_uc_d + i_deg_p + i_cert_d + i_conf_p + ahp + . - I(MPAR_0 ^2) + MPFD_0:i_est + I(i_emb^2) + TAOBIA_0:i_d13_d + LSI_0:i_ass_d - i_emb:i_cert_d - i_emb:i_flor13 + MPAR_0:SIDI - AWMPFD_0:ahp + MPAR_0:i_est 
                  - LSI_0 - PSSD_0 -i_flor13 - i_aberta - SIDI - PLAND_0 - MPFD_0 +MSI_0 +NP_0 - MPS_0 - BIA_0 - PSCOV_0 + SHDI + AWMSI_0 - i_d13_d - i_au - i_est - i_br - TAOBIA_0 - MPAR_0 + i_n_imov - i_emb - i_ass_p - iter - flor_13_p, data = amostrado)
modstep =  step(modcompleto, scale = 0 , direction = "both", trace = 0)
summary(modstep)

multiplot(
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = amostrado$incre_1417, x = modstep$fitted.values)),
  ggplot()+
    geom_point(aes(y = modstep$residuals, x = amostrado$incre_1417)),
  cols = 1)
residuos <- modstep$residuals 
shapiro.test(residuos)
hist(residuos, density = F, main = "Histograma dos Resíduos")
