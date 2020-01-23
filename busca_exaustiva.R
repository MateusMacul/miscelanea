
# Procura exaustiva -------------------------------------------------------
install.packages("leaps")
library(leaps)
leaps<-regsubsets(incre_1417 ~  + AWMPFD_0 + MPAR_0 + i_d13_d + LSI_0 + i_flor13 + PLAND_0 + BIA_0 
                  + SHDI + MPS_0  + SIDI + AWMSI_0 + PD_0 + ahp + dis_mn_est + TABO_0 
                  + i_emb + TAOBIA_0 + I(log(i_est_2)) + num_imov + dis_mn_deg + areakm_mea + i_deg_d 
                  + i_conf_d + PSSD_0 + i_decliv + i_deg_p + PSCOV_0 + dis_mn_crt + i_uc  + i_ass_d 
                  + i_au + i_tamimov + i_cert_d2 + area_confl + i_ass_p + i_conf_p + i_rios5 + dis_mn_uc
                  + i_uc_d + i_ti + i_ti_d + i_cert + i_aberta + i_vegse + i_flor13:i_d13_d + 1 , data = dados.amost5,nbest = 40, really.big=T)

plot(leaps,scale="adjr2", ylim = c(0.4,0.5))

modex <- lm(incre_1417 ~   (+AWMPFD_0 - MPAR_0 + i_d13_d + LSI_0 + i_flor13 + PLAND_0 + BIA_0 
            + SHDI + MPS_0  + SIDI + AWMSI_0 + PD_0 + ahp - dis_mn_est + TABO_0 
            + i_emb + TAOBIA_0 + I(log(i_est_2)) - num_imov + i_n_imov - dis_mn_deg + areakm_mea + i_deg_d 
            + i_conf_d + PSSD_0 + I(log(1-i_decliv)) + i_deg_p + PSCOV_0 - dis_mn_crt + i_uc  + i_ass_d 
            + i_au + i_tamimov2 + i_cert_d2 + area_confl + i_ass_p + i_conf_p + i_rios5 - dis_mn_uc
            + i_uc_d2 + i_ti + i_ti_d + i_cert + i_aberta + i_vegse)^2 + i_flor13:i_d13_d + 1 , data = dados.amost5)


step_modex =  step(modex, scale = 0.025, direction= "both")
print(summary(step_modex))
vif(step_modex)

dados.amost5$erros <- step_modex$residuals
dados.amost5$fitted <- step_modex$fitted.values
View(dados.amost5@data)

multiplot(
  ggplot()+
    geom_point(aes(y = step_modex$residuals, x = step_modex$fitted.values)),
  ggplot()+
    geom_point(aes(y = dados.amost5$incre_1417, x = step_modex$fitted.values)),
  ggplot()+
    geom_point(aes(y = step_modex$residuals, x = dados.amost5$incre_1417)),
  cols = 1)

# tentando arrumar algumas variáveis ----------------------------------------

# dis_mn_est
plot(dados.amost5[-c(C27L79, C28L79),]$dis_mn_est, dados.amost5[-c(C27L79, C28L79),]$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ dis_mn_est, data = dados.amost5[-c(C27L79, C28L79),]))$r.squared))
abline(lm(incre_1417 ~ I(dis_mn_est), data = dados.amost5[-c(C27L79, C28L79),]))

plot(1-dados.amost5$i_est, dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(1 - i_est), data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ I(1-i_est), data = dados.amost5))

plot(dados.amost5[-c(C27L79, C28L79),]$i_est_2, dados.amost5[-c(C27L79, C28L79),]$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(i_est_2), data = dados.amost5[-c(C27L79, C28L79),]))$r.squared))
abline(lm(incre_1417 ~ I((i_est_2)), data = dados.amost5[-c(C27L79, C28L79),]))

plot(log(dados.amost5[-c(C27L79, C28L79),]$i_est_2), dados.amost5[-c(C27L79, C28L79),]$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(log(i_est_2)), data = dados.amost5[-c(C27L79, C28L79),]))$r.squared))
abline(lm(incre_1417 ~ I(log(i_est_2)), data = dados.amost5[-c(C27L79, C28L79),]))

max(dados.amost5$i_est_2)
plot(dados.amost5$dis_mn_est/7000, dados.amost5$incre_1417)
plot(dados.amost5$dis_mn_est, dados.amost5[-c(6368, 6369),]$incre_1417)
plot((log(nozero$dis_mn_est)/9), nozero$incre_1417)
plot(((nozero$dis_mn_est)^0.3), nozero$incre_1417)


limiar = 6000 # era 4000

for (i in 1:length(dados.amost5$dis_mn_est)){
  if (dados.amost5$dis_mn_est[i] <= limiar){
    dados.amost5$i_est_2[i] = (dados.amost5$dis_mn_est[i]/limiar)} 
  else 
    {dados.amost5$i_est_2[i] = 1}}

summary(lm(incre_1417 ~ i_est, data = dados.amost5[-c(6368, 6369),]))
summary(lm(incre_1417 ~ I(dis_mn_est/4000), data = dados.amost5[-c(6368,6369),]))



# dis_mn_uc
plot(dados.amost5$dis_mn_uc, dados.amost5$incre_1417,  main = paste("R² = ", summary(lm(incre_1417 ~ dados.amost5$dis_mn_uc, data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ dados.amost5$dis_mn_uc, data = dados.amost5))

plot(dados.amost5$i_uc_d, dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ dados.amost5$i_uc_d, data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ dados.amost5$i_uc_d, data = dados.amost5))

plot((dados.amost5$i_uc_d2), dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(dados.amost5$i_uc_d2), data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ I(dados.amost5$i_uc_d2), data = dados.amost5))

plot(((dados.amost5$dis_mn_uc)^2)/6e+9, dados.amost5$incre_1417)
plot(log(dados.amost5$dis_mn_uc + 60), dados.amost5$incre_1417)
plot(dados.amost5$i_uc_d^2, dados.amost5$incre_1417)


limiar = 80000 # era 40000
for (i in 1:length(dados.amost5$dis_mn_uc)){
  if (dados.amost5$dis_mn_uc[i] <= limiar){
    dados.amost5$i_uc_d2[i] = dados.amost5$dis_mn_uc[i]/limiar}
  else {
    dados.amost5$i_uc_d2[i] = 1}
}

# dis_mn_crt
plot(dados.amost5$dis_mn_crt, dados.amost5$incre_1417)
abline(lm(incre_1417 ~ dis_mn_crt, data = dados.amost5 ))

plot(dados.amost5$i_cert_d, dados.amost5$incre_1417)
abline(lm(incre_1417 ~ i_cert_d, data = dados.amost5 ))

plot(dados.amost5$i_cert_d2, dados.amost5$incre_1417)
abline(lm(incre_1417 ~ i_cert_d2, data = dados.amost5 ))

summary(lm(incre_1417 ~ dis_mn_crt, data = dados.amost5 ))
summary(lm(incre_1417 ~ i_cert_d, data = dados.amost5 ))
summary(lm(incre_1417 ~ i_cert_d2, data = dados.amost5 ))


limiar = 25000 # era 16000
for (i in 1:length(dados.amost5$dis_mn_crt)){
  if (dados.amost5$dis_mn_crt[i] <= limiar ){
    dados.amost5$i_cert_d2[i] = dados.amost5$dis_mn_crt[i]/limiar}
  else {
    dados.amost5$i_cert_d2[i] = 1}
}



# areakm_mea

plot(dados.amost5$areakm_mea, dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ areakm_mea, data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ areakm_mea, data = dados.amost5))

plot((1 - dados.amost5$i_tamimov), dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(1 - i_tamimov), data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ I((1 - i_tamimov)), data = dados.amost5))

plot((1 - dados.amost5$i_tamimov2), dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(1 - i_tamimov2), data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ I((1 - i_tamimov2)), data = dados.amost5))

plot(dados.amost5[-c(C27L79, C28L79),]$areakm_mea, dados.amost5[-c(C27L79, C28L79),]$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ areakm_mea, data = dados.amost5[-c(C27L79, C28L79),]))$r.squared))
abline(lm(incre_1417 ~ areakm_mea, data = dados.amost5[-c(C27L79, C28L79),]))

plot((1 - dados.amost5[-c(C27L79, C28L79),]$i_tamimov), dados.amost5[-c(C27L79, C28L79),]$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ I(1 - i_tamimov), data = dados.amost5[-c(C27L79, C28L79),]))$r.squared))
abline(lm(incre_1417 ~ I((1 - i_tamimov)), data = dados.amost5[-c(C27L79, C28L79),]))

plot((1 - nozero$i_tamimov)^2, nozero$incre_1417)

plot((1 - dados.amost5$i_tamimov), dados.amost5$incre_1417)

plot((1 - dados.amost5$i_tamimov)^2, dados.amost5$incre_1417)

summary(lm(incre_1417 ~ I((1 - i_tamimov)^2), data = dados.amost5[-c(C27L79, C28L79),]))
summary(lm(incre_1417 ~ I(1 - i_tamimov), data = dados.amost5[-c(C27L79, C28L79),]))
summary(lm(incre_1417 ~ areakm_mea ,data = dados.amost5[-c(C27L79, C28L79),]))
summary(lm(incre_1417 ~ I(1 - i_tamimov), data = dados.amost5))

limiar = 30
for (i in 1:length(dados.amost5$areakm_mea)){
  if (dados.amost5$areakm_mea[i] <= limiar){
    dados.amost5$i_tamimov2[i] = 1 - (dados.amost5$areakm_mea[i]/limiar)
  }
  else {
    dados.amost5$i_tamimov2[i] = 0
  }
}

length(subset(dados.amost5,dados.amost5$id != "C27L79" & dados.amost5$id !="C28L79"))
      
length(dados.amost5[dados.amost5$id != "C27L79" & dados.amost5$id !="C28L79",])

C27L79 = which(dados.amost5$id== "C27L79")
C28L79 = which(dados.amost5$id== "C28L79")
length(dados.amost5[-c(C27L79, C28L79),])

View(dados.amost5@data)

# num_imov
limiar = 14
for (i in 1:length(dados.amost5$num_imov)){
  if (dados.amost5$num_imov[i] <= limiar){
    dados.amost5$i_n_imov[i] = dados.amost5$num_imov[i]/limiar
  }
  else {
    dados.amost5$i_n_imov[i] = 1
  }
}

plot(dados.amost5$num_imov, dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ num_imov, data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ num_imov, data = dados.amost5))

plot((dados.amost5$i_n_imov), dados.amost5$incre_1417, main = paste("R² = ", summary(lm(incre_1417 ~ i_n_imov, data = dados.amost5))$r.squared))
abline(lm(incre_1417 ~ i_n_imov, data = dados.amost5))

