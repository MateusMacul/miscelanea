
library("PerformanceAnalytics")
library("car")

# pegando os nomes e números de cada coluna no dado amostrado -------------

indices <- matrix(c(index(names(dados.amost4))),length(names(dados.amost4)),1)
rownames(indices) <- names(dados.amost4)
indices
View(indices)
# procurar indice ####
subset(indices, rownames(indices)=="area_ass")
subset(indices, rownames(indices)== paste0(c("incre_1417", "area_uc", "area_abert","area_vegse", 
                                            "flor_14_p", "flor_17_p", "dis_mn_ass", "dis_mn_deg", 
                                            "dis_mn_uc"), collapse = "|rownames(indices)=="))

colunas_retirar <- c(indices[grep(c(paste(c("id", "col", "row","ass_dist","uc_dist","ti_dist", "au_dist", 
                       "au_dist", "deg_dist", "estr_dist", "tl_sgf_dis", "confl_dis", "area_retir","incre_1417",
                       "flor_14_p", "areakm_med", "incre_14", "incre_15", "incre_16","incre_17", "flor_17_p",
                       "y1",  "CA_0", "TE_0",  "PRD", "NP_0", "ED_0","AWMPFD_0", "PLAND_0","MPAR_0",
                       "BIA_0","LSI_0","dis_mn_br","flor_13_p",
                       "ID_NUM","IJI_0", "SHEI", "SIEI"),collapse = "|")),rownames(indices), ignore.case = T)])
colunas_retirar

View(dados.amost4[,-colunas_retirar]@data)

###correlação entre as variáveis ####

chart.Correlation(amostrada[,-c(1,2,3,4,8,17,28,30)], histogram=TRUE, pch=19, font.labels = 4, cex.labels = NULL)
res1 <- cor.mtest(as.data.frame(dados.amost4@data[,-colunas_retirar]), conf.level = .95)
corrplot(cor(as.data.frame(dados.amost4@data[,-colunas_retirar])), diag = FALSE, order = "FPC",
         tl.pos = "td", tl.cex = 0.8,number.cex = 0.5, method = "number", type = "upper", p.mat = res1$p, insig = "blank")

chart.Correlation(as.data.frame(dados.amost4[,c(11:14,16:21,24:26,28:31,36:42,67)]), histogram=TRUE, pch=19, font.labels = 4, cex.labels = 10)
dados.amost4.df <-as.data.frame(dados.amost4[,c(11:14,16:19,21:22,25:26,30:31,37:38,40:43,67)])
m<-cor(dados.amost4.df)
corrplot(m,method = "number", type = "upper")
m2 <- cor(as.data.frame(dados.amost4[c(11:14,16:21,24:26,28:31,36:42,67)]))
corrplot(m2,method = "number", type = "upper")
m0.96 = cor(as.data.frame(dados.amost4[,c("incre_1417", "area_uc", "area_abert","area_vegse", 
                                          "flor_14_p", "flor_17_p", "dis_mn_ass", "dis_mn_deg", 
                                          "dis_mn_uc", "ahp")]))
corrplot(m0.96,method = "number", type = "upper")

m0.97 = cor(as.data.frame(dados.amost4[,c("incre_1417", "area_uc", "area_vegse", 
                                          "flor_14_p", "flor_17_p", "dis_mn_ass", "dis_mn_deg", 
                                          "dis_mn_uc", "ahp")]))
corrplot(m0.97,method = "number", type = "upper")


### elaborando o modelo #####
#modelo com todos as variáveis puras
fmla <- as.formula(paste("incre_1417 ~ ", paste(c(names(dados.amost4[,-colunas_retirar])), collapse= "+")))
fmla <- as.formula(paste("incre_1417 ~ ", paste(c(names(dados.amost4)[c(11:14,16:21,24:26,28:31,36:42,67)]), collapse= "+")))
reg = lm(fmla, data = dados.amost4)
stepRegression = step (reg, scale = 1, direction= "both")
summary(stepRegression)
summary(reg)

#tirando declividade
reg0.0 = lm(incre_1417 ~ area_confl + area_deg + area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + areakm_mea + num_imov + dis_mn_au + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.0)
#trocando a floresta 14 para floreesta 2013
reg0.0b = lm(incre_1417 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_13_p + areakm_mea + num_imov + dis_mn_au + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.0b)
plot(reg0.0b$fitted.values, reg0.0b$residuals, ylim = c(-1e-15, 1e-15))
plot(dados.amost4$incre_1417, reg0.0b$residuals)
plot(dados.amost4$incre_1417, reg0.0b$fitted.values)


#tirando declividade, area_confl 
reg0.1 = lm(incre_1417 ~ area_deg + area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + areakm_mea + num_imov + dis_mn_au + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.1)

#tirando declividade, area_confl, area_deg 
reg0.2 = lm(incre_1417 ~  area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + areakm_mea + num_imov + dis_mn_au + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.2)

#tirando declividade, area_confl, area_deg, dis_mn_au 
reg0.3 = lm(incre_1417 ~  area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + areakm_mea + num_imov  + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.3)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea 
reg0.4 = lm(incre_1417 ~  area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + num_imov  + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.4)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov 
reg0.5 = lm(incre_1417 ~  area_tlsgf + 
              area_ass + area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.5)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, 
reg0.6 = lm(incre_1417 ~  area_tlsgf + 
              area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.6)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf,
reg0.7 = lm(incre_1417 ~  area_uc + area_ti + area_embar + area_abert + 
              area_vegse + flor_14_p + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.7)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti
reg0.8 = lm(incre_1417 ~  area_uc  + area_embar + area_abert + 
              area_vegse + flor_14_p + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_ti + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.8)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti
reg0.9 = lm(incre_1417 ~  area_uc  + area_embar + area_abert + 
              area_vegse + flor_14_p + 
              dis_mn_br + dis_mn_est + dis_mn_r5 + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.9)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5
reg0.91 = lm(incre_1417 ~  area_uc  + area_embar + area_abert + 
              area_vegse + flor_14_p + 
              dis_mn_br + dis_mn_est + flor_17_p + dis_mn_ass + 
              dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
              ahp, data = dados.amost4)
summary(reg0.91)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5, area_embar
reg0.92 = lm(incre_1417 ~  area_uc  + area_abert + 
               area_vegse + flor_14_p + 
               dis_mn_br + dis_mn_est + flor_17_p + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
               ahp, data = dados.amost4)
summary(reg0.92)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5, area_embar, dis_mn_br 
reg0.93 = lm(incre_1417 ~  area_uc  + area_abert + 
               area_vegse + flor_14_p + dis_mn_est + flor_17_p + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + 
               ahp, data = dados.amost4)
summary(reg0.93)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5, area_embar, dis_mn_br, dis_mn_crt 
reg0.94 = lm(incre_1417 ~  area_uc  + area_abert + 
               area_vegse + flor_14_p + dis_mn_est + flor_17_p + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_uc + 
               ahp, data = dados.amost4)
summary(reg0.94)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5, area_embar, dis_mn_br, dis_mn_crt, dis_mn_est 
reg0.95 = lm(incre_1417 ~  area_uc  + area_abert + 
               area_vegse + flor_14_p + flor_17_p + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_uc + 
               ahp, data = dados.amost4)
summary(reg0.95)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5, area_embar, dis_mn_br, dis_mn_crt, dis_mn_est, dis_mn_cnf 
reg0.96 = lm(incre_1417 ~  area_uc  + area_abert + 
               area_vegse + flor_13_p + flor_17_p + dis_mn_ass + 
               dis_mn_deg + dis_mn_uc + 
               ahp, data = dados.amost4)
summary(reg0.96)

plot(reg0.96$fitted.values, reg0.96$residuals)
plot(dados.amost4$incre_1417, reg0.96$residuals)
plot(dados.amost4$incre_1417, reg0.96$fitted.values)

#tirando declividade, area_confl, area_deg, dis_mn_au, areakm_mea, num_imov, area_ass, area_tlsgf, area_ti, dis_mn_ti, dis_mn_r5, area_embar, dis_mn_br, dis_mn_crt, dis_mn_est, dis_mn_cnf, area_abert
reg0.97 = lm(incre_1417 ~  area_uc  +
               area_vegse + flor_14_p + flor_17_p + dis_mn_ass + 
               dis_mn_deg + dis_mn_uc + 
               ahp, data = dados.amost4)
summary(reg0.97)

#mundando o modelo a aprtir da análise de correlação

# retirados: flor_14_p, flor_17_p, dis_mn_br, area_uc, area_abert, dis_mn_ti
# regressão com os incr_1314 >=q 0.3 para DADOS.AMOST4

dados.amost4.maior = subset(dados.amost4, dados.amost4$incre_1417>= 0.3)
length(dados.amost4.maior)
reg0.98 = lm(y1 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
               area_ass + area_ti + area_embar +
               area_vegse + flor_13_p + areakm_mea + num_imov + dis_mn_au + 
               dis_mn_est + dis_mn_r5 + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + ahp, data = dados.amost4.maior)

summary(reg0.98)

plot(reg0.98$fitted.values, reg0.98$residuals)
plot(dados.amost4.maior$incre_1417, reg0.98$residuals)
plot(dados.amost4.maior$incre_1417, reg0.98$fitted.values)
# regressão com os incr_1314 >=q 0.3 para NOZERO

nozero.maior = subset(nozero, nozero$incre_1417>= 0.3)
length(nozero.maior)
reg0.98.1 = lm(incre_1417 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
               area_ass + area_ti + area_embar +
               area_vegse + flor_13_p + areakm_mea + num_imov + dis_mn_au + 
               dis_mn_est + dis_mn_r5 + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + ahp, data = nozero.maior)

summary(reg0.98.1)
stepRegressiony0.98.1 = step (reg0.98.1, scale = 0, direction= "both")
summary(stepRegressiony0.98.1)



plot(reg0.98.1$fitted.values, reg0.98.1$residuals)
plot(nozero.maior$incre_1417, reg0.98.1$residuals)
plot(nozero.maior$incre_1417, reg0.98.1$fitted.values)


# regressão com os incre_1314 < 0.3
dados.amost4.menor = subset(dados.amost4, dados.amost4$incre_1417< 0.05)
length(dados.amost4.menor)
reg0.99 = lm(incre_1417 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
               area_ass + area_ti + area_embar +dis_mn_br + area_uc +  area_abert + dis_mn_ti +
               area_vegse + flor_13_p + areakm_mea + num_imov + dis_mn_au + 
               dis_mn_est + dis_mn_r5 + dis_mn_ass + 
               dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + ahp, data = dados.amost4.menor)

summary(reg0.99)

plot(reg0.99$fitted.values, reg0.99$residuals)
plot(dados.amost4.menor$incre_1417, reg0.99$residuals)
plot(dados.amost4.menor$incre_1417, reg0.99$fitted.values)
  

# modelo com as métricas de paisagem 

nozero.maior = subset(nozero, nozero$incre_1417>= 0.3)
length(nozero.maior)
reg0.98.2 = lm(incre_1417 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
                 area_ass + area_ti + area_embar +
                 area_vegse + flor_13_p + areakm_mea + num_imov + dis_mn_au + 
                 dis_mn_est + dis_mn_r5 + dis_mn_ass + 
                 dis_mn_cnf + dis_mn_deg + dis_mn_crt + dis_mn_uc + ahp +
                 ED_0 + AWMPFD_0 + PSSD_0 + MPS_0, data = nozerob)

summary(reg0.98.2)
stepRegressiony0.98.2 = step (reg0.98.2, scale = 0, direction= "both")
summary(stepRegressiony0.98.2)
stepped = lm(incre_1417 ~ area_tlsgf + area_ass + area_ti + 
              num_imov + dis_mn_r5 + dis_mn_crt + 
              dis_mn_uc + ED_0 + PSSD_0 + MPS_0, data = nozerob)

summary(stepped)
multiplot(
  ggplot()+
    geom_point(aes(y = stepped$residuals, x = stepped$fitted.values))+
    ggtitle(paste0(stepped$call[2])), # não ficou bom
  ggplot()+
    geom_point(aes(y = nozerob$incre_1417, x = stepped$fitted.values)),
  cols = 1)


plot(reg0.98.2$fitted.values, reg0.98.2$residuals)
plot(nozero$incre_1417, reg0.98.2$residuals)
plot(dados.amost4$incre_1417, reg0.98.2$fitted.values)

# modelo com as métricas de paisagem do destamaento até 2013
reg0.98.3 = lm(incre_1417 ~ med_decliv + area_confl + area_deg + area_tlsgf + 
                 area_ass + area_ti + area_embar + area_uc + area_abert + dis_mn_ti +
                 area_vegse + areakm_mea + num_imov + dis_mn_au + 
                 dis_mn_est + dis_mn_r5 + dis_mn_ass + 
                 dis_mn_cnf + dis_mn_deg + dis_mn_crt +
                 PD_0 + MPS_0 + PSSD_0 + MSI_0 + 
                 AWMSI_0 + MPFD_0 + PSCOV_0 + 
                 TABO_0 + TAOBIA_0 + SHDI, data = dados.amost4)

summary(reg0.98.3)
stepRegressiony0.98.3 = step (reg0.98.3, scale = 0, direction= "both")
summary(stepRegressiony0.98.3)
vif(reg0.98.3)

plot(reg0.98.3$fitted.values, reg0.98.3$residuals)
plot(dados.amost4$incre_1417, reg0.98.3$residuals)
plot(dados.amost4$incre_1417, reg0.98.3$fitted.values)

# teste substituindo no modelo reg0.98.3 da stepwise variveis pela ahp

reg0.98.a = lm(formula = incre_1417 ~ area_deg + ahp +
                 dis_mn_ti + dis_mn_cnf + dis_mn_crt + 
                 PD_0 + MPS_0 + PSSD_0 + MSI_0 + MPFD_0 + TABO_0 + SHDI, data = dados.amost4)

summary(reg0.98.a)
vif(reg0.98)

#teste com y1 com todas as variáveis puras
fmlay <- as.formula(paste("y1 ~ ", paste(c(names(dados.amost4)[c(11:14,16:21,24:26,28:31,36:42,67)]), collapse= "+")))
regy = lm(fmlay, data = dados.amost4)
stepRegressiony = step (regy, scale = 1, direction= "both")
summary(stepRegressiony)
summary(regy)

#tirando variáveis devido a correlação: área aberta, dis_mn_au, dis_mn_br, flor14_p, flor17_p, area_retir, deg_mn_dis 
fmla0 <- as.formula(paste("incre_1417 ~ ", paste(c(names(dados.amost4[,c(11:14,16:19,21,25:26,30:31,37:38,40:43,67)])), collapse= "+")))
reg0 = lm(fmla0, data = dados.amost4)
stepRegression0 = step (reg0, scale = 1, direction= "both")
summary(stepRegression0)
summary(reg0)

# tirando var. deg_mn_dis
fmla0 <- as.formula(paste("incre_1417 ~ ", paste(c(names(dados.amost4[,c(11:14,16:19,21,25:26,30:31,37:38,41:43,67)])), collapse= "+")))
reg0 = lm(fmla0, data = dados.amost4)
stepRegression0 = step (reg0, scale = 1, direction= "both")
summary(stepRegression0)
summary(reg0)

# tirando var. area_ass.
fmla0 <- as.formula(paste("incre_1417 ~ ", paste(c(names(dados.amost4[,c(11:14,17:19,21,25:26,30:31,37:38,41:43,67)])), collapse= "+")))
reg0 = lm(fmla0, data = dados.amost4)
stepRegression0 = step (reg0, scale = 1, direction= "both")
summary(stepRegression0)
summary(reg0)

# tentativa de fazer stepwise forward manualmente

regff  = lm(dados.amost4$incre_1417 ~ dados.amost4$ahp + dados.amost4$flor_17_p +
              dados.amost4$dis_mn_deg + dados.amost4$area_vegse)
summary(regff)
plot(regff$fitted.values, regff$residuals)
plot(dados.amost4$incre_1417, regff$residuals)
plot(dados.amost4$incre_1417, regff$fitted.values)


# modelo só com var indice
fmla_i <- as.formula(paste("incre_1417 ~ ", paste(c(names(dados.amost4)[c(36,43:65)]), collapse= "+")))
reg_i = lm(fmla_i, data = dados.amost4)
stepRegression_i = step (reg_i, scale = 1, direction= "both")
summary(stepRegression_i)
summary(reg_i)

incre_1417 ~ flor_17_p + i_aberta + i_vegse + i_br + i_est + 
  i_au + i_tamimov + i_cert + i_decliv + i_rios5 + i_ass_p + 
  i_emb + i_flor14 + i_uc + i_ti + i_ass_d + i_ti_d + i_uc_d + 
  i_deg_d + i_deg_p + i_cert_d + i_conf_d + i_conf_p + ahp


# tentativas antigas ####
y = dados.amost$y1
x1 = dados.amost$ahp
x2 = dados.amost$i_aberta
x3 = dados.amost$i_vegse
x4 = dados.amost$i_br
x5 = dados.amost$i_est
x6 = dados.amost$i_au
x7 = dados.amost$i_tamimov
x8 = dados.amost$i_cert
x9 = dados.amost$i_decliv
x10 = dados.amost$i_rios5
x11 = dados.amost$i_ass_p
x12 = dados.amost$i_emb
x13 = dados.amost$i_flor14
x14 = dados.amost$i_uc
x15 = dados.amost$i_ti
x16 = dados.amost$
x17 = dados.amost$i_ass_d
x18 = dados.amost$i_ti_d
x19 = dados.amost$i_uc_d
x20 = dados.amost$i_deg_d
x21 = dados.amost$i_deg_p
x22 = dados.amost$i_cert_d
x23 = dados.amost$i_conf_d
x24 = dados.amost$i_conf_p
x25 = dados.amost$areakm_tot


reg = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + 
           x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
           x21 + x22 + x23 + x24 + x25, data = dados.amost)
stepRegression = step (reg, scale = 1, direction= "both")
summary(stepRegression)
summary(reg)
plot(reg$fitted.values,y)
teste1 = lm(y ~ x1 + x2 + x3 + x5 + x7)
plot (teste1$fitted.values,y)
chart.Correlation(reg$model, histogram=TRUE,pch=19, font.labels = 4, cex.labels = NULL)


