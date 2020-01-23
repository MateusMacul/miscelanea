library("PerformanceAnalytics")
library("car")
# eliminando os dados menores ------------------------------------------------------------------------------

dados.amost4.maior = subset(dados.amost4, dados.amost4$incre_1417>= 0.11205)
dados.amost4.maior %>% length()
# original --------------------------------------------------------------------------------------------------
modi.mais.o <- lm(incre_1417 ~ i_aberta + i_vegse + i_br + i_est + i_au + i_tamimov + i_cert + i_decliv + i_rios5 +
                    i_ass_p + i_emb + i_flor13 + i_uc + i_ti + i_ass_d + i_ti_d + i_uc_d + i_deg_d + i_deg_p +
                    i_cert_d + i_conf_d + i_conf_p + i_d13_d, data = dados.amost4.maior)
summary(modi.mais.o)
vif(modi.mais.o)
# removido devido ao vif: i_br, i_flor13, i_uc_d
# repetindo com os dados cortados modelo com os índices -----------------------------------------------------
modi.mais <- lm(incre_1417 ~ i_aberta + i_vegse + i_est - i_au + i_tamimov + i_cert + i_decliv + i_rios5 +
                  i_ass_p + i_emb + i_uc + i_ti + i_ass_d + i_ti_d + i_deg_d + i_deg_p +
                  i_cert_d + i_conf_d + i_conf_p + i_d13_d + SIDI, data = dados.amost4.maior)
summary(modi.mais)
vif(modi.mais)
modi.mais.step =  step(modi.mais, scale = 0, direction= "both")
summary(modi.mais.step)
modi.mais.step$call
# MOEDLO RESULTANTE -------------------------------------------------------------------------------------------------
modelo.resultante = lm(formula = incre_1417 ~ i_aberta + i_est + i_rios5 + i_emb + i_conf_d + SIDI, data = dados.amost4.maior)