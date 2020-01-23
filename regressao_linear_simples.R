###### regressão linear #######
reg = lm(nozero$desm14_are ~ nozero$area_vegse, data=nozero)
summary(reg)
plot(reg$model, x_label = "Proporção Desmatada", ylabel = "Proporção de Veg. Secundária")
abline(reg, col = "red")
#reg conflito
reg = lm(amostrada$prop_desm14 ~ amostrada$area_confl, data=amostrada)
summ = summary(reg)
plot(reg$model,  xlab = "Proporção Desmatada", ylab = "Proporção Conflitos", 
     main = paste0("beta = ",  round(reg$coefficients[2],4)," "," r² = ", round(summ$r.squared, 4)))
abline(reg, col = "red")
plot(reg$residuals)
#reg vegetacao secundaria
reg2 = lm(amostrada$prop_desm14 ~ amostrada$area_vegse, data=amostrada)
summ2 = summary(reg2)
plot(reg2$model,  xlab = "Proporção Desmatada", ylab = "Proporção de Veg. Secundária", 
     main = paste0("beta = ",  round(reg2$coefficients[2],4)," "," r² = ", round(summ2$r.squared, 4)))
abline(reg2, col = "red")
plot(reg2$residuals)
# reg Area aberta
reg3 = lm(amostrada$prop_desm14 ~ amostrada$area_pasto, data=amostrada)
summ3 = summary(reg3)
plot(reg3$model,  xlab = "Proporção Desmatada", ylab = "Proporção Área Aberta", 
     main = paste0("beta = ",  round(reg3$coefficients[2],4)," "," r² = ", round(summ3$r.squared, 4)))
abline(reg3, col = "red")
plot(reg3$residuals)
# reg Floresta
reg4 = lm(amostrada$prop_desm14 ~ amostrada$area_flor, data=amostrada)
summ4 = summary(reg4)
plot(reg4$model,  xlab = "Proporção Desmatada", ylab = "Proporção Floresta", 
     main = paste0("beta = ",  round(reg4$coefficients[2],4)," "," r² = ", round(summ4$r.squared, 4)))
abline(reg4, col = "red")
plot(reg4$residuals)
# reg estrada
reg5 = lm(amostrada$prop_desm14 ~ amostrada$estr_dist, data=amostrada)
summ5 = summary(reg5)
plot(reg5$model,  xlab = "Proporção Desmatada", ylab = "Distância Estradas", 
     main = paste0("beta = ",  round(reg5$coefficients[2],4)," "," r² = ", round(summ5$r.squared, 4)))
abline(reg5, col = "red")
plot(reg5$residuals)
# reg degradação
reg6 = lm(amostrada$prop_desm14 ~ amostrada$deg_dist, data=amostrada)
summ6 = summary(reg6)
plot(reg6$model,  xlab = "Proporção Desmatada", ylab = "Distância Degradação", 
     main = paste0("beta = ",  round(reg6$coefficients[2],4)," "," r² = ", round(summ6$r.squared, 4)))
abline(reg6, col = "red")
plot(reg6$residuals)
# reg degradação
reg7 = lm(amostrada$prop_desm14 ~ amostrada$ass_dist, data=amostrada)
summ7 = summary(reg7)
plot(reg7$model,  xlab = "Proporção Desmatada", ylab = "Distância Assentamento", 
     main = paste0("beta = ",  round(reg7$coefficients[2],4)," "," r² = ", round(summ7$r.squared, 4)))
abline(reg7, col = "red")
plot(reg7$residuals)

