###### regress�o linear #######
reg = lm(nozero$desm14_are ~ nozero$area_vegse, data=nozero)
summary(reg)
plot(reg$model, x_label = "Propor��o Desmatada", ylabel = "Propor��o de Veg. Secund�ria")
abline(reg, col = "red")
#reg conflito
reg = lm(amostrada$prop_desm14 ~ amostrada$area_confl, data=amostrada)
summ = summary(reg)
plot(reg$model,  xlab = "Propor��o Desmatada", ylab = "Propor��o Conflitos", 
     main = paste0("beta = ",  round(reg$coefficients[2],4)," "," r� = ", round(summ$r.squared, 4)))
abline(reg, col = "red")
plot(reg$residuals)
#reg vegetacao secundaria
reg2 = lm(amostrada$prop_desm14 ~ amostrada$area_vegse, data=amostrada)
summ2 = summary(reg2)
plot(reg2$model,  xlab = "Propor��o Desmatada", ylab = "Propor��o de Veg. Secund�ria", 
     main = paste0("beta = ",  round(reg2$coefficients[2],4)," "," r� = ", round(summ2$r.squared, 4)))
abline(reg2, col = "red")
plot(reg2$residuals)
# reg Area aberta
reg3 = lm(amostrada$prop_desm14 ~ amostrada$area_pasto, data=amostrada)
summ3 = summary(reg3)
plot(reg3$model,  xlab = "Propor��o Desmatada", ylab = "Propor��o �rea Aberta", 
     main = paste0("beta = ",  round(reg3$coefficients[2],4)," "," r� = ", round(summ3$r.squared, 4)))
abline(reg3, col = "red")
plot(reg3$residuals)
# reg Floresta
reg4 = lm(amostrada$prop_desm14 ~ amostrada$area_flor, data=amostrada)
summ4 = summary(reg4)
plot(reg4$model,  xlab = "Propor��o Desmatada", ylab = "Propor��o Floresta", 
     main = paste0("beta = ",  round(reg4$coefficients[2],4)," "," r� = ", round(summ4$r.squared, 4)))
abline(reg4, col = "red")
plot(reg4$residuals)
# reg estrada
reg5 = lm(amostrada$prop_desm14 ~ amostrada$estr_dist, data=amostrada)
summ5 = summary(reg5)
plot(reg5$model,  xlab = "Propor��o Desmatada", ylab = "Dist�ncia Estradas", 
     main = paste0("beta = ",  round(reg5$coefficients[2],4)," "," r� = ", round(summ5$r.squared, 4)))
abline(reg5, col = "red")
plot(reg5$residuals)
# reg degrada��o
reg6 = lm(amostrada$prop_desm14 ~ amostrada$deg_dist, data=amostrada)
summ6 = summary(reg6)
plot(reg6$model,  xlab = "Propor��o Desmatada", ylab = "Dist�ncia Degrada��o", 
     main = paste0("beta = ",  round(reg6$coefficients[2],4)," "," r� = ", round(summ6$r.squared, 4)))
abline(reg6, col = "red")
plot(reg6$residuals)
# reg degrada��o
reg7 = lm(amostrada$prop_desm14 ~ amostrada$ass_dist, data=amostrada)
summ7 = summary(reg7)
plot(reg7$model,  xlab = "Propor��o Desmatada", ylab = "Dist�ncia Assentamento", 
     main = paste0("beta = ",  round(reg7$coefficients[2],4)," "," r� = ", round(summ7$r.squared, 4)))
abline(reg7, col = "red")
plot(reg7$residuals)

