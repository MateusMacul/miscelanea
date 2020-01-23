rm(list=ls()) #limpar environment

library(ggplot2)
library (lmtest) #para teste Breusch-Pagan


#ENTRADA DAS TABELAS/VARIÁVEIS

xises = read.csv("BANCO DE DADOS/DISSERT/VAR_15td-NTL.csv")
yises = read.csv("BANCO DE DADOS/DISSERT/VAR_15td-INDIC.csv")

resultado = data.frame()
linha = 1
for(i in 1:12){
  x = xises[,i]
  for(k in 1:69){
    y = yises[,k]
    
    print(linha)
    
    resultado[linha,"relação"]=paste0(names(xises)[i],"/",names(yises)[k])
    
    dataso = data.frame(x=x, y=y)
    
    ######REGRESSÃO LINEAR SIMPLES######
    
    reg = lm(y ~ x, data = dataso)  
    
    resultado[linha,"r2"]=summary(reg)$r.squared
    resultado[linha,"r2_ajustado"]=summary(reg)$adj.r.squared
    #resultado[linha,"pvalor"]=summary(reg)$
    
    #TESTE DE NORMALIDADE 
    #Shapiro-Wilk
    resultado[linha, "shapiro"] = shapiro.test(reg$residuals)$p.value
    
    #TESTE DE HETEROCEDASTICIDADE - Breusch-Pagan (H0=homocedasticidade)
    resultado[linha, "hetero"] = bptest(reg$residuals ~ y, data = dataso)$p.value
    
    #TESTE DE AUTOCORRELAÇÃO (DEPENDÊNCIA - Durbin-Watson)
    #p = 0 - d ~ 2 (sem autocor); p = 1 - d ~ 0 (autocor pos); p = 1 - d ~ 4 (autocor neg)
    resultado[linha, "durbin"] = dwtest(reg$residuals ~ y, data = dataso, alternative = "two.sided")$p.value
    
    
    #PLOTANDO Y x REDÍDUO PADRONIZADO
    erropadr = (summary(reg))$sigma
    
    jpeg(paste0("BANCO DE DADOS/DISSERT/RESIDUOS15td/",paste0(names(xises)[i],"_",names(yises)[k]),".jpeg"))
    print (plot(y,reg$residuals/erropadr,xlab=names(yises)[k],ylab= "Erro Padrão", main = paste0(names(xises)[i],"/",names(yises)[k])))
    dev.off()
    
    
   linha = linha+1 
  }
  
  
}

View(resultado)

write.csv(resultado,"BANCO DE DADOS/DISSERT/result_reg15tds.csv",sep = ";",row.names = F,dec = ".")


