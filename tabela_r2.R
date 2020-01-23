

# Criando a tabela --------------------------------------------------------

analise_r2 = data.frame(row.names = names(dados.amost5@data))
for (i in 1:ncol(dados.amost5@data)){
  reg = lm(incre_1417 ~ dados.amost5@data[,i], data = dados.amost5)
  analise_r2[i,"r2"]=summary(reg)$r.squared
  analise_r2[i,"r2_ajustado"]=summary(reg)$adj.r.squared
  
  
}
View(analise_r2)
write.csv(analise_r2, file = "C:/Users/Padrao/OneDrive - inpe.br/Tabelas/analise_r2_amost5.csv")

summary(nozero)
analise_r2$var = row.names(analise_r2)
plot(analise_r2$var, analise_r2$r2)
