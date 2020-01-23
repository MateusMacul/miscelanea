#### criando uma tabelas com os atributos descritivos de cada variavel

descricao = data.frame(row.names = names(nozero@data[4:ncol(nozero@data)]))
for (g in 4:ncol(nozero@data)){
  i = g - 3
  descricao$min[i] =  min(nozero@data[,g])
  descricao$q1[i] =  quantile(nozero@data[,g], probs = c(0.25))
  descricao$med[i] = median(nozero@data[,g])
  descricao$mea[i] = mean(nozero@data[,g])
  descricao$q3[i] =  quantile(nozero@data[,g], probs = c(0.75))
  descricao$max[i] = max(nozero@data[,g])
}


write.csv(descricao, file = "C:/Users/Padrao/OneDrive - inpe.br/Tabelas/descricao.csv")
summary(nozero)
