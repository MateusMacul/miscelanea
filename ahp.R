
### intalações ####

install.packages("xlsx")

### DEPENDENCIAS ####
require(xlsx)
library("xlsx")
#### APH ####
# Deletar colunas q não serão usadas --------------------------------------
  
drop = c("V80")
drop = c(names(dados.ahp@data[,c(80:length(names(dados.ahp)))]))
drop
dados.ahp <- dados.ahp[,!(names(dados) %in% drop)]
str(dados.ahp@data)

#### importando a tabela com a simulacao 25 ####

#simu <- read.xlsx2("C:/Users/Padrao/OneDrive - inpe.br/Tabelas/AHP 6.0 simulacao.xlsx", sheetIndex = 4)
simu <- read.csv2("C:/Users/Padrao/OneDrive - inpe.br/Tabelas/simulado50.csv" )
dados.ahp = dados
View(simu)

simu[c(1:5),c(3:ncol(simu))]
s = 1
while (s <= length(simu$RC)){
  p1 = simu$Ã.rea.Aberta[s] #Area Aberta
  p2 = simu$VegetaÃ.Ã.o.SecundÃ.ria[s] #Vegetação Secundária
  p3 = simu$BR.163[s] #BR 163
  p4 = simu$Proximidade.a.Ã.rea.urbana[s] #Proximidade a área urbana
  p5 = simu$Estradas.vicinais[s] #Estradas vicinais
  p6 = simu$Tamanho.dos.imÃ³veis[s] #Tamanho dos imóveis
  p7 = simu$ImÃ³veis.certificados[s] #Imóveis certificados
  p8 = simu$Declividade[s] #Declividade
  p9 = simu$Rios[s] #Rios
  p10 = simu$Assentamento[s] #Assentamento
  p11 = simu$Embargo[s] #Embargo
  p12 = simu$Floresta[s] #Floresta
  p13 = simu$Unidade.de.ConservaÃ.Ã.o[s] #Unidade de Conservação
  p14 = simu$Terra.IndÃ.gena[s] #Terra Indígena
  
  dados.ahp@data[,ncol(dados.ahp)+1] <- (dados.ahp$i_aberta * p1)+(dados.ahp$i_vegse * p2)+
    (dados.ahp$i_br * p3)+(dados.ahp$i_au * p4)+(dados.ahp$i_est * p5)+
    (dados.ahp$i_tamimov * p6)+(dados.ahp$i_cert * p7)+(dados.ahp$i_decliv * p8)+
    (dados.ahp$i_rios5 * p9)+(dados.ahp$i_ass_p* p10) +(dados.ahp$i_emb* p11) +
    (dados.ahp$i_flor13* p12)+(dados.ahp$i_uc* p13)+(dados.ahp$i_ti* p14)
  
  s = s+1
  
}
str(dados.ahp@data[,c(80:954)])

for(i in 1:length(dados.ahp$id)){
  dados.ahp$q1[i] = quantile(dados.ahp@data[i,c(80:954)], c(0.25))[,1] #SEMPRE CHECAR AS COLUNAS
  dados.ahp$q3[i] = quantile(dados.ahp@data[i,c(80:954)], c(0.75))[,1] #SEMPRE CHECAR AS COLUNAS
 }
dados.ahp$sense = dados.ahp$q3 - dados.ahp$q1

View(dados.ahp@data)







#### importando a tabela com a simulacao 50 em dados2 ####

simu <- read.csv2("C:/Users/Padrao/OneDrive - inpe.br/Tabelas/simulado50.csv" )

View(simu)

s = 1
while (s <= length(simu$RC)){
  p1 = simu$Ã.rea.Aberta[s] #Area Aberta
  p2 = simu$VegetaÃ.Ã.o.SecundÃ.ria[s] #Vegetação Secundária
  p3 = simu$BR.163[s] #BR 163
  p4 = simu$Proximidade.a.Ã.rea.urbana[s] #Proximidade a área urbana
  p5 = simu$Estradas.vicinais[s] #Estradas vicinais
  p6 = simu$Tamanho.dos.imÃ³veis[s] #Tamanho dos imóveis
  p7 = simu$ImÃ³veis.certificados[s] #Imóveis certificados
  p8 = simu$Declividade[s] #Declividade
  p9 = simu$Rios[s] #Rios
  p10 = simu$Assentamento[s] #Assentamento
  p11 = simu$Embargo[s] #Embargo
  p12 = simu$Floresta[s] #Floresta
  p13 = simu$Unidade.de.ConservaÃ.Ã.o[s] #Unidade de Conservação
  p14 = simu$Terra.IndÃ.gena[s] #Terra Indígena
  
  dados2@data[,ncol(dados2)+1] <- (dados2$i_aberta * p1)+(dados2$i_vegse * p2)+
    (dados2$i_br * p3)+(dados2$i_au * p4)+(dados2$i_est * p5)+
    (dados2$i_frag * p6)+(dados2$i_cert * p7)+(dados2$i_decliv * p8)+
    (dados2$i_rios5 * p9)+(dados2$i_ass_p* p10) +(dados2$i_emb* p11) +
    (dados2$i_flor* p12)+(dados2$i_uc* p13)+(dados2$i_ti* p14)
  
  s = s+1
  
}

for(i in 1:length(dados2$id)){
  q1 = quantile(dados2@data[i,c(61:935)], c(0.25))
  q3 = quantile(dados2@data[i,c(61:935)], c(0.75))
  dados2$sense[i] = (q3[,1] - q1[,1])
}
