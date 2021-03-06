library(ggplot2)
### criando o nozero ##
nozero <- dados[dados$incre_1417 != 0,] # pegando s� as celulas com desmatamento dos meu dados originais
nozero.df = as.data.frame(nozero) # transformando para data frame
summary(nozero$incre_1417) 
# plotando e criando os arquivos jpg de cada gr�fico
for(i in 1:ncol(dados.amost5@data)){ # colocando as colunas que eu quero q fa�a a dispers�o
  x = dados.amost5@data[,i] # atribuindo a x a vari�vel independente dos meu dados
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/dispersoes/",paste0(names(dados.amost5@data)[i]),"_amostra.jpeg")) # abrindo um dispositivo "jpg" q vai virar meu arquivo
  print(ggplot(data = dados.amost5@data, aes(y = incre_1417, x = x )) + #determinando os dados e as vari�veis do meu gr�fico
    geom_point() + # dispers�o utilizando pontos
    geom_smooth(method = "lm" ) + # plotando no gr�fico de dispers�o a reta e o smooth da regress�o entre as duas vari�veis em quest�o
    xlab(names(dados.amost5@data[i]))+ # determinando o nome do eixo X
    ylab("incre_1417") + # determinando o nome do eixo Y
    ggtitle("amostrada"))
  dev.off()
}

# Olhando a dispers�o com os dados de maiores incre_1417
nozero.maior02 <- dados[dados$incre_1417 >= 0.2,]
length(nozero.maior02)
nozero.maior02.df = as.data.frame(nozero.maior02) # transformando para data frame
  
for(i in 11:ncol(nozero.maior02.df)){
  x = nozero.maior02.df[,i]
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/dispersoes/",paste0(names(nozero.df)[i]),"_maior02.jpeg"))
  print(ggplot(data = nozero.maior02.df, aes(y = incre_1417, x = x )) + 
          geom_point() +
          geom_smooth(method = "lm" ) +
          xlab(names(nozero.nozero.maior02.df[i]))+
          ylab("incre_1417"))
  dev.off()
}

  # Olhando a dispers�o com os dados de maiores incre_1417
nozero.maior012 <- dados[dados$incre_1417 >= 0.1186828,]
length(nozero.maior012)
nozero.maior012.df = as.data.frame(nozero.maior012) # transformando para data frame

for(i in 11:ncol(nozero.maior012.df)){
  x = nozero.maior012.df[,i]
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/dispersoes/",paste0(names(nozero.df)[i]),"_maior012.jpeg"))
  print(ggplot(data = nozero.maior012.df, aes(y = incre_1417, x = x )) + 
          geom_point() +
          geom_smooth(method = "lm" ) +
          xlab(names(nozero.maior012.df[i]))+
          ylab("incre_1417"))
  dev.off()
}

# plotando pland = 0 x incre_1417 -----------------------------------------
multiplot(
  ggplot()+
    geom_point(aes(y = nozero$incre_1417, x = (nozero$PLAND_0/100)))+
    ylab("Propor��o de Incremento 2014 a 2017")+
    xlab("Propor��o de desmatamento at� 2013")+
    xlim(0,1)+
    ylim(0,1) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")),
  ggplot()+
    geom_point(aes(y = nozero.cort$incre_1417, x = (nozero.cort$PLAND_0/100)))+
    ylab("Propor��o de Incremento 2014 a 2017")+
    xlab("Propor��o de desmatamento at� 2013")+
    xlim(0,1)+
    ylim(0,1) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")),
  cols = 2)




# gr�fico AHP x DIQ -------------------------------------------------------
jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/DIQxIVT.jpg"))
ggplot(dados.ahp@data, aes(y =  sense, x = ahp))+
  geom_point(size = 1, shape = 1)+
  ylab("DIQ")+
  xlab("IVT")+
  xlim(0.25,1)+
  ylim(0,0.008) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))
dev.off()
  

