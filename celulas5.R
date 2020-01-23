library(ggplot2) #plotar os gráficos
library(cartography) #para plotar os mapas

dados5 <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/celulas/celulas5_incre1417_landmetric.shp")
summary(dados5)
# deletando colunas q não serão usadas
drop = c("IJI_0","SHEI", "SIEI" )
dados5 <-dados5[,!(names(dados5) %in% drop)]
names(dados5)
#pegando os dados que não tem zero incre1417
nozero5 <- dados5[dados5$incre1417 != 0,]
nozero5.df = as.data.frame(nozero5) # transformando para data frame
summary(nozero5incre1417)
str(dados5@data)
summary(nozero5.df)
# plotando e criando os arquivos jpg de cada gráfico
for(i in 4:ncol(nozero5.df)){
  x = nozero5.df[,i]
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/dispersoes/celulas5/",paste0(names(nozero5.df)[i]),".jpeg"))
  print(ggplot(data = nozero5.df, aes(y = incre1417, x = x )) + 
          geom_point() +
          geom_smooth(method = "lm" ) +
          xlab(names(nozero5.df[i]))+
          ylab("incre1417"))
  dev.off()
}
# plotando mapa das celulas ####
metodo = "fisher-jenks"
classes = 10
pal = carto.pal("blue.pal",5,"orange.pal",5) #lembrar de mudar os numero de cores de acordo com o tamanho da classe
tema = "blue.pal"


for(i in 4:ncol(nozero5.df)){
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/dispersoes/celulas5/mapa_",paste0(names(nozero5.df)[i]),".jpeg"))
  titulo = paste0(names(nozero5.df[i]))
  variavel = paste0(names(nozero5.df[i]))
  choroLayer(spdf = dados5, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.pos = "topleft",legend.values.rnd = 2)
  layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
  plot(incremento, add = T)
  dev.off()
}



