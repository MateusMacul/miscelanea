
### plotando as métricas para celulas 2 km

for(i in c(45:60,62:67)){
  jpeg(paste0("C:/Users/Padrao/OneDrive - inpe.br/graficos/dispersoes/mapa_2km_",paste0(names(nozero.df)[i]),".jpeg"))
  titulo = paste0(names(nozero.df[i]))
  variavel = paste0(names(nozero.df[i]))
  choroLayer(spdf = dados, var = variavel, method = metodo, nclass = classes, col = pal, border = FALSE, legend.pos = "topleft",legend.values.rnd = 2)
  layoutLayer(title = titulo, scale = 10,north = TRUE, theme = tema, frame = TRUE, tabtitle = F)
  plot(incremento, add = T)
  dev.off()
}
