##### usando o tmap ####
install.packages("tmap")
library(tmap)

mapa_dados = tm_shape(dados)+tm_fill(col = "ahp", style = "fisher", palette = "Reds")
mapa_dados
mapa_dados + tm_shape(incremento) + tm_borders()

#### explorar a paleta de cores
install.packages("shinyjs")
tmaptools::palette_explorer()
tmap_tip()

tmap_mode("plot")
