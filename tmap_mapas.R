install.packages("tmap")

library(rgdal)
library(tmap)

setwd(dir = "C:\\Users\\Queimadas\\Documents\\Alessandro")

cel5 = readOGR("C:\\Users\\Queimadas\\Dropbox\\Projeto_tapajos\\tap_fire_data_codes\\4_grids_new\\cell_5km_allvar_cropped.shp")

tmap_mode("view")
tm_shape(shp = dados)+
  tm_polygons(c("bs_oct15_p", "af_oct15_c"), border.alpha = 0) +
  tm_facets(sync = TRUE, ncol = 2)
