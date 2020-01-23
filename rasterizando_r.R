library(raster)
library(rgdal)
setwd("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/2014")
data_source <- readOGR("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/227_65_wgs84_21s_prodes.shp", "227_65_wgs84_21s_prodes")
vetor = readOGR("Assentamentos_Oficial_Clip_wgs84_21s.shp", "Assentamentos_Oficial_Clip_wgs84_21s")

dst_filename <- "assentamentos.tif"

xmin = extent(data_source)[1]
xmax = extent(data_source)[2]
ymin = extent(data_source)[3]
ymax = extent(data_source)[4]

pixel = 30
x_res = round((xmax - xmin)/pixel,0)
y_res = round((ymax - ymin)/pixel,0)
rst  = raster(ncol = x_res, nrow = y_res)
rasterizado = rasterize(vetor, rst)
rasterizado
plot(rasterizado[1])
plot(data_source)
