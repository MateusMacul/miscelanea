library(raster)
library(rgdal)



setwd("C:/Users/Padrao/OneDrive - inpe.br/Banco de dados/imagens_LiSS")

img <- raster("COMPOSICAO_oli/O2276520130824.tif")
img
img@crs
hist(img, breaks = "Sturges")
plot(img)
img3 <- img == c(0:3)
img3
img
summary(img, maxsamp=59224191)
summary(img3)
plot(img3)
hist(img, main=NULL)
hist(img, breaks= img==c(0:1))
dataType(img)
mean(img)
summary(raster("COMPOSICAO_oli/O2276520130824.tif", band=1),maxsamp=59224191)
summary(raster("COMPOSICAO_oli/O2276520130824.tif", band=2),maxsamp=59224191)
summary(raster("COMPOSICAO_oli/O2276520130824.tif", band=3),maxsamp=59224191 )
hist(summary(raster("COMPOSICAO_oli/O2276520130824.tif", band=1),maxsamp=59224191)
)
hist(summary(raster("COMPOSICAO_oli/O2276520130824.tif", band=2),maxsamp=59224191)
)
hist(summary(raster("COMPOSICAO_oli/O2276520130824.tif", band=3),maxsamp=59224191 )
)
dataType(raster("COMPOSICAO_oli/O2276520130824.tif", band=1))
