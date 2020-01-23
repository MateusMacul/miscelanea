library(raster)

setwd("C:/Users/Mateus/Documents/1_LANDSAT/5_DADOS_SUPORTE/") # Directory with Monthly Rainfall Rasters

awfi = list.files( pattern = '.tif$', full.names = T)
awfi
awfi.stck = stack(awfi) # stacking all files in one Raster* object

stack.name <- names(awfi.stck)[1]

stack.name <- strtrim(stack.name, nchar(stack.name) -7)

writeRaster(awfi.stck, paste0("CBERS4_AWFI/", stack.name , ".tif"),overwrite = T)

writeFormats()
