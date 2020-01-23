## MCWD (Maximum Cumulative Water Deficit) Script ##
# Reference: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2006GL028946 #
# Library
library(raster)

setwd("C:\\Users\\Mateus\\Google Drive\\CHIRPS_area_cortada_") # Directory with Monthly Rainfall Rasters

# List of Monthly Rainfall Rasters

month.rainfall = list.files("./", pattern = '.tif$', full.names = T)


wd = stack(month.rainfall)-100 # 100 is the evapotranspiration in mm/month
# reprojecting the area of interest and cropping 
p <- shapefile("C:\\Users\\Mateus\\Dropbox\\Projeto_tapajos\\tap_fires_gis\\BancoQGIS\\Area_corte_Mateus.shp")
p <- spTransform(p, crs(wd))
wd = crop(wd,p)

# MCWD Function
mcwd.f = function(x){
  result= as.numeric(x)
  for(i in 1:length(result)){
    wdn = result[i]
    wdn1 = result[i-1]
    
    if(i==1){
      if(wdn>0){ result[i]=0}
      else{result[i]=wdn}
    }
    
    if(i!=1){
      cwd = wdn1+wdn
      if( cwd < 0){ result[i]=cwd}
      else{result[i]=0}
    }
  }
return(result)  
}


# Applying the Function
cwd = calc(wd, fun = mcwd.f)

str(wd)
str(cwd)
head(cwd)
summary(cwd)
# Determining the Annual MCDW
trimestre =  1 # Start Year of the Temporal Series
nome.trme <- c("1415_DJF","15_MAM", "15_JJA", "15_SON", "1516_DJF","16_MAM", "16_JJA", "16_SON")
for (i in seq(1,24,3)) { # Replace 132 by the Total Months of the Time Series
  cwd.a = cwd[[i:(i+2)]]
  mcwd.a = min(cwd.a)
  
  nome.ing = nome.trme[trimestre]
  # Saving the Annual MCWD
  print(paste0(trimestre, " - ", Sys.time()))
  writeRaster(mcwd.a, paste0("./MCWD/MCWD_", nome.ing, ".tif"),overwrite = T) # Create a Folder Called "MCWD" in your Working Directory
  trimestre = trimestre+1
}
