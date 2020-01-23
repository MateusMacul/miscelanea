## MCWD (Maximum Cumulative Water Deficit) Script ##
# Reference: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2006GL028946 #
# Library
library(raster)

setwd("C:\\Users\\Mateus\\OneDrive - inpe.br") # Directory with Monthly Rainfall Rasters

# List of Monthly Rainfall Rasters

month.rainfall = list.files("CHIRPS_10km", pattern = '.tif$', full.names = T)

chps = stack(month.rainfall) # stacking all files in one Raster* object

#sequence of month
month_seq=rep(seq(1,12,1),18)
year_seq=rep(2002:2019,each=12)

# cut to the number of month of the time series
month_seq=month_seq[1:dim(chps)[3]]
month_seq
year_seq=year_seq[1:dim(chps)[3]]
year_seq

names(chps)=paste("chirps",year_seq,sprintf("%02.0f",month_seq),sep="_")
summary(chps)

# building the monthly water daficit 
wd = stack(month.rainfall)-100 # 100 is the evapotranspiration in mm/month
names(wd)=paste("wd",year_seq,sprintf("%02.0f",month_seq),sep="_")
summary(wd)

# ploting
xlim = c(-73.99037, -43.016526) 
ylim= c(-16.29009, 5.272471)
plot(wd[[grep("2019",names(wd))]], xlim = xlim, ylim = ylim)
hist(wd[[grep("2019",names(wd))]], xlim = c(-110, 700)) 
# removing NA values before runing the function below
m <- c(NA, NA, -9999) # set the vetor of values 'from', 'to' and 'becomes'
rclmat <- matrix(m, ncol=3, byrow=TRUE) # transforming the vector to a matrix
wd.rec <- reclassify(wd, rclmat) # reclassifying using the matrix above

plot(wd.rec[[1]]) 
summary(wd.rec)
# wd[is.na(wd)] <- -9999
# wd[wd == -9999] <- NA
# plot(is.na(wd[[200]]))
# plot(wd[[200]])

# reprojecting the area of interest and cropping 
p <- shapefile("C:\\Users\\Mateus\\OneDrive - inpe.br\\Disciplinas\\TOPICOS_FLORESTAS\\AL_grade_shp\\AmazoniaLegal_Albers2.shp")
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
cwd = calc(wd.rec, fun = mcwd.f)
names(cwd)=paste("cwd",year_seq,sprintf("%02.0f",month_seq),sep="_")
summary(cwd)
# reclassifying to return pixels once -9999 to NA
m <- c(-Inf, -9999, NA) # vector
rclmat <- matrix(m, ncol=3, byrow=TRUE) #vector to matrix 
cwd.rec <- reclassify(cwd, rclmat) # reclassifying
names(cwd.rec)=paste("cwd",year_seq,sprintf("%02.0f",month_seq),sep="_")
summary(cwd.rec)
plot(cwd.rec[[grep("2019",names(wd))]], xlim = xlim, ylim= ylim)


# calculating the anomalies of water deficit
# monthly means
for (i in 1:12){
  # select image of the month i
  tmp=cwd.rec[[ seq(i,dim(cwd.rec)[3],12)]]
  #make the mean of all month i 
  tmp=calc(tmp,mean)
  # stock the mean in an other raster stack TRMM_mth
  if( i == 1) {cwd_mth=tmp} else {cwd_mth=stack(cwd_mth,tmp)}
}

plot(cwd_mth)
dim(cwd_mth)
# monthly standard deviation
for (i in 1:12){
  tmp=cwd.rec[[seq(i,dim(cwd.rec)[3],12)]]
  tmp=calc(tmp,sd)
  if( i == 1) {cwd_sd=tmp}
  if( i > 1) {cwd_sd=stack(cwd_sd,tmp)}
}


plot(cwd_sd==0, xlim = xlim, ylim= ylim)
dim(cwd_sd)
cwd_sd[cwd_sd == 0] <- 1 
plot(cwd_sd, xlim = xlim, ylim= ylim)


# create a raster brick of the same dimension of TRMM where we will stock the anomaly
cwd_anom=cwd.rec
# anomalies
for (i in 1:dim(cwd.rec)[3]){
  # selection the month of the image in position i
  month_tmp=month_seq[i]
  # computation of the anomale and stock in the raster brick TRMM_anom at the position i
  cwd_anom[[i]] = (cwd.rec[[i]]-cwd_mth[[month_tmp]])/cwd_sd[[month_tmp]]
} 
names(cwd_anom)=paste("cwd_anom",year_seq,sprintf("%02.0f",month_seq),sep="_")
plot(is.na(cwd_anom[[grep("2019",names(cwd_anom))]]), xlim = xlim, ylim= ylim)
plot(cwd_anom[[grep("2019",names(cwd_anom))]], xlim = xlim, ylim= ylim)
# criando tabela com as anomalias 
tabela <- data.frame(row.names = names(cwd_anom))
for (i in 1:dim(cwd_anom)[3]){
  tabela$media[i] <- cellStats(cwd_anom[[i]],'mean')
}
tabela$ano <- year_seq
tabela$mes <- month_seq
View(tabela)

p <- ggplot(data=tabela, aes(x=row.names(tabela), y=media)) +
  geom_line() +
  geom_point()
p
ggplotly(p)

write.csv(tabela,file = "Disciplinas/TOPICOS_FLORESTAS/tabelas/cwd_anomalias_amazonia_mensal__2002a2019_2.csv")
# saving each month cw anomaly map
for (i in 1:dim(cwd_anom)[3]) { # Replace 132 by the Total Months of the Time Series
  lyr = cwd_anom[[i]]
  nome.ing = names(cwd_anom[[i]])
  writeRaster(lyr, paste0("./Disciplinas/TOPICOS_FLORESTAS/CWD_anomalias/", nome.ing, ".tif"),overwrite = T) # Create a Folder Called "MCWD" in your Working Directory
}

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

# refazendo só para os anos de 2018 a 2019 --------------------------------

# Applying the funtion only to 2018 and 2019
cwd = calc(wd.rec[[grep("2018|2019",names(wd.rec))]], fun = mcwd.f)

# naming
# sequence of month
month_seq2=rep(seq(1,12,1),2)
year_seq2=rep(2018:2019,each=12)

# cut to the number of month of the time series
month_seq2=month_seq2[1:dim(cwd)[3]]
month_seq2
year_seq2=year_seq2[1:dim(cwd)[3]]
year_seq2

names(cwd)=paste("cwd",year_seq2,sprintf("%02.0f",month_seq2),sep="_")
summary(cwd)
# reclassifying to return pixels once -9999 to NA
m <- c(-Inf, -9999, NA) # vector
rclmat <- matrix(m, ncol=3, byrow=TRUE) #vector to matrix 
cwd.rec <- reclassify(cwd, rclmat) # reclassifying
names(cwd.rec)=names(cwd)
summary(cwd.rec)
plot(cwd.rec[[grep("2019",names(cwd.rec))]], xlim = xlim, ylim= ylim)

