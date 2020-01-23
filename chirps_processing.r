setwd("C:\\Users\\Mateus\\OneDrive - inpe.br")
list.files("CHIRPS_10km")



library(plotly)
library(ggplot2)
library(raster)
r=raster("3B43.19980101.7A.tif")
plot(r)

# usefull fonction rotate() if longitude from 0 to 360 and not -180 180

# crop an image
extend_new=extent(r)
extend_new@xmin= -80
extend_new@xmax= -40
extend_new@ymin= -20
extend_new@ymax=  10
r_crop=crop(r,extend_new)

plot(r)
plot(extend_new,add=T)
plot(r_crop)

# making the list of image to process
list=list.files()
list
list[grep(".enp",list)]

# remove other files than .tif
list=list[-grep(".enp",list)]
list=list[-grep("_bioma",list)]
#list=list[-grep("_ok",list)]
list=list[-grep("sta.",list)]
list=list[-grep(".hdr",list)]

# croping all images
for (i in 1:length(list)) {
# open the raster i and stock it in a temporary raster
  tmp=raster(list[i])  
# making the crop
  tmp=crop(tmp,extend_new)
# saving the raster and change the name  
writeRaster(tmp,file=paste("D:\\MYWORK2\\DSR\\Aulas\\TRMMcrop\\crop_",list[i],sep=""),overwrite=TRUE)  
}


r_crop=raster("D:\\MYWORK2\\DSR\\Aulas\\TRMMcrop\\crop_3B43.19991001.7A.tif")
plot(r_crop)

# making the raster stack
list=list.files("CHIRPS_10km/", pattern = ".tif$")
list

setwd("D:\\MYWORK2\\DSR\\Aulas\\TRMMcrop\\")
my.Area.tot <- sapply(list, raster) # this is equivalent as a loop
# for all the element of the list sapply will apply the function raster and make a list 
# with all the raster
my.Area.tot 
str(my.Area.tot )
my.Area.tot[[1]]
my.Area.tot[[2]]

# making a brick with all the image, take some time
chps <- stack(month.rainfall) 
# TRMM <- brick(list) 
# TRMM
summary(chps)
plot(chps)

# compute the mean over all the month 
chps_mean=calc(chps, mean)
# compute standard deviation over all the month
chps_sd=calc(chps, sd)
# the calc function apply a function for each pixels times series

op=par(mfrow=c(2,1)) # graphical paramater mfrow divide the graphics windows by nrow=1 ncol=2
plot(chps_mean)
plot(chps_sd)
par(op)

# monthly means
for (i in 1:12){
# select image of the month i
tmp=chps[[ seq(i,dim(chps)[3],12)]]
#make the mean of all month i 
tmp=calc(tmp,mean)
# stock the mean in an other raster stack TRMM_mth
if( i == 1) {chps_mth=tmp} else {chps_mth=stack(chps_mth,tmp)}
}
plot(chps_mth)

# by hand fro january
i=1
tmp=TRMM[[ seq(i,dim(TRMM)[3],12)]]
names(tmp)
tmp=calc(tmp,mean)
plot(tmp,main='mean precipitation January')


# monthly standard deviation
for (i in 1:12){
  tmp=chps[[ seq(i,dim(chps)[3],12)]]
  tmp=calc(tmp,sd)
  if( i == 1) {chps_sd=tmp}
  if( i > 1) {chps_sd=stack(chps_sd,tmp)}
}

plot(chps_sd)

# computation of the anomalies

#sequence of month
month_seq=rep(seq(1,12,1),18)
year_seq=rep(2002:2019,each=12)

# cut to the number of month of the time series
month_seq=month_seq[1:dim(chps)[3]]
month_seq

year_seq=year_seq[1:dim(chps)[3]]
year_seq

# create a raster brick of the same dimension of TRMM where we will stock the anomaly
chps_anom=chps
# anomalies
for (i in 1:dim(chps)[3]){
# selection the month of the image in position i
month_tmp=month_seq[i]
# computation of the anomale and stock in the raster brick TRMM_anom at the position i
 chps_anom[[i]] = (chps[[i]]-chps_mth[[month_tmp]])/chps_sd[[month_tmp]]
} 
plot(chps_anom[[205:214]])
dim(chps_anom)
# change the names of the images in the raster brick
paste("trmm",01,2106,sep="_")
paste("trmm",01,2106,sep=".")
paste("trmm","01","2016",".tif",sep="_")
# paste is very usefull, we can recreate the names of the images easily
# 3B43.19980101.7A.tif
paste("3B43.",year_seq,month_seq,"01",".tif",sep="")
# nice but month 1 is 1 and not 01
sprintf("%02.0f",1) #change 1 to 01
sprintf("%03.0f",1) # 1 to 001

paste("chps",year_seq,sprintf("%02.0f",month_seq),".tif",sep="_")

names(chps)=paste("chirps",year_seq,sprintf("%02.0f",month_seq),sep="_")
names(chps_anom)=paste("chirps_anom",year_seq,sprintf("%02.0f",month_seq),sep="_")

# making some plot with year selection
grep("2016",names(chps_anom))
names(chps_anom)[ grep("2016",names(chps_anom))  ]
library(ggplot2)
plot(chps[[grep("2019",names(chps))]],xlim = c(-80,-40),)
plot(chps_anom[[grep("2019",names(chps))]],xlim = c(-80,-40))
hist(chps_anom[[grep("2019",names(chps_anom))]], xlim = c(-3,3))  
hist(chps_anom[[grep("2019",names(chps_anom))]], xlim = c(-3,3))  
ggplot(chps_anom[[grep("2019",names(chps_anom))]])+
  geom_histogram(aes(values, fill=as.factor(names)))
library(mapview)

mapview(chps_anom[[grep("2019",names(chps_anom))]])  

# criando tabela com a média das anomalias para todo o bioma
tabela <- data.frame(row.names = names(chps_anom))
for (i in 1:dim(chps_anom)[3]){
  tabela$media[i] <- cellStats(chps_anom[[i]],'mean')
  }
tabela$ano <- year_seq
tabela$mes <- month_seq
View(tabela)

p <- ggplot(data=tabela, aes(x=row.names(tabela), y=media)) +
  geom_line() +
  geom_point()
p
ggplotly(p)

write.csv(tabela,file = "Disciplinas/TOPICOS_FLORESTAS/tabelas/chirps_anomalias_amazonia_mensal__2002a2019.csv")
# saving each month anomaly map
for (i in 1:dim(chps_anom)[3]) { # Replace 132 by the Total Months of the Time Series
  lyr = chps_anom[[i]]
  nome.ing = names(chps_anom[[i]])
  writeRaster(lyr, paste0("./Disciplinas/TOPICOS_FLORESTAS/CHIRPS_anomalias/", nome.ing, ".tif"),overwrite = T) # Create a Folder Called "MCWD" in your Working Directory
}


# time series
plot(c(chps[135]),typ="l")
pre=c(chps[135])
library(bfast)
t <- ts(as.numeric(pre), frequency = 12, start = 1998)
fit <- bfast(t, h = 0.23,  max.iter = 1)
plot(fit)
fit

?bfast
plot(harvest, ylab="NDVI")
fit <- bfast(harvest, season="harmonic", max.iter=1)
plot(fit)
fit <- bfast(harvest, season="harmonic", max.iter=1,breaks=2)
plot(fit)

##
f <- system.file("extdata/modisraster.grd", package="bfast")
library("raster")
modisbrick <- brick(f)
data <- as.vector(modisbrick[1])
ndvi <- bfastts(data, dates, type = c("16-day"))
plot(ndvi/10000)

## derive median NDVI of a NDVI raster brick
medianNDVI <- calc(modisbrick, fun=function(x) median(x, na.rm = TRUE))
plot(medianNDVI)

## helper function to be used with the calc() function
xbfastmonitor <- function(x,dates) {
  ndvi <- bfastts(x, dates, type = c("16-day"))
  ndvi <- window(ndvi,end=c(2011,14))/10000
  ## delete end of the time to obtain a dataset similar to RSE paper (Verbesselt et al.,2012)
  bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("ROC"))
  return(cbind(bfm$breakpoint, bfm$magnitude))
}

## apply on one pixel for testing
ndvi <- bfastts(as.numeric(modisbrick[1])/10000, dates, type = c("16-day"))
plot(ndvi)

bfm <- bfastmonitor(data = ndvi, start=c(2010,12), history = c("ROC"))
bfm$magnitude
plot(bfm)
xbfastmonitor(modisbrick[1], dates) ## helper function applied on one pixel

## Not run: 
## apply the bfastmonitor function onto a raster brick
library(raster)
timeofbreak <- calc(chps_anom, fun=function(x){
  res <- t(apply(x, 1, xbfastmonitor, dates))
  return(res)
})

plot(timeofbreak) ## time of break and magnitude of change
plot(timeofbreak,2) ## magnitude of change


