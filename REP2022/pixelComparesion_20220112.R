##########
rm(list=ls())

library(raster)
library(rgdal)

wd <- paste0("C:/Users/Y/Desktop/CHES/data/mangroveGiri")

#get observe data
observed_data <- raster(paste0(wd,'/1DegreeRas.tif'))
plot(observe_data)

#get model data
modeled_data <- raster(paste0(wd,'/1DegreeRas.tif'))
plot(model_data)
totalCell <- cellStats(model_data,'sum')


#difference by raster calculation
diff_ras <- observe_data-model_data
plot(diff_ras)


pixels <- data.frame(matrix(nrow=cellStats(model_data,'sum'),ncol=3))
colnames(pixels)<-c('pxl','centerLat','centerLon')


#get grid cell lat lon
row <- 1
for(nCell in 1:totalCell){
#nCell <- 1599
  
if(!is.na(model_data[nCell])& model_data[nCell]==1) { 
points(xyFromCell(model_data,nCell)[1],xyFromCell(model_data,nCell)[2],pch=1, col=adjustcolor('red',alpha.f = 0.1))
pixels[row,'pxl'] <-   nCell
pixels[row,'centerLat'] <-   xyFromCell(model_data,nCell)[2]
pixels[row,'centerLon'] <-   xyFromCell(model_data,nCell)[1]
row <- row +1  
}
}

#get data base on let lon
for(rr in 1:nrow(pixels)){
pixels[rr,'observed']<- raster::extract(observed_data, SpatialPoints(cbind(data[rr,'centerLon'] ,data[rr,'centerLat'])))
pixels[rr,'modeled']<- raster::extract(model_data, SpatialPoints(cbind(data[rr,'centerLon'] ,data[rr,'centerLat'])))
}

pixels[rr,'daysAfterStorm']<-

write.csv(pixels,paste0(wd,'/mngPxl1degree.csv'),row.names = FALSE)

##########


#scatter plot
plot(pixels$observed, pixels$modeled)





writeRaster(map_mng,paste0(wd,'/1DegreeRas.tif'))






