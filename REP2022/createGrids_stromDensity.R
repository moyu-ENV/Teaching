#######historic h3-h5
##########
#clear the environment 
rm(list=ls())

#load packages 
library(raster)
library(rgdal)

#set up paths
wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveModel/")


#create a glable layer of grids following the tile of night light data
if(TRUE){
x.r<-extent(-180, 180, -90, 90) #extent of the map
x.r<-raster(x.r)
res(x.r)<-10        #sizeo of the grids, in degree
projection(x.r)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")          #set projection 
x.r[]<-0       #values(x.r) <- x   #initial value of each grid set to zero
plot(x.r)      #plot to check; all the values are 0
}

#get storm frequency of in each grid 
if(TRUE){
  tracks <- list.files(paste0(wd,'data/ibtracs/byCategory/sixhr/',cat[c],'/'))       #get all storm track files 
 
   for (trc in tracks) {
    #trc <- tracks[1]        
    data <- read.csv(paste0(wd,'data/ibtracs/byCategory/sixhr/',cat[c],'/', trc))    #get one of the files
    data$ISO_TIME <- as.POSIXct(data$ISO_TIME,format='%Y-%m-%d %H:%M:%S')            
    data <- data[order(data$ISO_TIME),]
    for(row in 1:nrow(data)){                                                        #for each storm track point, locate onto one grid and add 1 to storm frequency of that grid  
    xy <- c(data$LON[row],data$LAT[row])
    tab <- table(cellFromXY(x.r, xy))
    x.r[as.numeric(names(tab))] <- x.r[as.numeric(names(tab))]+1                     
    }#for row
  }#for tracks

plot(x.r)  #grids should show different value 

writeRaster(x.r,paste0(wd,'data/ibtracs/hurricaneDensity/h1-5mean.tif'))            #save the storm frequency map
}

##########





