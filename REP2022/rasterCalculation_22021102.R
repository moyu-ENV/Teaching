#########
#input
#########
if(TRUE){
#library  
  library(raster)
  library(rgdal)  
  
  library(maptools) #for wrld_simple
  data(wrld_simpl)
  names <- wrld_simpl@data[,c('ISO3','NAME')]
  names$NAME <- as.character(names$NAME)
  
#set storm and tile 

##maria pass Puerto Rico at 2017 09 20
##Puerto Rico is in tile H11V07

#load storm track shape from: https://drive.google.com/drive/u/0/folders/1ryDnQOMUYHFM24kN1QF_ymrs_c8CZENf
track 
  
if(FALSE){
  shp <- readOGR(dsn=paste0(wd,'data/mangrove/byCountryBuffer'),paste0(cntry,'Buffer'))
  
}  

#load before image (downloaded from GEE before) !!how long did the download take? 
image_before0


#download after image !!how long does this take?

if(FALSE){
url<-paste0("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r10/all/shp/year/Year.",year,".ibtracs_all_lines.v03r10.zip")
destfile<-paste0(wd,"Year.",year,".ibtracs_all_lines.v03r10.zip")

download.file(url, destfile)
outDir<-paste0(wd,year,"lines")
unzip(destfile,exdir=outDir)
} 
#which layer?

image_after0


}

###########
#get storm impact
###########
if(TRUE){
#crop the images to make sure the same area
roi #use Puerto Rico as roi   
  
image_before <- mask(image_before0,roi)
image_after <- mask(image_after0,roi)

#calculate present change

image_precent_change <- (image_after-image_before)/ image_before

#output raster
writeRaster(x.r,paste0(wd,'data//ibtracs/byCategory/hurricaneDensity/h3h4h5Density.tif'))

#output pdf to see


}