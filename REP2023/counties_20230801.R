library(dplyr)  #need this library for: %>%
library(rgeos)
library(sf)

crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#USADataFrom2017 <- USA_data_CoordAndState %>%
USADataFrom2017 <- all %>%  
  filter(year >= "2017" & ISO3 == "USA") %>%
  select(segment,year,centroid,centerLon,centerLat)
USADataFrom2017$ID <- row.names(USADataFrom2017) #add a unique ID to each event

USADataFrom2017_sp <- USADataFrom2017
#convert to spatial polygon data frame
sp::coordinates(USADataFrom2017_sp) <- ~ centerLon + centerLat
sp::proj4string(USADataFrom2017_sp) <- crs
#raster::plot(USADataFrom2017_sp) #plot to check
USADataFrom2017_sf <- sf::st_as_sf(USADataFrom2017_sp) #conver to sf object 


FloridaCounties <- tigris::counties("Florida") 
FloridaCounties_reproj <- sf::st_transform(FloridaCounties, crs = crs)  #make the projection the same
FloridaCounties_reproj_sp <- as(FloridaCounties_reproj, 'Spatial') #convert to spatial polygon


test <- sp::over(USADataFrom2017_sp , FloridaCounties_reproj_sp)  #both needs to be spatial objects
test$ID <- row.names(test)
test <- test[which(!is.na(test$STATEFP)),]
USADataFrom2017_sp_fl <- USADataFrom2017_sp[which(USADataFrom2017_sp$ID%in%c(unique(test$ID))),] 

raster::plot(USADataFrom2017_sp,col='red') #need to call the raster package to use plot for spatial data
raster::plot(FloridaCounties_reproj_sp,add=TRUE) 
raster::plot(USADataFrom2017_sp_fl,col='blue',add=TRUE) #see if only the FL points are collect

#################################
### !!!! still need to match the county from the test data frame back to original dataframe (i.e., USADataFrom2017)
#####################################


