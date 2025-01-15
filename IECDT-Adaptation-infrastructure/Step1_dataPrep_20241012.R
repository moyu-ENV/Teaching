###################
#Task I: data clearning for ibtracs data
###################
rm(list = ls())
wd <- "C:/Users/Y/Desktop/Oxford/projects/IECDT/Lab/" #local

file_input <- "ibtracs.since1980.list.v04r00.csv"
input <- read.csv(paste0(wd,'/data/',file_input),stringsAsFactors = FALSE)


#clear up data set
if(TRUE){
#first row is empty
input1 <- input[-c(1),] 

#change basin NA to NAt
unique(input1$BASIN)
input1[which(is.na(input1$BASIN)),'BASIN'] <- 'NAt'
}

#selected relevant columns   
input2 <- input1[which(input1$WMO_WIND!=" " ),c("SID","SEASON","NUMBER","BASIN","NAME","NATURE","ISO_TIME","LAT","LON","WMO_WIND","WMO_PRES","WMO_AGENCY",
                                                  "DIST2LAND","LANDFALL",'USA_WIND','USA_RMW','REUNION_WIND','REUNION_RMW',
                                                  'BOM_WIND',"BOM_RMW","STORM_SPEED")]
  
#correct for measurements from different agencies
if(TRUE){
file_info <- "MSWAgencies.csv"
info <- read.csv(paste0(wd,'/data/ibtracs/',file_info),stringsAsFactors = FALSE)

input2$WMO_WIND <- as.numeric(input2$WMO_WIND)
input2$WMO_WIND2 <- as.numeric(input2$WMO_WIND)

unique(input2$WMO_AGENCY)
#[1] "wellington"     "reunion"        "bom"            "cphc"           "tokyo"          "hurdat_epa"     "hurdat_atl"     "newdelhi"      
#[9] "nadi"           "atcf"           "tcvightals"     " "              "tcvitals"       "tcgp"           "nhc_working_bt"

for(rr in 1:nrow(info)){
  #rr <- 4  
  prd <- info[rr,'Period'] 
  
  if (prd!=10){
    agency <- info[rr,'Agency']  
    
    input2[which(input2$WMO_AGENCY == agency),'WMO_WIND2'] <- input2[which(input2$WMO_AGENCY == agency),'WMO_WIND']*0.88 
  }  
}
}

output <- input2[,c("SID","SEASON","NUMBER","BASIN","NAME","NATURE","ISO_TIME","LAT","LON","WMO_WIND2","WMO_PRES","STORM_SPEED","DIST2LAND","LANDFALL")]
colnames(output) <- c("SID","SEASON","NUMBER","BASIN","NAME","NATURE","ISO_TIME","LAT","LON","WMO_WIND","WMO_PRES","STORM_SPEED","DIST2LAND","LANDFALL")
  
write.csv(output,paste0(wd, '/data/ibtracs_formatted.csv'),row.names = FALSE)
###################



###################
#Get storm information 
###################
rm(list = ls())
library(geosphere) #for distance betweeen lat lon
wd <- "C:/Users/Y/Desktop/Oxford/projects/IECDT/Lab" #local

file_input <- "blackout.csv"
input <- read.csv(paste0(wd,'/data/',file_input),stringsAsFactors = FALSE)

file_info <- 'ibtracs_formatted.csv'
info <- read.csv(paste0(wd,'/data/',file_info),stringsAsFactors =FALSE)

for(row in 1:nrow(input)){
#row <-1  
if(row%%5000==0)print(row)
sid <- strsplit(input[row,'sid'],'_')[[1]][1] 
landingTime <-strsplit(input[row,'sid'],'_')[[1]][2] 

stormInfo <- info[which(info$SID==sid),]
if(nrow(stormInfo)==0){next}

input[row,'storm'] <- stormInfo$NAME[1]
input[row,'season'] <- stormInfo$SEASON[1]
input[row,'ldyear'] <- substr(landingTime,1,4)
input[row,'ldmo'] <- substr(landingTime,5,6)
input[row,'ldday'] <- substr(landingTime,7,8)
input[row,'ldhr'] <- substr(landingTime,9,10)
input[row,'windmax'] <- max(stormInfo$WMO_WIND,na.rm = TRUE)
input[row,'speed'] <- mean(stormInfo$STORM_SPEED,na.rm = TRUE)
input[row,'distToTrack'] <- min(distm(input[row,c('lon','lat')],stormInfo[,c('LON','LAT')], fun = distHaversine)) #in meter
}

output <- input[which(!is.na(input$storm)),]
write.csv(output,paste0(wd, '/data/blackout_withStorm.csv'),row.names = FALSE)
###################



###################
#get Socio-Economic infomation
###################
rm(list = ls())
library(rworldmap)
countriesSP <- getMap(resolution = 'high')  
crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


wd <- "C:/Users/Y/Desktop/Oxford/projects/IECDT/Lab" #local

file_input <- "blackout_withStorm.csv"
input <- read.csv(paste0(wd,'/data/',file_input),stringsAsFactors = FALSE)

point = data.frame(lon=as.numeric(input$lon), lat=as.numeric(input$lat))
sp::coordinates(point) <- ~ lon + lat
sp::proj4string(point) <- CRS(proj4string(countriesSP))

indices <- over(point, countriesSP)

input$continent <- indices$REGION  
input$country <- indices$ADMIN  
input$iso3 <-indices$ISO3 
input$population <-indices$POP_EST 
input$GDP <-indices$GDP_MD_EST

#how many empty rows？
sum(is.na(input$iso3))

#fill in gaps
for(row in which(is.na(input$iso3))){
  #row <- which(is.na(input$iso3))[1]
  
  lat <- as.numeric(input[row,'lat'])
  lon <- as.numeric(input[row,'lon'])
  
  input$distTemp <- sqrt((input[,'lat']-lat)^2 + (input[,'lon']-lon)^2) 
  ddMNrow <- which(input$distTemp==min(input[which(!is.na(input$iso3)),'distTemp']))[1]
  
  input[row,'iso3'] <- input[ddMNrow,'iso3']
  input[row,'continent'] <- input[ddMNrow,'continent']
  input[row,'country'] <- input[ddMNrow,'country']  
  input[row,'population'] <- input[ddMNrow,'population']
  input[row,'GDP'] <- input[ddMNrow,'GDP']
} 

#how many empty rows now？
sum(is.na(input$iso3))

output <- input[which(!is.na(input$iso3)),]

write.csv(output,paste0(wd, '/data/blackout_withStorm_withSocio.csv'),row.names = FALSE)
################




