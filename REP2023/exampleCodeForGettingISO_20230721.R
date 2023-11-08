###format data
#############
if(TRUE){
  rm(list = ls())
  library(rworldmap)
  countriesSP <- getMap(resolution = 'high')  
  library(geosphere)
  
  
  
  wd <- "C:/Users/Y/Desktop/CHES/TCNL" #local
  #wd <- "C:/Users/cenv0794/Desktop/ym/CHES/TCNL" #server
  
  ###get country info
  if(TRUE){
    cntry <- read.csv(paste0(wd,'/data/income.csv'),stringsAsFactors = FALSE)
    colnames(cntry) <- c("GroupCode", "GroupName","CountryCode","CountryName" )
    
    data <- read.csv(paste0(wd,'/result/downloaded/NL/',files[ff]),stringsAsFactors = FALSE)  
    if(nrow(data)==0){next} 
    
    points = data.frame(lon=as.numeric(data$centerLon), lat=as.numeric(data$centerLat))
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    indices = over(pointsSP, countriesSP)
    
    data$continent <- indices$REGION  
    data$country <- indices$ADMIN  
    data$ISO3 <-indices$ISO3 
    #data$income <- strsplit(as.character(indices$GBD),',')[[1]][2]
    
    if(sum(is.na(data$ISO3))>0){
      for(rrr in which(is.na(data$ISO3))){
        #rrr <- which(is.na(data$ISO3))[1]
        
        lat <- as.numeric(data[rrr,'centerLat'])
        lon <- as.numeric(data[rrr,'centerLon'])
        
        dist <- data[,c('ISO3','centerLat','centerLon')]
        dist$row <- rownames(data)
        dist$dd <- sqrt((dist[,'centerLat']-lat)^2 + (dist[,'centerLon']-lon)^2) 
        ddMN <- min(dist[which(!is.na(dist$ISO3)),'dd'])
        ddMNrow <- which(dist$dd==ddMN)[1]
        
        #data[ddMNrow[2],'centerLat']
        #data[ddMNrow[2],'centerLon']
        
        data[rrr,'ISO3'] <- data[ddMNrow,'ISO3']
        data[rrr,'continent'] <- data[ddMNrow,'continent']
        data[rrr,'country'] <- data[ddMNrow,'country']  
      }  
      
    }
    
    for (row in 1:nrow(data)){
      #row <- 1  
      if(data[row,'ISO3'] %in% cntry$CountryCode){
        data[row,'income'] <-cntry[which(cntry$CountryCode==data[row,'ISO3']),'GroupCode']
      }
    }   
    
    write.csv(data,paste0(wd,'/result/formatted/',files[ff]),row.names = FALSE)
  }#country
  
  
  
  
}#format data
#############


