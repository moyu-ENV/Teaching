#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).


###########
#plot WM SP by lat long with mangrove 
############

  rm(list = ls())
  library(rworldmap)
  library(rgdal)
  
  worldmap <- getMap(resolution = "coarse")
  
  wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveModel/")
  data0 <- read.csv(paste0(wd,'result/landingWithVar.csv'),stringsAsFactors = FALSE)  
  

  #plot
  if(TRUE){
    pdf(file = paste0(wd,'result/figure/','corr_rmOurlier2','.pdf'), width = 8.5, height = 11) 
    par(oma=c(1,1,1,1),par(mar=c(4,4,3,2)),xpd=TRUE) 
    layout(matrix(seq(1,9,1), 3, 3, byrow = TRUE),heights=c(1))
    
    #map
    if(TRUE){
      plot(worldmap,  col="lightgrey",border = 'lightgrey', bg="white", ylim=c(-40,40),xlim=c(-180,180),xaxs="i",yaxs="i")
        reg_shp <- readOGR(dsn=paste0(wd,'data/basins/basinsPJ'),paste0(reg))
        plot(reg_shp,add=TRUE,border='darkgrey',lty=1,lwd=0.6)

      #points(data$landingLon,data$landingLat,pch=21,col='black',lwd=0.5,bg=adjustcolor(data$col,alpha.f = alpha)) 
        text(35,35,'N Ind',cex=cex_note,pos=4)

    }
    
   
    
    dev.off()  
  }
















