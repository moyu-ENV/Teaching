###plot
############
rm(list = ls())

library(dplyr)

wd <- "C:/Users/Y/Desktop/CHES/TCNL" #local

fd <- 'formattedWind'
files <- list.files(paste0(wd,'/result/',fd,'/'),'.csv')

dataAll <- read.csv(paste0(wd,'/result/all.csv'),stringsAsFactors = FALSE)


#test
if(FALSE){
ancova_model <- aov(durationMean ~ year * urbanLevel * ISO3, data = dataAll)
summary(ancova_model) # all < 0.001
}

cntList <-unique(dataAll$continent)
isoList <-unique(dataAll$ISO3)

varList <- c("sum","median",'n',"sumArea",'lossP')
mainList <- c('Total outage\nDuartion * Area',
              'Duration mean (days)','Number of Events','Impacted area (pxls)','Not recovered area,\none year (%)')
urList <- unique(dataAll$urbanLevel)
colList <- c(colHDC,colLDC,colRUR)
#col
if(TRUE){
  colHDC <- rgb(40,38,60, maxColorValue = 255)
  colLDC <- rgb(10,151,180, maxColorValue = 255)
  colRUR <- rgb(136,182,195, maxColorValue = 255)
}
cold <- cbind(urList,colList)
colnames(cold) <-c('urbanLevel','col')

#hist plot
if(TRUE){
  tiff(file = paste0(wd,'/result/figure/temporal.tiff'), width =183 , height =200,units = 'mm',res=150) 
  #pdf(file = paste0(wd,'/result/figure/worldMap.pdf'), width =8.5 , height =7) 
  par(oma=c(1,1,1,1),mar=c(3,2,2,0),xpd=FALSE) 
  layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))  
  
  for(pp in 1){
    #pp<-1
    if(pp==1){input<-dataAll
    rg <- 'Global'}
    #prepare data
    if(TRUE){
      output_agg <- input %>%
        group_by(year,urbanLevel) %>%
        summarise(itensityMedian = median(damageMean,na.rm = TRUE),
                  durationMedian = median(durationMean,na.rm = TRUE),
                  areaSum=sum(pxls,na.rm = TRUE),
                  lossSum=sum(loss,na.rm = TRUE),.groups = 'drop')
      
      output_evet <-unique(input[,c('segment','year')])
      output_event_count <- output_evet %>%
        group_by(year) %>%
        tally()
    }
    #bar plots
    varList <- c('itensityMedian','durationMedian','areaSum','lossSum','eventNumber')
    for(vv in 1:5){
      #vv <-1
      var <- varList[vv]  
      mm <- paste0(rg,': ',varList[vv])
      
      if(vv%in%c(1,2,3,4)){
      data_plot0 <- data.frame(urbanLevel=urList)
      for(yy in 2012:2021){
      #yy<-2012  
      data_plot0[,paste0('y',yy)]<- output_agg[which(output_agg$year==yy),var]  
      }
      data_plot <- as.matrix(data_plot0[,2:ncol(data_plot0)])
      bp<-barplot(data_plot,beside=TRUE,col=colList,main=mm,xaxt="n")
      axis(1,at=bp[1,],labels=unique(output_agg$year),lwd=0,tck=0,las=2)}
      
      if(vv%in%c(5)){bp<-barplot(output_event_count$n,col=colList2,main=mm,xaxt="n")
      axis(1,at=bp,labels=unique(output_event_count$year),lwd=0,tck=0,las=2)}
    }
  }
  dev.off()    
}  
 
#map
library(rgdal) #for plat spatial dataframe
worldmap <- rworldmap::getMap(resolution = "coarse")
yearList <- seq(2012,2021,1)

if(TRUE){
  tiff(file = paste0(wd,'/result/figure/worldMap.tiff'), width =183 , height =160,units = 'mm',res=150) 
  #pdf(file = paste0(wd,'/result/figure/worldMap.pdf'), width =8.5 , height =7) 
  par(oma=c(1,1,1,1),mar=c(0,2,3,0),xpd=FALSE) 
  layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE))  
  
  #map
  if(TRUE){
    #plot(mng_shp,add=TRUE,col=adjustcolor(col_mng,alpha.f = alpha),border=NA,lty=1,lwd=1)
    for(yy in c('damageMean','durationMean')){
    for(var in 1:3){
      plot(worldmap,  col="lightgrey",border = 'lightgrey', bg="white", ylim=c(-40,40),xlim=c(-180,180),
           main=paste0('Global, 2012-2021',':',yy),
           xaxs="i",yaxs="i")
      data_plot <- dataAll[which(dataAll$urbanLevel==urList[var]),]
      points(data_plot$centerLon, data_plot$centerLat,pch=21,bg=NA,lwd=1,
             col=adjustcolor(colList[var],alpha.f = 0.05),
             cex=log(data_plot[,yy]))
    }
    }
    #plot(c1_shp,add=TRUE,col=adjustcolor('yellow',alpha.f=0.5))
    #axis(3, at=seq(-180,180,30), labels=seq(-180,180,30), tck=0.03,pos=45)
    #axis(1, at=seq(-180,180,30), labels=seq(-180,180,30), tck=0.03,pos=-45)
    # axis(1,at=c(90),labels=c('90° E'),lwd=0,line=-2.1,cex.axis=cex_axs)
    #axis(2, at=seq(-45,45,15), labels=FALSE, tck=0.02,pos=0)
    #axis(1, at=seq(30,330,30), labels=FALSE, tck=axis_tck1,  pos=0)
  }  
  dev.off()    
  }
############