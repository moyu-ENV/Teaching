rm(list = ls())
library(dplyr)
  
wd <- paste0("C:/Users/Y/Desktop/TCD/projects/ZOU33070/2023") #local
data0 <- read.csv(paste0(wd,'/DOUGHNUTS_Lecture.csv'),stringsAsFactors = FALSE)
  
#t-test for outlet 1 & 2   
if(TRUE){  
data <- data0[which(data0$Outlet%in%c(1,2)),]

  
#col
if(TRUE){
  outList <- unique(data$Outlet)
  
  colorList <-c('red','orange','darkgreen','blue')
  data$col <-NA
  for(ii in 1:length(outList)){
    data[which(data$Outlet==outList[ii]),'col']<-colorList[ii]
  }  
}

#raw data
if(TRUE){
tiff(file = paste0(wd,'/raw.tif'), width = 170, height = 120, units = 'mm', res=300) 
par(oma=c(3,4.5,1.5,1.5),mar=c(0.1,0.1,0.1,0.1),xpd=FALSE,bg='NA') 
layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))

plot(data$Outlet,data$Fat,col=data$col,
     ylim=c(50,100),xlim=c(0.5,2.5),cex=5,
     xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",frame=FALSE,
     xlab='',ylab='')

axis(1,at=c(0.5,2.5),labels=FALSE,tck=0,pos=50)
axis(1,at=c(1,2),labels=c('Outlet 1','Outlet 2'),
     lwd=0,line=0.2,cex.axis=2)
#mtext('Observed damage  ', side=1, line=0.2, cex=0.55)

axis(2,at=seq(50,100,10),labels=FALSE,tck=-0.03,pos=0.5)
axis(2,at=seq(50,100,10),labels=seq(50,100,10),
     lwd=0,line=-0.3,cex.axis=2)
mtext('Fats (g/6 doughnuts) ', side=2, line=2.5, cex=2)

dev.off()
}

#mean and SD data
if(TRUE){
  agg <- data %>%
         group_by(Outlet) %>%
         summarize(mean=mean(Fat),
                   sd=sd(Fat))
  agg$col <- NA
  for(ii in 1:length(outList)){
    agg[which(agg$Outlet==outList[ii]),'col']<-colorList[ii]
  }  
  
  tiff(file = paste0(wd,'/meanSD.tif'), width = 170, height = 120, units = 'mm', res=300) 
  par(oma=c(3,4.5,1.5,1.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
  
  plot(agg$Outlet,agg$mean,col=agg$col,
       ylim=c(50,100),xlim=c(0.5,2.5),cex=10,pch='-',
       xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",frame=FALSE,
       xlab='',ylab='')
  
  arrows(agg$Outlet, agg$mean+agg$sd,agg$Outlet,agg$mean-agg$sd,       
         length=0.00, angle=90, code=3,lwd=15,
         col=adjustcolor(agg$col,alpha.f = 0.7))
  
  axis(1,at=c(0.5,2.5),labels=FALSE,tck=0,pos=50)
  axis(1,at=c(1,2),labels=c('Outlet 1','Outlet 2'),
       lwd=0,line=0.2,cex.axis=2)
  #mtext('Observed damage  ', side=1, line=0.2, cex=0.55)
  
  axis(2,at=seq(50,100,10),labels=FALSE,tck=-0.03,pos=0.5)
  axis(2,at=seq(50,100,10),labels=seq(50,100,10),
       lwd=0,line=-0.3,cex.axis=2)
  mtext('Fats (g/6 doughnuts) ', side=2, line=2.5, cex=2)
  
  dev.off()
}

  
#t test
  
t.test(data$Fat~data$Outlet,var.equal = TRUE)  

#t = -2.0624, df = 10, p-value = 0.06612
}

#ANOVA for outlet 1-4  
if(TRUE){  
data <- data0

#col
if(TRUE){
outList <- unique(data$Outlet)
      
colorList <-c('red','orange','darkgreen','blue')
data$col <-NA
for(ii in 1:length(outList)){
data[which(data$Outlet==outList[ii]),'col']<-colorList[ii]
}  
}
    
#mean and SD data
if(TRUE){
agg <- data %>%
        group_by(Outlet) %>%
        summarize(mean=mean(Fat),
                  sd=sd(Fat))
      agg$col <- NA
for(ii in 1:length(outList)){
agg[which(agg$Outlet==outList[ii]),'col']<-colorList[ii]
}  

#Plot            
tiff(file = paste0(wd,'/meanSD4G.tif'), width = 170, height = 120, units = 'mm', res=300) 
par(oma=c(3,4.5,1.5,1.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
   layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
      
plot(data$Outlet,data$Fat,col=data$col,
     ylim=c(40,100),xlim=c(0.5,4.5),cex=5,
     xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",frame=FALSE,
     xlab='',ylab='')
      
points(agg$Outlet,agg$mean,col=agg$col,cex=10,pch='-')
      
arrows(agg$Outlet, agg$mean+agg$sd,agg$Outlet,agg$mean-agg$sd,       
             length=0.00, angle=90, code=3,lwd=15,
             col=adjustcolor(agg$col,alpha.f = 0.7))
      
axis(1,at=c(0.5,4.5),labels=FALSE,tck=0,pos=40)
axis(1,at=c(1,2,3,4),labels=c('Outlet 1','Outlet 2','Outlet 3','Outlet 4'),
     lwd=0,line=0.2,cex.axis=1.8)

axis(2,at=seq(40,100,10),labels=FALSE,tck=-0.03,pos=0.5)
axis(2,at=seq(40,100,10),labels=seq(40,100,10),
     lwd=0,line=-0.3,cex.axis=2)
mtext('Fats (g/6 doughnuts) ', side=2, line=2.5, cex=2)
abline(h=mean(data$Fat))
dev.off()
}
    
#ANOVA
oneway.test(Fat~Outlet, data=data,var.equal = TRUE )
    
data$Outlet2 <- as.factor(data$Outlet)
aov <- aov(Fat~Outlet2, data=data)
summary(aov)  

TukeyHSD(aov)  

library(agricolae)
  
table <- agricolae::HSD.test(aov, c('Outlet'), group=TRUE)
table$level <- as.numeric(row.names(table))
table <- table[order(table$level),]
print(table)
  
#plot Tukey
if(TRUE){
  tiff(file = paste0(wd,'/meanSDTukey.tif'), width = 170, height = 120, units = 'mm', res=300) 
  par(oma=c(3,4.5,1.5,1.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
  
  plot(data$Outlet,data$Fat,col=NA,
       ylim=c(40,100),xlim=c(0.5,4.5),cex=5,
       xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",frame=FALSE,
       xlab='',ylab='')
  
  points(agg$Outlet,agg$mean,col=agg$col,cex=10,pch='-')
  
  arrows(agg$Outlet, agg$mean+agg$sd,agg$Outlet,agg$mean-agg$sd,       
         length=0.00, angle=90, code=3,lwd=15,
         col=adjustcolor(agg$col,alpha.f = 0.7))
  
  text(agg$Outlet,rep(98,4),table$groups,cex=1.5)
  
  axis(1,at=c(0.5,4.5),labels=FALSE,tck=0,pos=40)
  axis(1,at=c(1,2,3,4),labels=c('Outlet 1','Outlet 2','Outlet 3','Outlet 4'),
       lwd=0,line=0.2,cex.axis=1.8)
  #mtext('Observed damage  ', side=1, line=0.2, cex=0.55)
  
  axis(2,at=seq(40,100,10),labels=FALSE,tck=-0.03,pos=0.5)
  axis(2,at=seq(40,100,10),labels=seq(40,100,10),
       lwd=0,line=-0.3,cex.axis=2)
  mtext('Fats (g/6 doughnuts) ', side=2, line=2.5, cex=2)
  abline(h=mean(data$Fat))
  dev.off()
  }
}  








