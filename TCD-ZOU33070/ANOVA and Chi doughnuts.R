rm(list = ls())
library(dplyr)
  
wd <- paste0("C:/Users/Y/Desktop/TCD/projects/ZOU33070/2023") #local
data0 <- read.csv(paste0(wd,'/DOUGHNUTS_Lecture.csv'),stringsAsFactors = FALSE)

########################  
#t-test for outlet 1 & 2   
########################
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

########################
#one way ANOVA for outlet 1-4  
########################
if(TRUE){  
data <- data0
data$Outlet2 <- as.factor(data$Outlet)

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
 
  axis(2,at=seq(40,100,10),labels=FALSE,tck=-0.03,pos=0.5)
  axis(2,at=seq(40,100,10),labels=seq(40,100,10),
       lwd=0,line=-0.3,cex.axis=2)
  mtext('Fats (g/6 doughnuts) ', side=2, line=2.5, cex=2)
  abline(h=mean(data$Fat))
  dev.off()
  }
}  


########################
#two way ANOVA 
########################
if(TRUE){  
data <- data0
data$Outlet2 <- as.factor(data$Outlet)

#col
colorList <-c('red','orange','darkgreen','blue')
  
#summarize data
agg <- data %>%
       group_by(Outlet,Type) %>%
       summarize(mean=mean(Fat),
                sd=sd(Fat))
#Plot
if(TRUE){  
    tiff(file = paste0(wd,'/twoWayANOVA.tif'), width = 170, height = 120, units = 'mm', res=300) 
    par(oma=c(3,4.5,1.5,1.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
    layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
    
    data_plot <- agg[which(agg$Type=='jam') ,]
    plot(data_plot$Outlet,data_plot$mean,col='red',
         ylim=c(40,100),xlim=c(0.5,4.5),cex=2,pch=16,
         xaxs = 'i', yaxs = 'i', yaxt="n",xaxt="n",frame=FALSE,
         xlab='',ylab='')
    lines(data_plot$Outlet,data_plot$mean,lty=2,lwd=2,col='red')
    arrows(data_plot$Outlet, data_plot$mean+data_plot$sd,data_plot$Outlet,data_plot$mean-data_plot$sd,       
           length=0.05, angle=90, code=3,lwd=1.5,
           col=adjustcolor('red',alpha.f = 0.8))
    
    
    data_plot <- agg[which(agg$Type=='custard'),]
    points(data_plot$Outlet,data_plot$mean,col='orange',cex=2,pch=16,)
    lines(data_plot$Outlet,data_plot$mean,lty=3,lwd=2,col='orange')
    arrows(data_plot$Outlet, data_plot$mean+data_plot$sd,data_plot$Outlet,data_plot$mean-data_plot$sd,       
           length=0.05, angle=90, code=3,lwd=1.5,
           col=adjustcolor('orange',alpha.f = 0.8))
    
    axis(1,at=c(0.5,4.5),labels=FALSE,tck=0,pos=40)
    axis(1,at=c(1,2,3,4),labels=c('Outlet 1','Outlet 2','Outlet 3','Outlet 4'),
         lwd=0,line=0.2,cex.axis=1.8)
    
    axis(2,at=seq(40,100,10),labels=FALSE,tck=-0.03,pos=0.5)
    axis(2,at=seq(40,100,10),labels=seq(40,100,10),
         lwd=0,line=-0.3,cex.axis=2)
    mtext('Fats (g/6 doughnuts) ', side=2, line=2.5, cex=2)
    #abline(h=mean(data$Fat))
    dev.off()
  }
  
#ANOVA
aov2way <- aov(Fat ~ Outlet2 * Type, data = data)
summary(aov2way)

#aov2way-noInteraction <- aov(Fat ~ Outlet2 + Type, data = data)
#summary(aov2way-noInteraction)

tuk <- TukeyHSD(aov2way)
print(tuk)

sigLetter <- multcompView ::multcompLetters4(aov2way, tuk)
print(sigLetter)

  
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


########################
#Frequency plot
########################
data_freq <- read.csv(paste0(wd,'/DOUGHNUTS_freq_Lecture.csv'),stringsAsFactors = FALSE)

colorList <-c('yellow','red','orange','brown')
if(TRUE){
  tiff(file = paste0(wd,'/DoughnutsFreq','.tif'), width = 170, height = 120, units = 'mm', res=300) 
  par(oma=c(3,4.5,1.5,4.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
  
  bp <- barplot(data_freq[1:4,'Frequency'], ylim=c(0,400), xlim=c(0,5),
                col=adjustcolor(colorList,alpha.f = 0.8),
                xaxs = 'i', yaxs = 'i', yaxt="n",main='',
                xaxt="n", xlab="", ylab="")
  
  axis(1,at=c(0,15),labels=FALSE,tck=0)
  axis(1,at=bp,labels=data_freq$Category[1:4],lwd=0,line=-0.3,cex.axis=1.3)
 
  axis(2,at=seq(0,400,100),labels=FALSE,tck=-0.03,pos=0)
  axis(2,at=seq(0,400,100),labels=seq(0,400,100),
       lwd=0,line=-0.3,cex.axis=1.5)
  mtext('Count', side=2, line=2.5, cex=2)
  
  par(new=TRUE)
  plot(bp,data_freq$CumulativePercent[1:4],frame=FALSE,typ='l',col=adjustcolor('black',alpha.f = 1),
       ylim=c(0,100), xlim=c(0,5),lwd=3,
       xaxs = 'i', yaxs = 'i', yaxt="n",main='',
       xaxt="n", xlab="", ylab="")
  
  axis(4,at=seq(0,100,50),labels=FALSE,tck=-0.03,pos = 5)
  axis(4,at=seq(0,100,50),labels=seq(0,100,50),
       lwd=0,line=-0.3,cex.axis=1.5)
  mtext('Cumulative %', side=4, line=2.5, cex=2)  
  
  dev.off()
}



###########
#chi
###########
data_chi <- read.csv(paste0(wd,'/DOUGHNUTS_chi_Lecture.csv'),stringsAsFactors = FALSE)

colorList <-c('red','orange')
if(TRUE){
  tiff(file = paste0(wd,'/DoughnutsChi','.tif'), width = 170, height = 120, units = 'mm', res=300) 
  par(oma=c(3,4.5,1.5,4.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
  
  d <- matrix(data_chi$Count, nrow = 2, byrow = FALSE)
  bp <- barplot(height = d, col=adjustcolor(colorList,alpha.f = 0.8),             
          beside = TRUE, xaxs = 'i', yaxs = 'i', yaxt="n",main='',xlim = c(0.5,6),
                xaxt="n", xlab="", ylab="")

  axis(1,at=c(2,5),labels=c('Zoology','Geography'),lwd=0,line=-0.3,cex.axis=1.3)
  
  axis(2,at=seq(0,100,20),labels=FALSE,tck=-0.03,pos=0.5)
  axis(2,at=seq(0,100,20),labels=seq(0,100,20),
       lwd=0,line=-0.3,cex.axis=1.5)
  mtext('Count', side=2, line=2.5, cex=2)
  
  dev.off()
}

chisq.test(d, correct=FALSE)


###########
#wait time
###########
data_wait <- read.csv(paste0(wd,'/DOUGHNUTS_wait_Lecture.csv'),stringsAsFactors = FALSE)

if(TRUE){
  tiff(file = paste0(wd,'/DoughnutsWait-winter','.tif'), width = 170, height = 120, units = 'mm', res=300) 
  par(oma=c(4,4.5,1.5,0.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))

  hist(data_wait[which(data_wait$Season=='winter'),'Time'],xlim = c(0,1),ylim=c(0,250),col='blue',
       xaxs = 'i', yaxs = 'i', yaxt="n",main='',xaxt="n", xlab="", ylab="")
  
  axis(1,at=seq(0,1,0.2),labels=FALSE,tck=-0.03)
  axis(1,at=seq(0,1,0.2),labels=seq(0,60,12),lwd=0,line=-0.3,cex.axis=1.3)
  mtext('Wait time (minute)', side=1, line=2.2, cex=2)
  
  axis(2,at=seq(0,250,50),labels=FALSE,tck=-0.03,pos=0)
  axis(2,at=seq(0,250,50),labels=seq(0,250,50),
       lwd=0,line=-0.3,cex.axis=1.5)
  mtext('Count', side=2, line=2.5, cex=2)
  
  dev.off()
}

data_wait$cat <- 3
cuts<-c(5/60,10/60,20/60)
for(cc in length(cuts):1){
data_wait[which(data_wait$Time<=cuts[cc]),'cat'] <- (cc-1)  
}

table <- table(data_wait$Season,data_wait$cat)

if(TRUE){
  tiff(file = paste0(wd,'/DoughnutsWait-winter-cat','.tif'), width = 170, height = 120, units = 'mm', res=300) 
  par(oma=c(4,4.5,1.5,0.5),mar=c(0.1,0.1,0.1,0.1),lend=2,xpd=FALSE,bg='NA') 
  layout(matrix(seq(1,1,1), 1, 1, byrow = TRUE))
  
  bp <- barplot(table[2,],ylim=c(0,300),xlim=c(0,5),col='blue',
       xaxs = 'i', yaxs = 'i', yaxt="n",main='',xaxt="n", xlab="", ylab="")
  
  #axis(1,at=bp,labels=FALSE,tck=-0.03)
  axis(1,at=bp,labels=seq(0,3,1),lwd=0,line=-0.3,cex.axis=1.3)
  mtext('Wait time (catogory)', side=1, line=2.2, cex=2)
  
  axis(2,at=seq(0,300,50),labels=FALSE,tck=-0.03,pos=0)
  axis(2,at=seq(0,300,50),labels=seq(0,300,50),
       lwd=0,line=-0.3,cex.axis=1.5)
  mtext('Count', side=2, line=2.5, cex=2)
  
  dev.off()
}

chisq.test(data_wait$Season,data_wait$cat, correct=FALSE)
