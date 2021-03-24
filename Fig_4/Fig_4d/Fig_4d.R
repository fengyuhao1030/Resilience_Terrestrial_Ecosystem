rm(list = ls())
library(rgdal)
library(raster)
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_4\\Fig_4d')

##==== Function ====##
# Custom theme
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(5,5,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(size = 0.2),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.title.y = element_text(size = 10.5,margin = margin(0,3,0,0),face = 'bold',family = 'Times'),
                   axis.title.x = element_text(size = 10.5,margin = margin(4,0,0,0),face = 'bold',family = 'Times'),
                   axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
                   axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'))
  return(myTheme)
}
##==== Function ====##

##==== Part 1: Prepare data ====##
# Kendall
kendallRaster <- raster('..\\Auxiliary data\\Composite_36_Kendall_Ex.tif')
kendallData <- as.matrix(values(kendallRaster))
kendallCoord <- as.data.frame(coordinates(kendallRaster))
colnames(kendallCoord) <- c('Lon','Lat')
findSet <- which((kendallData != -32768)&(kendallData != -30000))
valueKendallCoord <- kendallCoord[findSet,]
valueKendallCoordPt <- SpatialPoints(coords = valueKendallCoord,proj4string = CRS('+init=epsg:4326'))
# WorldLocation
worldLocation <- read.csv('..\\Auxiliary data\\WorldLocation.csv')
# PRE CV
PRECVSlope <- raster('PRE_CV_Slope.tif')
PRECVSlope <- as.data.frame(extract(PRECVSlope,valueKendallCoordPt))
colnames(PRECVSlope) <- 'PRECVSlope'
PRECVEWISlope <- raster('PRE_CV_EWI_Slope.tif')
PRECVEWISlope <- as.data.frame(extract(PRECVEWISlope,valueKendallCoordPt))
colnames(PRECVEWISlope) <- 'PRECVEWISlope'
PRECVData <- cbind(valueKendallCoord,PRECVSlope,PRECVEWISlope)
selectIndex <- which((PRECVData$PRECVEWISlope > -3)&(PRECVData$PRECVEWISlope < 3))
PRECVData <- PRECVData[selectIndex,]
selectIndex <- which((PRECVData$PRECVSlope > quantile(PRECVData$PRECVSlope,0.025))&(PRECVData$PRECVSlope < quantile(PRECVData$PRECVSlope,0.975)))
PRECVData <- PRECVData[selectIndex,]
##==== Part 1: Prepare data ====##

Breaks <- c(0.0002,0.0004,0.0006,0.0008,0.001,0.0014,0.0018,0.0022)
colorPanal <- c('#f5fdf0','#e0f4db','#ccebc2','#addab9','#7ecbc3','#44b7d2','#308bba',
                '#0669aa','#0e3d83')

##==== Part 2: Draw ====##
PRECVSlopeMax <- 0.002
PRECVSlopeSeq <- seq(-PRECVSlopeMax,PRECVSlopeMax,length.out = 101)
PRECVEWISlopeSeq <- seq(-3,3,length.out = 101)
figDData <- as.data.frame(matrix(data = 0,nrow = 10000,ncol = 3))
colnames(figDData) <- c('X','Y','Value')
for(i in seq(1,(length(PRECVSlopeSeq)-1))){
  for(j in seq(1,(length(PRECVEWISlopeSeq)-1))){
    figDData[((i-1)*(length(PRECVEWISlopeSeq)-1)+j),1] <- i
    figDData[((i-1)*(length(PRECVEWISlopeSeq)-1)+j),2] <- j
    findSet <- which((PRECVData$PRECVSlope > PRECVSlopeSeq[i])&(PRECVData$PRECVSlope < PRECVSlopeSeq[i+1])&
                       (PRECVData$PRECVEWISlope > PRECVEWISlopeSeq[j])&(PRECVData$PRECVEWISlope < PRECVEWISlopeSeq[j+1]))
    figDData[((i-1)*(length(PRECVEWISlopeSeq)-1)+j),3] <- length(findSet)
  }
  print(i)
}
figDData$Value <- figDData$Value/sum(figDData$Value)
findSet <- which(figDData$Value == 0)
figDData$Value[findSet] <- NA
# Redistributed color
figDColor <- matrix(data = NA,nrow = nrow(figDData),ncol = 1)
findSet <- which(is.na(figDData$Value))
figDColor[findSet] <- '#f5fdf0'
findSet <- which(figDData$Value <= Breaks[1])
if(length(findSet) > 0){
  figDColor[findSet] <- colorPanal[1]
}
for(j in seq(2,length(Breaks))){
  findSet <- which((figDData$Value > Breaks[j-1])&(figDData$Value <= Breaks[j]))
  if(length(findSet) > 0){
    figDColor[findSet] <- colorPanal[j]
  }
}
findSet <- which(figDData$Value > Breaks[length(Breaks)])
if(length(findSet) > 0){
  figDColor[findSet] <- colorPanal[length(colorPanal)]
}
figDData <- cbind(figDData,figDColor)
# Draw
worldLocationD <- worldLocation[4,]
worldLocationD$X <- (worldLocationD$X + 0.002)/(2*0.002/100)
worldLocationD$Y <- (worldLocationD$Y + 0.003)/(2*0.003/100)
Fig_4d <- ggplot(data = figDData,mapping = aes(x = X,y = Y))+
  geom_tile(fill = figDData$figDColor)+
  geom_hline(yintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_vline(xintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_point(data = worldLocationD,mapping = aes(x = X,y = Y),size = 1.5)+
  geom_point(data = worldLocationD,mapping = aes(x = X,y = Y),color = '#FF6666',size = 0.8)+
  scale_x_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 5),
                     labels = c('-0.002','-0.001','0','0.001','0.002'),expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 7),
                     labels = c('-0.003','-0.002','-0.001','0','0.001','0.002','0.003'),expand = c(0.01,0.01))+
  xlab('Trend of PRE CV')+
  ylab('Trend of composite EWI')+
  theme_custom()
pdf(file = 'Fig_4d.pdf',width = 2.5,height = 2.3)
print(Fig_4d)
dev.off()
##==== Part 2: Draw ====##