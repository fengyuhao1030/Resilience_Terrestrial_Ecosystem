rm(list = ls())
library(rgdal)
library(raster)
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_4\\Fig_4c')

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
# PRE mean
PREMeanSlope <- raster('PRE_Mean_Slope.tif')
PREMeanSlope <- as.data.frame(extract(PREMeanSlope,valueKendallCoordPt))
colnames(PREMeanSlope) <- 'PREMeanSlope'
PREMeanEWISlope <- raster('PRE_Mean_EWI_Slope.tif')
PREMeanEWISlope <- as.data.frame(extract(PREMeanEWISlope,valueKendallCoordPt))
colnames(PREMeanEWISlope) <- 'PREMeanEWISlope'
PREMeanData <- cbind(valueKendallCoord,PREMeanSlope,PREMeanEWISlope)
selectIndex <- which((PREMeanData$PREMeanEWISlope > -3)&(PREMeanData$PREMeanEWISlope < 3))
PREMeanData <- PREMeanData[selectIndex,]
selectIndex <- which((PREMeanData$PREMeanSlope > quantile(PREMeanData$PREMeanSlope,0.025))&(PREMeanData$PREMeanSlope < quantile(PREMeanData$PREMeanSlope,0.975)))
PREMeanData <- PREMeanData[selectIndex,]
##==== Part 1: Prepare data ====##

Breaks <- c(0.0002,0.0004,0.0006,0.0008,0.001,0.0014,0.0018,0.0022)
colorPanal <- c('#f5fdf0','#e0f4db','#ccebc2','#addab9','#7ecbc3','#44b7d2','#308bba',
                '#0669aa','#0e3d83')

##==== Part 2: Draw ====##
PREMeanSlopeMax <- 0.04
PREMeanSlopeSeq <- seq(-PREMeanSlopeMax,PREMeanSlopeMax,length.out = 101)
PREMeanEWISlopeSeq <- seq(-3,3,length.out = 101)
figCData <- as.data.frame(matrix(data = 0,nrow = 10000,ncol = 3))
colnames(figCData) <- c('X','Y','Value')
for(i in seq(1,(length(PREMeanSlopeSeq)-1))){
  for(j in seq(1,(length(PREMeanEWISlopeSeq)-1))){
    figCData[((i-1)*(length(PREMeanEWISlopeSeq)-1)+j),1] <- i
    figCData[((i-1)*(length(PREMeanEWISlopeSeq)-1)+j),2] <- j
    findSet <- which((PREMeanData$PREMeanSlope > PREMeanSlopeSeq[i])&(PREMeanData$PREMeanSlope < PREMeanSlopeSeq[i+1])&
                       (PREMeanData$PREMeanEWISlope > PREMeanEWISlopeSeq[j])&(PREMeanData$PREMeanEWISlope < PREMeanEWISlopeSeq[j+1]))
    figCData[((i-1)*(length(PREMeanEWISlopeSeq)-1)+j),3] <- length(findSet)
  }
  print(i)
}
figCData$Value <- figCData$Value/sum(figCData$Value)
findSet <- which(figCData$Value == 0)
figCData$Value[findSet] <- NA
# Redistributed color
figCColor <- matrix(data = NA,nrow = nrow(figCData),ncol = 1)
findSet <- which(is.na(figCData$Value))
figCColor[findSet] <- '#f5fdf0'
findSet <- which(figCData$Value <= Breaks[1])
if(length(findSet) > 0){
  figCColor[findSet] <- colorPanal[1]
}
for(j in seq(2,length(Breaks))){
  findSet <- which((figCData$Value > Breaks[j-1])&(figCData$Value <= Breaks[j]))
  if(length(findSet) > 0){
    figCColor[findSet] <- colorPanal[j]
  }
}
findSet <- which(figCData$Value > Breaks[length(Breaks)])
if(length(findSet) > 0){
  figCColor[findSet] <- colorPanal[length(colorPanal)]
}
figCData <- cbind(figCData,figCColor)
# Draw
worldLocationC <- worldLocation[3,]
worldLocationC$X <- (worldLocationC$X + 0.04)/(2*0.04/100)
worldLocationC$Y <- (worldLocationC$Y + 0.003)/(2*0.003/100)
Fig_4c <- ggplot(data = figCData,mapping = aes(x = X,y = Y))+
  geom_tile(fill = figCData$figCColor)+
  geom_hline(yintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_vline(xintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_point(data = worldLocationC,mapping = aes(x = X,y = Y),size = 1.5)+
  geom_point(data = worldLocationC,mapping = aes(x = X,y = Y),color = '#FF6666',size = 0.8)+
  scale_x_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 5),
                     labels = c('-0.04','-0.02','0','0.02','0.04'),expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 7),
                     labels = c('-0.003','-0.002','-0.001','0','0.001','0.002','0.003'),expand = c(0.01,0.01))+
  xlab('Trend of PRE mean')+
  ylab('Trend of composite EWI')+
  theme_custom()
pdf(file = 'Fig_4c.pdf',width = 2.5,height = 2.3)
print(Fig_4c)
dev.off()
##==== Part 2: Draw ====##