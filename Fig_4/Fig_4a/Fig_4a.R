rm(list = ls())
library(rgdal)
library(raster)
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_4\\Fig_4a')

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
# TEM mean
TEMMeanSlope <- raster('TEM_Mean_Slope.tif')
TEMMeanSlope <- as.data.frame(extract(TEMMeanSlope,valueKendallCoordPt))
colnames(TEMMeanSlope) <- 'TEMMeanSlope'
TEMMeanEWISlope <- raster('TEM_Mean_EWI_Slope.tif')
TEMMeanEWISlope <- as.data.frame(extract(TEMMeanEWISlope,valueKendallCoordPt))
colnames(TEMMeanEWISlope) <- 'TEMMeanEWISlope'
TEMMeanData <- cbind(valueKendallCoord,TEMMeanSlope,TEMMeanEWISlope)
selectIndex <- which((TEMMeanData$TEMMeanEWISlope > -3)&(TEMMeanData$TEMMeanEWISlope < 3))
TEMMeanData <- TEMMeanData[selectIndex,]
selectIndex <- which((TEMMeanData$TEMMeanSlope > quantile(TEMMeanData$TEMMeanSlope,0.025))&(TEMMeanData$TEMMeanSlope < quantile(TEMMeanData$TEMMeanSlope,0.975)))
TEMMeanData <- TEMMeanData[selectIndex,]
##==== Part 1: Prepare data ====##

Breaks <- c(0.0002,0.0004,0.0006,0.0008,0.001,0.0014,0.0018,0.0022)
colorPanal <- c('#f5fdf0','#e0f4db','#ccebc2','#addab9','#7ecbc3','#44b7d2','#308bba',
                '#0669aa','#0e3d83')

##==== Part 2: Draw ====##
TEMMeanSlopeMax <- 0.006
TEMMeanSlopeSeq <- seq(-TEMMeanSlopeMax,TEMMeanSlopeMax,length.out = 101)
TEMMeanEWISlopeSeq <- seq(-3,3,length.out = 101)
figAData <- as.data.frame(matrix(data = 0,nrow = 10000,ncol = 3))
colnames(figAData) <- c('X','Y','Value')
for(i in seq(1,(length(TEMMeanSlopeSeq)-1))){
  for(j in seq(1,(length(TEMMeanEWISlopeSeq)-1))){
    figAData[((i-1)*(length(TEMMeanEWISlopeSeq)-1)+j),1] <- i
    figAData[((i-1)*(length(TEMMeanEWISlopeSeq)-1)+j),2] <- j
    findSet <- which((TEMMeanData$TEMMeanSlope > TEMMeanSlopeSeq[i])&(TEMMeanData$TEMMeanSlope < TEMMeanSlopeSeq[i+1])&
                       (TEMMeanData$TEMMeanEWISlope > TEMMeanEWISlopeSeq[j])&(TEMMeanData$TEMMeanEWISlope < TEMMeanEWISlopeSeq[j+1]))
    figAData[((i-1)*(length(TEMMeanEWISlopeSeq)-1)+j),3] <- length(findSet)
  }
  print(i)
}
figAData$Value <- figAData$Value/sum(figAData$Value)
findSet <- which(figAData$Value == 0)
figAData$Value[findSet] <- NA
# Redistributed color
figAColor <- matrix(data = NA,nrow = nrow(figAData),ncol = 1)
findSet <- which(is.na(figAData$Value))
figAColor[findSet] <- '#f5fdf0'
findSet <- which(figAData$Value <= Breaks[1])
if(length(findSet) > 0){
  figAColor[findSet] <- colorPanal[1]
}
for(j in seq(2,length(Breaks))){
  findSet <- which((figAData$Value > Breaks[j-1])&(figAData$Value <= Breaks[j]))
  if(length(findSet) > 0){
    figAColor[findSet] <- colorPanal[j]
  }
}
findSet <- which(figAData$Value > Breaks[length(Breaks)])
if(length(findSet) > 0){
  figAColor[findSet] <- colorPanal[length(colorPanal)]
}
figAData <- cbind(figAData,figAColor)
# Draw
worldLocationA <- worldLocation[1,]
worldLocationA$X <- (worldLocationA$X + 0.006)/(2*0.006/100)
worldLocationA$Y <- (worldLocationA$Y + 0.003)/(2*0.003/100)
Fig_4a <- ggplot(data = figAData,mapping = aes(x = X,y = Y))+
  geom_tile(fill = figAData$figAColor)+
  geom_hline(yintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_vline(xintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_point(data = worldLocationA,mapping = aes(x = X,y = Y),size = 1.5)+
  geom_point(data = worldLocationA,mapping = aes(x = X,y = Y),color = '#FF6666',size = 0.8)+
  scale_x_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 5),
                     labels = c('-0.006','-0.003','0','0.003','0.006'),expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 7),
                     labels = c('-0.003','-0.002','-0.001','0','0.001','0.002','0.003'),expand = c(0.01,0.01))+
  xlab('Trend of TEM mean')+
  ylab('Trend of composite EWI')+
  theme_custom()
pdf(file = 'Fig_4a.pdf',width = 2.5,height = 2.3)
print(Fig_4a)
dev.off()
##==== Part 2: Draw ====##