rm(list = ls())
library(rgdal)
library(raster)
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_4\\Fig_4b')

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
# TEM CV
TEMCVSlope <- raster('TEM_CV_Slope.tif')
TEMCVSlope <- as.data.frame(extract(TEMCVSlope,valueKendallCoordPt))
colnames(TEMCVSlope) <- 'TEMCVSlope'
TEMCVEWISlope <- raster('TEM_CV_EWI_Slope.tif')
TEMCVEWISlope <- as.data.frame(extract(TEMCVEWISlope,valueKendallCoordPt))
colnames(TEMCVEWISlope) <- 'TEMCVEWISlope'
TEMCVData <- cbind(valueKendallCoord,TEMCVSlope,TEMCVEWISlope)
selectIndex <- which((TEMCVData$TEMCVEWISlope > -3)&(TEMCVData$TEMCVEWISlope < 3))
TEMCVData <- TEMCVData[selectIndex,]
selectIndex <- which((TEMCVData$TEMCVSlope > quantile(TEMCVData$TEMCVSlope,0.025))&(TEMCVData$TEMCVSlope < quantile(TEMCVData$TEMCVSlope,0.975)))
TEMCVData <- TEMCVData[selectIndex,]
##==== Part 1: Prepare data ====##

Breaks <- c(0.0002,0.0004,0.0006,0.0008,0.001,0.0014,0.0018,0.0022)
colorPanal <- c('#f5fdf0','#e0f4db','#ccebc2','#addab9','#7ecbc3','#44b7d2','#308bba',
                '#0669aa','#0e3d83')

##==== Part 2: Draw ====##
TEMCVSlopeMax <- 0.001
TEMCVSlopeSeq <- seq(-TEMCVSlopeMax,TEMCVSlopeMax,length.out = 101)
TEMCVEWISlopeSeq <- seq(-3,3,length.out = 101)
figBData <- as.data.frame(matrix(data = 0,nrow = 10000,ncol = 3))
colnames(figBData) <- c('X','Y','Value')
for(i in seq(1,(length(TEMCVSlopeSeq)-1))){
  for(j in seq(1,(length(TEMCVEWISlopeSeq)-1))){
    figBData[((i-1)*(length(TEMCVEWISlopeSeq)-1)+j),1] <- i
    figBData[((i-1)*(length(TEMCVEWISlopeSeq)-1)+j),2] <- j
    findSet <- which((TEMCVData$TEMCVSlope > TEMCVSlopeSeq[i])&(TEMCVData$TEMCVSlope < TEMCVSlopeSeq[i+1])&
                       (TEMCVData$TEMCVEWISlope > TEMCVEWISlopeSeq[j])&(TEMCVData$TEMCVEWISlope < TEMCVEWISlopeSeq[j+1]))
    figBData[((i-1)*(length(TEMCVEWISlopeSeq)-1)+j),3] <- length(findSet)
  }
  print(i)
}
figBData$Value <- figBData$Value/sum(figBData$Value)
findSet <- which(figBData$Value == 0)
figBData$Value[findSet] <- NA
# Redistributed color
figBColor <- matrix(data = NA,nrow = nrow(figBData),ncol = 1)
findSet <- which(is.na(figBData$Value))
figBColor[findSet] <- '#f5fdf0'
findSet <- which(figBData$Value <= Breaks[1])
if(length(findSet) > 0){
  figBColor[findSet] <- colorPanal[1]
}
for(j in seq(2,length(Breaks))){
  findSet <- which((figBData$Value > Breaks[j-1])&(figBData$Value <= Breaks[j]))
  if(length(findSet) > 0){
    figBColor[findSet] <- colorPanal[j]
  }
}
findSet <- which(figBData$Value > Breaks[length(Breaks)])
if(length(findSet) > 0){
  figBColor[findSet] <- colorPanal[length(colorPanal)]
}
figBData <- cbind(figBData,figBColor)
# Draw
worldLocationB <- worldLocation[2,]
worldLocationB$X <- (worldLocationB$X + 0.001)/(2*0.001/100)
worldLocationB$Y <- (worldLocationB$Y + 0.003)/(2*0.003/100)
Fig_4b <- ggplot(data = figBData,mapping = aes(x = X,y = Y))+
  geom_tile(fill = figBData$figBColor)+
  geom_hline(yintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_vline(xintercept = 50,linetype = 'dashed',size = 0.4,color = '#000000')+
  geom_point(data = worldLocationB,mapping = aes(x = X,y = Y),size = 1.5)+
  geom_point(data = worldLocationB,mapping = aes(x = X,y = Y),color = '#FF6666',size = 0.8)+
  scale_x_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 5),
                     labels = c('-0.001','-0.0005','0','0.0005','0.001'),expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(1,100),breaks = seq(1,100,length.out = 7),
                     labels = c('-0.003','-0.002','-0.001','0','0.001','0.002','0.003'),expand = c(0.01,0.01))+
  xlab('Trend of TEM CV')+
  ylab('Trend of composite EWI')+
  theme_custom()
pdf(file = 'Fig_4b.pdf',width = 2.5,height = 2.3)
print(Fig_4b)
dev.off()
##==== Part 2: Draw ====##