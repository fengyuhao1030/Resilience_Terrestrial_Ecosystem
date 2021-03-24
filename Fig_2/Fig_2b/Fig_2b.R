rm(list = ls())
library(rgdal)
library(raster)
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_2\\Fig_2b')

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
                   axis.title.y = element_text(size = 10.5,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
                   axis.title.x = element_text(size = 10.5,margin = margin(2,0,0,0),face = 'bold',family = 'Times'),
                   axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
                   axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'))
  return(myTheme)
}
##==== Function ====##

##==== Prepare data ====##
kendallRaster <- raster('TEM_Total_Interpret_Ex.tif')
demRaster <- raster('..\\Auxiliary data\\ETOPO1_Bed_RS.tif')
kendallCoordinate <- coordinates(kendallRaster)
kendalValue <- values(kendallRaster)
rasterDataset <- as.data.frame(cbind(coordinates(kendallRaster),values(kendallRaster)))
colnames(rasterDataset) <- c('x','y','Value')
findSet <- which(is.na(rasterDataset$Value)|(rasterDataset$Value == -30000))
rasterDataset <- rasterDataset[-findSet,]
valueDataPt <- SpatialPoints(coords = rasterDataset[,c(1,2)],proj4string = CRS(kendallRaster@crs@projargs))

demEx <- extract(demRaster,valueDataPt)
rasterDataset <- cbind(rasterDataset,demEx)
findSet <- which(is.na(rasterDataset$demEx)|(rasterDataset$demEx < 0)|(rasterDataset$demEx > 5000))
if(length(findSet) > 0){
  rasterDataset <- rasterDataset[-findSet,]
}
##==== Prepare data ====##

##==== Calculate raster value ====##
minLat <- floor((floor(min(rasterDataset$y))+1)/2)*2
maxLat <- floor((ceiling(max(rasterDataset$y))+1)/2)*2
minAlt <- floor(min(rasterDataset$demEx))
maxAlt <- floor(max(rasterDataset$demEx))
latSeq <- seq(-90,90,2)
altSeq <- seq(minAlt,maxAlt,50)
resultMatrix <- as.data.frame(matrix(data = 0,nrow = (length(latSeq)-1)*(length(altSeq)-1),ncol = 3))
colnames(resultMatrix) <- c('Lat','Alt','Value')
for(i in seq(1,(length(latSeq)-1))){
  for(j in seq(1,(length(altSeq)-1))){
    resultMatrix[((i-1)*(length(altSeq)-1)+j),1] <- i
    resultMatrix[((i-1)*(length(altSeq)-1)+j),2] <- j
    tempMinLat <- latSeq[i]
    tempMaxLat <- latSeq[i+1]
    tempMinAlt <- altSeq[j]
    tempMaxAlt <- altSeq[j+1]
    findSet <- which((rasterDataset$y > tempMinLat)&(rasterDataset$y < tempMaxLat)&
                       (rasterDataset$demEx > tempMinAlt)&(rasterDataset$demEx < tempMaxAlt))
    if(length(findSet) > 0){
      resultMatrix[((i-1)*(length(altSeq)-1)+j),3] <- median(rasterDataset$Value[findSet],na.rm = TRUE)
    }else{
      resultMatrix[((i-1)*(length(altSeq)-1)+j),3] <- NA
    }
  }
  showInfo <- paste0(as.character(i),'_',as.character(length(latSeq)-1))
  print(showInfo)
}
##==== Calculate raster value ====##

##==== Draw ====##
Breaks <- c(min(resultMatrix$Value,na.rm = TRUE),100,200,300,400,max(resultMatrix$Value,na.rm = TRUE))
newValues <- resultMatrix$Value
for(j in seq(1,(length(Breaks)-1))){
  findSet <- which((newValues >= Breaks[j])&(newValues <= Breaks[j+1]))
  resultMatrix$Value[findSet] <- j
}
findSet <- which(is.na(resultMatrix$Value))
resultMatrix$Value[findSet] <- 0
resultMatrix$Value <- factor(resultMatrix$Value,levels = c(0,1,2,3,4,5),ordered = TRUE)
# Draw
Fig_2b <- ggplot(data = resultMatrix,mapping = aes(x = Alt,y = Lat))+
  geom_tile(mapping = aes(fill = Value))+
  scale_fill_manual(values = c('#FFFFFF','#f6f581','#f5d56e','#f3a95e','#ed7449','#f8303a'))+
  scale_x_continuous(breaks = c(0,40,80),labels = c('0','2000','4000'),expand = c(0.02,0.02))+
  scale_y_continuous(breaks = c(1,15,30,45,60,75,90),labels = c('-90','-60','-30','0','30','60','90'),expand = c(0.005,0.005))+
  xlab('Elevation (m)')+
  ylab('Latitude')+
  theme_custom()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank())
pdf(file = 'Fig_2b.pdf',width = 1.5,height = 2.64)
print(Fig_2b)
dev.off()
##==== Draw ====##