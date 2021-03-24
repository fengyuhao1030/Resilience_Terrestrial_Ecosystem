rm(list = ls())
library(rgdal)
library(raster)
library(ggplot2)
library(agricolae)
setwd('F:\\EWS_0111\\Fig_1\\Fig_1c')

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

##==== Part 1: Prepare data ====##
kendallRaster <- raster('Composite_36_Kendall_Ex.tif')
ecosystemRaster <- raster('..\\Auxiliary data\\Biomes.tif')
ecosystemData <- as.matrix(values(ecosystemRaster))
ecosystemCoord <- as.data.frame(coordinates(ecosystemRaster))
## 1 TrMBF: Tropical and Subtropical Moist Broadleaf Forests
findSet <- which(ecosystemData == 1)
TrMBFCoord <- ecosystemCoord[findSet,]
colnames(TrMBFCoord) <- c('Lon','Lat')
TrMBFCoordPt <- SpatialPoints(coords = TrMBFCoord,proj4string = CRS('+init=epsg:4326'))
TrMBFKendall <- extract(kendallRaster,TrMBFCoordPt)
# Remove NA
delIndex <- which(is.na(TrMBFKendall))
if(length(delIndex) > 0){
  TrMBFKendall <- TrMBFKendall[-delIndex]
}
## 2 TrDBF: Tropical and Subtropical Dry Broadleaf Forests
findSet <- which(ecosystemData == 2)
TrDBFCoord <- ecosystemCoord[findSet,]
colnames(TrDBFCoord) <- c('Lon','Lat')
TrDBFCoordPt <- SpatialPoints(coords = TrDBFCoord,proj4string = CRS('+init=epsg:4326'))
TrDBFKendall <- extract(kendallRaster,TrDBFCoordPt)
# Remove NA
delIndex <- which(is.na(TrDBFKendall))
if(length(delIndex) > 0){
  TrDBFKendall <- TrDBFKendall[-delIndex]
}
## 3 TrCF: Tropical and Subtropical Coniferous Forests
findSet <- which(ecosystemData == 3)
TrCFCoord <- ecosystemCoord[findSet,]
colnames(TrCFCoord) <- c('Lon','Lat')
TrCFCoordPt <- SpatialPoints(coords = TrCFCoord,proj4string = CRS('+init=epsg:4326'))
TrCFKendall <- extract(kendallRaster,TrCFCoordPt)
# Remove NA
delIndex <- which(is.na(TrCFKendall))
if(length(delIndex) > 0){
  TrCFKendall <- TrCFKendall[-delIndex]
}
## 4 TeBF: Temperate Broadleaf and Mixed Forests
findSet <- which(ecosystemData == 4)
TeBFCoord <- ecosystemCoord[findSet,]
colnames(TeBFCoord) <- c('Lon','Lat')
TeBFCoordPt <- SpatialPoints(coords = TeBFCoord,proj4string = CRS('+init=epsg:4326'))
TeBFKendall <- extract(kendallRaster,TeBFCoordPt)
# Remove NA
delIndex <- which(is.na(TeBFKendall))
if(length(delIndex) > 0){
  TeBFKendall <- TeBFKendall[-delIndex]
}
## 5 TeCF: Temperate Coniferous Forests
findSet <- which(ecosystemData == 5)
TeCFCoord <- ecosystemCoord[findSet,]
colnames(TeCFCoord) <- c('Lon','Lat')
TeCFCoordPt <- SpatialPoints(coords = TeCFCoord,proj4string = CRS('+init=epsg:4326'))
TeCFKendall <- extract(kendallRaster,TeCFCoordPt)
# Remove NA
delIndex <- which(is.na(TeCFKendall))
if(length(delIndex) > 0){
  TeCFKendall <- TeCFKendall[-delIndex]
}
## 6 BoFT: Boreal Forests/Taiga
findSet <- which(ecosystemData == 6)
BoFTCoord <- ecosystemCoord[findSet,]
colnames(BoFTCoord) <- c('Lon','Lat')
BoFTCoordPt <- SpatialPoints(coords = BoFTCoord,proj4string = CRS('+init=epsg:4326'))
BoFTKendall <- extract(kendallRaster,BoFTCoordPt)
# Remove NA
delIndex <- which(is.na(BoFTKendall))
if(length(delIndex) > 0){
  BoFTKendall <- BoFTKendall[-delIndex]
}
## 7 TrG: Tropical and subtropical grasslands, savannas, and shrublands
findSet <- which(ecosystemData == 7)
TrGCoord <- ecosystemCoord[findSet,]
colnames(TrGCoord) <- c('Lon','Lat')
TrGCoordPt <- SpatialPoints(coords = TrGCoord,proj4string = CRS('+init=epsg:4326'))
TrGKendall <- extract(kendallRaster,TrGCoordPt)
# Remove NA
delIndex <- which(is.na(TrGKendall))
if(length(delIndex) > 0){
  TrGKendall <- TrGKendall[-delIndex]
}
## 8 TeG: Temperate Grasslands, Savannas, and Shrublands
findSet <- which(ecosystemData == 8)
TeGCoord <- ecosystemCoord[findSet,]
colnames(TeGCoord) <- c('Lon','Lat')
TeGCoordPt <- SpatialPoints(coords = TeGCoord,proj4string = CRS('+init=epsg:4326'))
TeGKendall <- extract(kendallRaster,TeGCoordPt)
# Remove NA
delIndex <- which(is.na(TeGKendall))
if(length(delIndex) > 0){
  TeGKendall <- TeGKendall[-delIndex]
}
## 9 FlG: Flooded Grasslands and Savannas
findSet <- which(ecosystemData == 9)
FlGCoord <- ecosystemCoord[findSet,]
colnames(FlGCoord) <- c('Lon','Lat')
FlGCoordPt <- SpatialPoints(coords = FlGCoord,proj4string = CRS('+init=epsg:4326'))
FlGKendall <- extract(kendallRaster,FlGCoordPt)
# Remove NA
delIndex <- which(is.na(FlGKendall))
if(length(delIndex) > 0){
  FlGKendall <- FlGKendall[-delIndex]
}
## 10 MoG: Montane Grasslands and Shrublands
findSet <- which(ecosystemData == 10)
MoGCoord <- ecosystemCoord[findSet,]
colnames(MoGCoord) <- c('Lon','Lat')
MoGCoordPt <- SpatialPoints(coords = MoGCoord,proj4string = CRS('+init=epsg:4326'))
MoGKendall <- extract(kendallRaster,MoGCoordPt)
# Remove NA
delIndex <- which(is.na(MoGKendall))
if(length(delIndex) > 0){
  MoGKendall <- MoGKendall[-delIndex]
}
## 11 Tu: Tundra
findSet <- which(ecosystemData == 11)
TuCoord <- ecosystemCoord[findSet,]
colnames(TuCoord) <- c('Lon','Lat')
TuCoordPt <- SpatialPoints(coords = TuCoord,proj4string = CRS('+init=epsg:4326'))
TuKendall <- extract(kendallRaster,TuCoordPt)
# Remove NA
delIndex <- which(is.na(TuKendall))
if(length(delIndex) > 0){
  TuKendall <- TuKendall[-delIndex]
}
## 12 MeF: Mediterranean Forests, Woodlands, and Scrub
findSet <- which(ecosystemData == 12)
MeFCoord <- ecosystemCoord[findSet,]
colnames(MeFCoord) <- c('Lon','Lat')
MeFCoordPt <- SpatialPoints(coords = MeFCoord,proj4string = CRS('+init=epsg:4326'))
MeFKendall <- extract(kendallRaster,MeFCoordPt)
# Remove NA
delIndex <- which(is.na(MeFKendall))
if(length(delIndex) > 0){
  MeFKendall <- MeFKendall[-delIndex]
}
## 13 DXS: Deserts and Xeric Shrublands
findSet <- which(ecosystemData == 13)
DXSCoord <- ecosystemCoord[findSet,]
colnames(DXSCoord) <- c('Lon','Lat')
DXSCoordPt <- SpatialPoints(coords = DXSCoord,proj4string = CRS('+init=epsg:4326'))
DXSKendall <- extract(kendallRaster,DXSCoordPt)
# Remove NA
delIndex <- which(is.na(DXSKendall))
if(length(delIndex) > 0){
  DXSKendall <- DXSKendall[-delIndex]
}
## 14 Ma: Mangroves
findSet <- which(ecosystemData == 14)
MaCoord <- ecosystemCoord[findSet,]
colnames(MaCoord) <- c('Lon','Lat')
MaCoordPt <- SpatialPoints(coords = MaCoord,proj4string = CRS('+init=epsg:4326'))
MaKendall <- extract(kendallRaster,MaCoordPt)
# Remove NA
delIndex <- which(is.na(MaKendall))
if(length(delIndex) > 0){
  MaKendall <- MaKendall[-delIndex]
}
##==== Part 1: Prepare data ====##

##==== Part 2: Management data ====##
TrMBFKendall <- data.frame(Value = TrMBFKendall,Class = rep('TrMBF',length(TrMBFKendall)))
TrDBFKendall <- data.frame(Value = TrDBFKendall,Class = rep('TrDBF',length(TrDBFKendall)))
TrCFKendall <- data.frame(Value = TrCFKendall,Class = rep('TrCF',length(TrCFKendall)))
TeBFKendall <- data.frame(Value = TeBFKendall,Class = rep('TeBF',length(TeBFKendall)))
TeCFKendall <- data.frame(Value = TeCFKendall,Class = rep('TeCF',length(TeCFKendall)))
BoFTKendall <- data.frame(Value = BoFTKendall,Class = rep('BoFT',length(BoFTKendall)))
TrGKendall <- data.frame(Value = TrGKendall,Class = rep('TrG',length(TrGKendall)))
TeGKendall <- data.frame(Value = TeGKendall,Class = rep('TeG',length(TeGKendall)))
FlGKendall <- data.frame(Value = FlGKendall,Class = rep('FlG',length(FlGKendall)))
MoGKendall <- data.frame(Value = MoGKendall,Class = rep('MoG',length(MoGKendall)))
TuKendall <- data.frame(Value = TuKendall,Class = rep('Tu',length(TuKendall)))
MeFKendall <- data.frame(Value = MeFKendall,Class = rep('MeF',length(MeFKendall)))
DXSKendall <- data.frame(Value = DXSKendall,Class = rep('DXS',length(DXSKendall)))
MaKendall <- data.frame(Value = MaKendall,Class = rep('Ma',length(MaKendall)))
# Output
resultMatrix_1 <- rbind(TrMBFKendall,TrDBFKendall,TrCFKendall,TeBFKendall,TeCFKendall,BoFTKendall,
                      TrGKendall,TeGKendall,FlGKendall,MoGKendall,
                      TuKendall,MeFKendall,DXSKendall,MaKendall)
delIndex <- which(resultMatrix_1$Value == -30000)
if(length(delIndex) > 0){
  resultMatrix_1 <- resultMatrix_1[-delIndex,]
}
worldKendall <- resultMatrix_1
worldKendall$Class <- 'World'
resultMatrix_2 <- rbind(resultMatrix_1,worldKendall)
##==== Part 2: Management data ====##

##==== Part 3: S & SNK test ====##
tempModel <- aov(Value ~ Class,data = resultMatrix_1)
snkResult <- SNK.test(tempModel,'Class')
# Signal test
Classes <- unique(resultMatrix_2$Class)
errbarMatrix <- data.frame()
for(i in seq(1,length(Classes)))
{
  findSet <- which(resultMatrix_2$Class == Classes[i])
  tempResultMatrix <- resultMatrix_2[findSet,]
  tempResultMatrix <- data.frame(Classes[i],quantile(tempResultMatrix$Value,0.25),
                                 quantile(tempResultMatrix$Value,0.75),
                                 (quantile(tempResultMatrix$Value,0.75)-quantile(tempResultMatrix$Value,0.25)))
  colnames(tempResultMatrix) <- c('Class','Q25','Q75','Value')
  errbarMatrix <- rbind(errbarMatrix,tempResultMatrix)
}
sStatistics <- data.frame()
for(i in seq(1,length(Classes)))
{
  findSet <- which(resultMatrix_2$Class == Classes[i])
  tempResultMatrix <- resultMatrix_2[findSet,]
  s1 <- length(which(tempResultMatrix$Value < 0))
  s2 <- length(which(tempResultMatrix$Value > 0))
  k <- as.numeric(min(s1,s2))
  n <- as.numeric(s1 + s2)
  z <- (s2 - n/2)/sqrt(n/4)
  tempResultMatrix <- data.frame(Class = Classes[i],Sstatistics = z)
  sStatistics <- rbind(sStatistics,tempResultMatrix)
}
# S & SNK test
write.csv(snkResult$group,file = 'KendallSNKtest.csv')
write.csv(sStatistics,file = 'KendallStest.csv',row.names = FALSE)
##==== Part 3: S & SNK test ====##

##==== Part 4: Draw ====##
selectIndex <- which(resultMatrix_2$Class %in% c('TrMBF','TeBF','BoFT','TrG','TeG','MoG','Tu','DXS','World'))
drawMatrix <- resultMatrix_2[selectIndex,]
drawMatrix$Class <- factor(drawMatrix$Class,levels = c('TrMBF','TeBF','BoFT','TrG','TeG','MoG','Tu','DXS','World'),ordered = TRUE)
Fig_1c <- ggplot(data = drawMatrix,mapping = aes(x = Class,y = Value))+
  geom_hline(yintercept = 0,linetype = 'dashed',color = '#000000',size = 0.4)+
  geom_violin(mapping = aes(fill = Class),width = 0.7,size = 0.4,alpha = 0.6)+
  geom_boxplot(mapping = aes(fill = Class),outlier.shape = NA,width = 0.3,size = 0.3)+
  scale_fill_manual(values = c('#028501','#009985','#7ed4c9','#ffc653','#f3f14e','#97afeb','#6584df','#e3be94','#888888'))+
  scale_y_continuous(limits = c(-1000,1200),breaks = seq(-1000,1000,500),
                     labels = c('-1.0','-0.5','0','0.5','1.0'),expand = c(0,0))+
  ylab(expression(bold('Kendall¡¯s '*tau)))+
  theme_custom()+
  theme(axis.title.x = element_blank())
pdf(file = 'Fig_1c.pdf',width = 4.88,height = 2.3)
print(Fig_1c)
dev.off()
##==== Part 4: Draw ====##