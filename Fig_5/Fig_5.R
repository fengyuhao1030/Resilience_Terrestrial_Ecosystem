rm(list = ls())
library(rgdal)
library(raster)
library(earlywarnings)
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_5')

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

##==== Part 1: Prepare Data ====##
ecosystemRaster <- raster('.\\Auxiliary data\\Biomes.tif')
ecosystemData <- as.matrix(values(ecosystemRaster))
ecosystemCoord <- as.data.frame(coordinates(ecosystemRaster))
## 1 TrMBF: Tropical and Subtropical Moist Broadleaf Forests
findSet <- which(ecosystemData == 1)
TrMBFCoord <- ecosystemCoord[findSet,]
colnames(TrMBFCoord) <- c('Lon','Lat')
TrMBFCoordPt <- SpatialPoints(coords = TrMBFCoord,proj4string = CRS('+init=epsg:4326'))
## 2 TrDBF: Tropical and Subtropical Dry Broadleaf Forests
findSet <- which(ecosystemData == 2)
TrDBFCoord <- ecosystemCoord[findSet,]
colnames(TrDBFCoord) <- c('Lon','Lat')
TrDBFCoordPt <- SpatialPoints(coords = TrDBFCoord,proj4string = CRS('+init=epsg:4326'))
## 3 TrCF: Tropical and Subtropical Coniferous Forests
findSet <- which(ecosystemData == 3)
TrCFCoord <- ecosystemCoord[findSet,]
colnames(TrCFCoord) <- c('Lon','Lat')
TrCFCoordPt <- SpatialPoints(coords = TrCFCoord,proj4string = CRS('+init=epsg:4326'))
## 4 TeBF: Temperate Broadleaf and Mixed Forests
findSet <- which(ecosystemData == 4)
TeBFCoord <- ecosystemCoord[findSet,]
colnames(TeBFCoord) <- c('Lon','Lat')
TeBFCoordPt <- SpatialPoints(coords = TeBFCoord,proj4string = CRS('+init=epsg:4326'))
## 5 TeCF: Temperate Coniferous Forests
findSet <- which(ecosystemData == 5)
TeCFCoord <- ecosystemCoord[findSet,]
colnames(TeCFCoord) <- c('Lon','Lat')
TeCFCoordPt <- SpatialPoints(coords = TeCFCoord,proj4string = CRS('+init=epsg:4326'))
## 6 BoFT: Boreal Forests/Taiga
findSet <- which(ecosystemData == 6)
BoFTCoord <- ecosystemCoord[findSet,]
colnames(BoFTCoord) <- c('Lon','Lat')
BoFTCoordPt <- SpatialPoints(coords = BoFTCoord,proj4string = CRS('+init=epsg:4326'))
## 7 TrG: Tropical and subtropical grasslands, savannas, and shrublands
findSet <- which(ecosystemData == 7)
TrGCoord <- ecosystemCoord[findSet,]
colnames(TrGCoord) <- c('Lon','Lat')
TrGCoordPt <- SpatialPoints(coords = TrGCoord,proj4string = CRS('+init=epsg:4326'))
## 8 TeG: Temperate Grasslands, Savannas, and Shrublands
findSet <- which(ecosystemData == 8)
TeGCoord <- ecosystemCoord[findSet,]
colnames(TeGCoord) <- c('Lon','Lat')
TeGCoordPt <- SpatialPoints(coords = TeGCoord,proj4string = CRS('+init=epsg:4326'))
## 9 FlG: Flooded Grasslands and Savannas
findSet <- which(ecosystemData == 9)
FlGCoord <- ecosystemCoord[findSet,]
colnames(FlGCoord) <- c('Lon','Lat')
FlGCoordPt <- SpatialPoints(coords = FlGCoord,proj4string = CRS('+init=epsg:4326'))
## 10 MoG: Montane Grasslands and Shrublands
findSet <- which(ecosystemData == 10)
MoGCoord <- ecosystemCoord[findSet,]
colnames(MoGCoord) <- c('Lon','Lat')
MoGCoordPt <- SpatialPoints(coords = MoGCoord,proj4string = CRS('+init=epsg:4326'))
## 11 Tu: Tundra
findSet <- which(ecosystemData == 11)
TuCoord <- ecosystemCoord[findSet,]
colnames(TuCoord) <- c('Lon','Lat')
TuCoordPt <- SpatialPoints(coords = TuCoord,proj4string = CRS('+init=epsg:4326'))
## 12 MeF: Mediterranean Forests, Woodlands, and Scrub
findSet <- which(ecosystemData == 12)
MeFCoord <- ecosystemCoord[findSet,]
colnames(MeFCoord) <- c('Lon','Lat')
MeFCoordPt <- SpatialPoints(coords = MeFCoord,proj4string = CRS('+init=epsg:4326'))
## 13 DXS: Deserts and Xeric Shrublands
findSet <- which(ecosystemData == 13)
DXSCoord <- ecosystemCoord[findSet,]
colnames(DXSCoord) <- c('Lon','Lat')
DXSCoordPt <- SpatialPoints(coords = DXSCoord,proj4string = CRS('+init=epsg:4326'))
## 14 Ma: Mangroves
findSet <- which(ecosystemData == 14)
MaCoord <- ecosystemCoord[findSet,]
colnames(MaCoord) <- c('Lon','Lat')
MaCoordPt <- SpatialPoints(coords = MaCoord,proj4string = CRS('+init=epsg:4326'))
## 15 World
findSet <- which(ecosystemData %in% seq(1,14))
WorldCoord <- ecosystemCoord[findSet,]
colnames(WorldCoord) <- c('Lon','Lat')
WorldCoordPt <- SpatialPoints(coords = WorldCoord,proj4string = CRS('+init=epsg:4326'))
# NDVI Files
ndviFiles <- character()
for(i in seq(1,414)){
  ndviFiles[i] <- paste0('.\\NDVI_Fix_Mosaic\\NDVI_',as.character(i),'.tif')
}
dataMatrix <- as.data.frame(matrix(data = 0,nrow = length(ndviFiles),ncol = 15))
colnames(dataMatrix) <- c('TrMBF','TrDBF','TrCF','TeBF','TeCF','BoFT','TrG','TeG','FlG','MoG','Tu','MeF','DXS','Ma','World')
for(i in seq(1,length(ndviFiles))){
  tempFileName <- ndviFiles[i]
  tempRaster <- raster(tempFileName)
  values(tempRaster)[(values(tempRaster) == -3000)|(values(tempRaster) == -32768)|(values(tempRaster) == -9999)] <- NA
  dataMatrix[i,1] <- mean(extract(tempRaster,TrMBFCoordPt),na.rm = TRUE)
  dataMatrix[i,2] <- mean(extract(tempRaster,TrDBFCoordPt),na.rm = TRUE)
  dataMatrix[i,3] <- mean(extract(tempRaster,TrCFCoordPt),na.rm = TRUE)
  dataMatrix[i,4] <- mean(extract(tempRaster,TeBFCoordPt),na.rm = TRUE)
  dataMatrix[i,5] <- mean(extract(tempRaster,TeCFCoordPt),na.rm = TRUE)
  dataMatrix[i,6] <- mean(extract(tempRaster,BoFTCoordPt),na.rm = TRUE)
  dataMatrix[i,7] <- mean(extract(tempRaster,TrGCoordPt),na.rm = TRUE)
  dataMatrix[i,8] <- mean(extract(tempRaster,TeGCoordPt),na.rm = TRUE)
  dataMatrix[i,9] <- mean(extract(tempRaster,FlGCoordPt),na.rm = TRUE)
  dataMatrix[i,10] <- mean(extract(tempRaster,MoGCoordPt),na.rm = TRUE)
  dataMatrix[i,11] <- mean(extract(tempRaster,TuCoordPt),na.rm = TRUE)
  dataMatrix[i,12] <- mean(extract(tempRaster,MeFCoordPt),na.rm = TRUE)
  dataMatrix[i,13] <- mean(extract(tempRaster,DXSCoordPt),na.rm = TRUE)
  dataMatrix[i,14] <- mean(extract(tempRaster,MaCoordPt),na.rm = TRUE)
  dataMatrix[i,15] <- mean(extract(tempRaster,WorldCoordPt),na.rm = TRUE)
  print(i)
}
##==== Part 1: Prepare Data ====##

write.csv(dataMatrix,file = 'NDVI.csv',row.names = FALSE)


##==== Part 2: Calculate composite EWI ====##
winsize <- 36
sampleNum <- length(ndviFiles) - winsize + 1
totalEWIMatrix <- data.frame()
for(i in seq(1,ncol(dataMatrix))){
  tempData <- dataMatrix[,i]
  tempData <- ts(tempData,frequency = 12,start = c(1981,7))
  EWIMatrix <- as.data.frame(matrix(data = 0,nrow = sampleNum,ncol = 8))
  colnames(EWIMatrix) <- c('AR','ACF','RR','DR','SD','SK','KURT','Ecosystem')
  # Calculate individual EWI 
  detrending <- 'linear'
  tempData <- data.matrix(tempData)
  Y <- tempData
  timeIndex <- 1:dim(tempData)[1]
  smYY <- ksmooth(timeIndex, Y, kernel = 'normal', 
                  bandwidth = 36, range.x = dim(tempData)[1], x.points = seq(1,dim(tempData)[1]))
  nsmY <- resid(lm(Y~timeIndex))
  # nsmY <- Y - smYY$y
  mw <- winsize
  omw <- length(nsmY) - mw + 1
  low <- 6
  high <- omw
  nMR <- matrix(data = NA,nrow = mw,ncol = omw)
  nMR_1 <- matrix(data = NA,nrow = mw+1,ncol = omw)
  for(j in seq(1,omw)){
    Ytw <- nsmY[j:(j + mw - 1)]
    nMR[, j] <- Ytw
    if(j == omw){
      nMR_1[, j] <- nMR_1[, (j-1)]
    }else{
      Ytw <- nsmY[j:(j + mw)]
      nMR_1[, j] <- Ytw
    }
  }
  nSD <- apply(nMR,2,sd,na.rm = TRUE)
  for(j in seq(1,ncol(nMR))){
    arModel <- ar.ols(nMR_1[,j],aic = FALSE,order.max = 1,demean = FALSE,intercept = FALSE)
    tempY <- nMR_1[1:(nrow(nMR_1)-1),j]
    tempZ <- nMR_1[2:nrow(nMR_1),j]
    acfValue <- sum((tempY-mean(tempY))*(tempZ-mean(tempY)))/sum((tempY-mean(tempY))^2)
    xfreq <- arModel$frequency
    var.p <- as.vector(arModel$var.pred)
    freq <- seq.int(0, 0.5, length.out = omw-1)
    cs <- outer(freq,1, function(x, y) cos(2*pi*x*y)) %*% arModel$ar
    sn <- outer(freq,1, function(x, y) sin(2*pi*x*y)) %*% arModel$ar
    spec <- var.p/(xfreq * ((1 - cs)^2 + sn^2))
    EWIMatrix[j,1] <- arModel$ar
    EWIMatrix[j,2] <- acfValue
    EWIMatrix[j,3] <- 1/arModel$ar
    EWIMatrix[j,4] <- spec[low]/spec[omw-1]
    EWIMatrix[j,5] <- nSD[j]
    EWIMatrix[j,6] <- abs(moments::skewness(nMR[,j],na.rm = TRUE))
    EWIMatrix[j,7] <- moments::kurtosis(nMR[,j],na.rm = TRUE)
    EWIMatrix[j,8] <- colnames(dataMatrix)[i]
  }
  totalEWIMatrix <- rbind(totalEWIMatrix,EWIMatrix)
}
# Calculate composite EWI
compositeEWIMatrix <- as.data.frame(matrix(data = 0,nrow = sampleNum,ncol = 15))
colnames(compositeEWIMatrix) <- c('TrMBF','TrDBF','TrCF','TeBF','TeCF','BoFT','TrG','TeG','FlG','MoG','Tu','MeF','DXS','Ma','World')
for(i in seq(1,ncol(compositeEWIMatrix))){
  findSet <- which(totalEWIMatrix$Ecosystem == colnames(compositeEWIMatrix)[i])
  tempData <- totalEWIMatrix[findSet,(1:(ncol(totalEWIMatrix)-1))]
  stdTempData <- tempData
  for(j in seq(1,ncol(tempData))){
    stdTempData[,j] <- (tempData[,j] - mean(tempData[,j]))/sd(tempData[,j])
  }
  for(j in seq(1,nrow(stdTempData))){
    compositeEWIMatrix[j,i] <- mean(as.numeric(stdTempData[j,c(2,5,6,7)]))
  }
}
timeSeq <- as.data.frame(matrix(data = 0,nrow = sampleNum,ncol = 1))
timeSeq <- seq(1,(sampleNum))
compositeEWIMatrix <- cbind(timeSeq,compositeEWIMatrix)
compositeEWIMatrix <- compositeEWIMatrix[1:(nrow(compositeEWIMatrix)-1),]
colnames(compositeEWIMatrix)[1] <- 'Time'
kendallResult <- as.data.frame(matrix(data = 0,nrow = 1,ncol = 15))
colnames(kendallResult) <- colnames(compositeEWIMatrix)[2:ncol(compositeEWIMatrix)]
kendallPResult <- as.data.frame(matrix(data = NA,nrow = 1,ncol = 15))
colnames(kendallPResult) <- colnames(compositeEWIMatrix)[2:ncol(compositeEWIMatrix)]
for(i in seq(2,ncol(compositeEWIMatrix))){
  corResult <- cor.test(compositeEWIMatrix$Time,compositeEWIMatrix[,i],
                        alternative = c('two.sided'),method = c('kendall'),conf.level = 0.95)
  kendallResult[1,(i-1)] <- round(corResult$estimate,3)
  kendallPResult[1,(i-1)] <- as.character(round(corResult$p.value,3))
  if(nchar(kendallPResult[1,(i-1)]) < 5){
    for(j in seq((nchar(kendallPResult[1,(i-1)])+1),5)){
      if(j == 2){
        kendallPResult[1,(i-1)] <- paste0(kendallPResult[1,(i-1)],'.')
      }else{
        kendallPResult[1,(i-1)] <- paste0(kendallPResult[1,(i-1)],'0')
      }
    }
  }
  if(kendallPResult[1,(i-1)] == '0.000'){
    kendallPResult[1,(i-1)] <- 'P < 0.001'
  }else{
    kendallPResult[1,(i-1)] <- paste0('P = ',kendallPResult[1,(i-1)])
  }
}
##==== Part 2: Calculate composite EWI ====##

##==== Part 3: Draw ====##
groupInterval <- 4
drawMatrix <- compositeEWIMatrix[,c('Time','TrMBF','TeBF','BoFT','TrG','TeG','MoG','Tu','DXS','World')]
drawMatrix$TrMBF <- drawMatrix$TrMBF + 8*groupInterval
drawMatrix$TeBF <- drawMatrix$TeBF + 7*groupInterval
drawMatrix$BoFT <- drawMatrix$BoFT + 6*groupInterval
drawMatrix$TrG <- drawMatrix$TrG + 5*groupInterval
drawMatrix$TeG <- drawMatrix$TeG + 4*groupInterval
drawMatrix$MoG <- drawMatrix$MoG + 3*groupInterval
drawMatrix$Tu <- drawMatrix$Tu + 2*groupInterval
drawMatrix$DXS <- drawMatrix$DXS + 1*groupInterval
drawMatrix$World <- drawMatrix$World + 0*groupInterval
# Draw
Fig_5 <- ggplot(data = drawMatrix,mapping = aes(x = Time))+
  scale_x_continuous(limits = c(-5,383),breaks = seq(7,367,60),expand = c(0,0),
                     labels = c('1985','1990','1995','2000','2005','2010','2015'))+
  scale_y_continuous(limits = c(-4/4*groupInterval,(8*groupInterval+3/4*groupInterval)),
                     breaks = seq(0,8*groupInterval,groupInterval),expand = c(0,0),
                     labels = c('World','DXS','Tu','MoG','TeG','TrG','BoFT','TeBF','TrMBF'))+
  geom_hline(yintercept = 0*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 1*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 2*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 3*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 4*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 5*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 6*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 7*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_hline(yintercept = 8*groupInterval,linetype = 'dashed',color = '#000000',size = 0.3)+
  geom_line(mapping = aes(y = TrMBF),color = '#028501',size = 0.4)+
  geom_line(mapping = aes(y = TeBF),color = '#009985',size = 0.4)+
  geom_line(mapping = aes(y = BoFT),color = '#7ed4c9',size = 0.4)+
  geom_line(mapping = aes(y = TrG),color = '#ffc653',size = 0.4)+
  geom_line(mapping = aes(y = TeG),color = '#f3f14e',size = 0.4)+
  geom_line(mapping = aes(y = MoG),color = '#97afeb',size = 0.4)+
  geom_line(mapping = aes(y = Tu),color = '#6584df',size = 0.4)+
  geom_line(mapping = aes(y = DXS),color = '#e3be94',size = 0.4)+
  geom_line(mapping = aes(y = World),color = '#888888',size = 0.4)+
  xlab('Year')+
  ylab('Composite EWI')+
  theme_custom()
# Output
pdf(file = 'Fig_5.pdf',width = 4,height = 7)
print(Fig_5)
dev.off()
##==== Part 3: Draw ====##