rm(list = ls())
library(ggplot2)
setwd('F:\\EWS_0111\\Fig_6')

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

## 1 TrMBF: Tropical and Subtropical Moist Broadleaf Forests
TrMBFDataMatrix <- read.csv('.\\Output\\TrMBF.csv',header = FALSE)
TrMBFGlobalMatrix <- data.frame(Time = seq(1,nrow(TrMBFDataMatrix)),Value = as.numeric(TrMBFDataMatrix[,1]))
TrMBFLocalMatrix <- data.frame(Time = seq(1,nrow(TrMBFDataMatrix)),Value = as.numeric(TrMBFDataMatrix[,2]))
TrMBFFaiMatrix <- data.frame(Time = seq(1,nrow(TrMBFDataMatrix)),Value = 1 - as.numeric(TrMBFDataMatrix[,3]))
Fig_6a <- ggplot(data = TrMBFGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#028501',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(breaks = c(0.08,0.12,0.16))+
  ylab('TrMBF')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6a.pdf',width = 2.18,height = 1)
print(Fig_6a)
dev.off()
Fig_6b <- ggplot(data = TrMBFLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#028501',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.43,0.59),breaks = c(0.45,0.50,0.55))+
  ylab('TrMBF')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6b.pdf',width = 2.18,height = 1)
print(Fig_6b)
dev.off()
Fig_6c <- ggplot(data = TrMBFFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#028501',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.84,1.01),breaks = c(0.85,0.90,0.95,1.00))+
  ylab('TrMBF')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6c.pdf',width = 2.18,height = 1)
print(Fig_6c)
dev.off()

## 4 TeBF: Temperate Broadleaf and Mixed Forests
TeBFDataMatrix <- read.csv('.\\Output\\TeBF.csv',header = FALSE)
TeBFGlobalMatrix <- data.frame(Time = seq(1,nrow(TeBFDataMatrix)),Value = as.numeric(TeBFDataMatrix[,1]))
TeBFLocalMatrix <- data.frame(Time = seq(1,nrow(TeBFDataMatrix)),Value = as.numeric(TeBFDataMatrix[,2]))
TeBFFaiMatrix <- data.frame(Time = seq(1,nrow(TeBFDataMatrix)),Value = 1 - as.numeric(TeBFDataMatrix[,3]))
Fig_6d <- ggplot(data = TeBFGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#009985',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.127,0.153),breaks = c(0.13,0.14,0.15))+
  ylab('TeBF')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6d.pdf',width = 2.18,height = 1)
print(Fig_6d)
dev.off()
Fig_6e <- ggplot(data = TeBFLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#009985',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(1.65,1.79),breaks = c(1.68,1.72,1.76))+
  ylab('TeBF')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6e.pdf',width = 2.18,height = 1)
print(Fig_6e)
dev.off()
Fig_6f <- ggplot(data = TeBFFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#009985',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(breaks = c(0.993,0.994))+
  ylab('TeBF')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6f.pdf',width = 2.24,height = 1)
print(Fig_6f)
dev.off()

## 6 BoFT: Boreal Forests/Taiga
BoFTDataMatrix <- read.csv('.\\Output\\BoFT.csv',header = FALSE)
BoFTGlobalMatrix <- data.frame(Time = seq(1,nrow(BoFTDataMatrix)),Value = as.numeric(BoFTDataMatrix[,1]))
BoFTLocalMatrix <- data.frame(Time = seq(1,nrow(BoFTDataMatrix)),Value = as.numeric(BoFTDataMatrix[,2]))
BoFTFaiMatrix <- data.frame(Time = seq(1,nrow(BoFTDataMatrix)),Value = 1 - as.numeric(BoFTDataMatrix[,3]))
Fig_6g <- ggplot(data = BoFTGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#7ed4c9',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.153,0.171),breaks = c(0.16,0.17))+
  ylab('BoFT')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6g.pdf',width = 2.18,height = 1)
print(Fig_6g)
dev.off()
Fig_6h <- ggplot(data = BoFTLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#7ed4c9',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(3.68,3.97),breaks = c(3.70,3.80,3.90),labels = c('3.70','3.80','3.90'))+
  ylab('BoFT')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6h.pdf',width = 2.18,height = 1)
print(Fig_6h)
dev.off()
#
Fig_6i <- ggplot(data = BoFTFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#7ed4c9',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(breaks = c(0.9981,0.9982,0.9983),labels = c('9.981','9.982','9.983'))+
  ylab('BoFT')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6i.pdf',width = 2.24,height = 1)
print(Fig_6i)
dev.off()

## 7 TrG: Tropical and subtropical grasslands, savannas, and shrublands
TrGDataMatrix <- read.csv('.\\Output\\TrG.csv',header = FALSE)
TrGGlobalMatrix <- data.frame(Time = seq(1,nrow(TrGDataMatrix)),Value = as.numeric(TrGDataMatrix[,1]))
TrGLocalMatrix <- data.frame(Time = seq(1,nrow(TrGDataMatrix)),Value = as.numeric(TrGDataMatrix[,2]))
TrGFaiMatrix <- data.frame(Time = seq(1,nrow(TrGDataMatrix)),Value = 1 - as.numeric(TrGDataMatrix[,3]))
Fig_6j <- ggplot(data = TrGGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#ffc653',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.09,0.23),breaks = c(0.10,0.15,0.20))+
  ylab('TrG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6j.pdf',width = 2.18,height = 1)
print(Fig_6j)
dev.off()
Fig_6k <- ggplot(data = TrGLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#ffc653',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.97,1.25),breaks = c(1.00,1.10,1.20),labels = c('1.00','1.10','1.20'))+
  ylab('TrG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6k.pdf',width = 2.18,height = 1)
print(Fig_6k)
dev.off()
Fig_6l <- ggplot(data = TrGFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#ffc653',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.955,0.993),breaks = c(0.96,0.97,0.98,0.99))+
  ylab('TrG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6l.pdf',width = 2.18,height = 1)
print(Fig_6l)
dev.off()

## 8 TeG: Temperate Grasslands, Savannas, and Shrublands
TeGDataMatrix <- read.csv('.\\Output\\TeG.csv',header = FALSE)
TeGGlobalMatrix <- data.frame(Time = seq(1,nrow(TeGDataMatrix)),Value = as.numeric(TeGDataMatrix[,1]))
TeGLocalMatrix <- data.frame(Time = seq(1,nrow(TeGDataMatrix)),Value = as.numeric(TeGDataMatrix[,2]))
TeGFaiMatrix <- data.frame(Time = seq(1,nrow(TeGDataMatrix)),Value = 1 - as.numeric(TeGDataMatrix[,3]))
Fig_6m <- ggplot(data = TeGGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#f3f14e',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(1.84,2.13),breaks = c(1.90,2.00,2.10),labels = c('1.90','2.00','2.10'))+
  ylab('TeG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6m.pdf',width = 2.18,height = 1)
print(Fig_6m)
dev.off()
Fig_6n <- ggplot(data = TeGLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#f3f14e',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(2.24,2.63),breaks = c(2.3,2.4,2.5,2.6),labels = c('2.30','2.40','2.50','2.60'))+
  ylab('TeG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6n.pdf',width = 2.18,height = 1)
print(Fig_6n)
dev.off()
Fig_6o <- ggplot(data = TeGFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#f3f14e',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.28,0.40),breaks = c(0.30,0.34,0.38))+
  ylab('TeG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6o.pdf',width = 2.18,height = 1)
print(Fig_6o)
dev.off()

## 10 MoG: Montane Grasslands and Shrublands
MoGDataMatrix <- read.csv('.\\Output\\MoG.csv',header = FALSE)
MoGGlobalMatrix <- data.frame(Time = seq(1,nrow(MoGDataMatrix)),Value = as.numeric(MoGDataMatrix[,1]))
MoGLocalMatrix <- data.frame(Time = seq(1,nrow(MoGDataMatrix)),Value = as.numeric(MoGDataMatrix[,2]))
MoGFaiMatrix <- data.frame(Time = seq(1,nrow(MoGDataMatrix)),Value = 1 - as.numeric(MoGDataMatrix[,3]))
Fig_6p <- ggplot(data = MoGGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#97afeb',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(1.01,1.19),breaks = c(1.05,1.10,1.15))+
  ylab('MoG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6p.pdf',width = 2.18,height = 1)
print(Fig_6p)
dev.off()
Fig_6q <- ggplot(data = MoGLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#97afeb',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(1.88,2.16),breaks = c(1.90,2.00,2.10),labels = c('1.90','2.00','2.10'))+
  ylab('MoG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6q.pdf',width = 2.18,height = 1)
print(Fig_6q)
dev.off()
Fig_6r <- ggplot(data = MoGFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#97afeb',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.66,0.75),breaks = c(0.66,0.70,0.74))+
  ylab('MoG')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6r.pdf',width = 2.18,height = 1)
print(Fig_6r)
dev.off()

## 11 Tu: Tundra
TuDataMatrix <- read.csv('.\\Output\\Tu.csv',header = FALSE)
TuGlobalMatrix <- data.frame(Time = seq(1,nrow(TuDataMatrix)),Value = as.numeric(TuDataMatrix[,1]))
TuLocalMatrix <- data.frame(Time = seq(1,nrow(TuDataMatrix)),Value = as.numeric(TuDataMatrix[,2]))
TuFaiMatrix <- data.frame(Time = seq(1,nrow(TuDataMatrix)),Value = 1 - as.numeric(TuDataMatrix[,3]))
Fig_6s <- ggplot(data = TuGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#6584df',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(6.15,6.73),breaks = c(6.2,6.4,6.6),labels = c('6.20','6.40','6.60'))+
  ylab('Tu')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6s.pdf',width = 2.18,height = 1)
print(Fig_6s)
dev.off()
Fig_6t <- ggplot(data = TuLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#6584df',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(6.60,7.2),breaks = c(6.7,6.9,7.1),labels = c('6.70','6.90','7.10'))+
  ylab('Tu')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6t.pdf',width = 2.18,height = 1)
print(Fig_6t)
dev.off()
Fig_6u <- ggplot(data = TuFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#6584df',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.112,0.144),breaks = c(0.12,0.13,0.14))+
  ylab('Tu')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6u.pdf',width = 2.18,height = 1)
print(Fig_6u)
dev.off()

## 13 DXS: Deserts and Xeric Shrublands
DXSDataMatrix <- read.csv('.\\Output\\DXS.csv',header = FALSE)
DXSGlobalMatrix <- data.frame(Time = seq(1,nrow(DXSDataMatrix)),Value = as.numeric(DXSDataMatrix[,1]))
DXSLocalMatrix <- data.frame(Time = seq(1,nrow(DXSDataMatrix)),Value = as.numeric(DXSDataMatrix[,2]))
DXSFaiMatrix <- data.frame(Time = seq(1,nrow(DXSDataMatrix)),Value = 1 - as.numeric(DXSDataMatrix[,3]))
Fig_6v <- ggplot(data = DXSGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#e3be94',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.27,0.43),breaks = c(0.28,0.32,0.36,0.40))+
  ylab('DXS')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6v.pdf',width = 2.18,height = 1)
print(Fig_6v)
dev.off()
Fig_6w <- ggplot(data = DXSLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#e3be94',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.89,1.25),breaks = c(0.90,1.00,1.10,1.20),labels = c('0.90','1.00','1.10','1.20'))+
  ylab('DXS')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6w.pdf',width = 2.18,height = 1)
print(Fig_6w)
dev.off()
Fig_6x <- ggplot(data = DXSFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#e3be94',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.855,0.94),breaks = c(0.86,0.90,0.94))+
  ylab('DXS')+
  theme_custom()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())
pdf(file = '.\\Draw\\Fig_6x.pdf',width = 2.18,height = 1)
print(Fig_6x)
dev.off()

## 15 World
WorldDataMatrix <- read.csv('.\\Output\\World.csv',header = FALSE)
WorldGlobalMatrix <- data.frame(Time = seq(1,nrow(WorldDataMatrix)),Value = as.numeric(WorldDataMatrix[,1]))
WorldLocalMatrix <- data.frame(Time = seq(1,nrow(WorldDataMatrix)),Value = as.numeric(WorldDataMatrix[,2]))
WorldFaiMatrix <- data.frame(Time = seq(1,nrow(WorldDataMatrix)),Value = 1 - as.numeric(WorldDataMatrix[,3]))
Fig_6y <- ggplot(data = WorldGlobalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#888888',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(breaks = c(0.235,0.240,0.245))+
  xlab('Year')+
  ylab('World')+
  theme_custom()
pdf(file = '.\\Draw\\Fig_6y.pdf',width = 2.24,height = 1.42)
print(Fig_6y)
dev.off()
Fig_6z <- ggplot(data = WorldLocalMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#888888',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.327,0.352),breaks = c(0.33,0.34,0.35),labels = c('0.33','0.34','0.35'))+
  xlab('Year')+
  ylab('World')+
  theme_custom()
pdf(file = '.\\Draw\\Fig_6z.pdf',width = 2.18,height = 1.42)
print(Fig_6z)
dev.off()
Fig_6aa <- ggplot(data = WorldFaiMatrix,mapping = aes(x = Time,y = Value))+
  geom_line(color = '#888888',size = 0.4)+
  scale_x_continuous(limits = c(-5,383),breaks = seq(67,307,120),expand = c(0,0),
                     labels = c('1990','2000','2010'))+
  scale_y_continuous(limits = c(0.46,0.54),breaks = c(0.46,0.50,0.54))+
  xlab('Year')+
  ylab('World')+
  theme_custom()
pdf(file = '.\\Draw\\Fig_6aa.pdf',width = 2.18,height = 1.42)
print(Fig_6aa)
dev.off()

resultMatrix <- data.frame()
##==== Kendall's tau ====##
# TrMBF
tempModel_1 <- cor.test(x = TrMBFGlobalMatrix$Time,y = TrMBFGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = TrMBFLocalMatrix$Time,y = TrMBFLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = TrMBFFaiMatrix$Time,y = TrMBFFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'TrMBF',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# TeBF
tempModel_1 <- cor.test(x = TeBFGlobalMatrix$Time,y = TeBFGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = TeBFLocalMatrix$Time,y = TeBFLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = TeBFFaiMatrix$Time,y = TeBFFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'TeBF',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# BoFT
tempModel_1 <- cor.test(x = BoFTGlobalMatrix$Time,y = BoFTGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = BoFTLocalMatrix$Time,y = BoFTLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = BoFTFaiMatrix$Time,y = BoFTFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'BoFT',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# TrG
tempModel_1 <- cor.test(x = TrGGlobalMatrix$Time,y = TrGGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = TrGLocalMatrix$Time,y = TrGLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = TrGFaiMatrix$Time,y = TrGFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'TrG',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# TeG
tempModel_1 <- cor.test(x = TeGGlobalMatrix$Time,y = TeGGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = TeGLocalMatrix$Time,y = TeGLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = TeGFaiMatrix$Time,y = TeGFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'TeG',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# MoG
tempModel_1 <- cor.test(x = MoGGlobalMatrix$Time,y = MoGGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = MoGLocalMatrix$Time,y = MoGLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = MoGFaiMatrix$Time,y = MoGFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'MoG',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# Tu
tempModel_1 <- cor.test(x = TuGlobalMatrix$Time,y = TuGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = TuLocalMatrix$Time,y = TuLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = TuFaiMatrix$Time,y = TuFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'Tu',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# DXS
tempModel_1 <- cor.test(x = DXSGlobalMatrix$Time,y = DXSGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = DXSLocalMatrix$Time,y = DXSLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = DXSFaiMatrix$Time,y = DXSFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'DXS',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
# World
tempModel_1 <- cor.test(x = WorldGlobalMatrix$Time,y = WorldGlobalMatrix$Value,method = 'kendall')
tempModel_2 <- cor.test(x = WorldLocalMatrix$Time,y = WorldLocalMatrix$Value,method = 'kendall')
tempModel_3 <- cor.test(x = WorldFaiMatrix$Time,y = WorldFaiMatrix$Value,method = 'kendall')
tempResultMatrix <- data.frame(Biome = 'World',
                               GlobalTau = as.numeric(tempModel_1$estimate),GlobalP = as.numeric(tempModel_1$p.value),
                               LocalTau = as.numeric(tempModel_2$estimate),LocalP = as.numeric(tempModel_2$p.value),
                               FaiTau = as.numeric(tempModel_3$estimate),FaiP = as.numeric(tempModel_3$p.value))
resultMatrix <- rbind(resultMatrix,tempResultMatrix)
##==== Kendall's tau ====##
write.csv(resultMatrix,file = '.\\Draw\\TauList.csv',row.names = FALSE)