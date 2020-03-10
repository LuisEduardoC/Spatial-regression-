install.packages("rtweet")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("tidyr")
install.packages("gmodels")

testData = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/ESTRATO.csv")
testData1 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/VECI.csv")
testData2 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/edad.csv")
merge<-merge(testData,testData1,by="id")
merge1<-merge(merge,testData2,by="id")
head(merge1)
merge1
plot(merge1)
library(ggplot2)
ggplot(data = merge, aes(x = Alto, y = VECIN)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()
lm.model = lm(VECIN ~ Bajo, data = merge)
summary(lm.model)


library("rgdal")
library("rgeos")
library("sp")

testData1 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/VECI.csv")
head(testData1)
datos2=readOGR("C:/Users/ADMIN/Desktop/charo/db.shp")
head(datos2)
names (datos2)[6] = "id"
OA.Census <- merge(datos2, testData1,by="id")
proj4string(OA.Census) <- CRS("+init=EPSG:27700")
library(tmap)
library(leaflet)
qtm(OA.Census,fill ="VECIN",style = "col_blind")
OA.Census
sum(is.na(OA.Census))
library(mice)
library(Rcpp)
OA.Census1 <- as.data.frame(OA.Census)
md.pattern(OA.Census1)
c
library(VIM)
aggr(OA.Census1,numbers=T,sortVar=T)

columns <- c("VECIN")
columns
imputed_data <- mice(OA.Census1[,names(OA.Census1) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print=F)


imputed_Data <- mice(OA.Census1, m=1, maxit = 1, method = 'mean', seed = 500)
imputed_Data
complete.data <- mice::complete(imputed_Data)
xyplot(imputed_Data,VECIN ~id)

qtm(imputed_Data,fill ="VECIN",style = "col_blind")



library(sp)
library(spdep)
library(tidyverse)
library(sf)



neighbours <- poly2nb(OA.Census)
plot(OA.Census,border = 'lightgrey')
plot(neighbours, coordinates(OA.Census),add=TRUE,col= 'red')
neighbours2 <- poly2nb(OA.Census,queen =FALSE)

neighbours2
listw=nb2listw(neighbours2)



moran <- moran.plot(OA.Census$VECIN,listw = nb2listw(neighbours,style ="W"))

moran.test(OA.Census$VECIN)








install.packages("readOGR")
library(readOGR)
datos2=readOGR("C:/Users/ADMIN/Desktop/charo/db.shp")
plot(datos2)
head(datos2)
library(sp)
head(datos2)
names (datos2)[6] = "id"
head(datos2)
merge2<-merge(merge1,datos2,by="id")
plot(merge2)
proj4string(merge2) <- CRS("+init=EPSG:27700")
install.packages("tmap")
library(tmap)
library(leaflet)

library(raster)
library(rgdal)

library(ggplot2)
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(16)
plot(merge2, "VECIN", col.regions = colors)
qpal<-colorQuantile("OrRd", merge$VECIN, n=9)

datos=system.file("C:/Users/ADMIN/Desktop/charo/db.shp",package = "raster")
plot(datos)

p <- shapefile("C:/Users/ADMIN/Desktop/charo/db.shp", package="raster")
plot(p)

__________________________________________________________________________________



testData1 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/VECI.csv")
testData2 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/CASA.csv")
testData3 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/edad.csv")
testData4 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/ESTRATO.csv")
testData5 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/completedData.csv")


#IMPUTAR DATOS
require(VIM)
require(FactoMineR)
require(tidyr)
require(dplyr)
require(magrittr)
library("rgdal")
library("rgeos")
library("sp")


testData4 = read.csv("C:/Users/ADMIN/Desktop/Tesis_tadaSpat/ROBO.csv")
aggr(testData4, prop=FALSE, 
     numbers=TRUE, border=NA,
     combine=TRUE)
library(mice)
columns <- c("ROBO")
diameters <- as.data.frame(testData4)
diameters 
class(diameters)
imputed_data <- mice(diameters[,names(diameters) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print=F)

IMPU=mice(data = diameters, m = 5, method = "mean", maxit = 50, seed = 500)
completedData <- complete(IMPU,1)
plot(completedData)

write.csv(completedData, file = "C:/Users/ADMIN/Desktop/Tesis_tadaSpat/completedData.csv") 


OA.Census <- merge(testData1, testData2,by="id")
OA.Census1 <- merge(OA.Census, testData3,by="id")
OA.Census2 <- merge(OA.Census1, testData4,by="id")
OA.Census3 <- merge(OA.Census2, completedData,by="id")

#datos atipicos

boxplot(completedData$ROBO, 
        main = "Pressure Height",
        boxwex = 0.5,col="blue")
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}
imputed_data <- impute_outliers(completedData$ROBO)
imputed_data 
str(imputed_data)
par(mfrow = c(1,2))

boxplot(completedData$ROBO, main = "Presión con outliers",
        col = 3)
boxplot(imputed_data, main = "Presión sin outliers",col=2)
plot(imputed_data,VECIN)




shape=readOGR("C:/Users/ADMIN/Desktop/charo/db.shp")
names (shape)[6] = "id"
shape2 <- merge(shape, OA.Census3,by="id")
head(shape2)
summary(shape2)
spplot(shape2, "ROBO") 
library(spdep)
queen.nb <- poly2nb(shape2) 
rook.nb  <- poly2nb(shape2, queen=FALSE) 
queen.listw <- nb2listw(queen.nb,zero.policy=TRUE) 
rook.listw  <- nb2listw(rook.nb,zero.policy=TRUE)

listw1 <-  queen.listw

reg.eq1 <- ROBO ~ VECIN +  M?nos.de.25 
reg1 <- lm(reg.eq1, data = shape2)
summary(reg1)
lmMoranTest <- lm.morantest(reg1,listw1,zero.policy=TRUE)
lmMoranTest
install.packages("spatialreg")
reg3 <- lagsarlm(reg.eq1, data = shape2, listw1,zero.policy=TRUE)
summary(reg3)
https://rpubs.com/quarcs-lab/tutorial-spatial-regression





shape2
plot(OA.Census2$VECIN,OA.Census2$RO_CAS)
OA.Census2
lm(OA.Census2$VECIN ~ OA.Census2$RO_CAS)

library(ggplot2)
ggplot(data = OA.Census3) + 
  geom_point(mapping = aes(x = VECIN, y = ROBO))+geom_smooth(mapping = aes(x = VECIN, y = RO_CAS))

boxplot(OA.Census2$VECIN, 
        main = "Pressure Height",
        boxwex = 0.5,col="blue")
