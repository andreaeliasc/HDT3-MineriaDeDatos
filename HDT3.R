library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(tidyr)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(rpart)

# Analisis Exploratorio
train<- read.csv("train.csv", stringsAsFactors = FALSE)
test<- read.csv("test.csv", stringsAsFactors = FALSE)
train<-train[1:1460,]

glimpse(train[1:10,])

#graficas de correlacion de las variables que se utilizan vs el precio de compra
scatter.smooth(train$LotFrontage, train$SalePrice)
scatter.smooth(train$LotArea, train$SalePrice)
scatter.smooth(train$GrLivArea, train$SalePrice)
scatter.smooth(train$YearBuilt, train$SalePrice)
scatter.smooth(train$BsmtUnfSF, train$SalePrice)
scatter.smooth(train$TotalBsmtSF, train$SalePrice)
scatter.smooth(train$X1stFlrSF, train$SalePrice)
scatter.smooth(train$GarageYrBlt, train$SalePrice)
scatter.smooth(train$GarageArea, train$SalePrice)
scatter.smooth(train$YearRemodAdd, train$SalePrice)

scatter.smooth(train$TotRmsAbvGrd, train$SalePrice)
scatter.smooth(train$MoSold, train$SalePrice)
scatter.smooth(train$OverallQual, train$SalePrice)

# Clasificacion por Clustering
datos <- train[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]

datos <- na.omit(datos)

wss <- (nrow(datos)-1)*sum(apply(datos,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datos, centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#kmeans
testsL <- datos[complete.cases(datos),]
km<-kmeans(datos,3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
g2<- datos[datos$grupo==2,]
prop.table(table(g2$Species))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Species))*100

plotcluster(datos,km$cluster) 

#silueta
fviz_cluster(km, data = datos,geom = "point", ellipse.type = "norm")

max(g1["SalePrice"])
min(g1["SalePrice"])
max(g2["SalePrice"])
min(g2["SalePrice"])
max(g3["SalePrice"])
min(g3["SalePrice"])
# ARBOL DE CLASIFICACION
train<- read.csv("train.csv", stringsAsFactors = FALSE)
test<- read.csv("test.csv", stringsAsFactors = FALSE)
train<-train[1:1460,]

test <- na.omit(test)

porciento <- 70/100

datos <- train[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]
datos <- na.omit(datos)

cluster <- datos
km<-kmeans(datos,3)
datos$grupo<-km$cluster
datosFiltertree <- datos[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "grupo")]

datosFiltertree
porciento <- 70/100

set.seed(321)
trainRowsNumber<-sample(1:nrow(datosFiltertree),porciento*nrow(datosFiltertree))
train<-datosFiltertree[trainRowsNumber,]
test<-datosFiltertree[-trainRowsNumber,]

dt_model<-rpart(train$grupo~.,train,method = "class")
rpart.plot(dt_model)

prediccion <- predict(dt_model, newdata = test[1:10])

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta

cfm<-table(test$grupo,test$prediccion)
cfm

