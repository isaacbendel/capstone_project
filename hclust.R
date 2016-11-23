#' # Heirarchal Clustering
#+echo+FALSE
library(RColorBrewer)
library(formattable)
bluePalette<-colorRampPalette(brewer.pal(9,"Blues"))(100)
setwd('/home/bendel/R/Hand/')
hand <-read.csv('train.csv')
train<-hand[1:30000,]
test<-hand[30001:42000,]
source('showImage.R')
source('confusionMatrix.R')
library(parallel)

#+echo=TRUE
digits = list()
for (i in seq(1,10)){
  digits[[i]] <- train[which(train$label == i%%10),]
  }

centroids<-list()
tm <- system.time({
  for (k in seq(1,10)){
  d<-dist(digits[[k]][,2:785])
  e<-hclust(d)
  f<-cutree(e, k = 10)
  par(mfrow=c(2,5))
  for (j in seq(1,10)){
    g<-which(f == j)
    h<-digits[[k]][g,2:785]
    i<-sapply(h, FUN = mean)
    centroids<-rbind(centroids,i)    
    showImage(i, bluePalette)
  }
  }
})

set.seed(20)
library(class)
clust4<-knn(centroids, test[,2:785],  rep(seq(1,10), each =10)%%10, k = 1)
confusionMatrix(test[,1],clust4)

