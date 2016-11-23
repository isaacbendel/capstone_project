#' # k Nearest Neighbour Part 2
#+echo=FALSE
library(formattable)
library(ggplot2)
setwd('/home/bendel/R/Hand/')
source('showImage.R')
source('confusionMatrix.R')
library(class)
library(parallel)
library(RColorBrewer)
bluePalette<-colorRampPalette(brewer.pal(9,"Blues"))(100)

hand <-read.csv('train.csv')
train<-hand[1:30000,]
test<-hand[30001:42000,]

#+echo=TRUE
#' # Round 6
#' Let's merge k-means, and k nearest neighbour. Let's use k menas to find ten of each digit, and then use those as inputs for knn.
#' 
digits = list()
for (i in seq(1,10)){
  digits[[i]] <- train[which(train$label == i%%10),]
}

clust1<-list()
for(i in seq(1,10)){
  clust1[[i]] <- kmeans(digits[[i]], 10, iter.max = 20)
}  

centroids<-data.frame()
for (i in seq(1,10)){
  centroids<-rbind.data.frame(centroids,clust1[[i]]$centers)
}

a<-knn(centroids[,2:785], test[,2:785], centroids[,1], k = )
confusionMatrix(test[,1], a, justAccuracy = FALSE)

#' This looks pretty good.
#' Let's try and increase number of basic shapes per digit.

set.seed(20)
numberOfBasicShapesPerDigit<-function(j){
  clust1<-list()
  for(i in seq(1,10)){
    clust1[[i]] <- kmeans(digits[[i]], 10, iter.max = 50)
  }  
  
  centroids<-data.frame()
  for (i in seq(1,10)){
    centroids<-rbind.data.frame(centroids,clust1[[i]]$centers)
  }
  a<-knn(centroids[,2:785], test[,2:785], centroids[,1], k = 1)
  tmp<-confusionMatrix(test[,1], a, justAccuracy = TRUE)
  return(c(j,tmp))
}

accuracy<-mclapply(c(seq(10,100,10), seq(200,1000,100)), numberOfBasicShapesPerDigit , mc.cores=4)

q<-data.frame(t(rbind.data.frame(accuracy)))
names(q) <- c('Number of basic shapes per digit', 'Accuracy')
ggplot(data = q, aes(x = `Number of basic shapes per digit`, y = Accuracy)) + geom_line() + geom_point()
