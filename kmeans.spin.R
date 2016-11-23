library(RColorBrewer)
setwd('/home/bendel/R/Hand/')
getwd()
hand <-read.csv('train.csv')
nrow(hand)
#Split data into training set and testing set
train<-hand[1:30000,]
test<-hand[30001:42000,]
b<-colorRampPalette(brewer.pal(9,"Blues"))(100)

#This will show us an image of the n-th digit in the training set
n<-1
image(matrix(as.integer(train[n,2:785]), nrow = 28, byrow=FALSE)[,c(seq(28,1))],col = b)
colors()[grep('grey', colors())]
#Use k-means clustering on training set, group items into 10 clusters
#Took about one minute to run
set.seed(20)
clust<-kmeans(train, 10)

#These are the centroids
par(mfrow=c(2,5))
for (i in seq(1,10)){
  image(matrix(clust$centers[i,2:785], nrow = 28, byrow=FALSE)[,c(seq(28,1))], col = b, axes = FALSE, xlab = paste('Cluster number', i, sep = ' '))
}

#Doesn't look too obvious what some of these are meant to be.

#Try a different seed
set.seed(22)
clust<-kmeans(train, 10)

#These are the centroids
for (i in seq(1,10)){
  image(matrix(clust$centers[i,2:785], nrow = 28, byrow=FALSE)[,c(seq(28,1))], xlab = paste('Cluster number', i, sep = ' '))
}

#Try using 20 different clusters
k<-20
set.seed(20)
clust<-kmeans(train, k)

#These are the centroids
for (i in seq(1,k)){
  image(matrix(clust$centers[i,2:785], nrow = 28, byrow=FALSE)[,c(seq(28,1))], xlab = paste('Cluster number', i, sep = ' '))
}

#Try using 20 different clusters
k<-40
set.seed(20)
clust<-kmeans(train, 20, iter.max = 20)

?kmeans


#These are the centroids
par(mfrow = c(2,5))
for (i in seq(1,10)){
  image(matrix(clust$centers[i,2:785], nrow = 28, byrow=FALSE)[,c(seq(28,1))], xlab = paste(paste('Cluster number', i, sep = ' '), clust$centers[i,1], sep = ' '), col = b, axes = FALSE)
}
par(mfrow = c(2,5))
for (i in seq(11,20)){
  image(matrix(clust$centers[i,2:785], nrow = 28, byrow=FALSE)[,c(seq(28,1))], xlab = paste(paste('Cluster number', i, sep = ' '), clust$centers[i,1], sep = ' '), col = b, axes = FALSE)
}






for (i in seq(1,20)){
  q<-matrix(clust$centers[i,2:785], nrow = 28, byrow=FALSE)
  q[which(q<130)]<-0
  image(q[,c(seq(28,1))], col = b, axes = FALSE, xlab = i)
}

