#' # k Nearest Neighbour
#' # 
#+echo=FALSE
library(formattable)
library(ggplot2)
setwd('/home/bendel/R/Hand/')
source('showImage.R')
source('confusionMatrix.R')
library(class)

#+echo = TRUE

#'# Preparation
#'# 
#'## Load Data, and separate into train and test sets
hand <-read.csv('train.csv')
train<-hand[1:30000,]
test<-hand[30001:42000,]

#' # 
#' # Run a k-Nearest Neighbour classification
tm1 <- system.time(
  {
    a<-knn(train[1:1000,2:785], test[,2:785], train[1:1000,1])    
  })

#' # 
#' # Result
#' ## Time to run
tm1

#' # 
#' ## Confusion Matrix
confusionMatrix(test[,1],a)

#'# Analysis
#'
#' I tried running the knn on the entire train and test set. I was taking a long time. I tried with only 1000 from training set. It took about 2 minutes. k-means took only a few seconds to process the entire train and test set. This result was found using the default value of k = 1. We can now try playing with value of k, as well as considering greater slice of training set.

#'# 
#'# Round 2
#'# 
tm1 <- system.time(
  {
    a<-knn(train[1:1000,2:785], test[,2:785], train[1:1000,1], k = 5)
  })

#' ## Time to run

tm1

#' ## Confusion Matrix

confusionMatrix(test[,1],a)

#' # Analysis
#' The accuracy actually decreased slightly by using k = 5.

#' # Round 3

#' Until now I just took the first 1000 images from the test set. Now I'll try randomly selecting 100 of each image.

digits = list()
for (i in seq(1,10)){
  digits[[i]] <- train[which(train$label == i%%10),]
}

set.seed(20)
randomList<-sample(1:2700,100)

newDigits<-data.frame()
for (i in seq(1,10)){
  newDigits<-rbind.data.frame(newDigits,digits[[i]][randomList,1:785])
}

accuracy<-vector()
for (i in seq(1, 21, 2)){
  tm1 <- system.time(
  {
    a <-knn(newDigits[,2:785], test[,2:785], newDigits[,1], k = i)
  })
  print(c(i,tm1[3]))
  accuracy<-c(accuracy, confusionMatrix(test[,1], a, justAccuracy = TRUE))
}

#+echo+TRUE
q<- cbind.data.frame(seq(1,21,2), accuracy)
names(q) <- c('k', 'Accuracy')
ggplot(data = q, aes(x = k, y = Accuracy)) + geom_line() + geom_point()

#' It's fascinating that the accuracy goes down as we consider more neighbours.
#' 
#' # Round 4
#' Now try 200 from each digit.

set.seed(20)
randomList<-sample(1:2700,200)

newDigits<-data.frame()
for (i in seq(1,10)){
  newDigits<-rbind.data.frame(newDigits,digits[[i]][randomList,1:785])
}

accuracy<-vector()
for (i in seq(1, 21, 2)){
  tm1 <- system.time(
    {
      a <-knn(newDigits[,2:785], test[,2:785], newDigits[,1], k = i)
    })
  print(c(i,tm1[3]))
  accuracy<-c(accuracy, confusionMatrix(test[,1], a, justAccuracy = TRUE))
}

q<- cbind.data.frame(seq(1,21,2), accuracy)
names(q) <- c('k', 'Accuracy')
ggplot(data = q, aes(x = k, y = Accuracy)) + geom_line() + geom_point()

#' Interestingly enough - for the first time we get better accuracy when k goes from 1 to 2. Also we have broken the 90% accuracy mark for the first time.

