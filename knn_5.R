#' # k Nearest Neighbour Part 2
#+echo=FALSE
library(formattable)
library(ggplot2)
setwd('/home/bendel/R/Hand/')
source('showImage.R')
source('confusionMatrix.R')
library(class)
library(parallel)
hand <-read.csv('train.csv')
train<-hand[1:30000,]
test<-hand[30001:42000,]

#+echo=TRUE
#' # Round 5
#' Let's see how accuracy is affected by how many selections we make from each digit. We will keep k constant at k = 1.
digits = list()
for (i in seq(1,10)){
  digits[[i]] <- train[which(train$label == i%%10),]
}

set.seed(20)
nSamplesPerDigit<-function(i){
  randomList<-sample(1:2700,i)
  newDigits<-data.frame()
  
  for (j in seq(1,10)){
    newDigits<-rbind.data.frame(newDigits,digits[[j]][randomList,1:785])
  }
  a<-vector()
  tm1 <- system.time(
    {
      a <-knn(newDigits[,2:785], test[,2:785], newDigits[,1], k = 1)
    })
  #print(c(i, tm1[3]))
  #print(tm1)
  tmp<-confusionMatrix(test[,1], a, justAccuracy = TRUE)
  #accuracy<-c(accuracy, tmp)
  #return(c('try',accuracy))
  #print('buddy')
  return(c(i,tmp))
  #print('buddy')
}

accuracy<-mclapply(seq(100,1000,100), nSamplesPerDigit, mc.cores=4)

q<-data.frame(t(rbind.data.frame(accuracy)))
names(q) <- c('Number of samples from each digit', 'Accuracy')
ggplot(data = q, aes(x = `Number of samples from each digit`, y = Accuracy)) + geom_line() + geom_point()

