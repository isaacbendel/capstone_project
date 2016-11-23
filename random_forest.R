#' # k Nearest Neighbour Part 2
#+echo=FALSE
library(formattable)
library(ggplot2)
setwd('/home/bendel/R/Hand/')
source('showImage.R')
source('confusionMatrix.R')
library(RColorBrewer)
library(randomForest)
library(parallel)

bluePalette<-colorRampPalette(brewer.pal(9,"Blues"))(100)

hand <-read.csv('train.csv')
train<-hand[1:30000,]
test<-hand[30001:42000,]
train$label<-as.factor(train$label)


tm1 <- system.time({
          fit <- randomForest(label ~ ., train[1:1000,], ntree=2000)
          })

tm1
beepr::beep()
p<-predict(fit, test[,2:785])
confusionMatrix(test[,1],p)

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
      fit <- randomForest(label ~ ., newDigits, ntree=200)
    })
  print(c(i, tm1[3]))
  p<-predict(fit, test[,2:785])
  tmp<-confusionMatrix(test[,1], p, justAccuracy = TRUE)
  return(c(i,tmp))
}

accuracy<-mclapply(seq(100,2000,100), nSamplesPerDigit, mc.cores=4)

q<-data.frame(t(rbind.data.frame(accuracy)))
names(q) <- c('Number of samples from each digit', 'Accuracy')
ggplot(data = q, aes(x = `Number of samples from each digit`, y = Accuracy)) + geom_line() + geom_point()
beepr::beep()
