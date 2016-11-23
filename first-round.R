setwd('/home/bendel/R/Hand/')
getwd()
#hand <-read.csv('train.csv')
#handsmall<-head(hand, n=1000)
#write.csv(handsmall,'a')
hand<-read.csv('a')
head(hand)

(hand[1,])
m = matrix(as.integer(hand[6,2:785]), nrow = 28, byrow=FALSE)
image(matrix(as.integer(hand[10,2:785]), nrow = 28, byrow=FALSE))
image(matrix(as.integer(hand[3,2:785]), nrow = 28, byrow=FALSE))
image(apply(matrix(as.integer(hand[10,2:785]), nrow = 28, byrow=FALSE),2,rev))

hand.1<-hand[hand$label == 1,]
hand.2<-hand[hand$label == 2,]
hand.3<-hand[hand$label == 3,]
hand.4<-hand[hand$label == 4,]
hand.5<-hand[hand$label == 5,]
hand.6<-hand[hand$label == 6,]
hand.7<-hand[hand$label == 7,]
hand.8<-hand[hand$label == 8,]
hand.9<-hand[hand$label == 9,]
hand.0<-hand[hand$label == 0,]

tmp1<-matrix(as.integer(hand.1[1,2:785]), nrow=28, byrow=FALSE)
tmp2<-matrix(as.integer(hand.1[2,2:785]), nrow=28, byrow=FALSE)
tmp3<-matrix(as.integer(hand.1[3,2:785]), nrow=28, byrow=FALSE)
tmp1.1<-.colSums(tmp1,28,28)
tmp2.1<-.colSums(tmp2,28,28)
tmp3.1<-.colSums(tmp3,28,28)
plot(tmp1.1,type='l',col="green")
par(new=TRUE)
plot(tmp2.1,type='l',col="red")
par(new=TRUE)
plot(tmp3.1,type='l',col="blue")
dev.off()

tmp1.2<-.rowSums(tmp1,28,28)
tmp2.2<-.rowSums(tmp2,28,28)
tmp3.2<-.rowSums(tmp3,28,28)
plot(tmp1.2,type='l',col="green")
par(new=TRUE)
plot(tmp2.2,type='l',col="red")
par(new=TRUE)
plot(tmp3.2,type='l',col="blue")
dev.off()
image(tmp3)
library(ggplot2)
ggplot() + geom_line(aes(x= seq_along(tmp1.2), y= tmp1.2)) + geom_line(aes(x= seq_along(tmp2.2), y= tmp2.2)) + geom_line(aes(x= seq_along(tmp2.2), y= tmp3.2))
install.packages("shiny")


#Try again - Fri OCT 28 2016
hand[1,]
g = matrix(as.integer(hand[7,2:785]), nrow = 28, byrow=FALSE)
image(g)
order(m)
#This will return the indices of top 10 pixel values
topList <- order(m,decreasing = TRUE)[1:10]
n<-9
#this is row/column of pixel in toplist[n]
exwhy <-c((topList[n] - topList[n] %% 28)/28 + 1, topList[n] %% 28)
m[exwhy[2],exwhy[1]]
x<-g

findWidth<- function(x){
  image(q)
  q <- x
  a <- mean(q)
  a
  tmpPic<-matrix(0,nrow = 28, ncol = 28)
  image(tmpPic)
  for(k in seq(1,50)){
    topList<- which(q > a)
    print(length(topList))
    #topList <- order(x,decreasing = TRUE)
    #for (k in seq(1, 100)){
    #k<-21 
    exwhy <-c((topList[k] - topList[k] %% 28)/28 + 1, topList[1] %% 28)
    m<-6
    s<-vector()
    u<-list()
    w<-data.frame()
    for (i in seq(0,m-1)) {
      angle <- pi*i/m
      t<-vector()
      for (j in seq(1,4)) {
        addX<-round(j*cos(angle))
        addY<-round(j*sin(angle))
        t <- c(t,q[ exwhy[2] + addX, exwhy[1] +addY])
        t <- c(t, q[ exwhy[2] - addX, exwhy[1] -addY ])
        w<-rbind.data.frame(w, c(i,exwhy[2] + addX, exwhy[1] + addY,  x[ exwhy[2] + addX, exwhy[1] +addY]))
        w<-rbind.data.frame(w, c(i,exwhy[2] - addX, exwhy[1] - addY,  x[ exwhy[2] - addX, exwhy[1] -addY]))
        }
      u[[i+1]] <- t
      s <- c(s,length(which(t != 0)))
      }
    #print(s)
    #print(which(s == min(s))[1])
    names(w)<-c('a','b','c','d')
    bestAngle<-which(s == min(s))[1]
    #print(bestAngle)
    shortest<-u[[which(s == min(s))[1]]]
    bestSlice<-w[w$a == bestAngle,]
    #try<-vector()
    #for (i in seq(1,4)){
    #  try<-c(try, shortest[10 - 2*i])
    #}
    #try<-c(try,x[exwhy[2], exwhy[1]])
    #for (i in seq(1,4)){
    #  try<-c(try, shortest[-1 + 2*i])
    #}
    #print(try)
    #print(which(try != 0))
    ##print(7-round(mean(which(try != 0))))
    #distance<-5-round(mean(which(try != 0)))
    #print(distance)
    #offsetX <-  round(cos(bestAngle)*distance)
    #offsetY <-  round(sin(bestAngle)*distance)
      
    #print(c(offsetX,offsetY))
    #print(c(exwhy[2],exwhy[1]))
    #x[20,15]
    #print(c(exwhy[2]+ offsetX, exwhy[1] + offsetY))
    print(bestSlice)
    this<-bestSlice[which(bestSlice$d == max(bestSlice$d))[1],]
    tmpPic[this$b, this$c] <- 1
    #bestSlice<-bestSlice[-which(bestSlice$d == max(bestSlice$d))[1],]
    for (i in seq(1, nrow(bestSlice))){
      q[bestSlice[i,2], bestSlice[i,3]] <- 0
      }
  image(q)
  image(tmpPic)
  }
  #image(x)
  image(q, xlab = 'q')
  image(tmpPic, xlab = 't')
}


findWidth(g)
image(g)
warnings()

