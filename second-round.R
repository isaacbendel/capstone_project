setwd('/home/bendel/R/Hand/')
getwd()
#hand <-read.csv('train.csv')
#handsmall<-head(hand, n=1000)
#write.csv(handsmall,'a')
hand<-read.csv('a')
hand
m = matrix(as.integer(hand[8,2:785]), nrow = 28, byrow=FALSE)
image(m)
angleFineness<-8
#n<-m
m<-n
w<-data.frame()
tmpPic<-matrix(0,nrow = 28, ncol = 28)
image(as.matrix(tmpPic))
tmpPic
for (i in seq(1,angleFineness - 1)){
  angle<- pi*i/(angleFineness*2)
  aX<-cos(angle)
  aY<-sin(angle)
  t<-vector()
  found<-FALSE
  for (j in seq(1,28)){
  if(round(j*aX)*round(aY*j) > 0){
    if ((found == FALSE)&&(m[round(j*aX), round(j*aY)] > 0)) {
      found<-TRUE
      tmpPic[round(j*aX), round(j*aY)] <- 1
    }
  } 
  }
}

image(as.matrix(tmpPic))
image(m)
#t<-c(t, m[round(j*aX), round(j*aY)])
#print(c(round(j*aX), round(j*aY)))
#m[round(j*aX), round(j*aY)]<- - 10

#print(t)
#w<-rbind.data.frame(w,t)



w
image(m)
tmpPic<-matrix(0,nrow = 28, ncol = 28)
for (i in seq(1,28)){
  t<-vector()
  found<-FALSE
  for (j in seq(1,28)){
    if ((found == FALSE)&&(m[round(i), round(j)] > 0)) {
      found<-TRUE
      tmpPic[round(i), round(j)] <- 1
    }
    } 
  }
for (i in seq(1,28)){
  t<-vector()
  found<-FALSE
  for (j in seq(28,1)){
    if ((found == FALSE)&&(m[round(i), round(j)] > 0)) {
      found<-TRUE
      tmpPic[round(i), round(j)] <- 1
    }
  } 
}
image(tmpPic)
tmpPic1<-matrix(0,nrow = 28, ncol = 28)
for (i in seq(1,28)){
  t<-vector()
  found<-FALSE
  for (j in seq(1,28)){
    if ((found == FALSE)&&(m[round(j), round(i)] > 0)) {
      found<-TRUE
      tmpPic1[round(j), round(i)] <- 1
    }
  } 
}
for (i in seq(1,28)){
  t<-vector()
  found<-FALSE
  for (j in seq(28,1)){
    if ((found == FALSE)&&(m[round(j), round(i)] > 0)) {
      found<-TRUE
      tmpPic1[round(j), round(i)] <- 1
    }
  } 
}

image(tmpPic1)
image(tmpPic1 + tmpPic)
