confusionMatrix<-function(realValues, predictedValues, justAccuracy = FALSE){
  library(formattable)
  t<-table(realValues, predictedValues)
  t
  df<-data.frame()
  for (i in seq(1,nrow(t))){
    df<-rbind.data.frame(df,t[i,])
  }
  names(df)<-c('0','1','2','3','4','5','6','7','8','9')
  df$`True Value`<-seq(0,nrow(t)-1)
  df<-df[,c(11,1,2,3,4,5,6,7,8,9,10)]
  numCorrect <- 0
  for (i in seq(2,11)){
    numCorrect <- numCorrect + df[i-1,i]
  }
  accuracy <-numCorrect/sum(df)
  if (justAccuracy == TRUE){
    return(accuracy)
  }
  else{
    print(paste('Accuracy is ',accuracy, sep = ' '))
    return(formattable(df, list(area(col = c(2,3,4,5,6,7,8,9,10,11) ) ~ color_tile("white", "blue"))))
  }
}


