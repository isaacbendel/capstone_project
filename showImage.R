showImage<-function(x, palette, label = ''){
    return(image(matrix(as.integer(x[2:785]), nrow = 28, byrow=FALSE)[,c(seq(28,1))],col = palette, axes = FALSE, xlab = label))
}


knitr::spin('knn.R')
beepr::beep(8)
