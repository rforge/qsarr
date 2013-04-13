###http://www.inside-r.org/packages/cran/FNN/docs/knnx.dist
knn.ad <- function(cores=2,type=small,method=cv ,number = 15,repeats = 3,tuneLength = 15, ...){
{  
  library(doMC)
  doMC::registerDoMC(cores)
  fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = number,
    ## repeated three times
    repeats = repeats,
    ## Save all the resampling results
    returnResamp = "all")
knnFit.ad <- caret::train(Train.descriptors, unlist(Train.activity),
                       "knn",
                       tuneLength = tuneLength,
                       trControl = fitControl)
.GlobalEnv[["knnFit.ad"]] <- knnFit.ad
}
}

knn.ad.re <- function(cores=2,type=small, ...){
{
ktrain=knn.dist(Train.descriptors, k=5,  algorithm=c("cover_tree") )

ktest=knnx.dist(data=Train.descriptors,Test.descriptors, k=5, algorithm=c("cover_tree") ) 

Dc=(0.5*(sd(kt[,5])))+mean(kt[,5])
ktest[,5]<Dc
}
}

