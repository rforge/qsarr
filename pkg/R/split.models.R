#export some useful variables
random <- 1
ks.md <- 2
ks.ed <- 3
get.optimalpc <- function(prop=0.80,number = 5,repeats = 5,tuneLength = 15, ... ){
  #######################################################
  ##################GET OPTIMAL PCS######################
  ####################################################### 
  fitControl <- caret::trainControl(## 5-fold CV
    method = "repeatedcv",
    number = number,
    ## repeated three times
    repeats = repeats,
    ## Save all the resampling results
    returnResamp = "all")
  plsFit.pca<- caret::train(qsar.descriptors.cor, unlist(qsar.activity), 
                            "pls",
                            tuneLength = tuneLength,
                            trControl = fitControl)
  .GlobalEnv[["plsFit.pca"]] <- plsFit.pca
  pca.pls <- plsFit.pca$results[which.min(plsFit.pca$results[,2] ), ]
  .GlobalEnv[["pca.pls"]] <- pca.pls
  parameters.pca <- pca.pls$ncomp
  .GlobalEnv[["parameters.pca"]] <- parameters.pca
}


split <- function(split=random,prop=0.80,number = 5,repeats = 5,tuneLength = 15, parameters.pca = get.optimalpc(), ... ){
  library(caret)
  library(expm)
  library(matrixStats)
{if ((split) == random) { 
  set.seed(3456)
  TrainIndex.func <- caret::createDataPartition(unlist(qsar.activity), p=prop, list= FALSE, times = 1)
  assign("TrainIndex", TrainIndex.func, envir=.GlobalEnv)
  Train.activity.func <- (as.data.frame(qsar.activity))[TrainIndex, ]
  assign("Train.activity", Train.activity.func, envir=.GlobalEnv)
  training.func <- (as.data.frame(qsar.descriptors.cor))[TrainIndex, ]
  assign("training", training.func, envir=.GlobalEnv)
  Test.activity.func <- (as.data.frame(qsar.activity))[-TrainIndex, ]
  assign("Test.activity", Test.activity.func, envir=.GlobalEnv)
  test.func <- (as.data.frame(qsar.descriptors.cor))[-TrainIndex, ]
  assign("test", test.func, envir=.GlobalEnv)
  preProcValues.func <- caret::preProcess(training, method = c("center", "scale"))
  assign("preProcValues", preProcValues.func, envir=.GlobalEnv)
  Train.descriptors.func <- stats::predict(preProcValues, training)
  assign("Train.descriptors", Train.descriptors.func, envir=.GlobalEnv)
  Test.descriptors.func <- stats::predict(preProcValues, test)
  assign("Test.descriptors", Test.descriptors.func, envir=.GlobalEnv)
  TRAIN<-cbind(Train.activity,Train.descriptors)
  assign("TRAIN", TRAIN, envir=.GlobalEnv)
  TEST<-cbind(Test.activity,Test.descriptors)
  assign("TEST", TEST, envir=.GlobalEnv)
  
  
}else if ((split) == ks.md) { 
  
  pca.desc <-  prcomp(qsar.descriptors.cor)
  .GlobalEnv[["pca.desc"]] <- pca.desc
  kvalue <- prop*nrow(qsar.descriptors.cor)
  .GlobalEnv[["kvalue"]] <- kvalue
  #######################################################
  #################Calculating  KS################
  #######################################################
  kennardStone.desc <- kennardStone(pca.desc$x[ ,1:parameters.pca], profN = NULL, k =kvalue , distance = "MD", StartCenter = TRUE)
  .GlobalEnv[["kennardStone.desc"]] <- kennardStone.desc
  TrainIndex <- kennardStone.desc$cal
  .GlobalEnv[["TrainIndex"]] <- TrainIndex
  Train.activity.func <- (as.data.frame(qsar.activity))[TrainIndex, ]
  assign("Train.activity", Train.activity.func, envir=.GlobalEnv)
  training.func <- (as.data.frame(qsar.descriptors.cor))[TrainIndex, ]
  assign("training", training.func, envir=.GlobalEnv)
  Test.activity.func <- (as.data.frame(qsar.activity))[-TrainIndex, ]
  assign("Test.activity", Test.activity.func, envir=.GlobalEnv)
  test.func <-  (as.data.frame(qsar.descriptors.cor))[-TrainIndex, ]
  assign("test", test.func, envir=.GlobalEnv)
  preProcValues.func <- caret::preProcess(training, method = c("center", "scale"))
  assign("preProcValues", preProcValues.func, envir=.GlobalEnv)
  Train.descriptors.func <- stats::predict(preProcValues, training)
  assign("Train.descriptors", Train.descriptors.func, envir=.GlobalEnv)
  Test.descriptors.func <- stats::predict(preProcValues, test)
  assign("Test.descriptors", Test.descriptors.func, envir=.GlobalEnv)
  TRAIN<-cbind(Train.activity,Train.descriptors)
  assign("TRAIN", TRAIN, envir=.GlobalEnv)
  TEST<-cbind(Test.activity,Test.descriptors)
  assign("TEST", TEST, envir=.GlobalEnv)  
  
} else if ((split) == ks.ed){
  pca.desc <-  prcomp(qsar.descriptors.cor)
  .GlobalEnv[["pca.desc"]] <- pca.desc
  kvalue <- prop*nrow(qsar.descriptors.cor)
  .GlobalEnv[["kvalue"]] <- kvalue
  #######################################################
  #################Calculating  KS################
  #######################################################
  kennardStone.desc <- kennardStone(pca.desc$x[ ,1:parameters.pca], profN = NULL, k =kvalue , distance = "ED", StartCenter = TRUE)
  .GlobalEnv[["kennardStone.desc"]] <- kennardStone.desc
  TrainIndex <- kennardStone.desc$cal
  .GlobalEnv[["TrainIndex"]] <- TrainIndex
  Train.activity.func <- (as.data.frame(qsar.activity))[TrainIndex, ]
  assign("Train.activity", Train.activity.func, envir=.GlobalEnv)
  training.func <- (as.data.frame(qsar.descriptors.cor))[TrainIndex, ]
  assign("training", training.func, envir=.GlobalEnv)
  Test.activity.func <- (as.data.frame(qsar.activity))[-TrainIndex, ]
  assign("Test.activity", Test.activity.func, envir=.GlobalEnv)
  test.func <-  (as.data.frame(qsar.descriptors.cor))[-TrainIndex, ]
  assign("test", test.func, envir=.GlobalEnv)
  preProcValues.func <- caret::preProcess(training, method = c("center", "scale"))
  assign("preProcValues", preProcValues.func, envir=.GlobalEnv)
  Train.descriptors.func <- stats::predict(preProcValues, training)
  assign("Train.descriptors", Train.descriptors.func, envir=.GlobalEnv)
  Test.descriptors.func <- stats::predict(preProcValues, test)
  assign("Test.descriptors", Test.descriptors.func, envir=.GlobalEnv)
  TRAIN<-cbind(Train.activity,Train.descriptors)
  assign("TRAIN", TRAIN, envir=.GlobalEnv)
  TEST<-cbind(Test.activity,Test.descriptors)
  assign("TEST", TEST, envir=.GlobalEnv)
} else {      
  print="Empity Model"
}         
  }
}


