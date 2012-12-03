#export some useful variables
random <- 1
ks <- 2
##
split <- function(split=random,prop=0.75, ... ){
library(caret)
{if ((split) == random) { 
  set.seed(3456)
  TrainIndex.func <- caret::createDataPartition(unlist(qsar.activity), p=prop, list= FALSE, times = 1)
  assign("TrainIndex", TrainIndex.func, envir=.GlobalEnv)
  Train.activity.func <- qsar.activity[TrainIndex, ]
  assign("Train.activity", Train.activity.func, envir=.GlobalEnv)
  training.func <- qsar.descriptors.cor[TrainIndex, ]
  assign("training", training.func, envir=.GlobalEnv)
  Test.activity.func <- qsar.activity[-TrainIndex, ]
  assign("Test.activity", Test.activity.func, envir=.GlobalEnv)
  test.func <- qsar.descriptors.cor[-TrainIndex, ]
  assign("test", test.func, envir=.GlobalEnv)
  preProcValues.func <- caret::preProcess(training, method = c("center", "scale"))
  assign("preProcValues", preProcValues.func, envir=.GlobalEnv)
  Train.descriptors.func <- stats::predict(preProcValues, training)
  assign("Train.descriptors", Train.descriptors.func, envir=.GlobalEnv)
  Test.descriptors.func <- stats::predict(preProcValues, test)
  assign("Test.descriptors", Test.descriptors.func, envir=.GlobalEnv)
} else if ((split) == ks){
  pca.desc <-  prcomp(qsar.descriptors.cor)
  .GlobalEnv[["pca.desc"]] <- pca.desc
  kvalue <- prop*nrow(qsar.descriptors.cor)
  .GlobalEnv[["kvalue"]] <- kvalue
  kennardStone.desc <- kennardStone(pca.desc$x, profN = NULL, k =kvalue , distance = "MD", StartCenter = TRUE)
  .GlobalEnv[["kennardStone.desc"]] <- kennardStone.desc
  TrainIndex <- kennardStone.desc$cal
  .GlobalEnv[["TrainIndex"]] <- TrainIndex
  Train.activity.func <- qsar.activity[TrainIndex, ]
  assign("Train.activity", Train.activity.func, envir=.GlobalEnv)
  training.func <- qsar.descriptors.cor[TrainIndex, ]
  assign("training", training.func, envir=.GlobalEnv)
  Test.activity.func <- qsar.activity[-TrainIndex, ]
  assign("Test.activity", Test.activity.func, envir=.GlobalEnv)
  test.func <- qsar.descriptors.cor[-TrainIndex, ]
  assign("test", test.func, envir=.GlobalEnv)
  preProcValues.func <- caret::preProcess(training, method = c("center", "scale"))
  assign("preProcValues", preProcValues.func, envir=.GlobalEnv)
  Train.descriptors.func <- stats::predict(preProcValues, training)
  assign("Train.descriptors", Train.descriptors.func, envir=.GlobalEnv)
  Test.descriptors.func <- stats::predict(preProcValues, test)
  assign("Test.descriptors", Test.descriptors.func, envir=.GlobalEnv)
} else {      
  print="Empity Model"
}         
}
}

 



