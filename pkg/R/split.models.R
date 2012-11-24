#export some useful variables
random <- 1
ks <- 2
##
split <- function(type=random,prop=0.75, ... ){
library(caret)
{if ((type) == random) { 
  set.seed(3456)
  TrainIndex.func <- caret::createDataPartition(unlist(qsar.activity), p=prop, list= FALSE, times = 1)
  assign("TrainIndex", TrainIndex.func, envir=.GlobalEnv)
  environment(myVal) = globalenv(TrainIndex)
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
} else if ((type) == ks){
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
} else {      
  print="Empity Model"
}         
}
}

 



