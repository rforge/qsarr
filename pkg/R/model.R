#export some useful variables
small <- 1
standard <- 2
extended <- 3
kfold <- 4
cv <- 1

##
models <- function(cores=2,type=small,method=cv,number = 10,repeats = 3, ...){
{if (((type) == small & (method) == cv)) {
#######################################################
#############Calculating MODELS SMALL ##############
#######################################################
#######################################################
############Paralelization number of cores#############
#######################################################  
library(doMC)
doMC::registerDoMC(cores)
#######################################################
#######################CV method#######################
####################################################### 
fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = number,
    ## repeated three times
    repeats = repeats,
    ## Save all the resampling results
    returnResamp = "all")
#######################################################
#################Calculating  PLS model################
#######################################################
plsFit<- caret::train(Train.descriptors, unlist(Train.activity), 
                "pls",
                tuneLength = 20,
                trControl = fitControl)
.GlobalEnv[["plsFit"]] <- plsFit
predVals.pls <- caret::extractPrediction(list(plsFit),
                                  testX = Test.descriptors, 
                                  testY = Test.activity)
.GlobalEnv[["predVals.pls"]] <- predVals.pls
  png(file="models-pls.png", width=1430, height=1004, res=144)
  plot(caret::plotObsVsPred(predVals.pls))
  dev.off()
#######################################################
################Calculating  MARS model################
#######################################################
earthFit <- caret::train(Train.descriptors, unlist(Train.activity),
                  "earth",
                  tuneLength = 20,
                  trControl = fitControl)
.GlobalEnv[["earthFit"]] <- earthFit
predVals.earth <- caret::extractPrediction(list(earthFit),
                                    testX = Test.descriptors,
                                    testY = Test.activity)
.GlobalEnv[["predVals.earth"]] <- predVals.earth
png(file="models-earth.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.earth))
dev.off()
#######################################################
################Calculating  KNN model#################
#######################################################
knnFit <- care::train(Train.descriptors, unlist(Train.activity),
                "knn",
                tuneLength = 20,
                trControl = fitControl)
.GlobalEnv[["knnFit"]] <- knnFit
predVals.knn <- caret::extractPrediction(list(knnFit),
                                  testX = Test.descriptors,
                                  testY = Test.activity)
.GlobalEnv[["predVals.knn"]] <- predVals.knn
png(file="models-knn.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.knn))
dev.off()
#######################################################
##########Calculating random forest model#############
#######################################################
rfFit <- train(Train.descriptors, unlist(Train.activity),
               "rf",
               tuneLength = 20,
               trControl = fitControl)
.GlobalEnv[["rfFit"]] <- rfFit
predVals.rf <- extractPrediction(list(rfFit),
                                 testX = Test.descriptors,
                                 testY = Test.activity)
png(file="models-random_forest.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.rf))
dev.off()
#######################################################
#############Calculating MODELS STANDARD ##############
#######################################################
#######################################################
############Paralelization number of cores#############
#######################################################  
} else if (((type) == standard & (method) == cv)){;
  library(doMC)
  doMC::registerDoMC(cores)
                                                  
  fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = number,
    ## repeated three times
    repeats = repeats,
    ## Save all the resampling results
    returnResamp = "all")
  #################################
  #######################################################
  ###########Calculating Metric for PLS model############
  #######################################################
  #################################
earthFit <- train(Train.descriptors, unlist(Train.activity),
                    "earth",
                    tuneLength = 20,
                    trControl = fitControl)
predVals.earth <- extractPrediction(list(earthFit),
                                      testX = Test.descriptors,
                                      testY = Test.activity)
png(file="models-earth.png", width=1430, height=1004, res=144)
plot(plotObsVsPred(predVals.earth))
dev.off()
#######################################################
#############Calculating MODELS STANDARD ##############
#######################################################
#######################################################
############Paralelization number of cores#############
#######################################################  
} else if (((type) == extended & (method) == cv)){;
library(doMC)
doMC::registerDoMC(cores)
                                                  
fitControl <- caret::trainControl(## 10-fold CV
method = "repeatedcv",
number = number,
## repeated three times
repeats = repeats,
## Save all the resampling results
returnResamp = "all")
#######################################################
###########Calculating Metric for PLS model############
#######################################################
earthFit <- train(Train.descriptors, unlist(Train.activity),
                                    "earth",
                                     tuneLength = 20,
                                     trControl = fitControl)
predVals.earth <- extractPrediction(list(earthFit),
                                      testX = Test.descriptors,
                                      testY = Test.activity)
png(file="models-earth.png", width=1430, height=1004, res=144)
plot(plotObsVsPred(predVals.earth))
dev.off()

} else {      
  print="Empity Model"
}         
}
}
