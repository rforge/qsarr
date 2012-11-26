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
#############Calculating MODELS STANDARD ##############
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
###########Calculating Metric for PLS model############
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
