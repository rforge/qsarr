#export some useful variables
min <- 1
standard <- 2
extended <- 3
kfold <- 4
##
models <- function(type=standard, ...,envir = .GlobalEnv){
{if (((type) == standard)) { 
  fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated three times
    repeats = 3,
    ## Save all the resampling results
    returnResamp = "all")
#################################
#######################################################
###########Calculating Metric for PLS model############
#######################################################
#################################
plsFit<- caret::train(Train.descriptors, unlist(Train.activity), 
                "pls",
                tuneLength = 20,
                trControl = fitControl)
#assign("plsFit", plsFit.func, envir=.GlobalEnv)
predVals.pls <- caret::extractPrediction(list(plsFit),
                                  testX = Test.descriptors, 
                                  testY = Test.activity)
#assign("predVals.pls", predVals.pls.func, envir=.GlobalEnv)
png(file="models-pls.png", width=1430, height=1004, res=144)
caret::plotObsVsPred(predVals.pls)
dev.off()

} else {      
  print="Empity Model"
}         
}
}