model <- function(cores=2,type="pls",methodcv="repeatedcv" ,folds = 10,repeats.cv = 3,tuneLength = 15, ... ){
doMC::registerDoMC(2) #change
#fitControl
fitControl <- caret::trainControl(## 10-fold CV
  method = methodcv,
  number = folds,
  ## repeated three times
  repeats = repeats.cv,
  ## Save all the resampling results
  returnResamp = "all")
############################
modelFit<- caret::train(X, unlist(Y), 
                        type,
                        tuneLength = tuneLength,
                        trControl = fitControl)
print(modelFit)
.GlobalEnv[["modelFit"]] <- modelFit
predVals <- caret::extractPrediction(list(modelFit),
                                         testX = X, 
                                         testY = Y)

.GlobalEnv[["predVals"]] <- predVals
}


                        