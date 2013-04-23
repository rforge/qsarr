opt.models <- function(cores=2,type=pls,method=cv ,number = 10,repeats = 3,tuneLength = 50, ...){
{if (((type) == pls & (method) == cv)) {
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
      .GlobalEnv[["fitControl"]] <- fitControl
#######################################################
#################Calculating  PLS model################
#######################################################
plsFit<- caret::train(Train.descriptors, unlist(Train.activity), 
                        "pls",
                        tuneLength = tuneLength,
                        trControl = fitControl)
      .GlobalEnv[["plsFit"]] <- plsFit
predVals.pls <- caret::extractPrediction(list(plsFit),
                                           testX = Test.descriptors, 
                                           testY = Test.activity)
      .GlobalEnv[["predVals.pls"]] <- predVals.pls
png(file="models-pls.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.pls))
dev.off()
pls.train <- subset(predVals.pls, dataType == "Training")
pls.test <- subset(predVals.pls, dataType == "Test")
residuo.train.pls <- (pls.train$obs - pls.train$pred)
residuo.test.pls <- (pls.test$obs - pls.test$pred)
residuo.total.pls <- (predVals.pls$obs - predVals.pls$pred)
res.95 <- count(abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
      .GlobalEnv[["Train.activity.opt"]] <- Train.activity.opt
res.95ci <- (res.95$freq[2])*100/nrow(pls.train)
      .GlobalEnv[["Train.activity.opt"]] <- Train.activity.opt
Train.descriptors.opt <- subset(Train.descriptors, abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
      .GlobalEnv[["Train.activity.opt"]] <- Train.activity.opt
Train.activity.opt <- subset(Train.activity, abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
      .GlobalEnv[["Train.activity.opt"]] <- Train.activity.opt
plsFit.opt<- caret::train(Train.descriptors.opt, unlist(Train.activity.opt), 
                        "pls",
                        tuneLength = tuneLength,
                        trControl = fitControl)
      .GlobalEnv[["plsFit.opt"]] <- plsFit.opt
predVals.pls.opt <- caret::extractPrediction(list(plsFit.opt),
                                           testX = Test.descriptors, 
                                           testY = Test.activity)
      .GlobalEnv[["predVals.pls.opt"]] <- predVals.pls.opt
png(file="models-pls-opt.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.pls.opt))
dev.off()  
  } else {      
  print="Empity Model"
}         
}
}