opt.models <- function(cores=2,type=small,method=cv ,number = 10,repeats = 3,tuneLength = 15, ...){
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
  res.95ci <- (res.95$freq[2])*100/nrow(pls.train)

  Train.descriptors.opt <- subset(Train.descriptors, abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
  Train.activity.opt <- subset(Train.activity, abs(residuo.train.pls)< (2*sd(residuo.train.pls)))

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
  
  
  pls.train.opt <- subset(predVals.pls.opt, dataType == "Training")
  pls.test.opt <- subset(predVals.pls.opt, dataType == "Test")
  residuo.train.pls.opt <- (pls.train.opt$obs - pls.train.opt$pred)
  residuo.test.pls.opt <- (pls.test.opt$obs - pls.test.opt$pred)
  residuo.total.pls.opt <- (predVals.pls.opt$obs - predVals.pls.opt$pred)
  
  Train.descriptors.opt1 <- subset(Train.descriptors.opt, abs(residuo.train.pls.opt)< (2*sd(residuo.train.pls.opt)))
  Train.activity.opt1 <- subset(Train.activity.opt, abs(residuo.train.pls.opt)< (2*sd(residuo.train.pls.opt)))
  
  plsFit.opt1<- caret::train(Train.descriptors.opt1, unlist(Train.activity.opt1), 
                        "pls",
                        tuneLength = tuneLength,
                        trControl = fitControl)
  .GlobalEnv[["plsFit"]] <- plsFit
  predVals.pls <- caret::extractPrediction(list(plsFit),
                                           testX = Test.descriptors, 
                                           testY = Test.activity)
  
  
  } else {      
  print="Empity Model"
}         
}
}