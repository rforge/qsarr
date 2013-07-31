#######################################################
################ Model optimizartion##################
####################################################### 
#######################################################
################Some Useful Variables##################
####################################################### 
rf <- 1
cv=2
opt.models <- function(cores=2,type=rf,method=cv ,number = 5,repeats = 3,tuneLength = 15, ...){
{if (((type) == rf & (method) == cv)) {
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
fitControl <- caret::trainControl(## 5-fold CV
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
                        "rf",
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
residue.95 <- count(abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
      .GlobalEnv[["residue.95"]] <- residue.95
outilier.train.95.pls <- subset(row.names(Train.descriptors), subset = (abs(residuo.train.pls)>2*sd(residuo.train.pls)))
      .GlobalEnv[["outilier.train.95.pls"]] <- outilier.train.95.pls
cat("##### Outliers in Train Set over 95% of the Standard Deviation (2*SD)  #####\n")
print(outilier.train.95.pls)
predictivity.train <- (residue.95$freq[2])*100/nrow(pls.train)
      .GlobalEnv[["predictivity.train"]] <- predictivity.train
Train.descriptors.backup <- Train.descriptors
      .GlobalEnv[["Train.descriptors.backup"]] <- Train.descriptors.backup
Train.activity.backup  <-  Train.activity
      .GlobalEnv[["Train.activity.backup"]] <- Train.activity.backup
Train.descriptors.opt <- subset(Train.descriptors, abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
      .GlobalEnv[["Train.descriptors.opt"]] <- Train.descriptors.opt
Train.activity.opt <- subset(Train.activity, abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
      .GlobalEnv[["Train.activity.opt"]] <- Train.activity.opt
plsFit.opt<- caret::train(Train.descriptors.opt, unlist(Train.activity.opt), 
                        "rf",
                        tuneLength = tuneLength,
                        trControl = fitControl)
      .GlobalEnv[["plsFit.opt"]] <- plsFit.opt
predVals.pls.opt <- caret::extractPrediction(list(plsFit.opt),
                                           testX = Test.descriptors, 
                                           testY = Test.activity)
      .GlobalEnv[["predVals.pls.opt"]] <- predVals.pls.opt
cat("##### The Plot of Optimized PLS Model is saved - file models-pls-opt.png #####\n")
png(file="models-pls-opt.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.pls.opt))
dev.off()
Train.descriptors <- Train.descriptors.opt
      .GlobalEnv[["Train.descriptors"]] <- Train.descriptors
Train.activity <-  Train.activity.opt
      .GlobalEnv[["Train.activity"]] <- Train.activity
  } else {      
  print="Empity Model"
}         
}
}