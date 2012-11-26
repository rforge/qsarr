#######################################################
################Some Useful Variables##################
####################################################### 
small <- 1
standard <- 2
extended <- 3
kfold <- 4
cv <- 1
#######################################################
models <- function(cores=2,type=small,method=cv ,number = 10,repeats = 3,tuneLength = 15, ...){
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
#######################################################
################Calculating  MARS model################
#######################################################
earthFit <- caret::train(Train.descriptors, unlist(Train.activity),
                  "earth",
                  tuneLength = tuneLength,
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
knnFit <- caret::train(Train.descriptors, unlist(Train.activity),
                "knn",
                tuneLength = tuneLength,
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
rfFit <- caret::train(Train.descriptors, unlist(Train.activity),
               "rf",
               tuneLength = tuneLength,
               trControl = fitControl)
.GlobalEnv[["rfFit"]] <- rfFit
predVals.rf <- caret::extractPrediction(list(rfFit),
                                 testX = Test.descriptors,
                                 testY = Test.activity)
.GlobalEnv[["predVals.rf"]] <- predVals.rf
png(file="models-random_forest.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.rf))
dev.off()
#######################################################
###################Comparing All Models################
#######################################################
extract.all.models <- list(pls = plsFit, mars = earthFit, knn = knnFit, rf = rfFit)
.GlobalEnv[["extract.all.models"]] <- extract.all.models
all.modelos <- caret::extractPrediction(extract.all.models, testX = Test.descriptors, testY = Test.activity)
.GlobalEnv[["all.modelos"]] <- all.modelos
png(file="models-compare-all.png", width=1631, height=1050, res=102)
plot(plotObsVsPred(all.modelos))
dev.off()
} else if (((type) == standard & (method) == cv)){
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
#######################################################
################Calculating  MARS model################
#######################################################
earthFit <- caret::train(Train.descriptors, unlist(Train.activity),
                         "earth",
                         tuneLength = tuneLength,
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
knnFit <- caret::train(Train.descriptors, unlist(Train.activity),
                       "knn",
                       tuneLength = tuneLength,
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
rfFit <- caret::train(Train.descriptors, unlist(Train.activity),
                      "rf",
                      tuneLength = tuneLength,
                      trControl = fitControl)
.GlobalEnv[["rfFit"]] <- rfFit
predVals.rf <- caret::extractPrediction(list(rfFit),
                                        testX = Test.descriptors,
                                        testY = Test.activity)
.GlobalEnv[["predVals.rf"]] <- predVals.rf
png(file="models-random_forest.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.rf))
dev.off()
#######################################################
######Calculating Support Vector Machine model#########
#######################################################
svmFit <- caret::train(Train.descriptors, unlist(Train.activity),
                      "svmRadialCost",
                      tuneLength = tuneLength,
                      trControl = fitControl)
.GlobalEnv[["svmFit"]] <- svmFit
predVals.svm <- caret::extractPrediction(list(rfFit),
                                        testX = Test.descriptors,
                                        testY = Test.activity)
.GlobalEnv[["predVals.svm"]] <- predVals.svm
png(file="models-svm.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.svm))
dev.off()
#######################################################
############Calculating Elastic Net model##############
#######################################################
enFit <- caret::train(Train.descriptors, unlist(Train.activity),
                       "glmnet",
                       tuneLength = tuneLength,
                       trControl = fitControl)
.GlobalEnv[["enFit"]] <- enFit
predVals.en <- caret::extractPrediction(list(rfFit),
                                         testX = Test.descriptors,
                                         testY = Test.activity)
.GlobalEnv[["predVals.en"]] <- predVals.en
png(file="models-elastic_net.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.en))
dev.off()
#######################################################
###################Comparing All Models################
#######################################################
extract.all.models <- list(pls = plsFit, mars = earthFit, knn = knnFit, rf = rfFit, svm = svmFit, Elastic.Net = enFit)
.GlobalEnv[["extract.all.models"]] <- extract.all.models
all.modelos <- caret::extractPrediction(extract.all.models, testX = Test.descriptors, testY = Test.activity)
.GlobalEnv[["all.modelos"]] <- all.modelos
png(file="models-compare-all.png", width=1631, height=1050, res=102)
plot(plotObsVsPred(all.modelos))
dev.off()
}else if (((type) == extended & (method) == cv)){
#######################################################
#############Calculating MODELS EXTENDED##############
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
  #######################################################
  ################Calculating  MARS model################
  #######################################################
  earthFit <- caret::train(Train.descriptors, unlist(Train.activity),
                           "earth",
                           tuneLength = tuneLength,
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
  knnFit <- caret::train(Train.descriptors, unlist(Train.activity),
                         "knn",
                         tuneLength = tuneLength,
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
  rfFit <- caret::train(Train.descriptors, unlist(Train.activity),
                        "rf",
                        tuneLength = tuneLength,
                        trControl = fitControl)
  .GlobalEnv[["rfFit"]] <- rfFit
  predVals.rf <- caret::extractPrediction(list(rfFit),
                                          testX = Test.descriptors,
                                          testY = Test.activity)
  .GlobalEnv[["predVals.rf"]] <- predVals.rf
  png(file="models-random_forest.png", width=1430, height=1004, res=144)
  plot(caret::plotObsVsPred(predVals.rf))
  dev.off()
  #######################################################
  ######Calculating Support Vector Machine model#########
  #######################################################
  svmFit <- caret::train(Train.descriptors, unlist(Train.activity),
                         "svmRadialCost",
                         tuneLength = tuneLength,
                         trControl = fitControl)
  .GlobalEnv[["svmFit"]] <- svmFit
  predVals.svm <- caret::extractPrediction(list(svmFit),
                                           testX = Test.descriptors,
                                           testY = Test.activity)
  .GlobalEnv[["predVals.svm"]] <- predVals.svm
  png(file="models-svm.png", width=1430, height=1004, res=144)
  plot(caret::plotObsVsPred(predVals.svm))
  dev.off()
#######################################################
############Calculating Elastic Net model##############
#######################################################
  enFit <- caret::train(Train.descriptors, unlist(Train.activity),
                        "glmnet",
                        tuneLength = tuneLength,
                        trControl = fitControl)
  .GlobalEnv[["enFit"]] <- enFit
  predVals.en <- caret::extractPrediction(list(enFit),
                                          testX = Test.descriptors,
                                          testY = Test.activity)
  .GlobalEnv[["predVals.en"]] <- predVals.en
  png(file="models-elastic_net.png", width=1430, height=1004, res=144)
  plot(caret::plotObsVsPred(predVals.en))
  dev.off()
#######################################################
############Calculating bagEarth model##############
#######################################################
beFit <- caret::train(Train.descriptors, unlist(Train.activity),
                      "bagEarth",
                      tuneLength = tuneLength,
                      trControl = fitControl)
.GlobalEnv[["beFit"]] <- beFit
predVals.be <- caret::extractPrediction(list(beFit),
                                        testX = Test.descriptors,
                                        testY = Test.activity)
.GlobalEnv[["predVals.be"]] <- predVals.be
png(file="models-bagEarth.png", width=1430, height=1004, res=144)
plot(caret::plotObsVsPred(predVals.be))
dev.off()
  #######################################################
  ###################Comparing All Models################
  #######################################################
  extract.all.models <- list(pls = plsFit, mars = earthFit, knn = knnFit, rf = rfFit, svm = svmFit, Elastic.Net = enFit, bagEarth=beFit)
  .GlobalEnv[["extract.all.models"]] <- extract.all.models
  all.modelos <- caret::extractPrediction(extract.all.models, testX = Test.descriptors, testY = Test.activity)
  .GlobalEnv[["all.modelos"]] <- all.modelos
  png(file="models-compare-all.png", width=1631, height=1050, res=102)
  plot(plotObsVsPred(all.modelos))
  dev.off()
} else {      
  print="Empity Model"
}         
}
}
