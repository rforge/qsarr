#######################################################
###########Loading all Metric functions################
#######################################################
metrics <- function(type=small, ...){
{if ((type) == small) {
#######################################################
###########Calculating Metric for PLS model############
#######################################################
pls.train <- subset(predVals.pls, dataType == "Training")
pls.test <- subset(predVals.pls, dataType == "Test")
ntest <- nrow(Test.descriptors)
ntrain <- nrow(Train.descriptors)
residuo.train.pls <- (pls.train$obs - pls.train$pred)
residuo.test.pls <- (pls.test$obs - pls.test$pred)
residuo.total.pls <- (predVals.pls$obs - predVals.pls$pred)
################durbinWatsonTest##############
dw.train.pls<-durbinWatsonTest(residuo.train.pls)
dw.test.pls<-durbinWatsonTest(residuo.test.pls)
dw.pls<-durbinWatsonTest(residuo.total.pls)
################remover 3x maior vairaça##############
sigma=(sum(((residuo.total.pls - mean(residuo.total.pls))^2)))/(ntrain+ntest)
sigma=sqrt(sd(qsar.activity$pIC50))
pls.remove.out<-residuo.test.pls[(abs(residuo.test.pls)< (3*sigma))]
plsre=c(residuo.train.pls,pls.remove.out)
durbinWatsonTest(plsre)
###########Calculating the 95% CI for training##########
res.95 <- count(abs(residuo.train.pls)< (2*sd(residuo.train.pls)))
res.95ci <- (res.95$freq[2])*100/nrow(pls.train)
###########Calculating the 99% CI for testing##########
res.99 <- count(abs(residuo.test.pls)< sd(residuo.test.pls))
predictability<- (res.99$freq[2])*100/nrow(pls.test)
####################################################
pls.remove.out<-residuo.test.pls[(abs(residuo.test.pls)< sd(residuo.test.pls))]
residuo.total.pls<- 
###########Calculating the Classic Parameters##########
R2.pls.summary <- summary(lm(pls.train$pred ~ pls.train$obs))
R2.pls <- R2.pls.summary$r.squared
RMSEC.pls <- qsarm::RMSEC(pls.train$obs,pls.train$pred,ntrain)
CV.pls <- plsFit$results[which.min(plsFit$results[,2] ), ]
parameters.pls <- CV.pls$ncomp
RMSECV.pls <- CV.pls$RMSE
RMSEP.pls <- qsarm::RMSEP(pls.test$obs,pls.test$pred,ntest)
MAE.pls <- qsarm::MAE(pls.test$obs,pls.test$pred,ntest)
Q2.pls <- CV.pls$Rsquared
R2.pred.pls <- qsarm::r2pred(pls.test$pred,pls.test$obs,pls.train$obs)
.GlobalEnv[["R2.pred.pls"]] <- R2.pred.pls
###########Calculating the Q2f3##########
Q2f3.pls <- qsarm::Q2f3(pls.test$pred,pls.test$obs,pls.train$obs,ntest,ntrain)
.GlobalEnv[["Q2f3.pls"]] <- Q2f3.pls
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.pls <- qsarm::rm2(pls.train$obs,pls.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.pls <-  qsarm::rm2.reverse(pls.train$obs,pls.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.pls <- qsarm::average.rm2(pls.train$obs,pls.train$pred) 
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.pls <- qsarm::delta.rm2(pls.train$obs,pls.train$pred)  
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.pls <-  qsarm::rm2(pls.test$obs,pls.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.pls <-  qsarm::rm2.reverse(pls.test$obs,pls.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.pls <- qsarm::average.rm2(pls.test$obs,pls.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.pls <- qsarm::delta.rm2(pls.test$obs,pls.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.pls <-  qsarm::rm2(predVals.pls$obs,predVals.pls$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.pls <-  qsarm::rm2.reverse(predVals.pls$obs,predVals.pls$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.pls <- qsarm::average.rm2(predVals.pls$obs,predVals.pls$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.pls <- qsarm::delta.rm2(predVals.pls$obs,predVals.pls$pred)


#######################################################
###########Calculating Metric for MARS model###########
#######################################################
earth.train <- subset(predVals.earth, dataType == "Training")
earth.test <- subset(predVals.earth, dataType == "Test")
residuo.train.earth <- (earth.train$obs - earth.train$pred)
residuo.test.earth <- (earth.test$obs - earth.test$pred)
residuo.total.earth <- (predVals.earth$obs - predVals.earth$pred)
R2.earth.summary <- summary(lm(earth.train$pred ~ earth.train$obs))
R2.earth <- R2.earth.summary$r.squared
RMSEC.earth <- sqrt(mean(residuo.train.earth^2))
CV.earth <- earthFit$results[which.min(earthFit$results[,3] ), ]
parameter1.earth <- CV.earth$degree
parameter2.earth <- CV.earth$nprune
RMSECV.earth <- CV.earth$RMSE
RMSEP.earth <- sqrt(mean(residuo.test.earth^2))
Q2.earth <- CV.earth$Rsquared
R2.pred.earth <- qsarm::r2pred(earth.test$pred,earth.test$obs,earth.train$obs)
###########Calculating the Q2f3##########
Q2f3.earth <- qsarm::Q2f3(predVals.earth$obs,predVals.earth$pred,pls.train$pred,ntest.pls,ntrain.pls)
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.earth <-  qsarm::rm2(earth.train$obs,earth.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.earth <-  qsarm::rm2.reverse(earth.train$obs,earth.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.earth <- qsarm::average.rm2(earth.train$obs,earth.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.earth <- qsarm::delta.rm2(earth.train$obs,earth.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.earth <-  qsarm::rm2(earth.test$obs,earth.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.earth <-  qsarm::rm2.reverse(earth.test$obs,earth.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.earth <- qsarm::average.rm2(earth.test$obs,earth.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.earth <- qsarm::delta.rm2(earth.test$obs,earth.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.earth <-  qsarm::rm2(predVals.earth$obs,predVals.earth$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.earth <-  qsarm::rm2.reverse(predVals.earth$obs,predVals.earth$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.earth <- qsarm::average.rm2(predVals.earth$obs,predVals.earth$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.earth<- qsarm::delta.rm2(predVals.earth$obs,predVals.earth$pred)
#######################################################
###########Calculating Metric for KNN model############
#######################################################
knn.train <- subset(predVals.knn, dataType == "Training")
knn.test <- subset(predVals.knn, dataType == "Test")
residuo.train.knn <- (knn.train$obs - knn.train$pred)
residuo.test.knn <- (knn.test$obs - knn.test$pred)
residuo.total.knn <- (predVals.knn$obs - predVals.knn$pred)
R2.knn.summary <- summary(lm(knn.train$pred ~ knn.train$obs))
R2.knn <- R2.knn.summary$r.squared
RMSEC.knn <- sqrt(mean(residuo.train.knn^2))
CV.knn <- knnFit$results[which.min(knnFit$results[,2] ), ]
parameter.knn <- CV.knn$k
RMSECV.knn <- CV.knn$RMSE
RMSEP.knn <- sqrt(mean(residuo.test.knn^2))
Q2.knn <- CV.knn$Rsquared
R2.pred.knn <- qsarm::r2pred(predVals.knn$obs,predVals.knn$pred)
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.knn <-  qsarm::rm2(knn.train$obs,knn.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.knn <-  qsarm::rm2.reverse(knn.train$obs,knn.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.knn <- qsarm::average.rm2(knn.train$obs,knn.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.knn <- qsarm::delta.rm2(knn.train$obs,knn.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.knn <-  qsarm::rm2(knn.test$obs,knn.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.knn <-  qsarm::rm2.reverse(knn.test$obs,knn.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.knn <- qsarm::average.rm2(knn.test$obs,knn.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.knn <- qsarm::delta.rm2(knn.test$obs,knn.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.knn <-  qsarm::rm2(predVals.knn$obs,predVals.knn$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.knn <-  qsarm::rm2.reverse(predVals.knn$obs,predVals.knn$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.knn <- qsarm::average.rm2(predVals.knn$obs,predVals.knn$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.knn <- qsarm::delta.rm2(predVals.knn$obs,predVals.knn$pred)
#######################################################
###########Calculating Metric for KNN model############
#######################################################
rf.train <- subset(predVals.rf, dataType == "Training")
rf.test <- subset(predVals.rf, dataType == "Test")
residuo.train.rf <- (rf.train$obs - rf.train$pred)
residuo.test.rf <- (rf.test$obs - rf.test$pred)
residuo.total.rf <- (predVals.rf$obs - predVals.rf$pred)
R2.rf.summary <- summary(lm(rf.train$pred ~ rf.train$obs))
R2.rf <- R2.rf.summary$r.squared
RMSEC.rf <- sqrt(mean(residuo.train.rf^2))
CV.rf <- rfFit$results[which.min(rfFit$results[,2] ), ]
parameter.rf <- CV.rf$mtry
RMSECV.rf <- CV.rf$RMSE
RMSEP.rf <- sqrt(mean(residuo.test.rf^2))
Q2.rf <- CV.rf$Rsquared
R2.pred.rf <- qsarm::r2pred(predVals.rf$obs,predVals.rf$pred)
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.rf <-  qsarm::rm2(rf.train$obs,rf.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.rf <-  qsarm::rm2.reverse(rf.train$obs,rf.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.rf <- qsarm::average.rm2(rf.train$obs,rf.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.rf <- qsarm::delta.rm2(rf.train$obs,rf.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.rf <-  qsarm::rm2(rf.test$obs,rf.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.rf <-  qsarm::rm2.reverse(rf.train$obs,rf.train$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.rf <- qsarm::average.rm2(rf.train$obs,rf.train$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.rf <- qsarm::delta.rm2(rf.train$obs,rf.train$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.rf <-  qsarm::rm2(predVals.pls$obs,predVals.pls$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.rf <-  qsarm::rm2.reverse(predVals.rf$obs,predVals.rf$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.rf <- qsarm::average.rm2(predVals.rf$obs,predVals.rf$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.rf <- qsarm::delta.rm2(predVals.rf$obs,predVals.rf$pred)
#######################################################
###########Loading all Metric functions################
#######################################################
###
M <- list(Metrics=matrix(c(R2.pls,RMSEC.pls,Q2.pls,RMSECV.pls,RMSEP.pls,R2.pred.pls, R2.earth,Q2.earth,RMSEC.earth,RMSECV.earth,RMSEP.earth,R2.pred.earth,  R2.knn,Q2.knn,RMSEC.knn,RMSECV.knn,RMSEP.knn,R2.pred.knn,  R2.rf,Q2.rf,RMSEC.rf,RMSECV.rf,RMSEP.rf,R2.pred.rf),byrow=TRUE,ncol=6))
M$Metrics <- round(M$Metrics,digits=3)
.GlobalEnv[["M"]] <- M

Mdata <- list(Mdata=matrix(c(parameters.pls, "-", parameter1.earth,parameter2.earth,parameter.knn,"-",parameter.rf,"-"),byrow=TRUE,ncol=2))
.GlobalEnv[["Mdata"]] <- Mdata

#
#M <- list(Metrics=matrix(c(R2.pls,Q2.pls,RMSEC.pls,RMSECV.pls,RMSEP.pls,R2.pred.pls,delta.rm2.test.pls,R2.earth,Q2.earth,RMSEC.earth,RMSECV.earth,RMSEP.earth,R2.pred.earth,average.rm2.test.earth,delta.rm2.test.earth,R2.knn,Q2.knn,RMSEC.knn,RMSECV.knn,RMSEP.knn,R2.pred.knn,average.rm2.test.knn,delta.rm2.test.knn,R2.rf,Q2.rf,RMSEC.rf,RMSECV.rf,RMSEP.rf,R2.pred.rf,average.rm2.test.rf,delta.rm2.test.rf,R2.svm,Q2.svm,RMSEC.svm,RMSECV.svm,RMSEP.svm,R2.pred.svm,average.rm2.test.rf,delta.rm2.test.rf,R2.en,Q2.en,RMSEC.en,RMSECV.en,RMSEP.en,R2.pred.en,average.rm2.test.en,delta.rm2.test.en),byrow=TRUE,ncol=8))
#colnames(M$Metrics) <- c("r2","q2","RMSEC","RMSEcv","RMSEP", "r2pred", "Average rm2", "Delta rm2")
#rownames(M$Metrics) <- c("pls","mars", "knn", "rf", "svm","Elastic Net")        
  #        
  table.rm2.train <- list(Train=matrix(c(rm2.train.pls,rm2.reverse.train.pls,average.rm2.train.pls,delta.rm2.train.pls,rm2.train.earth,rm2.reverse.train.earth, average.rm2.train.earth, delta.rm2.train.earth,rm2.train.knn,rm2.reverse.train.knn, average.rm2.train.knn, delta.rm2.train.knn,rm2.train.rf,rm2.reverse.train.rf, average.rm2.train.rf, delta.rm2.train.rf),byrow=TRUE,ncol=4))
  rownames(table.rm2.train$Train) <- c("pls","mars", "knn", "rf")
  colnames(table.rm2.train$Train) <- c("Value","Reverse","Average","Delta")
  .GlobalEnv[["table.rm2.train"]] <- table.rm2.train
  table.rm2.train$Train <- round(table.rm2.train$Train,digits=3) 
  #rownames(table.rm2.train$Train) <- c("pls","mars", "knn", "rf", "svm","Elastic Net")
  table.rm2.test <- list(Test=matrix(c(rm2.test.pls,rm2.reverse.test.pls,average.rm2.test.pls,delta.rm2.test.pls,rm2.test.earth,rm2.reverse.test.earth,average.rm2.test.earth,delta.rm2.test.earth,rm2.test.knn,rm2.reverse.test.knn,average.rm2.test.knn,delta.rm2.test.knn,rm2.test.rf,rm2.reverse.test.rf,average.rm2.test.rf,delta.rm2.test.rf),byrow=TRUE,ncol=4))
  colnames(table.rm2.test$Test) <- c("Value","Reverse","Average","Delta")
  table.rm2.test$Test <- round(table.rm2.test$Test,digits=3)
  .GlobalEnv[["table.rm2.test"]] <- table.rm2.test
  #rownames(table.rm2.test$Test) <- c("PLS","MARS", "knn", "Random Forest", "Support Vector Machine","Elastic Net")        
  table.rm2.overall <- list(Overall=matrix(c(rm2.overall.pls,rm2.reverse.overall.pls,average.rm2.overall.pls,delta.rm2.overall.pls,rm2.overall.earth,rm2.reverse.overall.earth,average.rm2.overall.earth,delta.rm2.overall.earth,rm2.overall.knn,rm2.reverse.overall.knn,average.rm2.overall.knn,delta.rm2.overall.knn,rm2.overall.rf,rm2.reverse.overall.rf,average.rm2.overall.rf,delta.rm2.overall.rf),byrow=TRUE,ncol=4))
  colnames(table.rm2.overall$Overall) <- c("Value","Reverse","Average","Delta")
  table.rm2.overall$Overall <- round(table.rm2.overall$Overall,digits=3)
  .GlobalEnv[["table.rm2.overall"]] <- table.rm2.overall 
  #rownames(table.rm2.overall$Overall) <- c("pls","mars", "knn", "rf") 
#######################################################
###########Calculating Metric for KNN model############
#######################################################
models.stat.list <- list(pls = plsFit, mars = earthFit, knn = knnFit, rf = rfFit)
models.stat <- resamples(models.stat.list)
.GlobalEnv[["models.stat"]] <- models.stat
  difference.models <- diff(models.stat)
.GlobalEnv[["difference.models"]] <- difference.models
#######################################################
####################Making a REPORT####################
#######################################################
allPreds <- extractPrediction(models.stat.list,
                             testX = Test.descriptors,
                             testY = Test.activity)
testPreds <- subset(allPreds, dataType == "Test")
ddply(testPreds, .(model), defaultSummary)

#######################################################
####################Making a REPORT####################
#######################################################
odfWeave("template-report.odt", "report.odt")
  } else { 
    print="ERROR in CALCULATE METRICS"
  }
}}
