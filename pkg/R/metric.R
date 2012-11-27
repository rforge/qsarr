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
residuo.train.pls <- (pls.train$obs - pls.train$pred)
residuo.test.pls <- (pls.test$obs - pls.test$pred)
residuo.total.pls <- (predVals.pls$obs - predVals.pls$pred)
R2.pls.summary <- summary(lm(pls.train$pred ~ pls.train$obs))
R2.pls <- R2.pls.summary$r.squared
RMSEC.pls <- sqrt(mean(residuo.train.pls^2))
CV.pls <- plsFit$results[which.min(plsFit$results[,2] ), ]
parameters.pls <- CV.pls$ncomp
RMSECV.pls <- CV.pls$RMSE
RMSEP.pls <- sqrt(mean(residuo.test.pls^2))
Q2.pls <- CV.pls$Rsquared
R2.pred.pls <- 1-(sum((predVals.pls$obs-predVals.pls$pred)^2))/(sum(((predVals.pls$obs-mean(predVals.pls$obs))^2)))               
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.pls <-  rm2(pls.train$obs,pls.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.pls <-  rm2.reverse(pls.train$obs,pls.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.pls <- average.rm2(pls.train$obs,pls.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.pls <- delta.rm2(pls.train$obs,pls.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.pls <-  rm2(pls.test$obs,pls.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.pls <-  rm2.reverse(pls.test$obs,pls.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.pls <- average.rm2(pls.test$obs,pls.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.pls <- delta.rm2(pls.test$obs,pls.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.pls <-  rm2(predVals.pls$obs,predVals.pls$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.pls <-  rm2.reverse(predVals.pls$obs,predVals.pls$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.pls <- average.rm2(predVals.pls$obs,predVals.pls$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.pls <- delta.rm2(predVals.pls$obs,predVals.pls$pred)
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
R2.pred.earth <- 1-(sum((predVals.earth$obs-predVals.earth$pred)^2))/(sum(((predVals.earth$obs-mean(predVals.earth$obs))^2)))
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.earth <-  rm2(earth.train$obs,earth.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.earth <-  rm2.reverse(earth.train$obs,earth.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.earth <- average.rm2(earth.train$obs,earth.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.earth <- delta.rm2(earth.train$obs,earth.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.earth <-  rm2(earth.test$obs,earth.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.earth <-  rm2.reverse(earth.test$obs,earth.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.earth <- average.rm2(earth.test$obs,earth.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.earth <- delta.rm2(earth.test$obs,earth.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.earth <-  rm2(predVals.earth$obs,predVals.earth$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.earth <-  rm2.reverse(predVals.earth$obs,predVals.earth$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.earth <- average.rm2(predVals.earth$obs,predVals.earth$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.earth<- delta.rm2(predVals.earth$obs,predVals.earth$pred)
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
RMSECV.knn <- CV.knn$RMSE
RMSEP.knn <- sqrt(mean(residuo.test.knn^2))
Q2.knn <- CV.knn$Rsquared
R2.pred.knn <- 1-(sum((predVals.knn$obs-predVals.knn$pred)^2))/(sum(((predVals.knn$obs-mean(predVals.knn$obs))^2)))
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.knn <-  rm2(knn.train$obs,knn.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.knn <-  rm2.reverse(knn.train$obs,knn.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.knn <- average.rm2(knn.train$obs,knn.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.knn <- delta.rm2(knn.train$obs,knn.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.knn <-  rm2(knn.test$obs,knn.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.knn <-  rm2.reverse(knn.test$obs,knn.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.knn <- average.rm2(knn.test$obs,knn.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.knn <- delta.rm2(knn.test$obs,knn.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.knn <-  rm2(predVals.knn$obs,predVals.knn$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.knn <-  rm2.reverse(predVals.knn$obs,predVals.knn$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.knn <- average.rm2(predVals.knn$obs,predVals.knn$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.knn <- delta.rm2(predVals.knn$obs,predVals.knn$pred)
#random forest
rf.train <- subset(predVals.rf, dataType == "Training")
rf.test <- subset(predVals.rf, dataType == "Test")
residuo.train.rf <- (rf.train$obs - rf.train$pred)
residuo.test.rf <- (rf.test$obs - rf.test$pred)
residuo.total.rf <- (predVals.rf$obs - predVals.rf$pred)
R2.rf.summary <- summary(lm(rf.train$pred ~ rf.train$obs))
R2.rf <- R2.rf.summary$r.squared
RMSEC.rf <- sqrt(mean(residuo.train.rf^2))
CV.rf <- rfFit$results[which.min(rfFit$results[,1] ), ]
RMSECV.rf <- CV.rf$RMSE
RMSEP.rf <- sqrt(mean(residuo.test.rf^2))
Q2.rf <- CV.rf$Rsquared
R2.pred.rf <- 1-(sum((predVals.rf$obs-predVals.rf$pred)^2))/(sum(((predVals.rf$obs-mean(predVals.rf$obs))^2)))
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.rf <-  rm2(rf.train$obs,rf.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.rf <-  rm2.reverse(rf.train$obs,rf.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.rf <- average.rm2(rf.train$obs,rf.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.rf <- delta.rm2(rf.train$obs,rf.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.rf <-  rm2(rf.test$obs,rf.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.rf <-  rm2.reverse(rf.train$obs,rf.train$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.rf <- average.rm2(rf.train$obs,rf.train$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.rf <- delta.rm2(rf.train$obs,rf.train$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.rf <-  rm2(predVals.pls$obs,predVals.pls$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.rf <-  rm2.reverse(predVals.rf$obs,predVals.rf$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.rf <- average.rm2(predVals.rf$obs,predVals.rf$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.rf <- delta.rm2(predVals.rf$obs,predVals.rf$pred)
#Support Vector Machine
svm.train <- subset(predVals.svm, dataType == "Training")
svm.test <- subset(predVals.svm, dataType == "Test")
residuo.train.svm <- (svm.train$obs - svm.train$pred)
residuo.test.svm <- (svm.test$obs - svm.test$pred)
residuo.total.svm <- (predVals.svm$obs - predVals.svm$pred)
R2.svm.summary <- summary(lm(svm.train$pred ~ svm.train$obs))
R2.svm <- R2.svm.summary$r.squared
RMSEC.svm <- sqrt(mean(residuo.train.svm^2))
CV.svm <- svmFit$results[which.min(svmFit$results[,2] ), ]
RMSECV.svm <- CV.svm$RMSE
RMSEP.svm <- sqrt(mean(residuo.test.svm^2))
Q2.svm <- CV.svm$Rsquared
R2.pred.svm <- 1-(sum((predVals.svm$obs-predVals.svm$pred)^2))/(sum(((predVals.svm$obs-mean(predVals.svm$obs))^2)))
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.svm <-  rm2(svm.train$obs,svm.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.svm <-  rm2.reverse(svm.train$obs,svm.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.svm <- average.rm2(svm.train$obs,svm.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.svm <- delta.rm2(svm.train$obs,svm.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.svm <-  rm2(svm.test$obs,svm.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.svm <-  rm2.reverse(svm.test$obs,svm.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.svm <- average.rm2(svm.test$obs,svm.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.svm <- delta.rm2(svm.test$obs,svm.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.svm <-  rm2(predVals.svm$obs,predVals.svm$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.svm <-  rm2.reverse(predVals.svm$obs,predVals.svm$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.svm <- average.rm2(predVals.svm$obs,predVals.svm$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.svm <- delta.rm2(predVals.svm$obs,predVals.svm$pred)
#Elastic Net
###############################################################
###########Calculating Metric for Elastic Net model############
###############################################################      
en.train <- subset(predVals.en, dataType == "Training")
en.test <- subset(predVals.en, dataType == "Test")
residuo.train.en <- (en.train$obs - en.train$pred)
residuo.test.en <- (en.test$obs - en.test$pred)
residuo.total.en <- (predVals.en$obs - predVals.en$pred)
R2.en.summary <- summary(lm(en.train$pred ~ en.train$obs))
R2.en <- R2.en.summary$r.squared
RMSEC.en <- sqrt(mean(residuo.train.en^2))
CV.en <- enFit$results[which.min(enFit$results[,3] ), ]
RMSECV.en <- CV.en$RMSE
RMSEP.en <- sqrt(mean(residuo.test.en^2))
Q2.en <- CV.en$Rsquared
R2.pred.en <- 1-(sum((predVals.en$obs-predVals.en$pred)^2))/(sum(((predVals.en$obs-mean(predVals.en$obs))^2)))
###########Calculating the rm2 value for the TRAIN dataset###########
rm2.train.en<-  rm2(en.train$obs,en.train$pred)
###########Calculating the reverse rm2 value for the TRAIN dataset###########
rm2.reverse.train.en <-  rm2.reverse(en.train$obs,en.train$pred)
###########Calculating the Average rm2 value for the TRAIN dataset###########
average.rm2.train.en <- average.rm2(en.train$obs,en.train$pred)
###########Calculating the Delta rm2 value for the TRAIN dataset###########
delta.rm2.train.en <- delta.rm2(en.train$obs,en.train$pred)
###########Calculating the rm2 value for the TEST dataset###########
rm2.test.en <-  rm2(en.test$obs,en.test$pred)
###########Calculating the reverse rm2 value for the TEST dataset###########
rm2.reverse.test.en <-  rm2.reverse(en.test$obs,en.test$pred)
###########Calculating the Average rm2 value for the TEST dataset###########
average.rm2.test.en <- average.rm2(en.test$obs,en.test$pred)
###########Calculating the Delta rm2 value for the TEST dataset###########
delta.rm2.test.en <- delta.rm2(en.test$obs,en.test$pred)
###########Calculating the rm2 value for the OVERALL dataset###########
rm2.overall.en <-  rm2(predVals.en$obs,predVals.en$pred)
###########Calculating the reverse rm2 value for the OVERALL dataset###########
rm2.reverse.overall.en <-  rm2.reverse(predVals.en$obs,predVals.en$pred)
###########Calculating the Average rm2 value for the OVERALL dataset###########
average.rm2.overall.en <- average.rm2(predVals.en$obs,predVals.en$pred)
###########Calculating the Delta rm2 value for the OVERALL dataset###########
delta.rm2.overall.en <- delta.rm2(predVals.en$obs,predVals.en$pred)
##
M <- list(Metrics=matrix(c(R2.pls,Q2.pls,RMSEC.pls,RMSECV.pls,RMSEP.pls,R2.pred.pls),byrow=TRUE,ncol=6))
#colnames(M$Metrics) <- c("r2","q2","RMSEC","RMSEcv","RMSEP", "r2pred")
#rownames(M$Metrics) <- c("pls")
M$Metrics <- round(M$Metrics,digits=3)

Mdata <- list(Mdata=matrix(c(parameters.pls, "-", parameter1.earth,parameter2.earth),byrow=TRUE,ncol=2))
Mdata$Mdata <- round(Mdata$Mdata,digits=0.1)

#
M <- list(Metrics=matrix(c(R2.pls,Q2.pls,RMSEC.pls,RMSECV.pls,RMSEP.pls,R2.pred.pls,delta.rm2.test.pls,R2.earth,Q2.earth,RMSEC.earth,RMSECV.earth,RMSEP.earth,R2.pred.earth,average.rm2.test.earth,delta.rm2.test.earth,R2.knn,Q2.knn,RMSEC.knn,RMSECV.knn,RMSEP.knn,R2.pred.knn,average.rm2.test.knn,delta.rm2.test.knn,R2.rf,Q2.rf,RMSEC.rf,RMSECV.rf,RMSEP.rf,R2.pred.rf,average.rm2.test.rf,delta.rm2.test.rf,R2.svm,Q2.svm,RMSEC.svm,RMSECV.svm,RMSEP.svm,R2.pred.svm,average.rm2.test.rf,delta.rm2.test.rf,R2.en,Q2.en,RMSEC.en,RMSECV.en,RMSEP.en,R2.pred.en,average.rm2.test.en,delta.rm2.test.en),byrow=TRUE,ncol=8))
colnames(M$Metrics) <- c("r2","q2","RMSEC","RMSEcv","RMSEP", "r2pred", "Average rm2", "Delta rm2")
rownames(M$Metrics) <- c("pls","mars", "knn", "rf", "svm","Elastic Net")        
  #        
  table.rm2.train <- list(Train=matrix(c(rm2.train.pls,rm2.reverse.train.pls,average.rm2.train.pls,delta.rm2.train.pls,rm2.train.earth,rm2.reverse.train.earth, average.rm2.train.earth, delta.rm2.train.earth,rm2.train.knn,rm2.reverse.train.knn, average.rm2.train.knn, delta.rm2.train.knn,rm2.train.rf,rm2.reverse.train.rf, average.rm2.train.rf, delta.rm2.train.rf,rm2.train.svm,rm2.reverse.train.svm, average.rm2.train.svm, delta.rm2.train.svm,rm2.train.en,rm2.reverse.train.en, average.rm2.train.en, delta.rm2.train.en),byrow=TRUE,ncol=4))
  rownames(table.rm2.train$Train) <- c("pls","mars", "knn", "rf", "svm","Elastic Net")
  colnames(table.rm2.train$Train) <- c("Value","Reverse","Average","Delta")        
  table.rm2.train$Train <- round(table.rm2.train$Train,digits=3) 
  #rownames(table.rm2.train$Train) <- c("pls","mars", "knn", "rf", "svm","Elastic Net")
  table.rm2.test <- list(Test=matrix(c(rm2.test.pls,rm2.reverse.test.pls,average.rm2.test.pls,delta.rm2.test.pls,rm2.test.earth,rm2.reverse.test.earth,average.rm2.test.earth,delta.rm2.test.earth,rm2.test.knn,rm2.reverse.test.knn,average.rm2.test.knn,delta.rm2.test.knn,rm2.test.rf,rm2.reverse.test.rf,average.rm2.test.rf,delta.rm2.test.rf,rm2.test.svm,rm2.reverse.test.svm,average.rm2.test.svm,delta.rm2.test.svm,rm2.test.en,rm2.reverse.test.en,average.rm2.test.en,delta.rm2.test.en),byrow=TRUE,ncol=4))
  colnames(table.rm2.test$Test) <- c("Value","Reverse","Average","Delta")
  table.rm2.test$Test <- round(table.rm2.test$Test,digits=3)
  #rownames(table.rm2.test$Test) <- c("PLS","MARS", "knn", "Random Forest", "Support Vector Machine","Elastic Net")        
  table.rm2.overall <- list(Overall=matrix(c(rm2.overall.pls,rm2.reverse.overall.pls,average.rm2.overall.pls,delta.rm2.overall.pls,rm2.overall.earth,rm2.reverse.overall.earth,average.rm2.overall.earth,delta.rm2.overall.earth,rm2.overall.knn,rm2.reverse.overall.knn,average.rm2.overall.knn,delta.rm2.overall.knn,rm2.overall.rf,rm2.reverse.overall.rf,average.rm2.overall.rf,delta.rm2.overall.rf,rm2.overall.svm,rm2.reverse.overall.svm,average.rm2.overall.svm,delta.rm2.overall.svm,rm2.overall.en,rm2.reverse.overall.en,average.rm2.overall.en,delta.rm2.overall.en),byrow=TRUE,ncol=4))
  colnames(table.rm2.overall$Overall) <- c("Value","Reverse","Average","Delta")
  table.rm2.overall$Overall <- round(table.rm2.overall$Overall,digits=3) 
  #rownames(table.rm2.overall$Overall) <- c("PLS","MARS", "knn", "Random Forest", "Support Vector Machine","Elastic Net") 
  #models.stat <- resamples(list(pls = plsFit, mars = earthFit, knn = knnFit, randon.forest = rfFit, support.vector.machine = svmFit, elastic.net = enFit))
  #difference.models <- diff(models.stat)
  } else { 
    print="ERROR in CALCULATE METRICS"
  }
}
}