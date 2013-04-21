###http://www.inside-r.org/packages/cran/FNN/docs/knnx.dist
knn.ad <- function(cores=2, method=cv ,number = 20,repeats = 3,tuneLength = 15, ...){
{  
  library(doMC)
  doMC::registerDoMC(cores)
  fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = number,
    ## repeated three times
    repeats = repeats,
    ## Save all the resampling results
    returnResamp = "all")
knnFit.ad <- caret::train(Train.descriptors, unlist(Train.activity),
                       "knn",
                       tuneLength = tuneLength,
                       trControl = fitControl)
.GlobalEnv[["knnFit.ad"]] <- knnFit.ad
predVals.knn.ad <- caret::extractPrediction(list(knnFit.ad),
                                           testX = Test.descriptors,
                                           testY = Test.activity)
.GlobalEnv[["predVals.knn.ad"]] <- predVals.knn.ad
CV.knn <- knnFit.ad$results[which.min(knnFit.ad$results[,2] ), ]
parameter.knn <- CV.knn$k
.GlobalEnv[["parameter.knn"]] <- parameter.knn  
###
  ktrain=knn.dist(Train.descriptors, k=parameter.knn,  algorithm=c("cover_tree") )
  .GlobalEnv[["ktrain"]] <- ktrain
  ktest=knnx.dist(data=Train.descriptors,Test.descriptors, k=parameter.knn, algorithm=c("cover_tree") ) 
  .GlobalEnv[["ktest"]] <- ktest
  Dc=(0.5*(sd(ktrain[,parameter.knn])))+mean(ktrain[,parameter.knn])
  .GlobalEnv[["Dc"]] <- Dc
  cat("##### Train #####\n")
  print(ktrain[,parameter.knn]<Dc)
  print(row.names(Train.descriptors))
  cat("##### Test #####\n")
  print(ktest[,parameter.knn]<Dc)
  print(row.names(Test.descriptors))
  #######Plot
  knn.train.ad <- subset(predVals.knn.ad, dataType == "Training")
  knn.test.ad <- subset(predVals.knn.ad, dataType == "Test")
  stdres.train.knn=((knn.train.ad$obs - knn.train.ad$pred)-mean(knn.train.ad$obs - knn.train.ad$pred))/sd(knn.train.ad$obs - knn.train.ad$pred)
  stdres.test.knn=((knn.test.ad$obs - knn.test.ad$pred)-mean(knn.test.ad$obs - knn.test.ad$pred))/sd(knn.test.ad$obs - knn.test.ad$pred)
  mat.AD.train.knn=cbind(ktrain[,parameter.knn],stdres.train.knn)
  mat.AD.test.knn=cbind(ktest[,parameter.knn],stdres.test.knn)
  ktotal.y=as.data.frame(rbind(as.matrix(stdres.train.knn),as.matrix(stdres.test.knn)))
  ktotal.x=as.data.frame(rbind(as.matrix(ktrain[,parameter.knn]),as.matrix(ktest[,parameter.knn])))
  ktotal=cbind(ktotal.x,ktotal.y)
outliers.knn.first<-subset(row.names(qsar.descriptors.cor), subset = ktotal.x>Dc  | (ktotal.y < -3 | ktotal.y > 3))
      .GlobalEnv[["ooutliers.knn.first"]] <- outliers.knn.first
true.outliers.matrix=as.data.frame(rbind(as.matrix(rownames(Train.descriptors)),as.matrix(rownames(Test.descriptors))))
outliers.knn.m= as.matrix(true.outliers.matrix[as.numeric(outliers.knn.first), ])
outliers.knn <- as.numeric(outliers.knn.m)
      .GlobalEnv[["ooutliers.knn"]] <- outliers.knn
outliers.knn.xy<- ktotal[outliers.knn.first, ]
outliers.knn.x <- outliers.knn.xy[1]
outliers.knn.y <- outliers.knn.xy[2]
xmax.knn=(max(ktotal.x)+1)
xmin.knn=(min(ktotal.x)-1)
png(file="AD-knn.png", width=1430, height=1004, res=144)
plot(mat.AD.train.knn, xlab="Euclidean distance of the k nearest neighbors", ylab="Standardized residuals", ylim=c(-3.4,3.4),  xlim=c(xmin.knn,xmax.knn),pch=16,cex=0.8)
points(mat.AD.test.knn, col="red", pch=17, cex=0.8)
text(x=unlist(outliers.knn.x),y=unlist(outliers.knn.y),labels=outliers.knn, cex=0.6, pos=4, col="black")
  abline(h=3.0, lty=2);abline(h=-3.0, lty=2);abline(v=Dc, lty=2)
  legend((xmax.knn-1.6),3, c("Training Set","Test Set"), pch=c(16,17),col=c("black","red"),bty='n', cex=1.1)
  #mtext('100% Area'==.(Dc.text), line = -1, adj = 0.1)
  dev.off()
}
}

lm.ad <- function(cores=2, method=cv ,number = 20,repeats = 3,tuneLength = 15, ...){
{  
fitControl <- caret::trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated three times
  repeats = 3,
  ## Save all the resampling results
  returnResamp = "all")
plsFit<- caret::train(Train.descriptors, unlist(Train.activity), 
                      "pls",
                      tuneLength = 50,
                      trControl = fitControl)
.GlobalEnv[["plsFit"]] <- plsFit
predVals.pls <- caret::extractPrediction(list(plsFit),
                                         testX = Test.descriptors, 
                                         testY = Test.activity)
##
remove.endp=ncol(plsFit$trainingData)
Traind= as.matrix(plsFit$trainingData[,-remove.endp])
Testd= as.matrix(Test.descriptors)

Htrain= Traind %*% solve(t(Traind) %*%(Traind), tol=1e-40)  %*% t(Traind)
Htest= Testd %*% solve(t(Traind) %*%(Traind), tol=1e-40)  %*% t(Testd)
pls.train.ad <- subset(predVals.pls, dataType == "Training")
pls.test.ad <- subset(predVals.pls, dataType == "Test")
stdres.train.pls=((pls.train.ad$obs - pls.train.ad$pred)-mean(pls.train.ad$obs - pls.train.ad$pred))/sd(pls.train.ad$obs - pls.train.ad$pred)
stdres.test.pls=((pls.test.ad$obs - pls.test.ad$pred)-mean(pls.test.ad$obs - pls.test.ad$pred))/sd(pls.test.ad$obs - pls.test.ad$pred)
h1 <- (3*((ncol(Train.descriptors)+1))/nrow(Train.descriptors))
mat.leve.train=cbind((diag(Htrain)),stdres.train.pls)
mat.leve.test=cbind((diag(Htest)),stdres.test.pls)
plot(mat.leve.test, col="red", xlab="Leverage", ylab="Standardized residuals", ylim=c(-3.4,3.4))
text(row.names(mat.leve.test), cex=0.6, pos=3, col="red")
points(mat.leve.train, pch=2)
text(mat.leve.train, cex=0.6, pos=4, col="black")
abline(h=3.0, lty=2);abline(h=-3.0, lty=2);abline(v=h1, lty=2)
}
}


outside.ad <- function(cores=2, method=cv ,number = 20,repeats = 3,tuneLength = 15, ...){
{  
###Outside
Unknow= as.matrix(qsar.descriptors.cor[c(110,111,112,113,114,115,116,117,118),])
Hunk= Unknow %*% solve(t(Traind) %*%(Traind), tol=1e-40)  %*% t(Unknow)
print(diag(Hunk))
res.unk=diag(Hunk)<h1

###Outside KNN
Kunknow=knnx.dist(data=Train.descriptors,unknow, k=parameter.knn, algorithm=c("cover_tree") ) 
print(Kunknow[,parameter.knn]<Dc)
(print(row.names(unknow)))
}
}



bring.ad <- function(cores=2, method=cv ,number = 20,repeats = 3,tuneLength = 15, ...){
{  
remove.outliers.knn<- (as.matrix(row.names(qsar.descriptors.cor))[-as.numeric(outliers.knn), ])
Train.descriptors.knn.AD <- na.omit((Train.descriptors)[remove.outliers.knn,])
Train.activity.knn.AD <- as.numeric(na.omit((as.data.frame(Train.activity))[remove.outliers.knn,]))
Test.descriptors.knn.AD <- na.omit((Test.descriptors)[remove.outliers.knn,])
Test.activity.knn.AD <- na.omit(as.numeric(((as.data.frame(Test.activity))[remove.outliers.knn,])))


Train.activity.knn.AD <- Train.activity[-as.numeric(outliers.knn)]
Train.descriptors.knn.AD <- Train.descriptors[-as.numeric(outliers.knn)]
Test.activity.knn.AD <- Test.activity[-as.numeric(outliers.knn)]
Test.descriptors.knn.AD <- Test.descriptors[-as.numeric(outliers.knn)]

knnFit.knn.ad.remove <- caret::train(Train.descriptors, unlist(Train.activity),
                          "knn",
                          tuneLength = tuneLength,
                          trControl = fitControl)
.GlobalEnv[["knnFit.knn.ad.remove"]] <- knnFit.knn.ad.remove
predVals.knn.ad.remove <- caret::extractPrediction(list(knnFit.ad),
                                            testX = Test.descriptors,
                                            testY = Test.activity)
.GlobalEnv[["predVals.knn.ad.remove"]] <- predVals.knn.ad.remove 
}
}
