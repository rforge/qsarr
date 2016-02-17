knn.ad <- function(Train.descriptors= Train.descriptors, Test.descriptors= Test.descriptors,Train.activity=Train.activity, Test.activity=Test.activity, parameter.knn=5, DC.par=0.5, ...){
{
  #Libraries
  library(qsarr)
  library(FNN)
  ktrain=(knn.dist(Train.descriptors, k=parameter.knn  )[,parameter.knn])
  ktrain=abs(ktrain)
  .GlobalEnv[["ktrain"]] <- ktrain
  ktest=(knnx.dist(data=Train.descriptors,Test.descriptors, k=parameter.knn )[,parameter.knn])
  ktest=abs(ktest)
  .GlobalEnv[["ktest"]] <- ktest
  Dc=(DC.par*(sd(ktrain)))+mean(ktrain)
  .GlobalEnv[["Dc"]] <- Dc
}
}


knn.ad.plot <- function(Train.descriptors= Train.descriptors, Test.descriptors= Test.descriptors,Train.activity=Train.activity, Test.activity=Test.activity, parameter.knn=5, DC.par=0.5, ...){
{  
  ktrain=(knn.dist(Train.descriptors, k=parameter.knn,  algorithm=c("cover_tree") )[,parameter.knn])
  .GlobalEnv[["ktrain"]] <- ktrain
  ktest=(knnx.dist(data=Train.descriptors,Test.descriptors, k=parameter.knn, algorithm=c("cover_tree") )[,parameter.knn])
  .GlobalEnv[["ktest"]] <- ktest
  Dc=(DC.par*(sd(ktrain)))+mean(ktrain)
  print(Dc)
  .GlobalEnv[["Dc"]] <- Dc
  cat("##### Test #####\n")
  print(ktest<Dc)
  print(row.names(Test.descriptors))
  #######Plot
  outliers.knn.first<-subset(row.names(Test.descriptors), subset = ktest>Dc  )
  .GlobalEnv[["outliers.knn.first"]] <- outliers.knn.first
  modelcules.DA<-subset(row.names(Test.descriptors), subset = ktest<Dc  )
  Test.activity.df<-as.data.frame(Test.activity)
  Test.activity.df.int<-subset(row.names(Test.descriptors), subset = ktest<100000000000000*Dc )
  rownames(Test.activity.df) <- as.numeric(Test.activity.df.int)
  .GlobalEnv[["modelcules.DA"]] <- modelcules.DA
  true.outliers.matrix=as.data.frame(as.matrix(rownames(Test.descriptors)))
  outliers.knn.m= as.matrix(true.outliers.matrix[as.numeric(outliers.knn.first), ])
  outliers.knn <- as.numeric(outliers.knn.m)
  .GlobalEnv[["outliers.knn"]] <- outliers.knn
  cpd=as.data.frame(row.names(Test.descriptors))
  plotmatrix.da=(cbind(cpd,ktest))
  .GlobalEnv[["plotmatrix.da"]] <- plotmatrix.da
  chartad=barchart(ktest~row.names(Test.descriptors), data=plotmatrix.da,as.table = TRUE, horizontal = FALSE, ylab = "Euclidean distance of the k nearest neighbors", xlab = "compounds" , panel=function(x,y,...){
    panel.abline(h=Dc, col.line="black")
    panel.barchart(x,y,...)})
  .GlobalEnv[["chartad"]] <- chartad
}
}


pls.ad <- function(cores=2, method=cv ,number = 10,repeats = 3,tuneLength = 50, ...){
{  
  fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = number,
    ## repeated three times
    repeats = repeats,
    ## Save all the resampling results
    returnResamp = "all")
plsFit.ad<- caret::train(Train.descriptors, unlist(Train.activity), 
                           "pls",
                           tuneLength = tuneLength,
                           trControl = fitControl)
      .GlobalEnv[["plsFit.ad"]] <- plsFit.ad
predVals.pls.ad <- caret::extractPrediction(list(plsFit.ad),
                                              testX = Test.descriptors, 
                                              testY = Test.activity)
      .GlobalEnv[["predVals.pls.ad"]] <- predVals.pls.ad
##
Traind.pls= as.matrix(Train.descriptors)
      .GlobalEnv[["Traind.pls"]] <- Traind.pls
Testd.pls= as.matrix(Test.descriptors)
      .GlobalEnv[["Testd.pls"]] <- Testd.pls
Hat.train= diag(Traind.pls %*% solve(t(Traind.pls) %*%(Traind.pls), tol=1e-40)  %*% t(Traind.pls))
      .GlobalEnv[["Hat.train"]] <- Hat.train
Hat.test= diag(Testd.pls %*% solve(t(Traind.pls) %*%(Traind.pls), tol=1e-40)  %*% t(Testd.pls))  
      .GlobalEnv[["Hat.test"]] <- Hat.test
pls.train.ad <- subset(predVals.pls.ad, dataType == "Training")
pls.test.ad <- subset(predVals.pls.ad, dataType == "Test")
stdres.train.pls=((pls.train.ad$obs - pls.train.ad$pred)-mean(pls.train.ad$obs - pls.train.ad$pred))/sd(pls.train.ad$obs - pls.train.ad$pred)
stdres.test.pls=((pls.test.ad$obs - pls.test.ad$pred)-mean(pls.test.ad$obs - pls.test.ad$pred))/sd(pls.test.ad$obs - pls.test.ad$pred)
h1 <- round((3*((ncol(Train.descriptors)+1))/nrow(Train.descriptors)),digits=2)
      .GlobalEnv[["h1"]] <- h1
mat.leve.train=cbind(Hat.train,stdres.train.pls)
mat.leve.test=cbind(Hat.test,stdres.test.pls)
plstotal=rbind(mat.leve.train,mat.leve.test)
plstotal.x=as.data.frame(plstotal[,1])
plstotal.y=as.data.frame(plstotal[,2])
sigma3<-3*sd(pls.train.ad$obs - pls.train.ad$pred)
outliers.pls<-subset(row.names(plstotal.x), subset =plstotal.x >h1  | (plstotal.y < -sigma3 | plstotal.y > sigma3))
      .GlobalEnv[["outliers.pls"]] <- outliers.pls
cat("##### Compounds Outside of Applicability Domain (AD) #####\n")
print(outliers.pls)
outliers.pls.xy <- as.data.frame(plstotal)[outliers.pls,]
outliers.pls.x <- outliers.pls.xy[,1]
outliers.pls.y <- outliers.pls.xy[,2]


mat.leve.train=cbind(Hat.train,stdres.train.pls)

outliersTRAIN.pls<-subset(row.names(mat.leve.train), subset =mat.leve.train[,1] >h1  | (mat.leve.train[,2] < -sigma3 | mat.leve.train[,2] > sigma3))
toba=(as.matrix(row.names(Train.descriptors))[-as.numeric(outliersTRAIN.pls), ])
  Train.activity=(as.matrix(Train.activity))[-as.numeric(outliersTRAIN.pls), ]
Train.descriptors= na.omit((Train.descriptors)[toba,])
#Train.activity= as.numeric(na.omit((as.data.frame(Train.activity))[toba2,]))
#Test.descriptors= Test.descriptors[outliersTRAIN.pls,]
#Test.activity= Test.activity[outliersTRAIN.pls,]  
  
outlierTRAIN.pls.xy <- as.data.frame(mat.leve.train)[outliers.pls,]

  
  
  
xmax.pls=(max(plstotal[,1])+0.5)
xmin.pls=(min(plstotal[,1]))
ymax.pls=(max(plstotal[,2])+0.5)
ymin.pls=(min(plstotal[,2])-0.5)
posx.pls=(xmax.pls*0.2)
posy.pls=(ymax.pls*0.03)
png(file="AD-pls.png", width=1430, height=1004, res=144)
plot(mat.leve.train, xlab="Leverages", ylab="Standardized residuals",  xlim=c(xmin.pls,xmax.pls), ylim=c(ymin.pls,ymax.pls),pch=16,cex=0.8)
points(mat.leve.test, col="red", pch=17, cex=0.8)
text(x=unlist(outliers.pls.x),y=unlist(outliers.pls.y),labels=outliers.pls, cex=0.6, pos=4, col="black")
abline(h=sigma3, lty=2);abline(h=-sigma3, lty=2);abline(v=h1, lty=2)
legend((xmax.pls -(posx.pls)),ymax.pls-posy.pls, c("Training Set","Test Set"), pch=c(16,17),col=c("black","red"),bty='n', cex=1.1)
text(sigma3+(xmax.pls*0.05),h1+(ymax.pls*0.3), bquote( italic(h^"*") == .(h1) ) )
dev.off()
cat("##### Williams plot of standardized residual versus leverage #####\n")
}
}


outside.ad <- function(cores=2, method=cv ,number = 20,repeats = 3,tuneLength = 15, ...){
{  
  ###Outside
 
  
  Unknow= as.matrix(qsar.descriptors.cor[c(110,111,112,113,114,115,116,117,118),])
  Hunk= unknow %*% solve(t(Traind) %*%(Traind), tol=1e-40)  %*% t(unknow)
  Hat.train= diag(Traind.pls %*% solve(t(Traind.pls) %*%(Traind.pls), tol=1e-40)  %*% t(Traind.pls))
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
