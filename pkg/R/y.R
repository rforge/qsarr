random.y <- function(A) { 
  for (i in 1:nrow(A)) { A[,1] <- sample(A[,1]) } 
  outplut=list(A)
} 

yrandomization <- function(cores=2,type="pls",A=as.matrix(Train.activity),matrix.x=Train.descriptors, repl=10,tuneLength = 15,...){ 
{ 
  QY <- train(matrix.x, unlist(A),
              type,
              tuneLength = tuneLength,
              trControl = trainControl(method = "boot632", returnResamp = "all"))
  .GlobalEnv[["QY"]] <- QY
  result.QY<- QY$results[which.min(QY$results$RMSE), ]
  .GlobalEnv[["result.QY"]] <- result.QY
  QY.q2 <- result.QY$Rsquared
  .GlobalEnv[["QY.q2"]] <- QY.q2
}

{  
  random.A <- replicate(repl, random.y(A))
  random.mat.dataframe = data.frame(random.A)
  .GlobalEnv[["random.mat.dataframe"]] <- random.mat.dataframe
}


Q <- data.frame(sapply( 1:ncol( random.mat.dataframe ), 
                        function( i ) 
                          train(matrix.x, unlist(random.mat.dataframe[,i]),
                                type,
                                tuneLength = tuneLength,
                                trControl = trainControl(method = "boot632", returnResamp = "all"))))


.GlobalEnv[["Q"]] <- Q

extract <- data.frame(sapply( 1:ncol( Q ), 
                              function( i ) 
                                Q[,i]$results[which.min(Q[,i]$results$RMSE), ]))


.GlobalEnv[["extract"]] <- extract

extract.resample <- data.frame(sapply( 1:ncol( Q ), 
                                       function( i ) 
                                         Q[,i]$resample$Rsquared))


.GlobalEnv[["extract.resample"]] <- extract.resample


resample.part<- data.frame(sapply( 1:ncol( Q ), 
                          function( i ) 
                            as.data.frame(extract.resample[,i])))

.GlobalEnv[["resample.part"]] <- resample.part



extract.q <- data.frame(sapply( 1:ncol( extract ), 
                                function( i ) 
                                  extract[,i]$Rsquared))

yrandomization.resuts<-as.data.frame(extract.q)
colnames(yrandomization.resuts) <- "Qsquared"
.GlobalEnv[["yrandomization.resuts"]] <- yrandomization.resuts
cat("##### Y-randomization Test #####\n")
print(yrandomization.resuts)
yrandomization.mean<- mean(yrandomization.resuts$Qsquared)
cat("##### Y-randomization Qsquared mean #####\n")
print(yrandomization.mean)
.GlobalEnv[["yrandomization.mean"]] <- yrandomization.mean
}