random.y <- function(A) { 
  for (i in 1:nrow(A)) { A[,1] <- sample(A[,1]) } 
  outplut=list(A)
} 

yrandomization <- function(A=qsar.activity,matrix.x=qsar.descriptors.cor, repl=10) { 
{
  QY <- train(matrix.x, unlist(A),
              "pls",
              tuneLength = 20,
              trControl = trainControl(
                method = "boot632", returnResamp = "all"))
  .GlobalEnv[["QY"]] <- QY
  result.QY<- QY$results[which.min(QY$results[,2] ), ]
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
                                "pls",
                                tuneLength = 20,
                                trControl = trainControl(
                                  method = "boot632", returnResamp = "all"))
                        
))
.GlobalEnv[["Q"]] <- Q

extract <- data.frame(sapply( 1:ncol( Q ), 
                              function( i ) 
                                Q[,i]$results[which.min(Q[,i]$results[,2] ), ]
                              
))
.GlobalEnv[["extract"]] <- extract

extract.q <- data.frame(sapply( 1:ncol( extract ), 
                                function( i ) 
                                  extract[,i]$Rsquared
                                
))
.GlobalEnv[["extract.23"]] <- extract.q
}