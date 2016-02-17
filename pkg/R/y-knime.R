random.y <- function(A) { 
  for (i in 1:nrow(A)) { A[,1] <- sample(A[,1]) } 
  outplut=list(A)
} 

yrandomization.binary <- function(cores=2,type="pls",A=as.matrix(Train.activity),matrix.x=Train.descriptors, repl=10,tuneLength = 5,...){ 
{ 
  library(caret)
  library(doMC)
  doMC::registerDoMC(Ncores)
  
  Train.activity<-Y
  Train.descriptors<-X
  fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  fitControl <- trainControl(method = "repeatedcv",
                             number = folds,
                             repeats = repeats,
                             returnResamp = "all",    
                             savePredictions = TRUE,
                             ## Estimate class probabilities
                             classProbs=TRUE,
                             ## Evaluate performance using
                             ## the following function
                             summaryFunction = fiveStats)
  
  QY <- caret::train(matrix.x, unlist(Train.activity), 
                     type,
                     tuneLength = tuneLength,
                      metric = "Kappa",
                     trControl = fitControl)
  
  .GlobalEnv[["QY"]] <- QY
  result.QY<- QY$results[which.max(QY$results$Kappa), ]
  .GlobalEnv[["result.QY"]] <- result.QY

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
                                metric = "Kappa",
                                trControl = fitControl)))
                        

.GlobalEnv[["Q"]] <- Q

extract <- data.frame(sapply( 1:ncol( Q ), 
                              function( i ) 
                                Q[,i]$results[which.max(Q[,i]$results$Kappa), ]))

extract<-as.data.frame(t(extract))

colMeans2 <- function(x) {
  # This function tries to guess column type. Since all columns come as
  # characters, it first tries to see if x == "TRUE" or "FALSE". If
  # not so, it tries to coerce vector into integer. If that doesn't 
  # work it tries to see if there's a ' \" ' in the vector (meaning a
  # column with character), it uses that as a result. Finally if nothing
  # else passes, it means the column type is numeric, and it calculates
  # the mean of that.
  
  #   browser()
  
  # try if logical
  if (any(levels(x) == "TRUE" | levels(x) == "FALSE")) return(NA)
  
  # try if integer
  try.int <- strtoi(x)
  if (all(!is.na(try.int)))  return(try.int[1])
  
  # try if character
  if (any(grepl("\\\"", x))) return(x[1])
  
  # what's left is numeric
  mean(as.numeric(as.character(x)), na.rm = TRUE)
  # a possible warning about coerced NAs probably originates in the above line
}


yrandomization.mean<-apply(extract, MARGIN = 2, FUN = colMeans2)
.GlobalEnv[["yrandomization.mean"]] <- t(yrandomization.mean)

colStd <- function(x) {
  # This function tries to guess column type. Since all columns come as
  # characters, it first tries to see if x == "TRUE" or "FALSE". If
  # not so, it tries to coerce vector into integer. If that doesn't 
  # work it tries to see if there's a ' \" ' in the vector (meaning a
  # column with character), it uses that as a result. Finally if nothing
  # else passes, it means the column type is numeric, and it calculates
  # the standard Deviation of that. 
  
  #   browser()
  
  # try if logical
  if (any(levels(x) == "TRUE" | levels(x) == "FALSE")) return(NA)
  
  # try if integer
  try.int <- strtoi(x)
  if (all(!is.na(try.int)))  return(try.int[1])
  
  # try if character
  if (any(grepl("\\\"", x))) return(x[1])
  
  # what's left is numeric
  sd(as.numeric(as.character(x)), na.rm = TRUE)
  # a possible warning about coerced NAs probably originates in the above line
}

yrandomization.std<-apply(extract, MARGIN = 2, FUN = colStd)
.GlobalEnv[["yrandomization.std"]] <- t(yrandomization.std)

yrandomization.results<-as.data.frame(rbind(yrandomization.mean,yrandomization.std))
row.names(yrandomization.results) <- c("Mean", "Std")

yrandomization.results$BAC <- c((0.5*(yrandomization.results[1,3]+yrandomization.results[1,4])), (yrandomization.results[2,3]+yrandomization.results[2,4]))
.GlobalEnv[["yrandomization.results"]] <- yrandomization.results
}
