##' @title Collects the best model parameters for train models the caret package
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{qsarr}}.
##' @details
##' The object has the

model.class.binary <- function(model = modelFit, ... ){

#Libraries
library(qsarr)
library(devtools)
library(caretEnsemble)
library(doMC)
doMC::registerDoMC(Ncores)
  
Y<-R[,1]
assign("Y", Y, envir=.GlobalEnv)
ndesc.filtered <- ncol(R)
X <- R[,2:ndesc.filtered]
assign("X", X, envir=.GlobalEnv)
Y.train<-Y
X.train<-X
set.seed(42) #From random.org



#Setup CV Folds



fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

fitControl <- trainControl(method = "repeatedcv",
                           number = folds,
                           repeats = repeats,
                           returnResamp = "all",    
                           savePredictions = TRUE,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using
                           ## the following function
                           summaryFunction = fiveStats)


#Train some models
###########################
###########################
###########################
modelFit<- caret::train(X, Y, 
                        algorithm,
                        tuneLength = tuneLength,
                        metric = "Kappa",
                        trControl = fitControl)

assign("modelFit", modelFit, envir=.GlobalEnv)
}

model.class.binary.svm <- function(model = modelFit, ... ){
  
  #Libraries
  library(qsarr)
  library(devtools)
  library(caretEnsemble)
  library(doMC)
  doMC::registerDoMC(Ncores)
  
  Y<-R[,1]
  assign("Y", Y, envir=.GlobalEnv)
  ndesc.filtered <- ncol(R)
  X <- R[,2:ndesc.filtered]
  assign("X", X, envir=.GlobalEnv)
  Y.train<-Y
  X.train<-X
  set.seed(42) #From random.org
  
  
  
  #Setup CV Folds
  
  
  
  fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  
  fitControl <- trainControl(method = "repeatedcv",
                             number = folds,
                             repeats = repeats,
                             returnResamp = "all",    
                             savePredictions = TRUE,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             ## Evaluate performance using
                             ## the following function
                             summaryFunction = fiveStats)
  
  
  #Train some models
  ###########################
  ###########################
  ###########################
  modelFit<- caret::train(X, Y, 
                          algorithm,
                          tuneLength = tuneLength,
                          metric = "Kappa",
                          trControl = fitControl)
  
  assign("modelFit", modelFit, envir=.GlobalEnv)
}


model.multiclass <- function(model = modelFit, ... ){

  
  require(compiler)
  multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL){
    
    #Load Libraries
    require(Metrics)
    require(caret)
    
    #Check data
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
      stop("levels of observed and predicted data do not match")
    
    #Calculate custom one-vs-all stats for each class
    prob_stats <- lapply(levels(data[, "pred"]), function(class){
      
      #Grab one-vs-all data for the class
      pred <- ifelse(data[, "pred"] == class, 1, 0)
      obs  <- ifelse(data[,  "obs"] == class, 1, 0)
      prob <- data[,class]
      
      #Calculate one-vs-all AUC and logLoss and return
      cap_prob <- pmin(pmax(prob, .000001), .999999)
      prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
      names(prob_stats) <- c('ROC', 'logLoss')
      return(prob_stats) 
    })
    prob_stats <- do.call(rbind, prob_stats)
    rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
    
    #Calculate confusion matrix-based statistics
    CM <- confusionMatrix(data[, "pred"], data[, "obs"])
    
    #Aggregate and average class-wise stats
    #Todo: add weights
    class_stats <- cbind(CM$byClass, prob_stats)
    class_stats <- colMeans(class_stats)
    
    #Aggregate overall stats
    overall_stats <- c(CM$overall)
    
    #Combine overall with class-wise stats and remove some stats we don't want 
    stats <- c(overall_stats, class_stats)
    stats <- stats[! names(stats) %in% c('AccuracyNull', 
                                         'Prevalence', 'Detection Prevalence')]
    
    #Clean names and return
    names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
    return(stats)
    
  })
  
  #Libraries
  library(qsarr)
  library(devtools)
  library(caretEnsemble)
  library(doMC)
  doMC::registerDoMC(Ncores)
  
  Y<-R[,1]
  assign("Y", Y, envir=.GlobalEnv)
  ndesc.filtered <- ncol(R)
  X <- R[,2:ndesc.filtered]
  assign("X", X, envir=.GlobalEnv)
  Y.train<-Y
  X.train<-X
  set.seed(42) #From random.org
  
  
  
  #Setup CV Folds
  
  

  
  fitControl <- trainControl(method = "repeatedcv",
                             number = folds,
                             repeats = repeats,
                             returnResamp = "all",    
                             savePredictions = TRUE,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             ## Evaluate performance using
                             ## the following function
                             summaryFunction = multiClassSummary)
  
  ############################
  
  
  
  
  #Train some models
  ###########################
  ###########################
  ###########################
  modelFit<- caret::train(X, Y, 
                          algorithm,
                          tuneLength = tuneLength,
                          metric = "Accuracy",
                          trControl = fitControl)
  
  assign("modelFit", modelFit, envir=.GlobalEnv)
}


# Function to check whether package is installed
is.installed <- function(AMORE){
  is.element(AMORE, installed.packages()[,1])
} 

# check if package "hydroGOF" is installed
if (!is.installed("hydroGOF")){
  install.packages("hydroGOF")
}
  

