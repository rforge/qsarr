##' @title Collects the best model parameters for train models the caret package
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{qsarr}}.
##' @details
##' The object has the following items

class.model.bestTune <- function(model = modelFit, ... ){
  best.Model <- as.character(model$bestTune[1,])
  best.Model.name <- (as.matrix(colnames(model$bestTune)))[,1]
  best.col.total <- as.character(match(best.Model.name,names(model$results)))
  
  best.col.first <- match(best.Model.name[1] ,names(model$results))
  best.col.first.value <- as.character(model$bestTune[,1])
  
  if (length(model$bestTune) > 2)  { 
    
    best.col.second <- match(best.Model.name[2] ,names(model$results))
    best.col.second.value <- as.character(model$bestTune[,2])
    best.col.tirth <-  match(best.Model.name[3] ,names(model$results))
    best.col.tirth.value <-  as.character(model$bestTune[,3])
    
    
    class.best.model <-model$results[which(model$results[,best.col.first]  == best.col.first.value   & model$results[, best.col.second] == best.col.second.value  &model$results[,best.col.tirth] == best.col.tirth.value ) , ]
    assign("class.best.model", class.best.model, envir=.GlobalEnv)
    round(class.best.model, digits=2)
    
  } else if (length(model$bestTune) <2) {
    
    class.best.model <- model$results[which(model$results[,best.col.first]  == best.col.first.value) , ]
    
    assign("class.best.model", class.best.model, envir=.GlobalEnv)
    round(class.best.model, digits=2)
    
  } else { 
    
    best.col.second <- match(best.Model.name[2] ,names(model$results))
    best.col.second.value <- as.character(model$bestTune[,2])
    
    class.best.model <-model$results[which(model$results[,best.col.first]  == best.col.first.value   & model$results[, best.col.second] == best.col.second.value) , ]
    
    assign("class.best.model", class.best.model, envir=.GlobalEnv)
    round(class.best.model, digits=2)     
  }
}

##' @title get model prediction FOLDS for train objects from the caret package
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{qsarr}}.
##' @details
##' The object has the following items
get.folds.pred <- function(model = modelFit, ... ){
  
  best.Model <- model$bestTune[1,]
  best.Model.name <- (as.matrix(colnames(model$bestTune)))[,1]
  best.col.total <- as.character(match(best.Model.name ,names(model$pred)))
  
  best.col.first <- match(best.Model.name[1] ,names(model$pred))
  best.col.first.value <- as.character(model$bestTune[,1])
  
  
  if (length(model$bestTune) > 2)  { 
    
    best.col.second <- match(best.Model.name[2] ,names(model$pred))
    best.col.second.value <- as.character(model$bestTune[,2])
    best.col.tirth <-  match(best.Model.name[3] ,names(model$pred))
    best.col.tirth.value <-  as.character(model$bestTune[,3])
    
    model.folds <-model$pred[which(model$pred[,best.col.first]  == best.col.first.value   & model$pred[, best.col.second] == best.col.second.value  &model$pred[,best.col.tirth] == best.col.tirth.value ) , ]
    
    assign("model.folds.pred", model.folds, envir=.GlobalEnv)
    
  } else if (length(model$bestTune) <2) {
    
    model.folds <- model$pred[which(model$pred[,best.col.first]  == best.col.first.value ) , ]
    
    assign("model.folds.pred", model.folds, envir=.GlobalEnv)
    
  } else { 
    
    best.col.second <- match(best.Model.name[2] ,names(model$pred))
    best.col.second.value <- as.character(model$bestTune[,2])
    
    model.folds <- model$pred[which(model$pred[,best.col.first]  == best.col.first.value   &  model$pred[, best.col.second] == best.col.second.value ) , ]
    
    assign("model.folds.pred", model.folds, envir=.GlobalEnv)
    
  } 
 }

prediction.AD.Folds <- function(model = modelFit, Y.class= Y.class, X.class=X.class, DC.par=0.5,parameter.knn=5, ... ){
  
  Fold.Training.Y <- (as.matrix(Y.class)[unlist(model$control$index),])
  Fold.Training.X <- (as.matrix(X.class)[unlist(model$control$index),])
  Fold.Test.X <- (as.matrix(X.class)[unlist(model$control$indexOut),])
  Fold.Test.Y <- (as.matrix(Y.class)[unlist(model$control$indexOut),])
  
  knn.ad(Train.descriptors= Fold.Training.X, Test.descriptors= Fold.Test.X, Train.activity= Fold.Training.Y, Test.activity= Fold.Test.Y, DC.par=0.5, parameter.knn=5)
  DA.table <- as.data.frame(ktest<=Dc)
  colnames(DA.table) <- "AD"
  DA.table$AD[DA.table$AD=="TRUE"] <- "Reliable"
  DA.table$AD[DA.table$AD=="FALSE"] <- "Unreliable"
  qsarr::get.folds.pred(model)
  Evaluate.Fold.AD <-cbind(model.folds.pred,DA.table)
  
  assign("Evaluate.Fold.AD", Evaluate.Fold.AD, envir=.GlobalEnv)
}

prediction.AD <- function(Y.class= Y.class, X.class=X.class, DC.par=0.5,parameter.knn=5,algorithm.knn=c("cover_tree"), ... ){
  
  ktrain=(knn.dist(Train.descriptors, k=parameter.knn,  algorithm=algorithm.knn )[,parameter.knn])
  .GlobalEnv[["ktrain"]] <- ktrain
  ktest=(knnx.dist(data=Train.descriptors,Test.descriptors, k=parameter.knn, algorithm=algorithm.knn )[,parameter.knn])
  .GlobalEnv[["ktest"]] <- ktest
  Dc=(DC.par*(sd(ktrain)))+mean(ktrain)
  print(Dc)
  .GlobalEnv[["Dc"]] <- Dc
  DA.table <- as.data.frame(ktest<Dc)
  colnames(DA.table) <- "AD"
  DA.table$AD[DA.table$AD=="TRUE"] <- "Reliable"
  DA.table$AD[DA.table$AD=="FALSE"] <- "Unreliable"
  table.AD<-DA.table
  assign("table.AD", table.AD, envir=.GlobalEnv)
}
