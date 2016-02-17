clean <- function(data = qsar.descriptors, ... ){
  library(caret)
  qsar.descriptors.na.func <- subset(data, select=colMeans(is.na(qsar.descriptors)) == 0)
  assign("qsar.descriptors.na", qsar.descriptors.na.func, envir=.GlobalEnv)
  nzv.func <- caret::nearZeroVar(qsar.descriptors.na)
  assign("nzv", nzv.func, envir=.GlobalEnv)
  if (length(nzv > 1) != 0)  { 
    # Remove Zero variance
    nzv.func <- caret::nearZeroVar(qsar.descriptors.na)
    assign("nzv.func", nzv.func, envir=.GlobalEnv)
    qsar.descriptors.nzv.func <- qsar.descriptors.na[, -nzv]
    assign("qsar.descriptors.nzv", qsar.descriptors.nzv.func, envir=.GlobalEnv)
    # Remover correlação acima 90%
    descriptors.correlation.func <- cor(qsar.descriptors.nzv)
    assign("descriptors.correlation", descriptors.correlation.func, envir=.GlobalEnv)
    descriptors.correlation.75.func <- caret::findCorrelation(descriptors.correlation, cutoff = .75)
    assign("descriptors.correlation.75", descriptors.correlation.75.func, envir=.GlobalEnv)
    qsar.descriptors.cor.remove <- qsar.descriptors.nzv[,-descriptors.correlation.75]
    assign("qsar.descriptors.cor.remove", qsar.descriptors.cor.remove, envir=.GlobalEnv)
    preProcValues.func <- caret::preProcess(qsar.descriptors.cor.remove, method = c("center", "scale"))
    assign("preProcValues", preProcValues.func, envir=.GlobalEnv)
    qsar.descriptors.cor <- stats::predict(preProcValues, qsar.descriptors.cor.remove)
    assign("qsar.descriptors.cor", qsar.descriptors.cor, envir=.GlobalEnv)
  } else { 
    qsar.descriptors.cor.func <- qsar.descriptors.na
    assign("qsar.descriptors.cor", qsar.descriptors.cor.func, envir=.GlobalEnv)
  } 
}

clean.noPre <- function(data = qsar.descriptors, ... ){
  library(caret)
  qsar.descriptors.na.func <- subset(data, select=colMeans(is.na(qsar.descriptors)) == 0)
  assign("qsar.descriptors.na", qsar.descriptors.na.func, envir=.GlobalEnv)
  nzv.func <- caret::nearZeroVar(qsar.descriptors.na)
  assign("nzv", nzv.func, envir=.GlobalEnv)
  if (length(nzv > 1) != 0)  { 
    # Remove Zero variance
    nzv.func <- caret::nearZeroVar(qsar.descriptors.na)
    assign("nzv.func", nzv.func, envir=.GlobalEnv)
    qsar.descriptors.nzv.func <- qsar.descriptors.na[, -nzv]
    assign("qsar.descriptors.nzv", qsar.descriptors.nzv.func, envir=.GlobalEnv)
    # Remover correlação acima 90%
    descriptors.correlation.func <- cor(qsar.descriptors.nzv)
    assign("descriptors.correlation", descriptors.correlation.func, envir=.GlobalEnv)
    descriptors.correlation.75.func <- caret::findCorrelation(descriptors.correlation, cutoff = .75)
    assign("descriptors.correlation.75", descriptors.correlation.75.func, envir=.GlobalEnv)
    qsar.descriptors.cor.remove <- qsar.descriptors.nzv[,-descriptors.correlation.75]
    assign("qsar.descriptors.cor.remove", qsar.descriptors.cor.remove, envir=.GlobalEnv)
    assign("qsar.descriptors.cor", qsar.descriptors.cor.remove, envir=.GlobalEnv)
  } else { 
    qsar.descriptors.cor.func <- qsar.descriptors.na
    assign("qsar.descriptors.cor", qsar.descriptors.cor.func, envir=.GlobalEnv)
  } 
}