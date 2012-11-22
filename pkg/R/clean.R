clean <- function(data = qsar.descriptors, ... ){
  qsar.descriptors.na <- subset(data, select=colMeans(is.na(qsar.descriptors)) == 0)
  assign("qsar.descriptors.na", qsar.descriptors.na, envir=.GlobalEnv)
  nzv <- nearZeroVar(qsar.descriptors.na) 
  if (length(nzv > 1) != 0)  { 
    # Remove Zero variance
    nzv <- nearZeroVar(qsar.descriptors.na)
    qsar.descriptors.nzv <- qsar.descriptors.na[, -nzv]
    # Remover correlação acima 90%
    descriptors.correlation <- cor(qsar.descriptors.nzv)
    descriptors.correlation.9 <- findCorrelation(descriptors.correlation, cutoff = .9)
    qsar.descriptors.cor <- qsar.descriptors.nzv[,-descriptors.correlation.9]
  } else { 
    qsar.descriptors.cor <- qsar.descriptors.na
  } 
}