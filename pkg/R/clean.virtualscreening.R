clean.vs <- function(data = VS.mix.DB, ... ){
  library(caret)
VS.mix.DB.na <- subset(data, select=colMeans(is.na(VS.mix.DB)) == 0)
      assign("VS.mix.DB.na", VS.mix.DB.na, envir=.GlobalEnv)
nzv.vs <- caret::nearZeroVar(VS.mix.DB.na)
assign("nzv.vs", nzv.vs, envir=.GlobalEnv)
  if (length(nzv > 1) != 0)  { 
    # Remove Zero variance
nzv.vs <- caret::nearZeroVar(VS.mix.DB.na)
      assign("nzv.vs", nzv.vs, envir=.GlobalEnv)
qsar.descriptors.nzv.vs <- VS.mix.DB.na[, -nzv.vs]
      assign("qsar.descriptors.nzv.vs", qsar.descriptors.nzv.vs, envir=.GlobalEnv)
# Remove hightly data ~ 75%
descriptors.correlation.vs <- cor(qsar.descriptors.nzv.vs)
      assign("descriptors.correlation.vs", descriptors.correlation.vs, envir=.GlobalEnv)
descriptors.correlation.75.vs <- caret::findCorrelation(descriptors.correlation.vs, cutoff = .75)
      assign("descriptors.correlation.75.vs", descriptors.correlation.75.vs, envir=.GlobalEnv)
qsar.descriptors.cor.remove.vs <- qsar.descriptors.nzv.vs[,-descriptors.correlation.75.vs]
      assign("qsar.descriptors.cor.remove.vs", qsar.descriptors.cor.remove.vs, envir=.GlobalEnv)
    preProcValues.vs <- caret::preProcess(qsar.descriptors.cor.remove.vs, method = c("center", "scale"))
    assign("preProcValues.vs", preProcValues.vs, envir=.GlobalEnv)
    qsar.descriptors.cor.vs <- stats::predict(preProcValues.vs, qsar.descriptors.cor.remove.vs)
    assign("qsar.descriptors.cor.vs", qsar.descriptors.cor.vs, envir=.GlobalEnv)
  } else { 
    qsar.descriptors.cor.vs <- qsar.descriptors.na.vs
    assign("qsar.descriptors.cor.vs", qsar.descriptors.cor.vs, envir=.GlobalEnv)
  } 
}