deps.qsarr <- function(Train.descriptors= Train.descriptors,  ...){

install.packages(c("ada", "AMORE", "nnet", "caret", "earth", "mda", "arm", "kohonen", "party", "mboost", "plyr", "Boruta", "randomForest", "brnn", "C50", "rrcovHD", "Cubist", "elmNN"),  dependencies = TRUE, repos = "http://cran.us.r-project.org")  

install.packages(c("elasticnet", "evtree", "extraTrees", "foba", "mgcv", "mboost", "gam", "kernlab", "gbm", "glmnet", "MASS", "gpls", "hda", "HDclassif", "fastICA", "RWeka", "pls", "kknn", "KRLS", "lars", "leaps", "rrcov", "logicFS", "caTools", "LogicReg", "class", "mda", "HiDimDA", "RSNNS", "klaR", "neuralnet", "nodeHarvest"),  dependencies = TRUE, repos = "http://cran.us.r-project.org")  


install.packages(c("obliqueRF", "pamr", "partDSA", "penalized", "penalizedLDA", "stepPlr", "proxy", "protoclass", "quantregForest", "qrnn", "relaxo", "rFerns", "HiDimDA", "rknn", "rocc", "rpart", "RRF", "rrlda", "rrcovHD", "sda", "SDDA", "ipred", "sparseLDA", "spls", "klaR", "superpc", "ipred", "vbmp", "bst", "caret","odfWeave","ggplot2","matrixStats","expm","FactoMineR","XML","DMwR","FNN", "rcdklibs", "devtools", "doMC", "Metrics", "mlbench", "caTools", "rcdk", "foreach", "gbm", "caTools", "lattice"),  dependencies = TRUE, repos = "http://cran.us.r-project.org")

library(devtools)

install_github('medley', 'mewo2')

install_github('caretEnsemble', 'zachmayer')
}