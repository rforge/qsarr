system("defaults write org.R-project.R force.LANG en_US.UTF-8")
#carrega bibliotecas necessárias


qsar.workflow <- function(cores=2,qsar, ... ){
  library(rcdk)
  library(ggplot2)
  library(caret)
  library(pROC)
  library(chemometrics)
  library(doMC)
  library(odfWeave)
  cdk()
  registerDoMC(cores)
  import(qsar)
  clean()
}
