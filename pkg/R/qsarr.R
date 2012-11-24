#system("defaults write org.R-project.R force.LANG en_US.UTF-8")
#carrega bibliotecas necessárias


qsar.workflow <- function(cores=2,qsar,type=random, prop=0.75, ... ){
  library(doMC)
  library(caret)
  library(ggplot2)
  library(odfWeave)
  cdk()
  doMC::registerDoMC(cores)
  import(qsar)
  clean()
  split(type, prop)
  models()
}
