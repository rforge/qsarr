#system("defaults write org.R-project.R force.LANG en_US.UTF-8")
#carrega bibliotecas necessárias


qsar.workflow <- function(cores=2,qsar, ... ){
  cdk()
  registerDoMC(cores)
  import(qsar)
  clean()
}
