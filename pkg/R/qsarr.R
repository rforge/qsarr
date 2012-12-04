#system("defaults write org.R-project.R force.LANG en_US.UTF-8")
#carrega bibliotecas necessárias

qsar.workflow <- function(cores=2,qsar=2,activ=NULL,descrip=NULL,split=random,prop=0.75,type=small,method=cv,number=10,repeats=3,tuneLength=15, ... ){
 # library(doMC)
  #cdk()
  #import(qsar,activ, descrip)
  #clean()
  #split(split, prop, number,repeats,tuneLength)
  #models(cores,type,method,number,repeats,tuneLength)
  metrics(type)
}


