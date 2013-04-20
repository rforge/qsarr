#export some useful variables
data <- 4
electronic <- 5
protein <- 6
topological <- 7
geometrical <- 8
constitutional <- 9
hybrid <- 10
molecule <-11
#import function
import <- function(qsar=11 , activ=NULL, descrip=NULL, ... ){
{if ((qsar) == 2) {
  library(rcdk)
      cdk()
qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
      assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
cat("##### The CDK implements a number of descriptors divided into three main groups - atomic, molecular and bond. This method evaluates 2D molecular descriptors  - Please thanks to Rajarshi Guha <rajarshi.guha@gmail.com> ###############\n")
  qsar.descriptors.func <- (rcdk::eval.desc(molecules, cdk.2d, verbose=TRUE))
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv) 
  
} else if ((qsar) == 3){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  cat("##### The CDK implements a number of descriptors divided into three main groups - atomic, molecular and bond. This method evaluates 3D molecular descriptors  - Please thanks to Rajarshi Guha <rajarshi.guha@gmail.com> ###############\n")
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.3d, verbose=TRUE)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv) 

  } else if ((qsar) == (data)) {;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  qsar.descriptors.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == electronic){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.electronic);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == protein){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.protein);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == topological){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.topological);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == geometrical){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.geometrical);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == constitutional){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.constitutional);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == constitutional){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.constitutional)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == hybrid){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.hybrid)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == molecule){
  cdk()
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.2d)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else {      
  print="ERROR to IMPORT"
}         
}
}


#where.sdf<- (file.choose())
#moliter <- iload.molecules(where.sdf, type="sdf")
#while(hasNext(moliter)) {
#  mol <- nextElem(moliter)
#  print(get.property(mol, "cdk:Title"))
#  rcdk::eval.desc(mol, cdk.geometrical)
#}
#qsar.descriptors.func <- rcdk::eval.desc(mol, cdk.2d) 
#assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)

#moliter <- iload.molecules(where.sdf, type="sdf")
#while(hasNext(moliter)) {
#  mol <- nextElem(moliter)
 # a=print(rcdk::eval.desc(mol, cdk.geometrical)) 
#}