#export some useful variables
dragon <- 4
electronic <- 5
protein <- 6
topological <- 7
geometrical <- 8
constitutional <- 9
hybrid <- 10
data <-11
#import function
import <- function(qsar=11 , activ=NULL, descrip=NULL, ... ){
{if ((qsar) == 2) {
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.2d)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == 3){
  library(rcdk)
  cdk()
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- rcdk::load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- rcdk::eval.desc(molecules, cdk.3d)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv) 
} else if ((qsar) == (dragon)) {;
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
} else if ((qsar) == data){
  qsar.activity <- activ
  qsar.activity <- as.data.frame(qsar.activity)
  .GlobalEnv[["qsar.activity"]] <- qsar.activity
  qsar.descriptors <- descrip
  .GlobalEnv[["qsar.descriptors"]] <- qsar.descriptors
} else {      
  print="ERROR to IMPORT"
}         
}
}