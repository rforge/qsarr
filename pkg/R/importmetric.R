#export some useful variables
default <- 1
sybyl <- 2
experimental <- 3
#import function
import.metrics <- function(stantard=1, ... ){
{if ((stantard) == 1) {
  
  Trainind.Data.func <- as.data.frame(read.csv(file.choose(),sep = ",", header=TRUE))
  assign("Trainind", Trainind.Data.func, envir=.GlobalEnv)
  Test.Data.func <- as.data.frame(read.csv(file.choose(),sep = ",", header=TRUE))
  assign("Test", Test.Data.func, envir=.GlobalEnv)
  evaluate()
} else if ((stantard) == 2){
  
  Training <- as.data.frame(read.table(file.choose(), header=TRUE))
  assign("Training", Training, envir=.GlobalEnv)
  Test <- as.data.frame(read.table(file.choose(), header=TRUE))
  assign("Test", Test , envir=.GlobalEnv)
  evaluate() 
  
} else if ((stantard) == 3){
  
  Training.obs <- as.data.frame(read.csv(file.choose(),sep = ",", header=FALSE))
  assign("Training.obs", Training.obs, envir=.GlobalEnv)
  Test.obs <- as.data.frame(read.csv(file.choose(),sep = ",", header=FALSE))
  assign("Test.obs", Test.obs, envir=.GlobalEnv)
  
} else {      
  print="ERROR to IMPORT"
}         
}
}