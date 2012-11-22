#import variable dragon
dragon <- 4
electronic <- 5
protein <- 6
topological <- 7
geometrical <- 8
constitutional <- 9
hybrid <- 10

import <- function(qsar=11 , ... ){
{if ((qsar) == 2) {; 
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.2d);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == 3){;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.3d);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv) 
} else if ((qsar) == (dragon)) {;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  qsar.descriptors.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == electronic){
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.electronic);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == protein){
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.protein);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == topological){;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.topological);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == geometrical){;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.geometrical);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == constitutional){;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.constitutional);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == constitutional){;
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.constitutional);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else if ((qsar) == hybrid){
  qsar.activity.func <- read.csv(file.choose(),sep = ",", header=TRUE)
  assign("qsar.activity", qsar.activity.func, envir=.GlobalEnv)
  molec <- load.molecules(file.choose())
  assign("molecules", molec, envir=.GlobalEnv)
  qsar.descriptors.func <- eval.desc(molecules, cdk.hybrid);
  assign("qsar.descriptors", qsar.descriptors.func, envir=.GlobalEnv)
} else {      
  print="Empity Model"
}         
}
}

