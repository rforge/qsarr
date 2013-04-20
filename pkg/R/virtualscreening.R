###VS
#export some useful variables
#import function
vs <- function(split=random,prop=0.75,number = 10,repeats = 3,tuneLength = 15, ... ){
################# Assign the Db's
VS.ml <- import(2)
      assign("VS.ml", VS.ml, envir=.GlobalEnv)
VS.data <- import(molecule)
      assign("VS.data", VS.data, envir=.GlobalEnv)
  
################# Mix and Clean the Db's  
VS.mix.DB<- rbind(VS.ml,VS.data)
      assign("VS.mix.DB", VS.mix.DB, envir=.GlobalEnv)
VS.mix.DB.clean <- clean.vs()
      assign("VS.mix.DB.clean", VS.mix.DB.clean, envir=.GlobalEnv)
  
  ################# Mix and Clean the Db's  
qsar.descriptors.cor <- VS.mix.DB.clean[c(row.names(VS.ml)),]
      assign("qsar.descriptors.cor", qsar.descriptors.cor, envir=.GlobalEnv)
vs.descritors.cpd <- VS.mix.DB.clean[c(row.names(VS.data)),]
      assign("vs.descritors.cpd", vs.descritors.cpd, envir=.GlobalEnv)
split()

################# Model  
knn.ad()
Kunknow=knnx.dist(data=Train.descriptors,vs.descritors.cpd, k=parameter.knn.ad, algorithm=c("cover_tree") ) 
      assign("Kunknow", Kunknow, envir=.GlobalEnv)
print(Kunknow[,parameter.knn.ad]<Dc)
vs.descritors.inside.AD <- subset(vs.descritors.cpd,(Kunknow[,parameter.knn.ad]<Dc))
      assign("vs.descritors.inside.AD", vs.descritors.inside.AD, envir=.GlobalEnv)
}         

