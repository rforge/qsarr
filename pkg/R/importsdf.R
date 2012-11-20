source('call.R')
###########Generating 3D CDK descriptors###########
# importar dados no formato sdf na sequência: 1. y resposta biológica 2. x - descritores
qsar.activity <- read.csv(file.choose(),sep = ",", header=TRUE)
molecules <- load.molecules(file.choose())
qsar.descriptors <- eval.desc(molecules, cdk.2d)
