source('call.R')
# importar dados no formato csv na sequência: 1. y resposta biológica 2. x - descritores
qsar.activity <- read.csv(file.choose(),sep = ",", header=TRUE)
qsar.descriptors <- read.csv(file.choose(),sep = ",", header=TRUE)