#carrega bibliotecas necessárias
library(rcdk)
library(ggplot2)
library(caret)
library(pROC)
library(chemometrics)
library(doMC)
library(odfWeave)
system("defaults write org.R-project.R force.LANG en_US.UTF-8")
#parallel
pal <- function(par,  ... ){
  registerDoMC(par)
}
cdk <- function() { source('cdk.R', echo=TRUE)}
import <- function() { source('import.R', echo=TRUE)}
importsdf <- function() { source('importsdf.R', echo=TRUE)}
importsmiles <- function() { source('importsmiles.R', echo=TRUE)}
clean <- function() {source('clean.R', echo=TRUE) }
split <- function() {source('split.R', echo=TRUE) }
models <- function() {source('models.R', echo=TRUE) }
models.extented <- function() {source('models_extended.R', echo=TRUE) }
models.failsafe1 <- function() {source('models.failsafe1.R', echo=TRUE) }
models.failsafe2 <- function() {source('models.failsafe2.R', echo=TRUE) }
models <- function() {source('models.R', echo=TRUE) }
plotmodels <- function() {source('plotmodels.R', echo=TRUE) }
metrics <- function() {source('metrics.R', echo=TRUE) }
bias <- function() {source('bias.R', echo=TRUE) }
bias.failsafe2 <- function() {source('bias.failsafe2.R') }
desc <- get.desc.names( type= "topological")
desc1 <- get.desc.names( type= "electronic")

qsar.workflow.2d <- function(core, ... ){
  registerDoMC(core)
  cdk()
  importsdf()
  clean()
  split()
  models()
  metrics()
  bias()
}

qsar.workflow.2d.extended <- function(core, ... ){
  registerDoMC(core)
  cdk()
  importsdf()
  clean()
  split()
  models.extended()
  metrics()
  bias()
}