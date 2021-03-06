evaluate <- function(stantard=1, ... ){
#####################################################
#SYBYL
Yobs.training <- as.matrix(Training[2])
Ypred.training <- as.matrix(Training[3])
Ncompounds.training <- nrow(Training)
Yobs.test  <- as.matrix(Test[2])
Ypred.test <- as.matrix(Test[3])
Ncompounds.test <- nrow(Test)
#####################################################
cat("##### Q2f1 for Test or External Validation - Old R2Pred #####\n")
print(qsarm::r2pred(Ypred.test, Yobs.test, Yobs.training))
cat("##### Q2f2 for Test or External Validation #####\n")
print(qsarm::Q2f2(Ypred.test, Yobs.test, Yobs.training))
cat("##### Q2f3 for Test or External Validation - Most Accepeted #####\n")
print(qsarm::Q2f3(Ypred.test, Yobs.test,Yobs.training, Ncompounds.test, Ncompounds.training))
cat("##### CCC (concordance correlation coefficient) for Test or External Validation#####\n")
print(qsarm::CCC(Ypred.test, Yobs.test, Ncompounds.test))
cat("##### RMSEC - Root Mean Square (RMS) for Training Set#####\n")
print(qsarm::RMSEC(Yobs.training,Ypred.training, Ncompounds.training))
cat("##### RMSEP - Root Mean Square (RMS) for Test Set#####\n")
print(qsarm::RMSEP(Yobs.test,Ypred.test, Ncompounds.test))
cat("##### MAE - Mean Absolute Error for Test Set#####\n")
print(qsarm::MAE(Yobs.test,Ypred.test, Ncompounds.test))
cat("#####Rm2 for Test or External Validation#####\n")
qsarm::rm2(Yobs.test,Ypred.test)
cat("#####Rm2 Reverse for Test or External Validation#####\n")
qsarm::rm2.reverse(Yobs.test,Ypred.test)
cat("##### AVERAGE Rm2 for Test or External Validation#####\n")
qsarm::average.rm2(Yobs.test,Ypred.test)
cat("#####DELTA Rm2 for Test or External Validation#####\n")
qsarm::delta.rm2(Yobs.test,Ypred.test)
cat("#####Durbin-Watson Test for the Training Set#####\n")
print(durbinWatsonTest(as.vector(((Yobs.training))  - ((Ypred.training)))))
cat("#####Durbin-Watson Test for the Test Set#####\n")
print(durbinWatsonTest(as.vector(((Yobs.test))  - ((Ypred.test)))))
cat("#####Durbin-Watson Test for the Overall Set#####\n")
print(durbinWatsonTest(as.vector((rbind(Yobs.training,Yobs.test))  - (rbind(Ypred.training,Ypred.test)))))
}