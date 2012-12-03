#######################################################
#########Loading all Classic Metric functions##########
#######################################################
r2 <- function(y, equation, ... ){
  1 - (sum((y-predict(equation))^2)/sum((y-mean(y))^2))
}
#####
r2pred <- function(Ypred.test, Yobs.test, Yobs.training, ... ){
  1-(sum((Ypred.test-Yobs.test)^2))/(sum( ((Yobs.test-mean(Yobs.training))^2) ))
}
#######################################################
###########Loading all rm2 Metric functions############
#######################################################
rm2 <- function(y, x, ... ){  
  if ((r2(y,(lm(y ~ x)))) > r2(y,(lm(y ~ -1 + x)))) { 
    print(r2(y,(lm(y ~ x)))*( 1-(sqrt(r2(y,(lm(y ~ x)))-r2(y,(lm(y ~ -1 + x)))))))
  } else { 
    print(r2(y,(lm(y ~ x))))
  } 
}
rm2.reverse <- function(y, x, ... ){
  print(r2(x,(lm(x ~ y)))*( 1-(sqrt(r2(x,(lm(x ~ y)))-r2(x,(lm(x ~ -1 + y)))))))
}
average.rm2 <- function(y, x, ... ){
  if ((r2(y,(lm(y ~ x)))) > r2(y,(lm(y ~ -1 + x)))) {
    print(((r2(y,(lm(y ~ x)))*( 1-(sqrt(r2(y,(lm(y ~ x)))-r2(y,(lm(y ~ -1 + x))))))+ r2(x,(lm(x ~ y)))*( 1-(sqrt(r2(x,(lm(x ~ y)))-r2(x,(lm(x ~ -1 + y))))))))/2)
  } else { 
    print(((r2(y,(lm(y ~ x))))  + (r2(x,(lm(x ~ y)))*( 1-(sqrt(r2(x,(lm(x ~ y)))-r2(x,(lm(x ~ -1 + y)))))))  )/2)
  }
}
delta.rm2 <- function(y, x, ... ){
  if ((r2(y,(lm(y ~ x)))) > r2(y,(lm(y ~ -1 + x)))) {
    print(abs((r2(y,(lm(y ~ x)))*( 1-(sqrt(r2(y,(lm(y ~ x)))-r2(y,(lm(y ~ -1 + x))))))  -  r2(x,(lm(x ~ y)))*( 1-(sqrt(r2(x,(lm(x ~ y)))-r2(x,(lm(x ~ -1 + y)))))))))
  } else { 
    print(abs((r2(y,(lm(y ~ x)))) - (r2(x,(lm(x ~ y)))*( 1-(sqrt(r2(x,(lm(x ~ y)))-r2(x,(lm(x ~ -1 + y)))))))  ))
  }
}


Q2f1 <- function(Ypred.test, Yobs.test, Yobs.training, ... ){
  1-(sum((Ypred.test-Yobs.test)^2))/(sum( ((Yobs.test-mean(Yobs.training))^2) ))
}

Q2f2 <- function(Ypred.test, Yobs.test, Yobs.training, ... ){
  1-(sum((Ypred.test-Yobs.test)^2))/(sum( ((Yobs.test-mean(Yobs.test))^2) ))
}

Q2f3 <- function(Ypred.test, Yobs.test,Yobs.training, Ncompounds.test, Ncompounds.training, ... ){
  1 - ((sum((Ypred.test-Yobs.test)^2))/Ncompounds.test) / ((sum((Yobs.training-mean(Yobs.training))^2) )/Ncompounds.training)
}

CCC <- function(Ypred.test, Yobs.test, Ncompounds.test, ... ){
  (2*(sum((Yobs.test-mean(Yobs.test))*(Ypred.test-mean(Ypred.test)))))/( (sum(((Yobs.test-mean(Yobs.test))^2))) + (sum(((Ypred.test-mean(Ypred.test))^2))) +  (Ncompounds.test*(mean(Yobs.test)-mean(Ypred.test)^2))  )
}


RMSEP <- function(Yobs.test,Ypred.test, Ncompounds.test, ... ){
  sqrt((sum((Ypred.test-Yobs.test)^2)) / Ncompounds.test)
}

RMSEC <- function(Yobs.traininging,Ypred.training, Ncompounds.training, ... ){
  sqrt((sum((Ypred.training-Yobs.traininging)^2)) / Ncompounds.training)
}

MAE <- function(Yobs.test,Ypred.test, Ncompounds.test, ... ){
  abs((sum(Ypred.test-Yobs.test)) / Ncompounds.test)
}