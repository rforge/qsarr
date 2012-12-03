#######################################################
#########Loading all Classic Metric functions##########
#######################################################
r2 <- function(y, equation, ... ){
  1 - (sum((y-predict(equation))^2)/sum((y-mean(y))^2))
}
#####
r2pred <- function(ypred.test, yobs.test, yobs.train, ... ){
  1-(sum((ypred.test-yobs.test)^2))/(sum( ((yobs.test-mean(yobs.train))^2) ))
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


Q2f1 <- function(ypred.test, yobs.test, yobs.train, ... ){
  1-(sum((ypred.test-yobs.test)^2))/(sum( ((yobs.test-mean(yobs.train))^2) ))
}

Q2f2 <- function(ypred.test, yobs.test, yobs.train, ... ){
  1-(sum((ypred.test-yobs.test)^2))/(sum( ((yobs.test-mean(yobs.test))^2) ))
}

Q2f3 <- function(ypred.test, yobs.test,yobs.train, nex, ntrain, ... ){
  1 - ((sum((ypred.test-yobs.test)^2))/nex) / ((sum((yobs.train-mean(yobs.train))^2) )/ntrain)
}

CCC <- function(ypred.test, yobs.test, nex, ... ){
  (2*(sum((yobs.test-mean(yobs.test))*(ypred.test-mean(ypred.test)))))/( (sum(((yobs.test-mean(yobs.test))^2))) + (sum(((ypred.test-mean(ypred.test))^2))) +  (nex*(mean(yobs.test)-mean(ypred.test)^2))  )
}

RMSEP <- function(ypred.test, nex, ... ){
  (sum((ypred.test-mean(ypred.test))^2)) / nex
}

RMSEC <- function(ypred.train, ntrain, ... ){
  (sum((ypred.train-mean(ypred.train))^2)) / ntrain
}