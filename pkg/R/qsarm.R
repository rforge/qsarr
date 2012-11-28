#######################################################
#########Loading all Classic Metric functions##########
#######################################################
r2 <- function(y, equation, ... ){
  1 - (sum((y-predict(equation))^2)/sum((y-mean(y))^2))
}
#####
r2pred <- function(y, x, ... ){
  1-(sum((y-x)^2))/(sum(((y-mean(y))^2)))
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

Q2f3 <- function(y,x, ytrain,nex, ntrain, ... ){
  1- ((sum(y-x)^2)/nex) / ((sum(((y-mean(ytrain))^2)))/ntrain)
}
