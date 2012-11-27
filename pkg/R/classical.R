#######################################################
#########Loading all Classic Metric functions##########
#######################################################
r2 <- function(y, equation, ... ){
  1 - (sum((y-predict(equation))^2)/sum((y-mean(y))^2))
}
####
r2pred <- function(y, x, ... ){
1-(sum((y-x)^2))/(sum(((y-mean(y))^2)))
}