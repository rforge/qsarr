#Thanks to Leonardo Ramirez and Antoine Stevens for these two functions (Mahalanobis distances and kennardStone)
## Created by: Leonardo Ramirez and Antoine Stevens
## 2012
## antoine.stevens@uclouvain.be
## leonardo.ramirez@uclouvain.be

## This function computes Mahalanobis distances for data matrices
md <- function(x, x2=NULL, center = TRUE, scale = TRUE){
  
  if(length(x2) == 0){
    if(nrow(x) < ncol(x)) {stop("For Mahalanobis distance computations the number of rows must be higher than the number the number of colunms")}
  }else{
    if((nrow(x) + nrow(x2)) < ncol(x)) {stop("For Mahalanobis distance computations the nrow(x) + nrow(x2) must be hihgher tha the number of colunms")}
  }
  
  
  if(length(find.package("matrixStats",quiet=TRUE))==0) 
  if(!require("matrixStats")){require("matrixStats")}
  
  if(length(find.package("expm",quiet=TRUE))==0) 
  if(!require("expm")){require("expm")}
  
  
  if(length(x2) == 0){
    if(center == TRUE){
      x <- sweep(x = x, MARGIN = 2, FUN="-", STATS = colMeans(x))
    }
    
    if(scale == TRUE){
      x <- sweep(x = x, MARGIN = 2, FUN="/", STATS = colSds(x))
    }
    
    x <- as.matrix(x)
    vcv <- cov(x)
    sq_vcv <- sqrtm(vcv)
    sq_S <- solve(sq_vcv)
    ms_x <- x %*% sq_S
    md <-as.matrix(dist(ms_x))
    return(md)
  }
  
  if(length(x2) > 0){
    x <- rbind(x,x2)
    if(center == TRUE){
      x <- sweep(x = x, MARGIN = 2, FUN="-", STATS = colMeans(x))
    }
    
    if(scale == TRUE){
      x <- sweep(x = x, MARGIN = 2, FUN="/", STATS = colSds(x))
    }
    
    x <- as.matrix(x)
    vcv <- cov(x)
    sq_vcv <- sqrtm(vcv)
    sq_S <- solve(sq_vcv)
    x <- x %*% sq_S ## projected
    
    x2 <- x[(nrow(x) - nrow(x2) +1):nrow(x), ]
    if(length(nrow(x2)) == 0){x2 <- t(x2)}
    x <- x[1:(nrow(x) - nrow(x2)), ]
    
    md <- matrix(NA, nrow(x), nrow(x2))
    for(i in 1:nrow(x2))
    {
      subs <- sweep(x = x, MARGIN = 2, FUN="-", STATS = x2[i, ])
      md[,i] <- (rowSums(subs^2))^0.5
    }
    return(md)
  }
}

kennardStone <- function(pcv, profN = NULL, k = NULL, distance = "MD", StartCenter = TRUE, ...){
  
  if(length(k)==0){
    if(length(prof)==0){
      round(0.75*nrow(pcv))} else {
        nProf=length(intersect(profN,profN))
        k=0.75*nProf 
      }
  }
  
  # Sanity checks
  if(class(pcv)!="data.frame" & class(pcv)!="matrix"){stop("Invalid argument: 'pcv' has to be of class 'data.frame' or 'matrix'.")};
  if(k > nrow(pcv)){stop("k must be lower than the number of samples in your matrix")};
  if(length(profN)>0){
    if(class(profN)!="numeric" & class(profN)!="integer"){stop("Invalid argument: 'profN' must be a numerical vector, sorry ;-)")};
  }
  if(ncol(pcv)<2){stop("pcv must have at least 2 columns")};
  if(distance!="MD"){if(distance!="ED"){stop("Invalid argument: 'distance' must be 'MD' or 'ED'.")}}
  if(k<2){stop("Invalid argument: 'k' has to be higher than 2")};
  
  
  rownames(pcv) <- 1:nrow(pcv)
  nm <- rownames(pcv)
  pcvInclCenter <- rbind(pcv, colMeans(pcv))
  
  if(distance=="MD"){M<-md(x = pcvInclCenter, center = FALSE, scale = FALSE)};
  if(distance=="ED"){M<-as.matrix(dist(pcvInclCenter))}
  
  Mdist <- M[-(ncol(M)),-(ncol(M))]
  Mc <- M[(ncol(M)),-(ncol(M))]
  
  # Get the first two points S1 and S2
  
  if(StartCenter == TRUE){S1=(which(min(Mc) == Mc, arr.ind = FALSE))[[1]]}
  if(StartCenter == FALSE){S1=(which(max(Mc) == Mc, arr.ind = FALSE))[[1]]}
  rm(M,Mc)
  
  S2 <- (which(max(Mdist[,S1])==Mdist[,S1], arr.ind = FALSE))[[1]]
  
  KSs <- c(S1,S2)
  
  if(length(profN)>0){
    prf <- profN[KSs]
    KSs <- as.numeric(rownames(pcv)[profN %in% prf])
  }
  
  pb <- txtProgressBar(style=3)
  # Get the rest of the points
  for(ii in 3:k)
  {
    ipb <- 3:k
    ipb <- (ipb - min(ipb)) / (max(ipb) - min(ipb))
    setTxtProgressBar(pb, ipb[ii])
    nsv <- max(rowMins(Mdist[-KSs,KSs],na.rm=TRUE))
    nsr_rc <- data.frame(which(Mdist[,KSs] == nsv, arr.ind = TRUE))
    nsr <- nsr_rc$row
    KSs <- c(KSs,nsr)
    
    if(length(profN)>0){
      prf <- profN[KSs]
      KSs <- as.numeric(rownames(pcv)[profN %in% prf])
    }
    
  }
  close(pb)
  pc <- ncol(pcv)
  
  val <- nm[-KSs]
  rowN <- list(cal = KSs, val = as.numeric(val))
  
  if(length(profN)>0){
    rowN <- list(cal = sort(rowN$cal), val = sort(rowN$val))
  }
  return(rowN)
}