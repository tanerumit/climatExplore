XMeanStd <- function(Xvar){
  nyear <- dim(Xvar)[1]
  nint <- dim(Xvar)[2]
  Xmean <- apply(Xvar,2,mean)
  Xmin <- apply(Xvar,2,min)
  Xmax <- apply(Xvar,2,max)
  
  sumsq <- array(0,dim=nint)
  for (iyr in 1:nyear){
    p <- Xvar[iyr,]-Xmean
    sumsq <- sumsq + p*p
  }
  Xstd <- sqrt(sumsq/nyear)
  XMeanStd <- list("mean"=Xmean,"std"=Xstd,"min"=Xmin,"max"=Xmax)
}
