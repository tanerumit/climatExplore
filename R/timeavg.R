timeavg <- function(Xstore,tstore,navgdays){
  nyr <- dim(Xstore)[1]
  
  tvar <- tstore
  tavg <- navgdays
  ctvar <- paste0(tavg," days")
  dstart <- seq(tvar[(1+tavg/2)],tvar[365],by = ctvar)
  nint <- length(dstart)
  indx <- rep(dstart[1],tavg)
  for (ii in c(2:nint)){
    indx <- c(indx,rep(dstart[ii],tavg))
  }
  iv <- indx[1:365]
  indx <- iv
  
  Xavg<-array(NA,dim=c(nyr,nint))
  for (iyr in 1:nyr){
    Xavg[iyr,] <- tapply(Xstore[iyr,],indx,mean)
  }
  timeavg <- list("Xavg"=Xavg,"Tavg"=dstart,"nint"=nint)
}