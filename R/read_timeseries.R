read_timeseries <- function(indir,yy){
  
  # piece of code to write time series file
  
  infile <- paste0(indir,"/eraland_timeseries_",yy,".nc")
  Hfile <- open.nc(infile)
  time <- var.get.nc(Hfile, "time")
  tunits <- att.get.nc(Hfile, "time", "units")
  ntim <- length(time)
  reftime <- as.POSIXct(substring(tunits,13,nchar(tunits)))
  timeaxis <- reftime + time*3600
  
  lonminar <- var.get.nc(Hfile, "lonmin")
  lonmaxar <- var.get.nc(Hfile, "lonmax")
  latminar <- var.get.nc(Hfile, "latmin")
  latmaxar <- var.get.nc(Hfile, "latmax")
  arealist <- var.get.nc(Hfile, "areaname")
  narea <- length(arealist)
  
  Etim <- var.get.nc(Hfile, "Etim")
  Rtim <- var.get.nc(Hfile, "Rtim")
  Ptim <- var.get.nc(Hfile, "Ptim")
  dWtim <- var.get.nc(Hfile, "dWtim")
  
  close.nc(Hfile)
  
  read_timeseries <- list("time"=time,"timeaxis"=timeaxis,"ntim"=ntim,
                   "lonminar"=lonminar,"lonmaxar"=lonmaxar,
                   "latminar"=latminar,"latmaxar"=latmaxar,
                   "arealist"=arealist,"narea"=narea,
                   "Etim"=Etim,"Rtim"=Rtim,"Ptim"=Ptim,"dWtim"=dWtim)
}

