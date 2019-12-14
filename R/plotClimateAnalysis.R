
source("./R/read_timeseries.R")
source("./R/XMeanStd.R")
source("./R/timeavg.R")

Sys.setlocale("LC_ALL","English")

library(RNetCDF)
library(ncdf4)
library(maps)

# set project settings
# ====================
#yylist <- c(1979:2010)
#iarea <- 10
#navgdays <- 7  # averaging interval (days)
#rangetype <- "minmax" # "minmax" or "std"
  

hydroclimAnalysis <- function(yylist = c(1979:2010), iarea = 10, navgdays = 7, rangetype = "minmax") {
  
  # prepare data storage
  nyear <- length(yylist)
  Estore <- array(NA,dim=c(nyear,365))
  Pstore <- array(NA,dim=c(nyear,365))
  Rstore <- array(NA,dim=c(nyear,365))
  dWstore <- array(NA,dim=c(nyear,365))
  tstore <- array(NA,dim=c(nyear,365))
  
  # read the data
  # =============
  iyr <- 0
  for (yy in yylist){
    
    iyr <- iyr + 1
    indata <- read_timeseries("./input/timeseries/",yy)
    
    lonminar <- indata$lonminar
    lonmaxar <- indata$lonmaxar
    latminar <- indata$latminar
    latmaxar <- indata$latmaxar
    
    time <- indata$time
    timeaxis <- indata$timeaxis
    ntim <- indata$ntim
    arealist <- indata$arealist
    narea <- length(arealist)
    
    # read area averaged time series
    Etim <-indata$Etim
    Ptim <-indata$Ptim
    Rtim <-indata$Rtim
    dWtim <-indata$dWtim
    
    # manage the storage; skip 29 feb
    if (ntim > 365){
      tlist <-c(1:59,61:366)
    } else{
      tlist <- c(1:365)
    }
    Estore[iyr,] <- Etim[tlist,iarea]
    Pstore[iyr,] <- Ptim[tlist,iarea]
    Rstore[iyr,] <- Rtim[tlist,iarea]
    dWstore[iyr,] <- dWtim[tlist,iarea]
    tstore <- timeaxis[tlist]
  }
  
  # do time averaging
  # =================
  xdum <- timeavg(Estore,tstore,navgdays)
  Estore <- xdum$Xavg
  xdum <- timeavg(Pstore,tstore,navgdays)
  Pstore <- xdum$Xavg
  xdum <- timeavg(Rstore,tstore,navgdays)
  Rstore <- xdum$Xavg
  xdum <- timeavg(dWstore,tstore,navgdays)
  dWstore <- xdum$Xavg
  tstore <- xdum$Tavg
  nint <- xdum$nint
  
  # Calculate variables to be plotted
  # =================================
  tvar <- tstore
  tXar <- c(tvar,tvar[nint:1])
  
  Xvar <- XMeanStd(Estore)
  if(rangetype == "std"){
    range1 <- Xvar$mean - Xvar$std
    range2 <- Xvar$mean + Xvar$std
  }else if (rangetype == "minmax" ){
    range1 <- Xvar$min
    range2 <- Xvar$max
  }
  EYar <- c(range1,range2[nint:1])
  Eline <- Xvar$mean
  
  Xvar <- XMeanStd(Pstore)
  if(rangetype == "std"){
    range1 <- Xvar$mean - Xvar$std
    range2 <- Xvar$mean + Xvar$std
  }else if (rangetype == "minmax" ){
    range1 <- Xvar$min
    range2 <- Xvar$max
  }
  PYar <- c(range1,range2[nint:1])
  Pline <- Xvar$mean
  
  Xvar <- XMeanStd(Rstore)
  if(rangetype == "std"){
    range1 <- Xvar$mean - Xvar$std
    range2 <- Xvar$mean + Xvar$std
  }else if (rangetype == "minmax" ){
    range1 <- Xvar$min
    range2 <- Xvar$max
  }
  RYar <- c(range1,range2[nint:1])
  Rline <- Xvar$mean
  
  Xvar <- XMeanStd(dWstore)
  if(rangetype == "std"){
    range1 <- Xvar$mean - Xvar$std
    range2 <- Xvar$mean + Xvar$std
  }else if (rangetype == "minmax" ){
    range1 <- Xvar$min
    range2 <- Xvar$max
  }
  dWYar <- c(range1,range2[nint:1])
  dWline <- Xvar$mean
  
  if (dev.cur()>1) dev.off()
  
  # plot some time series
  Gshade <- rgb(0,255,0,max=255,alpha=60)
  Bshade <- rgb(0,0,255,max=255,alpha=60)
  Rshade <- rgb(255,0,0,max=255,alpha=60)
  Oshade <- rgb(255,165,0,max=255,alpha=60)
  
  
  ir_lim <- round(range(c(EYar,PYar,RYar,dWYar)))
  ir_brk <- seq(min(ir_lim), max(ir_lim)) 
  
  #### EVAPORATION
  df_avg <- tibble(tvar = tvar, Evap = Eline, Precip = Pline, Runoff = Rline, dS = dWline) %>%
    gather(key = var, value = value, -tvar)
  
  df_rng <- tibble(tvar = tXar, Evap = EYar, Precip = PYar, Runoff = RYar, dS = dWYar) %>%
    gather(key = var, value = value, -tvar) %>%
    group_by(tvar, var) %>%
    summarize(min = min(value), max = max(value))
  
  df <- df_avg %>% left_join(df_rng, by = c("tvar", "var")) %>%
    mutate(tvar = as.Date(tvar)) %>%
    mutate(var = factor(var, levels = c("Precip", "Runoff", "Evap", "dS")))
  
  var_colors <- c("#377eb8", "#e41a1c","#4daf4a","#ff7f00")
  
  
  tit <- paste0("Mean annual cycle +/- ",rangetype,"; region ",arealist[iarea]," lon:[",lonminar[iarea],"]-[",
                lonmaxar[iarea],"], lat:[",latminar[iarea],"]-[",latmaxar[iarea],"], years:[",
                min(yylist),"-",max(yylist),"]; avg interval: ",navgdays,"days")
  
  tit1 <- paste0("Region ",arealist[iarea]," lon:[",lonminar[iarea],"]-[",
                 lonmaxar[iarea],"], lat:[",latminar[iarea],"]-[",latmaxar[iarea],"], Years:[",
                 min(yylist),"-",max(yylist),"]; Avg interval: ",navgdays,"days")
  
  
  p <- ggplot(df, aes(x = tvar)) +
    ggtitle(paste0("Mean annual cycle +/- ", rangetype), subtitle = tit1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = ir, breaks = ir_brk) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_color_manual(values = var_colors) +
    scale_fill_manual(values = var_colors) +
    labs(x = "", y = "mm/day", color = "variable", fill = "variable") +
    geom_line(aes(y = value, color = var), size = 1) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = var), alpha = 0.2) +
    geom_hline(yintercept = 0)
  
  return(p)
  
}







