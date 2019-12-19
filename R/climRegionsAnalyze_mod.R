
### UI-Side function
climRegionsAnalyze_mod_UI <- function(id) {
  
  ns = NS(id)
  
  highchartOutput(ns("regionPlot1"))
  #plotOutput(ns("regionPlot1"))
  
}

### Server-side function
climRegionsAnalyze_mod <- function(input, output, session, iareaR = NULL, 
                                   rangetypeR = NULL, navgdaysR = NULL,
                                   yylistR = NULL) {

  require(RNetCDF)
  require(ncdf4)
  require(maps)
  
  var_colors <- c("#377eb8", "#e41a1c","#4daf4a","#ff7f00")
  
  plotData <- reactive({
  
    # Reactive terms
    iarea     <- as.numeric(iareaR())
    navgdays  <- navgdaysR()
    rangetype <- rangetypeR()
    yylist    <- yylistR()[1]:yylistR()[2]
    
    # prepare data storage
    nyear   <- length(yylist)
    Estore  <- array(NA, dim =c(nyear,365))
    Pstore  <- array(NA, dim =c(nyear,365))
    Rstore  <- array(NA, dim =c(nyear,365))
    dWstore <- array(NA, dim =c(nyear,365))
    tstore  <- array(NA, dim =c(nyear,365))
    
    # read the data
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
    
    Estore[iyr,]  <- Etim[tlist,iarea]
    Pstore[iyr,]  <- Ptim[tlist,iarea]
    Rstore[iyr,]  <- Rtim[tlist,iarea]
    dWstore[iyr,] <- dWtim[tlist,iarea]
    tstore        <- timeaxis[tlist]
  }
  
  # do time averaging
  xdum    <- timeavg(Estore,tstore,navgdays)
  Estore  <- xdum$Xavg
  xdum    <- timeavg(Pstore,tstore,navgdays)
  Pstore  <- xdum$Xavg
  xdum    <- timeavg(Rstore,tstore,navgdays)
  Rstore  <- xdum$Xavg
  xdum    <- timeavg(dWstore,tstore,navgdays)
  dWstore <- xdum$Xavg
  tstore  <- xdum$Tavg
  nint    <- xdum$nint
  
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
  
  EYar  <- c(range1,range2[nint:1])
  Eline <- Xvar$mean
  
  Xvar <- XMeanStd(Pstore)
  
  if(rangetype == "std"){
    
    range1 <- Xvar$mean - Xvar$std
    range2 <- Xvar$mean + Xvar$std
    
  } else if (rangetype == "minmax" ){
    
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
    
  } else if (rangetype == "minmax" ){
    
    range1 <- Xvar$min
    range2 <- Xvar$max
  }
  
  dWYar <- c(range1,range2[nint:1])
  dWline <- Xvar$mean
  
  ir <- c(EYar,PYar,RYar,dWYar)
  ir_lim <- c(floor(min(ir)), ceiling(max(ir)))
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
 
  tit0 <- paste0("Mean annual cycle +/- ", rangetype)
  
  tit1 <- paste0("Region ",arealist[iarea]," lon:[",lonminar[iarea],"]-[",
                 lonmaxar[iarea],"], lat:[",latminar[iarea],"]-[",latmaxar[iarea],"], Years:[",
                 min(yylist),"-",max(yylist),"]; Avg interval: ",navgdays,"days")
  
  list(df = df, tit0 = tit0, tit1 = tit1, ir_lim = ir_lim, ir_brk = ir_brk)

})
  
  # output$regionPlot1 <- renderPlot({
  #   
  #   data <- plotData()
  #   
  #   ggplot(data$df, aes(x = tvar)) +
  #     # Geoms
  #     geom_line(aes(y = value, color = var), size = 1) +
  #     geom_ribbon(aes(ymin = min, ymax = max, fill = var), alpha = 0.2) +
  #     geom_hline(yintercept = 0) +
  #     # Scales 
  #     scale_y_continuous(limits = data$ir_lim, breaks = data$ir_brk) +
  #     scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  #     scale_color_manual(values = var_colors) +
  #     scale_fill_manual(values = var_colors) +   
  #     # Themes  
  #     theme_minimal() +
  #     theme(panel.grid.minor = element_blank(),
  #           legend.position = c(.95, .95),
  #           legend.justification = c("right", "top"),
  #           legend.box.just = "right",
  #           legend.margin = margin(6, 6, 6, 6)
  #     ) +
  #     # Labels/titles
  #     labs(x = "", y = "mm/day", color = "variable", fill = "variable") +
  #     ggtitle(data$tit0, subtitle = data$tit1) 
  #   
  # })
  
  
  
  output$regionPlot1 <- renderHighchart({

    data <- plotData()
    
    hchart(data$df, hcaes(x = tvar, y = value, group = var), type = "line") %>% 
      hc_add_series(data$df, hcaes(x = tvar, low = min, high = max, group = var), 
                    type = "arearange", fillOpacity = 0.2, showInLegend = F) %>%     
      hc_plotOptions(
          arearange = list(marker = list(enabled = FALSE)),
          series = list(marker = list(enabled = FALSE))) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = T) %>%
      hc_legend(align = "right", verticalAlign = "top", 
                layout = "vertical", x = 0, y = 50)  %>%
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(Month = '%b')) 

    
   
    # hchart(data$df, "line", hcaes(x = tvar, y = value, group = var)) %>%
    #   hc_chart(borderWidth = 0.1) %>% 
    #   hc_add_series(data$df, type = "arearange", lineWidth = 0.1,
    #                 hcaes(x = tvar, low = min, high = max, group = var),
    #                 fillOpacity = 0.5, showInLegend = T) %>%
    #   #hc_colors() %>%
    #   hc_add_theme(hc_theme_smpl()) %>%
    #   
    #   hc_exporting(enabled = T) %>%
    #   
    #   
      
                 
      #               fillOpacity = 0.2, showInLegend = T) %>%
      # hc_add_series(data$df, type = "line",
      #               hcaes(color = "scenario", group = "scenario")) %>%
      # 
      # hc_tooltip(crosshairs = TRUE,
      #            formatter = JS("function(){ return (' Scenario: ' + this.point.scenario +' <br> Year: ' + this.x + ' <br> Value: ' + this.y)}"),
      #            borderWidth = 2, 
      #            backgroundColor = "rgba(255,255,255,0.8)",
      #            borderColor = "black") %>%
      # hc_exporting(enabled = T) %>%
      # hc_xAxis(title = list(text = "Year"),
      #          plotLines = list(
      #            list(color = "#e0e0e0", width = 2, value = 1900),
      #            list(color = "#e0e0e0", width = 2, value = 1950),
      #            list(color = "#e0e0e0", width = 2, value = 2000),
      #            list(color = "#e0e0e0", width = 2, value = 2050),
      #            list(color = "#e0e0e0", width = 2, value = 2100))) %>%
      # hc_yAxis(tickPositions = c(-6, -4, -2, 0, 2, 4, 6, 8, 10, 12)) %>%
      # hc_plotOptions(marker = list(lineWidth = 1),
      #                series = list(area = list(states = list(hover = list(enabled = FALSE))))) %>%
      # hc_size(1000, 600)
    
    
    # hchart(data$df, "line", hcaes(x = tvar, y = value, group = var)) %>%
    #   hc_exporting(enabled = TRUE) %>% 
    #   hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
    #              shared = TRUE, borderWidth = 2) %>%
    #   hc_title(text="Time series plot of Inflation Rates for Economic Unions",align="center") %>%
    #   hc_subtitle(text="Data Source: IMF",align="center") %>%
    #   hc_add_theme(hc_theme_elementary()) #%>%
    #   #hc_yAxis(title = list(text = "mm/day"),
    #   #         showFirstLabel = FALSE,
    #   #         showLastLabel = FALSE,
    #   #         plotLines = list(color = "black", width = 2, value = 0))
    
  }) 
    
    

}


