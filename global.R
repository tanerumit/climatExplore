### LIBRARIES
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinydashboard)
library(shinyLP)
library(highcharter)

library(fontawesome)
library(DT)

library(leaflet)
library(tidyr)
library(readr)
library(dplyr)
library(curl) 
library(ggplot2)
library(data.table)
library(jstable)
library(parcoords)

### General options(max allowable size for uploads)
options(shiny.maxRequestSize = 30*1024^2)

### Sourcing
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#sourceDir("./modules/")


source("./R/read_timeseries.R")
source("./R/XMeanStd.R")
source("./R/timeavg.R")

Sys.setlocale("LC_ALL","English")

library(RNetCDF)
library(ncdf4)
library(maps)

