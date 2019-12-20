### LIBRARIES
library(shiny)
library(leaflet)
library(tidyr)
library(readr)
library(dplyr)
library(highcharter)
library(ggplot2)
library(RNetCDF)
library(ncdf4)
library(maps)
library(sp)
library(shinyLP)
library(shinythemes)

# library(shinyBS)

# library(shinydashboard)
# 
# library(highcharter)
# 
# library(fontawesome)
# library(DT)
# 
# library(leaflet)
# library(tidyr)
# library(readr)
# library(dplyr)
# library(curl) 
# 
# library(data.table)
# library(jstable)
# library(parcoords)
# 


### General options(max allowable size for uploads)
options(shiny.maxRequestSize = 30*1024^2)
Sys.setlocale("LC_ALL","English")

source("./R/read_timeseries.R")
source("./R/XMeanStd.R")
source("./R/timeavg.R")
source("./R/climRegionsAnalyze_mod.R")

dataTemp <- readRDS("./data/climRegionsCoords.Rds")



