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

library(RNetCDF)
library(ncdf4)
library(maps)

### General options(max allowable size for uploads)
options(shiny.maxRequestSize = 30*1024^2)
Sys.setlocale("LC_ALL","English")

source("./R/read_timeseries.R")
source("./R/XMeanStd.R")
source("./R/timeavg.R")
source("./R/climRegionsAnalyze_mod.R")





