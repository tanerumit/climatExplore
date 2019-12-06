

################################################################################
############################# UI-SIDE ##########################################
################################################################################

source("global.R")


Tab1 <- tabPanel("About", icon = icon("calendar"),
                 jumbotron(header  = "ClimateExplore", 
                           content = "Climate Data Exploration Tool",
                           button  = FALSE)
)

Tab2 <- tabPanel("Analyze",
                 fluidRow(
                   column(width = 3
                   ), # column close
                   column(width = 9
                   ), # column close
                 ) # fluidrow close
)

appUI <- navbarPage(
  title = "Results Viewer",  
  theme = shinytheme("cerulean"),
  Tab1,
  Tab2
) 

################################################################################
############################# SERVER-SIDE ######################################
################################################################################


# Define server logic required to draw a histogram
appServer <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
}


  

################################################################################
########################### SHINY-APP ##########################################
################################################################################

shinyApp(
  ui = appUI,
  server = appServer,
  onStart = NULL,
  enableBookmarking = NULL
)

################################################################################


