

################################################################################
############################# UI-SIDE ##########################################
################################################################################

source("global.R")

region_list <- c("AUS","AMZ", "SSA","CAM","WNA","CNA", "ENA","ALA","GRL","MED","NEU","WAF", 
                 "EAF", "SAF", "SAH", "SEA", "EAS", "SAS","CAS","TIB","NAS")

reg <- 1:21
names(reg) <- region_list

#names(reg) <- region_list

Tab1 <- tabPanel("About", icon = icon("calendar"),
                 jumbotron(header  = "Explore Regional Climate Data", 
                           content = "Work-in-progress tool to explore climate data accross different regions",
                           button  = FALSE)
)

Tab2 <- tabPanel("Analyze",
                 fluidRow(
                   column(width = 5,
                          strong("Choose region"),
                          img(src="land-regions.png", height="125%", width="90%", align="left"),
                          br(),
                          selectInput("iarea", label="", choices = reg, selected = 10),
                          sliderInput("yylist", label = "Choose analysis period", min = 1979, max = 2010, value = c(1979, 2010), sep = "", step = 5, ticks = FALSE),
                          sliderInput("navgdays", label = "Choose averaging interval", min = 1, max = 15, value = 7, step = 1, ticks = FALSE),
                          selectInput("rangetype", label = "Choose data range Type", choices = c("minmax", "std"), selected = "minmax"),
                   ), # column close
                   column(width = 7,
                          climRegionsAnalyze_mod_UI("regionPlot1")
                   ), # column close
                 ) # fluidrow close
)

appUI <- navbarPage(
  title = "Climate Explore",  
  theme = shinytheme("cerulean"),
  Tab1,
  Tab2
) 

################################################################################
############################# SERVER-SIDE ######################################
################################################################################

appServer <- function(input, output, session) {

  session$onSessionEnded(stopApp)
  
  vars <- reactiveValues()
  vars$yylistR   <- reactive(input$yylist)
  vars$iareaR     <- reactive(input$iarea)
  vars$navgdaysR  <- reactive(input$navgdays)
  vars$rangetypeR <- reactive(input$rangetype)
  
  callModule(climRegionsAnalyze_mod, "regionPlot1", 
             yylistR    = vars$yylistR, 
             iareaR     = vars$iareaR,
             navgdaysR  = vars$navgdaysR,
             rangetypeR = vars$rangetypeR)
}




################################################################################
########################### SHINY-APP ##########################################
################################################################################

shinyApp(
  ui = appUI,
  server = appServer,
)

################################################################################


