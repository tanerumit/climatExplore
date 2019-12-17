

################################################################################
############################# UI-SIDE ##########################################
################################################################################

source("global.R")

region_list <- c("AUS","AMZ", "SSA","CAM","WNA","CNA", "ENA","ALA","GRL",
                 "MED","NEU","WAF", "EAF", "SAF", "SAH", "SEA", "EAS", "SAS",
                 "CAS","TIB","NAS")

reg <- 1:21
names(reg) <- region_list

#names(reg) <- region_list

Tab1 <- tabPanel("About", icon = icon("home"),
                 jumbotron(header  = "Explore Regional Climate Data", 
                           content = "Analyze climate data accross different regions (under-development)",
                           button  = FALSE)
)

Tab2 <- tabPanel("Analyze", icon = icon("cog"),
                 fluidRow(
                   column(width = 4, 
                      wellPanel(
                          strong("Climate region"),
                          tags$style(".worldimg {
                          padding-left:2px; padding-right:2px; padding-top:1px; padding-bottom:3px
                          }"),
                          div(class="worldimg",img(src="land-regions.png", height="90%", width="100%")),
                          #br(),
                          selectInput("iarea", label="", choices = reg, selected = 10),
                          sliderInput("yylist", label = "Analysis period", min = 1979, max = 2010, value = c(1979, 2010), sep = "", step = 5, ticks = FALSE),
                          sliderInput("navgdays", label = "Averaging interval (days)", min = 1, max = 15, value = 7, step = 1, ticks = FALSE),
                          selectInput("rangetype", label = "Range type", choices = c("minmax", "std"), selected = "minmax")
                      ) # well panel close
                   ), # column close
                   column(width = 8, 
                      wellPanel(
                          climRegionsAnalyze_mod_UI("regionPlot1")
                      ) # wellPanel close
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


