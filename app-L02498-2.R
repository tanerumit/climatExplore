

################################################################################
############################# UI-SIDE ##########################################
################################################################################

source("global.R")
source("R/cleanRegionCoords.R")

region_list <- c("AUS","AMZ", "SSA","CAM","WNA","CNA", "ENA","ALA","GRL",
                 "MED","NEU","WAF", "EAF", "SAF", "SAH", "SEA", "EAS", "SAS",
                 "CAS","TIB","NAS")

regColors <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e')

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
    column(4, 
      h2("Climate Data Explorer (Beta)"), h5("Explore natural climate variability in the selected region"),
      leafletOutput("Map"),
      selectInput("iarea", label="", choices = reg, selected = 10)
    ),
    column(8,
      fluidRow(
        column(4, sliderInput("yylist", label = "Analysis period", min = 1979, max = 2010, value = c(1979, 2010), sep = "", step = 5, ticks = FALSE)),
        column(4, sliderInput("navgdays", label = "Averaging interval (days)", min = 1, max = 15, value = 7, step = 1, ticks = FALSE)),
        column(4, selectInput("rangetype", label = "Range type", choices = c("minmax", "std"), selected = "minmax"))
      ), #fluidrow close
      climRegionsAnalyze_mod_UI("regionPlot1")
    ) #column close
  ) #fluidrow close
)
                 
                 

Tab3 <- tabPanel("MAP", icon = icon("cog"),
                 fluidRow(
                   #column(width = 4, 
                   #       leafletOutput("Map")
                   #) #column close
                ) # fluidrow close
) # tab close




appUI <- navbarPage(
  title = "Climate Explore",  
  theme = shinytheme("cerulean"),
  Tab1,
  Tab2,
  Tab3

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
  
  ####################### MAP COMPONENT ##############################

  
  output$Map <- renderLeaflet({
    leaflet(dataTemp, options = leafletOptions(dragging = TRUE, minZoom = 1, maxZoom = 3)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng=0, lat=57, zoom=1) %>%
    addPolygons(lat = ~lon, lng = ~lat, color = "#444444", weight = 1,
                  opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  label = ~ as.character(name),
                  labelOptions(permanet = TRUE,textsize = 15)
                  ) #, 
                  #smoothFactor = 0.5,
                  #opacity = 1.0, fillOpacity = 0.5,
                  #fillColor = ~brewer.pal(7,"Spectral"),
                  #highlightOptions = highlightOptions(color = "white", weight = 2,
                  #                                    bringToFront = TRUE))
                                                      
                  #fillColor = brewer.pal(7,"Spectral"), 
                  #fillOpacity = 0.2,
                  #weight = 1,
                  #stroke = TRUE,
                  #label = dataTemp$name,
                  #popup = paste("Region: ", dataTemp$name, "<br>"))
    
  })

}




################################################################################
########################### SHINY-APP ##########################################
################################################################################

shinyApp(
  ui = appUI,
  server = appServer,
)

################################################################################


