

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

Tab3 <- tabPanel("MAP", icon = icon("cog"),
                 fluidRow(
                   column(width = 4, 
                          leafletOutput("Map")
                   ) #column close
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
#south -
#west -

data <- list(
  beam1 = data.frame(lat = c(-115,-125, -125, -115),
                     lon = c(32, 32, 45,45)),
  beam2 =     data.frame(lat = c(-100, -111, -111, -100),
                         lon = c(42, 42, 50,50))
)





# x_coord <- c(105.000, 168.022, 168.022, 105.000) #longitude
# y_coord <- c(60.000,  60.000,  72.554, 72.554) # latitude
# 
# 
# xym <- cbind(x_coord, y_coord)
# 
# p = Polygon(xym)
# ps = Polygons(list(p),1)
# sps = SpatialPolygons(list(ps))
# data = data.frame(f=1)
# spdf = SpatialPolygonsDataFrame(sps,data)



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
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
      setView(
        lng=0, lat=57, zoom=1) %>%
        addPolygons(dataTemp[,"lat"],
                  dataTemp[,"lon"],
                  color = regColors, weight = 1,
                  popup = paste("Region: ", dataTemp$name, "<br>"))
    
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


