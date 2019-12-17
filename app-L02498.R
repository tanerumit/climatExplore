

################################################################################
############################# UI-SIDE ##########################################
################################################################################

source("global.R")

#sourceDir("C:/Users/taner/OneDrive - Stichting Deltares/_DELTARES/03 Tools/_DEV/SHINY-VIEWER/modules/")

region_list <- c("AUS","AMZ", "SSA","CAM","WNA","CNA", "ENA","ALA","GRL","MED","NEU","WAF", 
                 "EAF", "SAF", "SAH", "SEA", "EAS", "SAS","CAS","TIB","NAS")

reg <- 1:21

#names(reg) <- region_list

Tab1 <- tabPanel("About", icon = icon("calendar"),
                 jumbotron(header  = "ClimateExplore", 
                           content = "Climate Data Exploration Tool",
                           button  = FALSE)
)

Tab2 <- tabPanel("Analyze",
                 fluidRow(
                   column(width = 4,
                          selectInput('x', 'X', choices = c("manufacturer", "model", "year", "cyl", "class"),
                                      selected = "class"),
                          selectInput('y', 'Y', choices = c( "trans", "fl", "drv"), 
                                      selected = "drv")
                          
                          
                          #selectInput("regions", label = "regions", choices = reg, selected = 1)
                   ), # column close
                   column(width = 8,
                          #img(src="land-regions.png"),
                          plotOutput("outplot")
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

appServer <- function(input, output) {

  output$outplot <- renderPlot({
    ggplot(mpg, aes_string(x = input$x)) +
      geom_bar(aes_string(fill= input$y), position = position_stack(reverse = TRUE)) +
      coord_flip() + 
      theme(legend.position = "top")
  })
  
  # 
  # output$plot1 <- renderPlot({
  #   
  #   p <- hydroclimAnalysis(yylist = c(1979:2010), iarea = input$regions, 
  #                     navgdays = 7, rangetype = "minmax")
  #   
  #   print(p)
  #   
  #})
}


  

################################################################################
########################### SHINY-APP ##########################################
################################################################################

shinyApp(
  ui = appUI,
  server = appServer,
)

################################################################################


