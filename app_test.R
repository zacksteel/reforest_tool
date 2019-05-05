#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(leaflet)
library(mapview)
library(tidyverse)
library(rgdal)
library(raster)
## This extra bit seems necessary for saving from web
## Not needed locally so can comment out to save time when building
webshot::install_phantomjs()

## Define forest/aoi options and read in shape file
forest <- st_read("app_data", "SN_NFs")
district <- st_read("app_data", "SN_districts")

aoi_id <- c("", as.character(forest$FORESTNAME), as.character(district$aoi)) %>%
  sort()

## Set up user interface
ui <- fluidPage(
  ## Change font size everywhere
  tags$head(tags$style(HTML("
        .selectize-input {
          font-size: 70%;
        }
        "))),
  tabsetPanel(
    tabPanel("Prioritization tool",
             # Application title
             titlePanel("Reforestation Prioritization Tool"),
             
             # Sidebar with user-input widgets
             # sidebarLayout(
             #   sidebarPanel(
             ## Moving sidebar to top with multiple columns
             fluidRow(
               column(3,
                      selectInput(inputId = "Forest", 
                                  label = h4("Step 1: Select area of interest"),
                                  selected = "",
                                  choices = aoi_id),
                      ## Horrizontal line
                      tags$hr(),
                      
                      h4("Step 2: Select reforestation need threshold:"),  
                      
                      sliderInput("Need", tags$tbody("Need Threshold (% Biomass loss)"), 
                                  10, 100, 50,
                                  width = '80%', step = 10)
               ),
               h4("Step 3: Select data layer weights:"),
               column(3,
                      
                      
                      sliderInput("cwd", "Climatic Water Deficit", 0, 1, 0,
                                  width = '80%', step = .25),
                      sliderInput("HSZ2", "High-severity Fire (Zone 2)", 0, 1, 0,
                                  width = '80%', step = .25),
                      sliderInput("WUI", "Wildland Urban Interface", 0, 1, 0,
                                  width = '80%', step = .25)
               ),
               column(3,
                      
                      sliderInput("Rec", "Recreation Areas", 0, 1, 0,
                                  width = '80%', step = .25),
                      sliderInput("CASPO", "Spotted Owl PACs", -1, 1, 0,
                                  width = '80%', step = .25),
                      sliderInput("Fisher", "Fisher Core Habitat", -1, 1, 0,
                                  width = '80%', step = .25)
               ),
               
               column(3,
                      h4("Step 4: Run prioritization"),
                      
                      actionButton("Calc", "Calculate"),
                      
                      tags$hr(),
                      h4("Step 5: Download map and/or data"),
                      
                      ## Buttons for downloading current map and tif
                      #### Maybe add one or combine for generating a short "report"
                      downloadButton("dl", "Download Map Image")
                      # downloadButton("dl_tif", "Download Priority GeoTiff")
                      
                      
               ))
             ),

             leafletOutput("map")#, height = 600, width = 800)
    
  )
)

# Define server code
server <- function(input, output) {
  
  
  ## Set up the default map function
  map_reactive <- reactive({
    m <- leaflet() %>%
      # addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topo") %>%
      addPolygons(data = forest,
                       color = "black",
                       fillColor = "green",
                       fill = F,
                       weight = 2,
                       opacity = 1,
                       fillOpacity = 0.2,
                       label = forest$FORESTNAME,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = TRUE),
                       group = "Forests") #%>%
        # addPolygons(data = district,
        #             color = "black",
        #             fillColor = "green",
        #             fill = T,
        #             weight = 1,
        #             fillOpacity = 0.05,
        #             label = district$aoi,
        #             highlightOptions = highlightOptions(color = "white", weight = 2,
        #                                                 bringToFront = TRUE),
        #             group = "Forests")
  })
  
  ## Run default map function
  output$map <- renderLeaflet({
    map_reactive()
  })
  
  ## Run a parallel map when saving. If using leafletProxy, duplicate funtionality here. Otherwise just add current view.
  user_created_map <- reactive({
    map_reactive() %>%
      setView(lng = input$map_center$lng, lat = input$map_center$lat,
              zoom = input$map_zoom)
  })
  
  # observeEvent(input$dl, {
  #   priority$cur_map <- map_reactive() %>%
  #     setView(lng = input$map_center$lng, lat = input$map_center$lat,
  #             zoom = input$map_zoom)
  #   # output$dl <- downloadHandler(
  #   #   filename = "map.png",
  #   #   content = function(file = filename) {
  #   #     # mapshot(user_created_map(), file = file)
  #   #     mapshot(cur_map, file = file)
  #     # })
  # })
  
  #### Using this returns an html of the full app rather than a png of the map as expected
  # observeEvent(input$dl, {
  #   m <- map_reactive()
  #   mapshot(x = m, file='exported_map.png')#, url='exported_map.html')
  # })
  
  ## Save map and geotiff when user requests it
  output$dl <- downloadHandler(
    filename = "map.png",
    content = function(file) {
      mapshot(user_created_map(), file = file)
      # mapshot(priority$cur_map, file = file)
    })
  
  #### Currently crashes the site if I haven't run the calculation yet, need to fix
  #### Alternatively, would it be possible to have the button only show up after the calculation?
  # if(is.null(priority$raster)) {
  #   output$dl_tif <- showNotification("This is a notification.")
  # } else{
  # output$dl_tif <- downloadHandler(
  #   filename = "prioritization.tif",
  #   content = function(file = filename) {
  #     writeRaster(priority$raster, file = file)
  #   })
  
}
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)

