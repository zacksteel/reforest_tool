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
library(dplyr)
library(sp)
library(rgdal)

## Must adjust for application directory & leaflet expect lat long data
fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
        "Stanislaus", "Inyo", "Sequoia", "Sierra")
forest <- st_read("../../../data/Spatial", "Ca_NFBoundaries") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  filter(FORESTNAME %in% fn)

#unique(forest$FORESTNAME)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reforestation Prioritization Tool"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("Forest", "Step 1: Select area of interest",
                    # selected = "",
                    fn),
        checkboxGroupInput("Databases", "Step 2: Select data layers:",
                           choices = c(
                             "Fire", "WUI", "wilderness")
                           ),
        actionButton("recalc", "Execute")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map", height = 600, width = 600)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Reactive mapping object
  aoe <- reactive({
    filter(forest, FORESTNAME == input$Forest)
  })
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
  
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
   
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = aoe(),
                  color = "red",
                  fill = F,
                  opacity = 0.8,
                  weight = 5) %>%
      addPolygons(data = forest,
                  color = "black",
                  fillColor = "green",
                  fill = T,
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.2,
                  label = forest$FORESTNAME) %>%

      # setView(lng = aoe()$lng, lat = aoe()$lat, zoom = aoe()$map_zoom) %>%
      addProviderTiles(provider = "Esri.WorldShadedRelief")#"CartoDB.Positron")
   })
  
  observe({
    leafletProxy("map") %>%
      ## Zoom to selection
      flyToBounds(lng1 = as.numeric(st_bbox(aoe())$xmin),
                lat1 = as.numeric(st_bbox(aoe())$ymin),
                lng2 = as.numeric(st_bbox(aoe())$xmax),
                lat2 = as.numeric(st_bbox(aoe())$ymax))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

