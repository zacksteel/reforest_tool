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

## Must adjust for application directory & leaflet expect lat long data
fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
        "Stanislaus", "Inyo", "Sequoia", "Sierra")
forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
  st_buffer(0) %>% ## fixes problems with ring self-intersection
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  filter(FORESTNAME %in% fn) 

## Bring in mortality layer
# mort <- st_read("data/Spatial", "ADSMort15") %>%
#   st_transform(crs = "+proj=longlat +datum=WGS84")
# # mort <- st_read("data/Spatial", "mort15_simple") 
# ## Leaflet doesn't like named geometries, which st_write adds
# names(st_geometry(mort)) <- NULL


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
                             "Mortality (ADS)", "Slope", "Fire Severity", "WUI", "CWD"),
                           selected = c("Mortality (ADS)", "Slope")
                           ),
        h5(strong("Step 3: Execute prioritization:")),
        actionButton("Calc", "Execute")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map", height = 600, width = 600)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Reactive mapping objects
  aoi <- reactive({
    filter(forest, FORESTNAME == input$Forest)
  })
  # mortshow <- reactive({
  #   if("Mortality (ADS)" %in% input$Databases) {st_intersection(mort, aoi())} else
  #   {mort[0,]}
  #   # st_intersection(mort, aoi())
  # })
  priority <- reactive({
    if(input$Calc > 0 & input$Forest != "") {
      temp <- st_read("data/Outputs/Priorities_Spatial", input$Forest)
      names(st_geometry(temp)) <- NULL
      return(temp)
    } else (forest[0,])
  })
  # priority <- reactive({
  #   temp <- st_read("data/Outputs/Priorities_Spatial", "Stanislaus") %>%
  #     dplyr::select(priority)
  #   names(st_geometry(temp)) <- NULL
  #   return(temp)
  # })
   
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
      addPolygons(data = aoi(),
                  color = "blue",
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
      # addPolygons(data = mortshow(),
      #             color = "grey",
      #             opacity = 1,
      #             weight = 0.5,
      #             fill = T,
      #             fillColor = heat.colors(5, alpha = NULL),
      #             fillOpacity = 0.8,
      #             label = mortshow()$TPA) %>%
      addProviderTiles(provider = "Esri.WorldShadedRelief")#"CartoDB.Positron")
   })
  
  
  observe({
    leafletProxy("map") %>%
      ## Zoom to selection
      flyToBounds(lng1 = as.numeric(st_bbox(aoi())$xmin),
                lat1 = as.numeric(st_bbox(aoi())$ymin),
                lng2 = as.numeric(st_bbox(aoi())$xmax),
                lat2 = as.numeric(st_bbox(aoi())$ymax)) %>%
      addPolygons(data = priority(),
                opacity = 0,
                fill = T,
                fillColor = c("#CC9933", "#996600", "#993300"),
                fillOpacity = 0.8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

