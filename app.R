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
library(tidyverse)
library(raster)

## Must adjust for application directory & leaflet expect lat long data
fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
        "Stanislaus", "Inyo", "Sequoia", "Sierra")
forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
  st_buffer(0) %>% ## fixes problems with ring self-intersection
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") %>%
  filter(FORESTNAME %in% fn) 

## Bring in mortality layer
# mort <- st_read("data/Spatial", "ADSMort15") %>%
#   st_transform(crs = "+proj=longlat +datum=WGS84")
mort <- st_read("data/Spatial", "mort15_simple")
## Leaflet doesn't like named geometries, which st_write adds
names(st_geometry(mort)) <- NULL

## Bring in accessibility raster layer
#### scenb and forest layers are slightly mis-aligned. imprecise crs transformation somewhere along the way?
sb <- raster("data/Spatial/scenb_mask") 


# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Prioritization tool",
             # Application title
             titlePanel("Reforestation Prioritization Tool"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("Forest", h4("Step 1: Select area of interest"),
                             selected = "",
                             fn),
                 checkboxGroupInput("Display", tags$i("Display Layers:"),
                                    choices = c("Mortality (ADS)", "Inaccessible"),
                                    selected = c("Mortality (ADS)", "Inaccessible")),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 # checkboxGroupInput("Databases", "Step 2: Select data layers:",
                 #                    choices = c("Slope", "Fire Severity", "WUI", "CWD"),
                 #                    selected = c("Slope")),
 
                 h4("Step 2: Select reforestation need threshold:"),  
                 
                 sliderInput("Need", tags$tbody("Need Threshold (TPA mortality)"), 0, 100, 10,
                             width = '80%'),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 h4("Step 3: Select data layer weights:"),

                 sliderInput("WUI", "Wildland Urban Interface", 0, 1, 0.5,
                             width = '80%', step = .25),
                 sliderInput("HSZ2", "High-severity Fire (Zone 2)", 0, 1, 0.5,
                             width = '80%', step = .25),
                 sliderInput("CASPO", "Spotted Owl Habitat", 0, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("Fisher", "Fisher Habitat", 0, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("Rec", "Recreation Sites", 0, 1, 0,
                             width = '80%', step = .25),
                 h4("Step 4: Execute prioritization"),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 actionButton("Calc", "Execute")
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 leafletOutput("map", height = 600, width = 800)
               )
             )
    ),
    ## Plot data tab
    tabPanel("Stand data summary", "under construction"),
    tabPanel("BMP guide", "under construction")
  )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Reactive mapping objects
  ## Limit forest shp to selection forest
  aoi <- reactive({
    filter(forest, FORESTNAME == input$Forest)
  })
  
  ## Crop feasibility raster by aoi; much slower if you don't crop
  ra <- reactive({
    ## Conditional necessary to avoid crop error
    
    if(input$Forest == "") {NULL} else {
      crop(sb, filter(forest, FORESTNAME == input$Forest), snap = "in") %>%
        ## mask out areas beyond AOI; slower than crop so helps to do this step-wise
        mask(mask = filter(forest, FORESTNAME == input$Forest)) %>%
        cut(breaks = c(-0.5,0.5)) # effectively removes ones; highlighting inaccesible areas
    }
  })
  
  mortshow <- reactive({
    st_intersection(mort, aoi())
  })
  # priority <- reactive({
  #   if(input$Calc > 0 & input$Forest != "") {
  #     temp <- st_read("data/Outputs/Priorities_Spatial", input$Forest)
  #     names(st_geometry(temp)) <- NULL
  #     return(temp)
  #   } else (forest[0,])
  # })
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
      # addTiles() %>%
      ## raster seems to be added with a zIndex between 150 and 200, but can't change, moving everything else instead
      addMapPane("overlay", zIndex = 150) %>% #used to define layer order
      addMapPane("base", zIndex = 100) %>%
      addProviderTiles(provider = "Esri.WorldShadedRelief", 
                       options = pathOptions(pane = "base")) %>%#"CartoDB.Positron")
      addPolygons(data = aoi(),
                  color = "blue",
                  fill = F,
                  opacity = 0.8,
                  weight = 3) %>%
      addPolygons(data = forest,
                  color = "black",
                  fillColor = "green",
                  fill = T,
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.2,
                  label = forest$FORESTNAME,
                  options = pathOptions(pane = "overlay")) 
      
   })
  
  
  observe({
    proxy <- leafletProxy("map") 
    ## If a AOI is selected do some stuff
    if(input$Forest != "") {
      proxy %>%
        ## Zoom to selection
        flyToBounds(lng1 = as.numeric(st_bbox(aoi())$xmin),
                    lat1 = as.numeric(st_bbox(aoi())$ymin),
                    lng2 = as.numeric(st_bbox(aoi())$xmax),
                    lat2 = as.numeric(st_bbox(aoi())$ymax)) 
    }
    ## If Mortality layer is selected add that layer
    if(input$Forest != "" & ("Mortality (ADS)" %in% input$Display)) {
      proxy %>%
        addPolygons(data = mortshow(),
                  color = "transparent",
                  fill = T,
                  fillColor = c("transparent","red"), #heat.colors(5, alpha = NULL),
                  fillOpacity = 0.8,
                  options = pathOptions(pane = "overlay")) 
    }
    ## If Inaccessible mask is selection add that layer
    ## Set colors for accessibility mask
    if(input$Forest != "" & ("Inaccessible" %in% input$Display)) {
      pal <- colorNumeric(c("black"), values(ra()), na.color = "transparent")
      proxy %>%
        addRasterImage(x = ra(), colors = pal, opacity = 0.5, project = FALSE)
    }
      

      # addPolygons(data = priority(),
      #           opacity = 0,
      #           fill = T,
      #           fillColor = c("#CC9933", "#996600", "#993300"),
      #           fillOpacity = 0.8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

