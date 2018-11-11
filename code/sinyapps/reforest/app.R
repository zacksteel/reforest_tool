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




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reforestation Prioritization Tool"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      #    sliderInput("bins",
      #                "Number of bins:",
      #                min = 1,
      #                max = 50,
      #                value = 30)
        p(),
        actionButton("recalc", "New points")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map", height = 800, width = 800)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
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
  
  ## Read in data
  fire_per <- st_read("C:/Users/zlsteel/Documents/CaFire/FireBirds/GIS2017/Fires",
                      "sev_pers_wgs") %>%
    filter(AGENCY == "NPS") 
   
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
      # addMarkers(data = points()) %>%
      # addPolygons(data = fire_per,
      #              color = "black",
      #              fill = T,
      #              weight = 20,
      #              opacity = 0.5,
      #              label = fire_per$VB_ID)
       ## Add fire perimeters
        #%>%
       # addProviderTiles("Stamen.Watercolor")
   })
  
  observe({
    leafletProxy("map", data = fire_per) %>%
      addPolygons(color = "black",
                  fill = T,
                  weight = 20,
                  opacity = 0.5,
                  label = fire_per$VB_ID)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

