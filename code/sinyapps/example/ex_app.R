library(shiny)
library(DT)
library(tidyverse)

faithful$observers <- c("Ryan", "Nistara", "Pamela", "Rich")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30),
       
       checkboxGroupInput("observers",
                          "Select your favorite observer",
                          choices = c("Ryan", "Nistara", "Pamela", "Rich"),
                          selected = "Nistara"
                          )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("histogram",
                 plotOutput("distPlot")),
        tabPanel("ggplot",
                 plotOutput("ggplot")
                 ),
        tabPanel("data table",
                 DT::dataTableOutput("data_table"))
      )
    )
  )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$ggplot <- renderPlot({ 
    
    faithful %>% filter(observers %in% input$observers) %>% 
      ggplot() + geom_point(aes(waiting, eruptions, color = observers))
    
    })
  
  output$data_table <- renderDataTable({ DT::datatable(faithful) })
  
}

shinyApp(ui, server)