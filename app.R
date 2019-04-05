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
library(colorspace)


## Must adjust for application directory & leaflet expect lat long data
fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
        "Stanislaus", "Inyo", "Sequoia", "Sierra")
forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
  st_buffer(0) %>% ## fixes problems with ring self-intersection
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  # st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") %>%
  filter(FORESTNAME %in% fn) 

## Bring in "need" layer
bloss <- raster("data/Spatial/biomassloss.tif")

## Bring in accessibility raster layer
#### scenb and forest layers are slightly mis-aligned. imprecise crs transformation somewhere along the way?
sb <- raster("data/Spatial/scenb.tif") 

## Bring in land class layers
rec <- raster("data/Spatial/RecAreas.tif")
wui <- raster("data/Spatial/WUI.tif")


# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Prioritization tool",
             # Application title
             titlePanel("Reforestation Prioritization Tool"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Forest", 
                             label = h4("Step 1: Select area of interest"),
                             selected = "",
                             choices = fn),
                 ## Move this display option to map widget
                 # checkboxGroupInput("Display", tags$i("Display Layers:"),
                 #                    choices = c("Biomass Loss (2012-16)", "Inaccessible"),
                 #                    selected = c("Biomass Loss (2012-16)")),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 # checkboxGroupInput("Databases", "Step 2: Select data layers:",
                 #                    choices = c("Slope", "Fire Severity", "WUI", "CWD"),
                 #                    selected = c("Slope")),
 
                 h4("Step 2: Select reforestation need threshold:"),  
                 
                 sliderInput("Need", tags$tbody("Need Threshold (Biomass loss)"), 10, 100, 50,
                             width = '80%', step = 10),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 h4("Step 3: Select data layer weights:"),

                 sliderInput("WUI", "Wildland Urban Interface", 0, 1, 0.5,
                             width = '80%', step = .25),
                 # sliderInput("HSZ2", "High-severity Fire (Zone 2)", 0, 1, 0.5,
                 #             width = '80%', step = .25),
                 # sliderInput("CASPO", "Spotted Owl Habitat", 0, 1, 0,
                 #             width = '80%', step = .25),
                 # sliderInput("Fisher", "Fisher Habitat", 0, 1, 0,
                 #             width = '80%', step = .25),
                 sliderInput("Rec", "Recreation Sites", 0, 1, 0,
                             width = '80%', step = .25),
                 h4("Step 4: Execute prioritization"),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 actionButton("Calc", "Execute"),
                 #### Trying to figure out downlaod
                 actionButton("dl", "Download")
                 # downloadButton("dl", "Download current map")
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
      ## Need to convert to spatial object for crop (but not for mask)
      tmp <- filter(forest, FORESTNAME == input$Forest) %>%
        as_Spatial()
      crop(sb, tmp, snap = "in") %>%
        ## mask out areas beyond AOI; slower than crop so helps to do this step-wise
        mask(mask = filter(forest, FORESTNAME == input$Forest)) %>%
        cut(breaks = c(-0.5,0.5)) # effectively removes ones; highlighting inaccesible areas
    }
  })
  
  mortshow <- reactive({
    ## Need to convert to spatial object for crop (but not for mask)
    tmp <- filter(forest, FORESTNAME == input$Forest) %>%
      as_Spatial()
    crop(bloss, tmp, snap = "in") %>%
      ## mask out areas beyond AOI; slower than crop so helps to do this step-wise
      mask(mask = filter(forest, FORESTNAME == input$Forest))
    # st_intersection(mort, aoi())
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
  
  ## Can't add sierra-wide rasters to full map, too big to render

  #### Struggling with how to save map on the screen. From here to ~line 238 prints what is generated in user_created_map(),
  #### From here: https://community.rstudio.com/t/solved-error-when-using-mapshot-with-shiny-leaflet/6765/7
  #### But that function doesn't replace proxy functions (i.e. you still need to run the observer(proxy...) code for the user to see the rasters)
  #### May just be able to make it redundent, code chuck for the user to see and another to render the saved map
  #### Also, not sure how working directory is interpreted for a remote user, may need to integrate downloadHander() to make this work
  map_reactive <- reactive({
    leaflet() %>%
      # addTiles(options = tileOptions(minZoom = 2, maxZoom = 18)) %>%
      addProviderTiles(provider = "Esri.WorldShadedRelief", group = "Relief") %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "Aerial Imagery") %>%
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topo") %>%
      addProviderTiles(provider = "Esri.WorldGrayCanvas", group = "Grey") %>%
      
      # add scale bar
      addMeasure(position = "topleft",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters",
                 activeColor = "#3D535D",
                 completedColor = "#7D4479") %>%
      
      addPolygons(data = aoi(),
                  color = "blue",
                  fill = F,
                  opacity = 0.8,
                  weight = 3,
                  group = "AOI") %>%
      addPolygons(data = forest,
                  color = "black",
                  fillColor = "green",
                  fill = T,
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.2,
                  label = forest$FORESTNAME,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Forests") %>%
      
      # add controls for basemaps and data
      addLayersControl(
        baseGroups = c("Grey", "Topo", "Relief", "Aerial Imagery"),
        overlayGroups = c("Forests", "AOI", "Inaccessible", "Biomass Loss"),
        position = c("topright"),
        options = layersControlOptions(collapsed = T))
  })
  
  output$map <- renderLeaflet({
    map_reactive()
  })
  
  user_created_map <- reactive({
    m = map_reactive() %>%
      setView(lng = input$map_center$lng, lat = input$map_center$lat, 
              zoom = input$map_zoom)
    if(input$Forest != "") {
      pal <- colorNumeric("Reds", domain = c(0,1), na.color = "transparent")
      m %>%
        addRasterImage(x = mortshow(), colors = pal,opacity = 0.5,
                       project = FALSE, group = "Biomass Loss")
    }
  })
  
  observeEvent(input$dl, {
    mapshot(user_created_map(), file=paste0(getwd(), '/exported_map.png'))
  })
  
  
  #### Code below saves default map to user-defined location
  #### from here: https://stackoverflow.com/questions/44259716/how-to-save-a-leaflet-map-in-shiny
  # map <- reactiveValues(dat = 0)
  # output$map <- renderLeaflet({
  #   # map$dat <- 
  #     leaflet() %>%
  #     ## raster seems to be added with a zIndex between 150 and 200, but can't change, moving everything else instead
  #     # addMapPane("overlay", zIndex = 150) %>% #used to define layer order
  #     # addMapPane("base", zIndex = 100) %>%
  #     addTiles(options = tileOptions(minZoom = 2, maxZoom = 18)) %>%
  #     addProviderTiles(provider = "Esri.WorldShadedRelief", group = "Relief") %>%
  #     #                  options = pathOptions(pane = "base")) %>%
  #     addProviderTiles(provider = "Esri.WorldImagery", group = "Aerial Imagery") %>%
  #     #                  options = pathOptions(pane = "base")) %>%
  #     addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topo") %>%
  #     addProviderTiles(provider = "Esri.WorldGrayCanvas", group = "Grey") %>%
  #     
  #     # add scale bar
  #     addMeasure(position = "topleft",
  #                primaryLengthUnit = "meters",
  #                primaryAreaUnit = "sqmeters",
  #                activeColor = "#3D535D",
  #                completedColor = "#7D4479") %>%
  #     
  #     addPolygons(data = aoi(),
  #                 color = "blue",
  #                 fill = F,
  #                 opacity = 0.8,
  #                 weight = 3,
  #                 group = "AOI") %>%
  #     addPolygons(data = forest,
  #                 color = "black",
  #                 fillColor = "green",
  #                 fill = T,
  #                 weight = 2,
  #                 opacity = 1,
  #                 fillOpacity = 0.2,
  #                 label = forest$FORESTNAME,
  #                 highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                     bringToFront = TRUE),
  #                 group = "Forests") %>%
  #                 # options = pathOptions(pane = "overlay")) %>%
  #     
  #     # add controls for basemaps and data
  #     addLayersControl(
  #       baseGroups = c("Grey", "Topo", "Relief", "Aerial Imagery"),
  #       overlayGroups = c("Forests", "AOI", "Inaccessible", "Biomass Loss"),
  #       position = c("topright"),
  #       options = layersControlOptions(collapsed = T))
  # 
  #  })
  
  ## Function to download current map on button click; when testing Run app external to RStudio or file naming wont work
  #### Currently this only downloads the defaul, doesn't adjust to user changes ####
  # output$dl <- downloadHandler(
  #   filename = "map.png",
  #   content = function(file = filename) {
  #     mapshot(map$dat, file = file)
  #   })
  
  
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
    if(input$Forest != "") {# & ("Biomass Loss (2012-16)" %in% input$Display)) {
      pal <- colorNumeric("Reds", domain = c(0,1), na.color = "transparent")
      proxy %>%
        addRasterImage(x = mortshow(), colors = pal,opacity = 0.5,
                       project = FALSE, group = "Biomass Loss")
                       # options = tileOptions( minZoom = 10)) ## It seems you can't do this for rasters currently, may fix in future releases...

    }
    ## If Inaccessible mask is selection add that layer
    ## Set colors for accessibility mask
    if(input$Forest != "") {# & ("Inaccessible" %in% input$Display)) {
      pal <- colorNumeric(c("black"), values(ra()), na.color = "transparent")
      proxy %>%
        addRasterImage(x = ra(), colors = pal, opacity = 0.5,
                       project = FALSE, group = "Inaccessible")
    }


      # addPolygons(data = priority(),
      #           opacity = 0,
      #           fill = T,
      #           fillColor = c("#CC9933", "#996600", "#993300"),
      #           fillOpacity = 0.8)

    ## It seems I need to re-run these layer controls here (at least how I currently have it coded)
    proxy %>%
      # add controls for basemaps and data
      addLayersControl(
        baseGroups = c("Topo", "Relief", "Aerial Imagery", "Grey"),
        overlayGroups = c("Forests", "AOI", "Inaccessible", "Biomass Loss"),
        position = c("topright"),
        options = layersControlOptions(collapsed = F))

  })
  
  
  ## When the user executes the prioritization
  observeEvent(input$Calc, {
    ## Only run if AOI has been selected
    if(input$Forest == "") {NULL} else {
      
      ## Limit the range of biomass loss considered based on user-defined threshold
      blmin <- input$Need/100
      minmask <- cut(bloss, breaks = c(-0.01,blmin,1)) - 1 ## returns a 0 and 1 raste
        
      ## Run raster calculation based on user-defined weights
      pr <- (bloss + wui*input$WUI + rec*input$Rec) * sb * minmask
      
      ## Scale to have a max of 1; may want to do after mask step below
      ## Max will be equal to the number of input layers adjusted for weights
      maxp <- 1 + input$WUI + input$Rec
      pr01 <- pr / maxp
      
      ## Limit to AOI (this seems to be the slow step so do it last)
      aoi_shp <- filter(forest, FORESTNAME == input$Forest) %>%
        as_Spatial()
      pr_aoi <- crop(pr01, aoi_shp, snap = "in") %>%
        ## mask out areas beyond AOI; slower than crop so helps to do this step-wise
        mask(mask = aoi_shp)
      ## Create a not treatable layer for AOI
      nt <- crop(sb, aoi_shp, snap = "in") %>%
        mask(mask = aoi_shp)
      
      
      ## Reclassify to three classes based on quantile thirds above the minimum threshold
      ## occasionally have problems of breaks not being unique. add a bit to upper quantiles
      q3 <- quantile(pr_aoi[pr_aoi > blmin], probs = c(0.33, 0.67)) + c(0.000, 0.001)
      
      breaks <- c(-0.01, blmin, as.numeric(q3), 1.00)
      pr3 <- cut(pr_aoi, breaks = breaks) %>%
        subs(data.frame(ID = c(1,2,3,4), Priority = c("No Need","Low", "Moderate", "High")))
      
      
      ##https://datacarpentry.org/r-raster-vector-geospatial/02-raster-plot/index.html
      ##https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf

      ## make a background raster
      aoi_r <- raster(extent(aoi_shp), res = res(pr3))
      #### Slow step
      aoi_r <- rasterize(aoi_shp, aoi_r)
      aoi_df <- as.data.frame(aoi_r, xy = T) %>%
        mutate(Priority = ifelse(!is.na(layer_OBJECTID), "Not Treatable", NA),
               Priority = factor(Priority, levels = c("High", "Moderate", "Low", "No Need", "Not Treatable"))) %>%
        filter(!is.na(Priority))

      ## make not treatable dataframe
      nt_df <- as.data.frame(nt, xy = T) %>%
        mutate(Priority = ifelse(scenb == 1, NA, "Not Treatable"),
               Priority = factor(Priority, levels = c("High", "Moderate", "Low", "No Need", "Not Treatable"))) %>%
        filter(!is.na(Priority))
      
      ## Set up dataframe from raster
      #### This is a slow step
      pr3_df <- as.data.frame(pr3, xy = T) %>%
        rename(Priority = Priority_Priority) %>%
        mutate(Priority = factor(Priority, levels = c("High", "Moderate", "Low", "No Need", "Not Treatable"))) %>%
        filter(!is.na(Priority))
      
      ## Set up palette and replace last level as mask
      pal <- sequential_hcl(5, palette = "Inferno")
      
      p <- ggplot() +
        geom_raster(data = aoi_df, aes(x = x, y = y, fill = Priority)) +
        geom_raster(data = pr3_df, aes(x = x, y = y, fill = Priority)) +
        geom_raster(data = nt_df, aes(x = x, y = y, fill = Priority)) +
        scale_fill_manual(values = c("High" = pal[1], "Moderate" = pal[2], "Low" = pal[3],
                                     "No Need" = "grey90", "Not Treatable" = "grey60"),
                          breaks = c("High", "Moderate", "Low", "No Need", "Not Treatable")) +
        theme_bw() +
        theme(axis.title = element_blank()) + 
        coord_quickmap()
      
      ## Prompt user to pick a directory to save to
      wd <- choose.dir(default = "Computer", caption = "Select output directory")
      setwd(wd)
      
      ## Create an output directory to save into
      new.dir <- "ReforestPriority"
      ## If one alread exists, add a number until it doesn't
      x <- 1
      repeat{
        if(dir.exists(new.dir)) {
        new.dir <- paste0("ReforestPriority",x)
        x <- x + 1
        } else
          break
        }
      dir.create(new.dir)
      
      ## Add products to directory
      ggsave(paste0(new.dir,"/scratch_priority.png"), plot = p)
      # png(paste0(new.dir,"/scratch_priority.png"))
      # plot(pr_aoi)
      # dev.off()
      
      writeRaster(pr_aoi, paste0(new.dir,"/priority.tif"))
    }
     
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

