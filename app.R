
## Setup ####

library(shiny)
library(shinyBS) 
library(sf)
library(leaflet)
library(mapview)
library(tidyverse)
library(rgdal)
library(raster)
library(shinyjs)
library(shinycssloaders)


## This extra bit seems necessary for saving from web
## Not needed locally so can comment out to save time when building
webshot::install_phantomjs()

## Define forest/aoi options and read in shape file
forest <- st_read("app_data", "SN_NFs")
district <- st_read("app_data", "SN_districts")
## Simplified shapefile for faster display; still use full for calculations
dist_s <- st_read("app_data", "SN_districts_simple")

aoi_id <- c("", as.character(forest$FORESTNAME), as.character(district$aoi)) %>%
  sort()

## Read in stand data
stand <- read.csv("app_data/stand_prepped.csv")
stand_aois <- st_read("app_data", "stand_aois")
aoi_st <- c("All", sort(as.character(stand_aois$aoi)), "BLM")
regen <- read.csv("app_data/regen_prepped.csv")
## And define some stand metric associations
metrics <- data.frame(label = c("Stand Density","Regeneration","Basal Area","Canopy Cover","Mean DBH","Max DBH",
                                "Stand Density","Basal Area","Canopy Cover","Mean DBH","Max DBH"), 
                      metric = c("tpha_live", "spha","ba_live", "live_cc", "dbh_mn_live", "dbh_max_live", 
                                 "tpha_dead", "ba_dead", "dead_cc", "dbh_mn_dead", "dbh_max_dead"),
                      unit = c("trees/ha", "Stems/ha", "sq m", "%", "cm", "cm", 
                               "trees/ha", "sq m", "%", "cm", "cm"),
                      status = c(rep("live",6),rep("dead",5)))

## User interface ####
ui <- fluidPage(
  ## initializes shinyjs e.g. for disabling buttons when not ready
  useShinyjs(),
  ## Change font size everywhere
  ## Display properties for MapLayers panel
  tags$style(
    HTML("
      .selectize-input { font-size: 100%; }
      #MapLayers {opacity: 0.65;
                  transition: opacity 500ms; }
      #MapLayers:hover {opacity: 0.95; }
         ")),
  
  tabsetPanel(id = "tabs",
    tabPanel("About",
             includeHTML("about.html")),
             # includeMarkdown("about.Rmd")),
    
    #### Prioritation Tool - UI ####
    tabPanel("Prioritization tool", 
                 
             # Application title
             titlePanel("Post-Mortality Reforestation Prioritization Tool"),
               column(4,
                      sidebarPanel(
                        width = 12, style = "overflow-y:scroll; max-height: 800px",
                        selectInput(inputId = "Forest", 
                                    label = h4(tags$b("Step 1: Select area of interest"), 
                                               style = "font-size:110%; color:darkblue"),
                                               selected = "",
                                               choices = aoi_id),
                        bsPopover("Forest", title = NULL, 
                                  content = "Prioritization and map layer display will be limited to the area selected",
                                  placement = "right"),
                        
                        ## Horrizontal line
                        tags$hr(),
                                          
                        h4(tags$b("Step 2: Select reforestation need threshold:"), 
                           style = "font-size:110%; color:darkblue"),
                        sliderInput("Need", tags$p("Mortality Threshold (% loss)", style = "font-size:90%;"),
                                    10, 100, 50,
                                    width = '80%', step = 10),
                        bsPopover("Need", title = NULL,
                                  "Areas with less biomass loss than the selected threshold will be excluded from prioritization."),
                                   
                        tags$hr(),
                        
                        h4(id = "Step3", tags$b("Step 3: Select mechanical constraint scenario:"),
                           style = "font-size:110%; color:darkblue"),
                        radioButtons("MechScen", label = NULL, choices = c("Moderate constraints",
                                                                           "Fewer constraints")),
                        bsPopover(id = "MechScen", title = NULL,
                                   "The 'Few constraints' scenario extends access further from existing roads and to steaper slopes. <br/>See Technical Info for more details"),
                        
                        ## Horrizontal line
                        tags$hr(),
                                   
                        h4(id = "Step4", tags$b("Step 4: Select data layer weights:"), 
                           style = "font-size:110%; color:darkblue"),
                        bsPopover("Step4", title = NULL,
                                  "Layers assigned a negative value will decrease piority. <br/> Layers assigned a positive value will increase priority. <br/> Layers assigned a zero value will not affect prioritization."),
                        sliderInput("cwd", tags$p("Drought Risk (CWD)", style = "font-size:90%;"), 
                                    -1, 0, 0, width = '80%', step = .25),
                        bsPopover("cwd", title = NULL,
                                  "Areas of higher drought risk will decrease priority. CWD = climate water deficit (1981-2010 average)."),
                        sliderInput("HSZ2", tags$p("High-severity Fire Core", style = "font-size:90%;"), 
                                    0, 1, 0, width = '80%', step = .25),
                        bsPopover("HSZ2", title = NULL,
                                  "Areas within high-severity cores will increase priority. Cores are those areas more than 650ft (200m) from seed trees within high-severity wildfire patches. Fires from 2012-2017 are included."),
                        sliderInput("WUI", tags$p("Wildland-Urban Interface", style = "font-size:90%;"), 
                                    0, 1, 0, width = '80%', step = .25),
                        bsPopover("WUI", title = NULL,
                                  "Areas within the wildland-urban interface will increase priority."),
                        sliderInput("Rec", tags$p("Recreation Areas", style = "font-size:90%;"), 
                                    0, 1, 0, width = '80%', step = .25),
                        bsPopover("Rec", title = NULL,
                                  "Recreation areas will increase priority."),
                        sliderInput("CASPO", tags$p("Spotted Owl PACs", style = "font-size:90%;"), 
                                    -1, 1, 0, width = '80%', step = .25),
                        bsPopover("CASPO", title = NULL,
                                  "California Spotted Owl PACs can increase or decrease priority depending on management objectives."),
                        sliderInput("Fisher", tags$p("Fisher Core Habitat", style = "font-size:90%;"), 
                                    -1, 1, 0, width = '80%', step = .25),
                        bsPopover("Fisher", title = NULL,
                                  "Pacific fisher core habitats can increase or decrease priority depending on management objectives."),
                                          
                        ## Horrizontal line
                        tags$hr(),
                                   
                        h4(tags$b("Step 5: Run prioritization"), 
                           style = "font-size:110%; color:darkblue"),
                        actionButton("Calc", "Calculate"),
                        bsPopover("Calc", title = NULL,
                                  "Generates a 3-level priority layer using biomass and weighted data layers. Non-forest service lands, areas with mechanical constraints, and those with biomass loss below the designated need threshold are excluded. This will likely take a few seconds to run."),
                                    
                        tags$hr(),
                                   
                        h4(tags$b("Step 6: Download map and/or data"), 
                           style = "font-size:110%; color:darkblue"),
                                    
                        ## Buttons for downloading current map and tif
                        downloadButton("dl", "Download Map Image"),
                        bsPopover("dl", title = NULL,
                                  "Generates and downloads a map using the current view."),
                        downloadButton("dl_tif", "Download Priority Raster"),
                        bsPopover("dl_tif", title = NULL,
                                  "Downloads a priority raster layer for further analysis.")
                        ),
                      
                      tags$hr()),
             
             column(8,
                    mainPanel(width = 12,
                              leafletOutput("map", width = "100%", height = 500),
                              
                              absolutePanel(id = "MapLayers", class = "panel panel-default", fixed = F,
                                            draggable = T, top = 10, left = 20, right = "auto", bottom = "auto",
                                            width = 220, height = "auto", 
                                            checkboxGroupInput("Display", label = tags$b("Select display Layers:"),
                                                               choices = c("Area of Interest", 
                                                                           "Non-USFS Land",
                                                                           "Mechanical Constraints",
                                                                           "Forest Biomass Loss (2012-16)", 
                                                                           "High-severity Fire Core",
                                                                           "Drought Risk (CWD)",
                                                                           "Recreation Areas",
                                                                           "Wildland-Urban Interface",
                                                                           "Spotted Owl PACs",
                                                                           "Fisher Core Habitat"),
                                                               selected = c("Area of Interest"))
                                            ),
                              plotOutput(outputId = "vegPlot", height = "350px")
                              )
                    )
             ),
    
    #### Stand Summary - UI ####
    tabPanel("Stand summary tool", 
             titlePanel("Post-drought Stand Condition Summary Tool"),
             fluidRow(
               column(4,
                      sidebarPanel(width = 12, #using sidebarPanel for formating
                        selectInput(inputId = "Forest_st", 
                                    label = h4(tags$b("Step 1: Select area of interest", 
                                                      style = "font-size:90%; color:darkblue")),
                                    selected = "All",
                                    choices = aoi_st),
                        bsPopover("Forest_st", title = NULL, 
                                  content = "Summary statistics and map layer display will be limited to the area selected",
                                  placement = "right"),
                        selectInput(inputId = "metric_st",
                                    label = h4(tags$b("Step 2: Select stand metric to summarize", 
                                                      style = "font-size:90%; color:darkblue")),
                                    selected = "",
                                    choices = metrics$label),
                        plotOutput(outputId = "boxPlot", height = "300px"),
                        tags$br(),
                        h4(id = "info", tags$b("More info"), 
                           style = "font-size:80%; color:blue"),
                        bsPopover("info", title = NULL, 
                                  "This tool summarizes forestry plot data collected in 2016 and 2017 following the 2012-2016 drought. Stand structure and condition statistics for individual plots can be viewed by clicking on a marker in the map. Summary statistics for plots within a given area can be viewed using the boxplots above. Half of the surveyed plots were wihtin pre-drought fuels reduction treatments.")
                      )),
               column(8,
                      leafletOutput("map2", height = "500px"),
                      fixedRow(
                        column(6,
                               h4(tags$b("Treated Example")),
                               shiny::img(src='155157_2017_S.jpg',
                                          align = "left", width = "100%")),
                        column(6,
                               h4(tags$b("Untreated Example")),
                               shiny::img(src='155168_2017_S.jpg',
                                          align = "left", width = "100%"))
                      )

             )
             )),
    tabPanel("BMP guide",
             includeHTML("bmp.html")),
             # includeMarkdown("bmp.Rmd")),
    tabPanel("Technical Info",
             includeMarkdown("tech_info.Rmd"))
  )
)




#### Server code ####
server <- function(input, output, session) {
  
  ## Reactive mapping objects
  ## Call up rasters as needed (alternatively, could read in all when AOI is assigned; would shift around processing)
  ## Limit forest shp to selection forest or district
  ## Only run if user wants it displayed to save runtime
  
  ## Calculate button disabled until AOI is selected
  observe({
    shinyjs::disable("Calc")
    shinyjs::hide("MapLayers")
    })
  
  aoi <- reactive({
    if(input$Forest %in% as.character(forest$FORESTNAME))
      {
      ## Also enable Calculate button
      shinyjs::enable("Calc")
      shinyjs::show("MapLayers")
      filter(forest, FORESTNAME == input$Forest)
      } else 
        {
        if(input$Forest %in% as.character(district$aoi))
          ## Also enable Calculate button
          shinyjs::enable("Calc")
          shinyjs::show("MapLayers")
          filter(district, aoi == input$Forest)
        }
  })

  ## NF Rasters ####
  ## Select rasters specific to the AOI
  fs <- reactive({
    ## Read in study area raster
    raster(paste0("app_data/NF_Limits/fs_",aoi()$FORESTNAME, ".tif"))
  })
  ## non-forest service land layer
  nfs <- reactive({
    raster(paste0("app_data/NF_Limits/nfs_",aoi()$FORESTNAME, ".tif"))
  })
  
  ra <- reactive({
    ## Read in AOI national forest-specific raster
    if(input$MechScen == "Moderate constraints") {
      raster(paste0("app_data/NF_Limits/sb_",aoi()$FORESTNAME, ".tif")) %>%
        cut(breaks = c(-0.5,0.5)) # effectively removes ones; highlighting inaccesible areas
    } else
      if(input$MechScen == "Fewer constraints") {
        raster(paste0("app_data/NF_Limits/sd_",aoi()$FORESTNAME, ".tif")) %>%
          cut(breaks = c(-0.5,0.5))
      }
  })
  
  mortshow <- reactive({
    # bloss <- raster(paste0("app_data/NF_Limits/bloss_", aoi()$FORESTNAME, ".tif"))
    bloss <- raster(paste0("app_data/NF_Limits/intloss_", aoi()$FORESTNAME, ".tif")) #integrated version
  })
  
  rec <- reactive({
      raster(paste0("app_data/NF_Limits/rec_", aoi()$FORESTNAME, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
  })
    
  wui <- reactive({
      raster(paste0("app_data/NF_Limits/wui_", aoi()$FORESTNAME, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
  })
  
  cwd <- reactive({
      raster(paste0("app_data/NF_Limits/cwd_", aoi()$FORESTNAME, ".tif"))
  })
  
  hs <- reactive({
      raster(paste0("app_data/NF_Limits/hs_", aoi()$FORESTNAME, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros; highlighting zone 2
  })
  
  spow <- reactive({
      raster(paste0("app_data/NF_Limits/spow_", aoi()$FORESTNAME, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
  })
  
  fisher <- reactive({
      raster(paste0("app_data/NF_Limits/fisher_", aoi()$FORESTNAME, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
  })
  
  cveg <- raster("app_data/evt12.grd")

  
  ## Main Map ####

  map_reactive <- reactive({
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      ## puts zoom control at topright
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
      
      # Base Groups
      #### For some reason if I change the basemap, I can no longer display the AOI-dependent layers. 
      #### Probably something to do with how things are rendered. Trouble shoot later
      
      # addProviderTiles(provider = "Esri.WorldShadedRelief", group = "Relief") %>%
      # addProviderTiles(provider = "Esri.WorldImagery", group = "Aerial Imagery") %>%
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topo", options(zIndex = 0)) 

    # Overlay Groups
    ## Add layers if user-selected
    ## remove forest layer when zooming to aoi for now; makes saving crash
    if(input$Forest == "") {
        m <- addPolygons(m, data = dist_s,
                    color = "black",
                    fillColor = "green",
                    fill = T,
                    weight = 2,
                    opacity = 1,
                    fillOpacity = 0.2,
                    label = dist_s$aoi,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    group = "Forests")
    }
    
    if("Area of Interest" %in% input$Display & input$Forest != "") {
      val <- input$Forest
      m <- addPolygons(m,
                  data = aoi(),
                  color = "blue",
                  fill = F,
                  opacity = 0.8,
                  weight = 2,
                  group = "AOI") %>%
        ## Zoom to selection
        #### Can we revise this so that this only happens when the AOI changes?
        fitBounds(lng1 = as.numeric(st_bbox(aoi())$xmin),
                  lat1 = as.numeric(st_bbox(aoi())$ymin),
                  lng2 = as.numeric(st_bbox(aoi())$xmax),
                  lat2 = as.numeric(st_bbox(aoi())$ymax)) %>%
        addLegend(position = "bottomright", 
                  colors = "blue",
                  opacity = 0.8,
                  labels = "Area of Interest")
    }
    
    ## If Area Considered is selected, add that layer
    # if(input$Forest != "" & ("USFS Land" %in% input$Display)) {
    #   
    #   m <- m %>%
    #     addRasterImage(x = fs(), colors = c("white", "green"), 
    #                    opacity = 0.5,
    #                    project = FALSE) %>%
    #     addLegend(position = "bottomright", colors = c("white", "green"),
    #               labels = c("No", "Yes"),
    #               title = "USFS Land")
    # }
    if(input$Forest != "" & ("Non-USFS Land" %in% input$Display)) {
      pal <- colorNumeric(c("black"), values(nfs()), na.color = "transparent")
      m <- m %>%
        addRasterImage(x = nfs(), colors = pal, 
                       opacity = 0.4,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "black",
                  opacity = 0.5,
                  # labels = c("No", "Yes"),
                  labels = "Non-USFS Land")
    }
    
    ## If Inaccessible mask is selection add that layer
    if(input$Forest != "" & ("Mechanical Constraints" %in% input$Display)) {
      pal <- colorNumeric(c("black"), values(ra()), na.color = "transparent")
      m <- m %>%
        addRasterImage(x = ra(), colors = pal, opacity = 0.4,
                       project = FALSE, group = "Mechanical Constraints") %>%
        addLegend(position = "bottomright", 
                  colors = "black",
                  opacity = 0.5,
                  labels = "Mechanical Constraints")
    }
    
    ## SPOW PACs
    if(input$Forest != "" & ("Spotted Owl PACs" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = spow(), colors = "darkcyan", opacity = 0.4,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "darkcyan",
                  opacity = 0.5,
                  labels = "Spotted Owl PACs")
    }
    
    ## Fisher habitat
    if(input$Forest != "" & ("Fisher Core Habitat" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = fisher(), colors = "orchid", opacity = 0.4,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "orchid",
                  opacity = 0.5,
                  labels = "Fisher Core Habitat")
    }
    
    ## If Recreation areas selected
    if(input$Forest != "" & ("Recreation Areas" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = rec(), colors = "darkgreen", opacity = 0.4,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "darkgreen",
                  opacity = 0.5,
                  labels = c("Recreation Areas"))
    }
    
    ## WUI
    if(input$Forest != "" & ("Wildland-Urban Interface" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = wui(), colors = "midnightblue", opacity = 0.4,
                       project = FALSE, group = "Wildland-Urban Interface") %>%
        addLegend(position = "bottomright", 
                  colors = "midnightblue",
                  opacity = 0.5,
                  labels = c("Wildland-Urban Interface"))
    }
    
    ## If wildfire areas selected
    if(input$Forest != "" & ("High-severity Fire Core" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = hs(), colors = "darkred", opacity = 0.5,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "darkred",
                  opacity = 0.5,
                  labels = c("High-severity Fire Core"))
    }
    
    ## Climatic water deficit
    if(input$Forest != "" & ("Drought Risk (CWD)" %in% input$Display)) {
      cwd_min <- cellStats(cwd(), stat = "min") %>%
        round(0)
      cwd_max <- cellStats(cwd(), stat = "max") %>%
        round(0)
      pal <- colorNumeric("BrBG", domain = c(cwd_min, cwd_max), 
                          na.color = "transparent", reverse = T)
      m <- m %>%
        addRasterImage(x = cwd(), colors = pal, opacity = 0.4,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  pal = pal, 
                  values = c(cwd_min, cwd_max),
                  bins = 4,
                  title = "Drought Risk (mm CWD)")
    }
    
    ## If Mortality layer is selected add that layer
    if(input$Forest != "" & ("Forest Biomass Loss (2012-16)" %in% input$Display)) {
      pal <- colorNumeric("Oranges", domain = c(0,1), na.color = "transparent")
      at <- seq(0, 1, .2)
      cb <- colorBin(palette = pal, bins = at, domain = at)
      m <- m %>%
        addRasterImage(x = mortshow(), colors = pal, opacity = 0.4,
                       project = FALSE, group = "Biomass Loss") %>%
        addLegend(position = "bottomright", 
                  pal = cb, values = at,
                  labFormat = labelFormat(
                    between = " - ",
                    transform = function(x) 100 * x
                  ), 
                  title = "Biomass Loss (%)")
    }
    
    ## If priority layer select, add that layer
    if(input$Forest != "" & 
       ("Prioritization" %in% input$Display) & 
       !(is.null(priority$raster))) {
      
      m <- m %>%
        addRasterImage(x = priority$raster, colors = c("grey60", "yellow", "orange", "red"), 
                       opacity = 0.5,
                       project = FALSE, group = "Priority") %>%
        addLegend(position = "bottomright", colors = c("grey", "yellow", "orange", "red"),
                  labels = c("Lower Mortality","3rd Priority", "2nd Priority", "1st Priority"),
                  title = "Priority Level")
    }
    
    ## Return map object
    m
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

  ## Downloads ####
  ## Save map and geotiff when user requests it
  output$dl <- downloadHandler(
    filename = "map.png",
    content = function(file) {
      mapshot(user_created_map(), file = file, vwidgth = 1200, vheight = 600)
    })
  
  observe({
    ## grey's out download button when no data to download
    if(is.null(priority$raster))
      shinyjs::disable("dl_tif")
    
    output$dl_tif <- downloadHandler(
      filename = "prioritization.tif",
      content = function(file) {
        writeRaster(priority$raster, file = file)
      })
  })

  #### Prioritization Calculation ####
  ## Set up Null priority value to be replaced during calculation
  priority <- reactiveValues(raster = NULL)
  ## When the user executes the prioritization
  observeEvent(input$Calc, {
      
      ## read in national forest-specific rasters
      # bloss <- raster(paste0("app_data/NF_Limits/bloss_", aoi()$FORESTNAME, ".tif"))
      bloss <- raster(paste0("app_data/NF_Limits/intloss_", aoi()$FORESTNAME, ".tif"))
      ra <- if(input$MechScen == "Moderate constraints") {
        raster(paste0("app_data/NF_Limits/sb_",aoi()$FORESTNAME, ".tif")) } else {
          if(input$MechScen == "Fewer constraints") {
            raster(paste0("app_data/NF_Limits/sd_",aoi()$FORESTNAME, ".tif"))
          }
        }
      rec <- raster(paste0("app_data/NF_Limits/rec_", aoi()$FORESTNAME, ".tif"))
      wui <- raster(paste0("app_data/NF_Limits/wui_", aoi()$FORESTNAME, ".tif"))
      cwd <- raster(paste0("app_data/NF_Limits/cwd_", aoi()$FORESTNAME, ".tif"))
      hs <- raster(paste0("app_data/NF_Limits/hs_", aoi()$FORESTNAME, ".tif"))
      spow <- raster(paste0("app_data/NF_Limits/spow_", aoi()$FORESTNAME, ".tif"))
      fisher <- raster(paste0("app_data/NF_Limits/fisher_", aoi()$FORESTNAME, ".tif"))

      ## Limit the range of biomass loss considered based on user-defined threshold
      blmin <- input$Need/100
      minmask <- cut(bloss, breaks = c(-0.01,blmin,1)) - 1 ## cut returns values of 1 & 2, -1 ensures 0 and 1 raster

      ## Run raster calculation based on user-defined weights
      pr <- (bloss + #More biomass loss increases priority
               hs*input$HSZ2 + #Zone 2 of high-severity increases priority
               spow*input$CASPO + #SPOW pacs can either increase or decrease priority
               fisher*input$Fisher + #Fisher core areas can either increase or decrease priority
               wui*input$WUI + #being in the WUI increases priority
               rec*input$Rec + #being in a rec area increases priority
               ## scale non-binary rasters with max of 1
               cwd/maxValue(cwd)*input$cwd) 
      
      ## Limit to AOI
      pr_aoi <- crop(pr, aoi(), snap = "in") %>%
        mask(mask = aoi())
      
      ## Scale to have a min of 0 and max of 1; also adjust need treshold
      pr_aoi2 <- (pr_aoi - minValue(pr_aoi)) / (maxValue(pr_aoi) - minValue(pr_aoi))
      
      ## mask out areas of mechanical constraints & below need threshold
      ## ra and minmaks have broader extents, only calculating intersection as desired, suppressing warning
      pr_aoi3 <- suppressWarnings(pr_aoi2 * ra * minmask)
      
      ## find adjusted minimum value
      need_adj <- min(pr_aoi3[pr_aoi3 > 0])
      
      ## Reclassify to three classes based on quantile thirds above the adjusted minimum threshold
      q3 <- quantile(pr_aoi3[pr_aoi3 >= need_adj], probs = c(0, 0.33, 0.67, 1)) %>%
        as.numeric() 
      breaks <- c(cellStats(pr_aoi3, stat="min")-0.001, q3)
      
      rcl <- matrix(c(breaks[1], breaks[2], 1,
                      breaks[2], breaks[3], 2,
                      breaks[3], breaks[4], 3,
                      breaks[4], breaks[5], 4),
                    ncol = 3, byrow = T)
      
      pr3 <- reclassify(pr_aoi3, rcl, right = NA) %>%
        subs(data.frame(ID = c(1,2,3,4), Priority = c("Lower mortality",
                                                      "3rd Priority", "2nd Priority", "1st Priority")))


      ## Return priority raster
      priority$raster <- pr3
      
      ## Enables download tif button
      shinyjs::enable("dl_tif")
      
      ## Add priority option to checkbox and reset
      updateCheckboxGroupInput(session, inputId = "Display", 
                               label = "Select display Layers:",
                               choices = c(
                                 "Area of Interest", 
                                 "Non-USFS Land",
                                 "Prioritization",
                                 "Mechanical Constraints",
                                 "Forest Biomass Loss (2012-16)", 
                                 "High-severity Fire Core",
                                 "Drought Risk (CWD)",
                                 "Recreation Areas",
                                 "Wildland-Urban Interface",
                                 "Spotted Owl PACs",
                                 "Fisher Core Habitat"),
                               selected = c("Area of Interest", "Mechanical Constraints", "Prioritization"))
      # }
    })
  
  #### Veg plot ####
  output$vegPlot <- renderPlot({
    
    if(!is.null(priority$raster)) {
      r2 <- crop(cveg, priority$raster) %>%
        mask(priority$raster)
        # raster::resample(priority$raster, method = "ngb")
      
      ptab <- data.frame(Priority = c(1,2,3,4), 
                         p_lab = c("Lower mortality",
                                   "3rd Priority", "2nd Priority", "1st Priority"))
      
      ct <- crosstab(priority$raster, r2, long = T) %>%
        merge(levels(cveg)[[1]], by.x = "ID2", by.y = "ID") %>%
        merge(ptab) %>%
        group_by(type, p_lab) %>%
        summarize(freq = sum(Freq)) %>%
        filter(p_lab != "Lower mortality") %>%
        nest(data = c(p_lab, freq)) %>%
        mutate(type_tot = map_dbl(data, ~{sum(.x$freq)})) %>%
        unnest(cols = c(data)) %>%
        arrange(desc(type_tot), desc(freq)) %>%
        ungroup() 
      
      mutate(ct, type = fct_relevel(type, as.character(unique(ct$type)))) %>%
        ggplot(aes(x = type, y = freq, fill = p_lab)) +
        geom_bar(position = position_dodge(preserve = "single"), 
                 stat = "identity", color = "grey30") +
        scale_fill_manual(name = NULL, values = c("red", "orange", "yellow")) +
        ylab("Hectares") + xlab(NULL) +
        scale_y_continuous(trans='log10') +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
        theme_bw() +
        theme(legend.position = c(1,.99), legend.justification = c(1,1),
              axis.text.x = element_text(angle = 60, hjust = 1, size = 10))
    }

  })
  
  #### Stand Summary Map ####
  
  ## subset data
  aoi_plots <- reactive({
    if(input$Forest_st == "All") {
      st_as_sf(stand, coords = c("long","lat"),
               crs = st_crs(stand_aois))
    } else {
      filter(stand, aoi == input$Forest_st) %>%
        st_as_sf(coords = c("long","lat"),
                 crs = st_crs(stand_aois))
    }
  })
  
  output$map2 <- renderLeaflet({
    m2 <- leaflet(stand) %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "Aerial Imagery") %>%
      addPolygons(data = stand_aois,
                  label = stand_aois$aoi,
                  color = "green",
                  fillColor = "darkgreen", fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addMarkers(~long, ~lat,
                 label = paste0("Location: ", stand$loc, 
                                " - (click for more info)"),
                 popup = paste0("PlotID: ", stand$plot, "<br>",
                                "Area: ", stand$aoi, "<br>",
                                "Location: ", stand$loc, "<br>",
                                "Treated: ", stand$treated, "<br><br>",
                                
                                "Canopy Cover (Live): ", stand$live_cc, " (%)<br>",
                                "Shrub Cover: ", stand$shrub_c, " (%)<br>",
                                "Litter: ", stand$litter, " (%)<br>",
                                "Woody Debris: ", stand$woody_debris, " (%)<br><br>",
                                
                                "Density (Live): ", stand$tpha_live, " (trees/ha)<br>",
                                "Basal Area (Live): ", stand$ba_live, " (sq m)<br>",
                                "Mean DBH (Live): ", stand$dbh_mn_live, " (cm)<br>",
                                "Max DBH (Live): ", stand$dbh_max_live, " (cm)<br><br>",
                                
                                "Density (Dead): ", stand$tpha_dead, " (trees/ha)<br>",
                                "Basal Area (Dead): ", stand$ba_dead, " (sq m)<br>",
                                "Mean DBH (Dead): ", stand$dbh_mn_dead, " (cm)<br>",
                                "Max DBH (Dead): ", stand$dbh_max_dead, " (cm)")) 
    
    ## Select and zoom to AOI
      if(input$Forest_st != "") {
        m2 <- m2 %>%
          ## Zoom to selection
          fitBounds(lng1 = as.numeric(st_bbox(aoi_plots())$xmin - 0.0005),
                    lat1 = as.numeric(st_bbox(aoi_plots())$ymin - 0.001),
                    lng2 = as.numeric(st_bbox(aoi_plots())$xmax),
                    lat2 = as.numeric(st_bbox(aoi_plots())$ymax + 0.001)) 
      }
    
    ## Return map
    m2
  })
  
  #### Stand plot ####
  output$boxPlot <- renderPlot({
    
    ulab <- input$metric_st
    plab <- paste0(ulab, " (", metrics[metrics$label == ulab, "unit"][1], ")")
    
    vars <- filter(metrics, label == ulab) %>%
      pull(metric) %>%
      as.character()
    
    ## just plots in aoi
    if(input$Forest_st == "All") {
      stand2 <- stand
      regen2 <- regen} else
      {stand2 <- filter(stand, aoi == input$Forest_st)
      regen2 <- filter(regen, aoi == input$Forest_st)}
    
    ## Different plot for regen
    if(ulab == "Regeneration") {
      regen2 <- dplyr::select(regen2, plot, aoi, treated, species, spha) %>%
        mutate(treat = ifelse(treated == "Yes", "Treated Plots", "Untreated Plots"))
      
      p <- ggplot(regen2, aes(x = species, y = spha, color = species)) +
        geom_boxplot(na.rm = T) +
        geom_jitter(na.rm = T) +
        facet_grid(~ treat) +
        ylab(plab) + xlab(NULL) +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      
    } else {
      ## For other metrics
      stand2 <- dplyr::select(stand2, plot, aoi, treated, {{vars}}) %>%
        gather(key = metric, value = value, -plot, -aoi, -treated) %>%
        merge(metrics, by = "metric") %>%
        mutate(treat = ifelse(treated == "Yes", "Treated Plots", "Untreated Plots"),
               text = paste("value:",value))
      
      p <- ggplot(stand2, aes(x = status, y = value)) +
        geom_boxplot(na.rm = T) +
        geom_jitter(na.rm = T) +
        facet_grid(~ treat) +
        ylab(plab) + xlab("Tree Status")
    }
    
    p
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

