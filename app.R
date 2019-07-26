
## Setup ####

library(shiny)
library(sf)
library(leaflet)
library(mapview)
library(tidyverse)
library(rgdal)
library(raster)
library(shinyjs)
library(shinycssloaders)
library(shinyBS) #for tooltip funcitonality

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
             
             # Sidebar with user-input widgets
             # sidebarLayout(
             #   sidebarPanel(
             ## Moving sidebar to top with multiple columns
             fluidRow(
               column(3,
                      selectInput(inputId = "Forest", 
                             label = h4(tags$b("Step 1: Select area of interest"), 
                                        style = "font-size:120%; color:darkblue"),
                             selected = "",
                             choices = aoi_id),
                      bsTooltip("Forest",
                                "Prioritization and map layer display will be limited to the area selected",
                                placement = "right"),
                      ## Horrizontal line
                      tags$hr(),
       
                      h4(tags$b("Step 2: Select reforestation need threshold:"), 
                         style = "font-size:120%; color:darkblue"),  
                       
                      sliderInput("Need", tags$tbody("Need Threshold (% Biomass loss)"), 
                                  10, 100, 50,
                                  width = '80%', step = 10),
                      bsTooltip("Need",
                                "Areas with less biomass loss than the selected threshold will be excluded from prioritization.")
               ),
               
               h4(id = "Step3", tags$b("Step 3: Select data layer weights:"), 
                  style = "font-size:120%; color:darkblue"),
               bsTooltip("Step3",
                         "Layers assigned a negative value will decrease piority. <br/> Layers assigned a positive value will increase priority. <br/> Layers assigned a zero value will not affect prioritization."),
               column(3,

                      
                      sliderInput("cwd", "Drought Risk (CWD)", -1, 0, 0,
                                  width = '80%', step = .25),
                      bsTooltip("cwd",
                                "Areas of higher drought risk will decrease priority. CWD = climate water deficit (1981-2010 average)."),
                      sliderInput("HSZ2", "High-severity Fire Core", 0, 1, 0,
                                  width = '80%', step = .25),
                      bsTooltip("HSZ2",
                                 "Areas within high-severity cores will increase priority. Cores are those areas more than 650ft (200m) from seed trees within high-severity wildfire patches. Fires from 2012-2017 are included."),
                      sliderInput("WUI", "Wildland-Urban Interface", 0, 1, 0,
                                  width = '80%', step = .25),
                      bsTooltip("WUI",
                                 "Areas within the wildland-urban interface will increase priority.")
               ),
               
               column(3,

                      sliderInput("Rec", "Recreation Areas", 0, 1, 0,
                                  width = '80%', step = .25),
                      bsTooltip("Rec",
                                "Recreation areas will increase priority."),
                      sliderInput("CASPO", "Spotted Owl PACs", -1, 1, 0,
                                  width = '80%', step = .25),
                      bsTooltip("CASPO",
                                "California Spotted Owl PACs can increase or decrease priority depending on management objectives."),
                      sliderInput("Fisher", "Fisher Core Habitat", -1, 1, 0,
                                  width = '80%', step = .25),
                      bsTooltip("Fisher",
                                "Pacific fisher core habitats can increase or decrease priority depending on management objectives.")
                      ),
               
               column(3,
                      h4(tags$b("Step 4: Run prioritization"), 
                         style = "font-size:120%; color:darkblue"),
                      actionButton("Calc", "Calculate"),
                      bsTooltip("Calc",
                                "Generates a 3-level priority layer using biomass and weighted data layers. Non-forest service lands, areas with mechanical constraints, and those with biomass loss below the designated need threshold are excluded. This will likely take a few seconds to run."),
                      
                      tags$hr(),
                      h4(tags$b("Step 5: Download map and/or data"), 
                         style = "font-size:120%; color:darkblue"),
                      
                      ## Buttons for downloading current map and tif
                      #### Maybe add one or combine for generating a short "report"
                      downloadButton("dl", "Download Map Image"),
                      bsTooltip("dl",
                                "Generates and downloads a map using the current view."),
                      downloadButton("dl_tif", "Download Priority Raster"),
                      bsTooltip("dl_tif",
                                "Downloads a priority raster layer for further analysis.")
                      

               )),
             tags$hr(),

             fluidRow(
                 mainPanel(width = 12,
                   leafletOutput("map", width = "100%", height = 600),

                 absolutePanel(id = "MapLayers", class = "panel panel-default", fixed = F,
                               draggable = F, top = 10, left = 20, right = "auto", bottom = "auto",
                               width = 220, height = "auto", 
                               
                               checkboxGroupInput("Display", label = tags$b("Select display Layers:"),
                                                  # inline = T,
                                                  choices = c("Area of Interest", 
                                                              "USFS Land",
                                                              "Mechanical Constraints",
                                                              "Forest Biomass Loss (2012-16)", 
                                                              "High-severity Fire Core",
                                                              "Drought Risk (CWD)",
                                                              "Recreation Areas",
                                                              "Wildland-Urban Interface",
                                                              "Spotted Owl PACs",
                                                              "Fisher Core Habitat"),
                                                  # "Prioritization"),
                                                  selected = c("Area of Interest"))
                 )
               )
             )
    ),
    
    #### Stand Summary - UI ####
    tabPanel("Stand summary tool", 
             titlePanel("Post-drought Stand Condition Summary Tool"),
             sidebarPanel(
               selectInput(inputId = "Forest_st", 
                           label = h4(tags$b("Step 1: Select area of interest", 
                                             style = "font-size:80%; color:darkblue")),
                           selected = "All",
                           choices = aoi_st),
               selectInput(inputId = "metric_st",
                           label = h4(tags$b("Step 2: Select stand metric to summarize", 
                                             style = "font-size:80%; color:darkblue")),
                           selected = "",
                           choices = metrics$label),
               plotOutput(outputId = "boxPlot", height = "300px")
             ),
             mainPanel(leafletOutput("map2", width = "100%", height = 600))
             ),
    tabPanel("BMP guide",
             includeHTML("bmp.html")),
             # includeMarkdown("bmp.Rmd")),
    tabPanel("Technical Info",
             includeMarkdown("tech_info.Rmd"))
  )
)


## Server code ####
server <- function(input, output, session) {
  
  ## Dialogs ####
  ## The quickstart dialog for prioritization tool
  # observe({
  #   if(input$tabs == "Prioritization tool") {
  #     showModal(modalDialog(
  #       title = "Welcome",
  #       HTML("Here you will find a: <br> 1) Spatial prioritization tool for post-mortality reforestation <br>
  #   2) A tool summarizing stand-level data collected following the 2012-2016 drought <br>
  #   3) A best management practices (BMP) guide for post-mortality event reforestation. <br> <br>
  #   Good Luck! <br> <br>
  #   (This application is still under construction. More instructions and options will be added when we get around to it...)"),
  #       easyClose = TRUE
  #     )) 
  #   }
  # })
  
  ## Tooltips
  #### These inexplicably stopped working. implimented using bsTooltip above, but can't delay as you can here
  # addTooltip(session, id = "Forest",
  #            "Prioritization and map layer display will be limited to the national forest or ranger district selected",
  #            placement = "bottom", trigger = "hover",
  #            options = list(delay = list(show=500, hide=300)))
  # addTooltip(session, id = "Need",
  #           title = "Areas with less biomass loss than the selected threshold will be excluded from prioritization",
  #           placement = "bottom", trigger = "hover",
  #           options = list(delay = list(show=500, hide=300)))
  # addTooltip(session, id = "cwd",
  #           title = "Drought Risk is defined as the 1981-2010 average climate water deficit (CWD), as modeled by...",
  #           placement = "bottom", trigger = "hover", 
  #           options = list(delay = list(show=500, hide=300)))
  # addTooltip(session, id = "HSZ2",
  #            title = "Areas greater than 650ft (200m) from seed trees within high-severity wildfire patches where natural recruitment of non-serotinous conifer species is unlikely. Fires from 2012-2017 included.",
  #            placement = "bottom", trigger = "hover", 
  #            options = list(delay = list(show=500, hide=300)))
  
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
  
  ra <- reactive({
    ## Read in AOI national forest-specific raster
    sb <- raster(paste0("app_data/NF_Limits/sb_",aoi()$FORESTNAME, ".tif")) %>%
      cut(breaks = c(-0.5,0.5)) # effectively removes ones; highlighting inaccesible areas
  })
  
  mortshow <- reactive({
    bloss <- raster(paste0("app_data/NF_Limits/bloss_", aoi()$FORESTNAME, ".tif"))
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
      # addProviderTiles(provider = "Esri.WorldGrayCanvas", group = "Grey") 
    
    # add controls for basemaps and data
    # m <- addLayersControl(m,
    #                       baseGroups = c("Topo", "Relief", "Aerial Imagery", "Grey"))
    # # overlayGroups = layers,
    # # position = c("topright"),
    # # options = layersControlOptions(collapsed = F)) %>%
    # #   hideGroup(c("Inaccessible", "Biomass Loss"))

    # Overlay Groups
    ## Add layers if user-selected
    ## remove forest layer when zooming to aoi for now; makes saving crash
    if(input$Forest == "") {
      # m <- addPolygons(m,
      #                  data = forest,
      #                  color = "black",
      #                  fillColor = "green",
      #                  fill = F,
      #                  weight = 2,
      #                  opacity = 1,
      #                  fillOpacity = 0.2,
      #                  label = forest$FORESTNAME,
      #                  highlightOptions = highlightOptions(color = "white", weight = 2,
      #                                                      bringToFront = TRUE),
      #                  group = "Forests") %>%
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
        # addLegend(position = "bottomright", 
        #           color = "black",
        #           opacity = 0.2,
        #           labels = "Forests")
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
    if(input$Forest != "" & ("USFS Land" %in% input$Display)) {
      
      m <- m %>%
        addRasterImage(x = fs(), colors = c("white", "green"), 
                       opacity = 0.5,
                       project = FALSE) %>%
        addLegend(position = "bottomright", colors = c("white", "green"),
                  labels = c("No", "Yes"),
                  title = "USFS Land")
    }
    
    ## If Inaccessible mask is selection add that layer
    if(input$Forest != "" & ("Mechanical Constraints" %in% input$Display)) {
      pal <- colorNumeric(c("black"), values(ra()), na.color = "transparent")
      m <- m %>%
        addRasterImage(x = ra(), colors = pal, opacity = 0.3,
                       project = FALSE, group = "Mechanical Constraints") %>%
        addLegend(position = "bottomright", 
                  colors = "black",
                  opacity = 0.5,
                  labels = "Mechanical Constraints")
    }
    
    ## SPOW PACs
    if(input$Forest != "" & ("Spotted Owl PACs" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = spow(), colors = "purple", opacity = 0.3,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "purple",
                  opacity = 0.5,
                  labels = "Spotted Owl PACs")
    }
    
    ## Fisher habitat
    if(input$Forest != "" & ("Fisher Core Habitat" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = fisher(), colors = "orchid", opacity = 0.3,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "orchid",
                  opacity = 0.5,
                  labels = "Fisher Core Habitat")
    }
    
    ## If Recreation areas selected
    if(input$Forest != "" & ("Recreation Areas" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = rec(), colors = "darkgreen", opacity = 0.3,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "darkgreen",
                  opacity = 0.5,
                  labels = c("Recreation Areas"))
    }
    
    ## WUI
    if(input$Forest != "" & ("Wildland-Urban Interface" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = wui(), colors = "midnightblue", opacity = 0.3,
                       project = FALSE, group = "Wildland-Urban Interface") %>%
        addLegend(position = "bottomright", 
                  colors = "midnightblue",
                  opacity = 0.5,
                  labels = c("Wildland-Urban Interface"))
    }
    
    ## If wildfire areas selected
    if(input$Forest != "" & ("High-severity Fire Core" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = hs(), colors = "darkred", opacity = 0.3,
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
        addRasterImage(x = cwd(), colors = pal, opacity = 0.3,
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
        addRasterImage(x = mortshow(), colors = pal, opacity = 0.3,
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
                  labels = c("No Need","Low", "Moderate", "High"),
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
      bloss <- raster(paste0("app_data/NF_Limits/bloss_", aoi()$FORESTNAME, ".tif"))
      sb <- raster(paste0("app_data/NF_Limits/sb_",aoi()$FORESTNAME, ".tif")) 
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
               cwd/maxValue(cwd)*input$cwd) *
               # cwd/cellStats(cwd, stat='max')*input$cwd) * #high cwd decreases priority 
        sb * minmask #mask out areas of mechanical constraints & below need threshold


      ## Scale to have a max of 1; may want to do after mask step below
      ## Max will be equal to the number of input layers adjusted for weights
      # maxp <- 1 + input$HSZ2 + input$CASPO + input$Fisher + input$WUI + input$Rec + input$cwd
      pr01 <- pr / maxValue(pr) #maxp

      ## Limit to AOI (this seems to be the slow step so do it last)
      pr_aoi <- crop(pr01, aoi(), snap = "in") %>%
        mask(mask = aoi())

      ## Reclassify to three classes based on quantile thirds above the minimum threshold
      ## occasionally have problems of breaks not being unique. add a bit to upper quantiles
      q3 <- quantile(pr_aoi[pr_aoi > blmin], probs = c(0, 0.33, 0.67, 1)) + c(-0.001, 0.000, 0.001, 0.001)
      breaks <- c(cellStats(pr_aoi, stat="min")-0.001, q3)
      
      ## Reclassify into areas of no need and approximate thirds of the remaining
      pr3 <- cut(pr_aoi, breaks = breaks) %>%
        subs(data.frame(ID = c(1,2,3,4), Priority = c("No Need","Low", "Moderate", "High")))

      ## Return priority raster
      priority$raster <- pr3
      
      ## Enables download tif button
      shinyjs::enable("dl_tif")
      
      ## Add priority option to checkbox and reset
      updateCheckboxGroupInput(session, inputId = "Display", 
                               # label = tags$b("Select display Layers:"),
                               label = "Select display Layers:",
                               choices = c(
                                 "Area of Interest", 
                                 "USFS Land",
                                 "Prioritization",
                                 "Mechanical Constraints",
                                 "Forest Biomass Loss (2012-16)", 
                                 "High-severity Fire Core",
                                 "Drought Risk (CWD)",
                                 "Recreation Areas",
                                 "Wildland-Urban Interface",
                                 "Spotted Owl PACs",
                                 "Fisher Core Habitat"),
                               selected = c("Area of Interest", "Prioritization"))
      # }
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
      ## puts zoom control at topright
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "Aerial Imagery") %>%
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
                                "Max DBH (Dead): ", stand$dbh_max_dead, " (cm)")) %>%
      addPolygons(data = stand_aois,
                  label = stand_aois$aoi,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) 
    
    ## Select and zoom to AOI
      if(input$Forest_st != "") {
        m2 <- m2 %>%
          # addMarkers(~aoi_plots()$long, ~aoi_plots()$lat) %>%
          ## Zoom to selection
          #### Can we revise this so that this only happens when the AOI changes?
          fitBounds(lng1 = as.numeric(st_bbox(aoi_plots())$xmin - 0.0005),
                    lat1 = as.numeric(st_bbox(aoi_plots())$ymin - 0.0005),
                    lng2 = as.numeric(st_bbox(aoi_plots())$xmax),
                    lat2 = as.numeric(st_bbox(aoi_plots())$ymax)) 
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
        geom_boxplot() +
        geom_jitter() +
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
        geom_boxplot() +
        geom_jitter() +
        facet_grid(~ treat) +
        ylab(plab) + xlab("Tree Status")
    }
    
    p
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

