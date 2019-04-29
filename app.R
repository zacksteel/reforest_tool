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

## Define forest/aoi options and read in shape file
fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
        "Stanislaus", "Inyo", "Sequoia", "Sierra")
forest <- st_read("data/Spatial", "SN_NFs")

#### Pre-mask rasters according to national forests and reading in when AOI is selected will probably save user time
## Bring in "need" layer
# bloss <- raster("data/Spatial/biomassloss.tif")
# 
# ## Bring in accessibility raster layer
# #### scenb and forest layers are slightly mis-aligned. imprecise crs transformation somewhere along the way?
# sb <- raster("data/Spatial/scenb.tif") 
# 
# ## Bring in land class layers; cwd
# rec <- raster("data/Spatial/RecAreas.tif")
# wui <- raster("data/Spatial/WUI.tif")
# cwd <- raster("data/spatial/cwd_sn.tif")


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
                 
                 ## Horrizontal line
                 tags$hr(),
 
                 h4("Step 2: Select reforestation need threshold:"),  
                 
                 sliderInput("Need", tags$tbody("Need Threshold (Biomass loss)"), 10, 100, 50,
                             width = '80%', step = 10),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 h4("Step 3: Select data layer weights:"),

                 sliderInput("cwd", "Climatic Water Deficit", 0, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("HSZ2", "High-severity Fire (Zone 2)", 0, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("WUI", "Wildland Urban Interface", 0, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("Rec", "Recreation Areas", 0, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("CASPO", "Spotted Owl PACs", -1, 1, 0,
                             width = '80%', step = .25),
                 sliderInput("Fisher", "Fisher Core Habitat", -1, 1, 0,
                             width = '80%', step = .25),

                 
                 h4("Step 4: Run prioritization"),
                 
                 ## Horrizontal line
                 tags$hr(),
                 
                 actionButton("Calc", "Calculate"),
                 
                 h4("Step 5: Download map and/or data"),
                 tags$hr(),
                 
                 ## Move this display option to map widget
                 checkboxGroupInput("Display", tags$i("Select display Layers:"),
                                    choices = c("Forests", 
                                                "Area of Interest", 
                                                "Mechanical Constraints",
                                                "Biomass Loss (2012-16)", 
                                                "High-severity Fire (Zone 2)",
                                                "Climatic Water Deficit",
                                                "Recreation Areas",
                                                "Wildland-Urban Interface",
                                                "Spotted Owl PACs",
                                                "Fisher Core Habitat",
                                                "Prioritization"),
                                    selected = c("Forests", "Area of Interest")),
                 
                 ## Buttons for downloading current map and tif
                 #### Maybe add one or combine for generating a short "report"
                 downloadButton("dl", "Download Map Image"),
                 downloadButton("dl_tif", "Download Priority GeoTiff")
               ),
               
               #### Show a plot of relative weights or data layers?
               
               ## Define size of map
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

# Define server code
server <- function(input, output) {
  
  ## Reactive mapping objects
  ## Call up rasters as needed (alternatively, could read in all when AOI is assigned; would shift around processing)
  ## Limit forest shp to selection forest
  ## Only run if user wants it displayed to save runtime
  aoi <- reactive({
    if(input$Forest != "" & ("Area of Interest" %in% input$Display)) {
      filter(forest, FORESTNAME == input$Forest)
    }
  })

  ra <- reactive({
    ## Conditional necessary to avoid crop error
    if(input$Forest != "" & ("Mechanical Constraints" %in% input$Display)) {
      ## Read in AOI national forest-specific raster
      sb <- raster(paste0("data/spatial/nf_limits/sb_",input$Forest, ".tif")) %>%
        cut(breaks = c(-0.5,0.5)) # effectively removes ones; highlighting inaccesible areas
    } 
  })
  
  mortshow <- reactive({
    if(input$Forest != "" & ("Biomass Loss (2012-16)" %in% input$Display)) {
      bloss <- raster(paste0("data/spatial/nf_limits/bloss_", input$Forest, ".tif"))
    } 
  })
  
  rec <- reactive({
    if(input$Forest != "" & ("Recreation Areas" %in% input$Display)) {
      raster(paste0("data/Spatial/nf_limits/rec_", input$Forest, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
    }
  })
    
  wui <- reactive({
    if(input$Forest != "" & ("Wildland-Urban Interface" %in% input$Display)) {
      raster(paste0("data/Spatial/nf_limits/wui_", input$Forest, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
    }
  })
  
  cwd <- reactive({
    if(input$Forest != "" & ("Climatic Water Deficit" %in% input$Display)) {
      raster(paste0("data/spatial/nf_limits/cwd_", input$Forest, ".tif"))
    }
  })
  
  hs <- reactive({
    if(input$Forest != "" & ("High-severity Fire (Zone 2)" %in% input$Display)) {
      raster(paste0("data/spatial/nf_limits/hs_", input$Forest, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros; highlighting zone 2
    }
  })
  
  spow <- reactive({
    if(input$Forest != "" & ("Spotted Owl PACs" %in% input$Display)) {
      raster(paste0("data/spatial/nf_limits/spow_", input$Forest, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
    }
  })
  
  fisher <- reactive({
    if(input$Forest != "" & ("Fisher Core Habitat" %in% input$Display)) {
      raster(paste0("data/spatial/nf_limits/fisher_", input$Forest, ".tif")) %>%
        cut(breaks = c(0.5,1.5)) # effectively removes zeros
    }
  })
  
  
  ## Set up Null priority value to be replaced during calculation
  priority <- reactiveValues(raster = NULL)

  
  ## Set up the default map function
  map_reactive <- reactive({
    m <- leaflet() %>%
      # addTiles(options = tileOptions(minZoom = 2, maxZoom = 18)) %>%
      
      # Base Groups
      #### For some reason if I change the basemap, I can no longer display the AOI-dependent layers. 
      #### Probably something to do with how things are rendered. Trouble shoot later
      
      # addProviderTiles(provider = "Esri.WorldShadedRelief", group = "Relief") %>%
      # addProviderTiles(provider = "Esri.WorldImagery", group = "Aerial Imagery") %>%
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topo") 
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
    if("Forests" %in% input$Display) {
      m <- addPolygons(m,
                       data = forest,
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
                  weight = 4,
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
    if(input$Forest != "" & ("High-severity Fire (Zone 2)" %in% input$Display)) {
      m <- m %>%
        addRasterImage(x = hs(), colors = "darkred", opacity = 0.3,
                       project = FALSE) %>%
        addLegend(position = "bottomright", 
                  colors = "darkred",
                  opacity = 0.5,
                  labels = c("High-severity Fire (Zone 2)"))
    }
    
    ## Climatic water deficit
    if(input$Forest != "" & ("Climatic Water Deficit" %in% input$Display)) {
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
                  title = "CWD (mm)")
    }
    
    ## If Mortality layer is selected add that layer
    if(input$Forest != "" & ("Biomass Loss (2012-16)" %in% input$Display)) {
      pal <- colorNumeric("Oranges", domain = c(0,1), na.color = "transparent")
      at <- seq(0, 1, .2)
      cb <- colorBin(palette = pal, bins = at, domain = at)
      m <- m %>%
        addRasterImage(x = mortshow(), colors = pal, opacity = 0.3,
                       project = FALSE, group = "Biomass Loss") %>%
        addLegend(position = "bottomright", 
                  pal = cb, values = at,
                  # pal = pal, values = c(0,1),
                  # bins = 4,
                  labFormat = labelFormat(
                    # prefix = "(", 
                    # suffix = " %", 
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
        addRasterImage(x = priority$raster, colors = c("grey", "yellow", "orange", "red"), 
                       opacity = 0.5,
                       project = FALSE, group = "Priority") %>%
        addLegend(position = "bottomright", colors = c("grey", "yellow", "orange", "red"),
                  labels = c("No Need","Low", "Moderate", "High"))
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
    m <- map_reactive() %>%
      setView(lng = input$map_center$lng, lat = input$map_center$lat,
              zoom = input$map_zoom)
  })
  

  ## Save map and geotiff when user requests it
  output$dl <- downloadHandler(
    filename = "map.png",
    content = function(file = filename) {
      mapshot(user_created_map(), file = file)
    })
  
  #### Currently crashes the site if I haven't run the calculation yet, need to fix
  #### Alternatively, would it be possible to have the button only show up after the calculation?
  # if(is.null(priority$raster)) {
  #   output$dl_tif <- showNotification("This is a notification.")
  # } else{
    output$dl_tif <- downloadHandler(
      filename = "prioritization.tif",
      content = function(file = filename) {
        writeRaster(priority$raster, file = file)
      })
  # }

  
  ## When the user executes the prioritization
  observeEvent(input$Calc, {
    ## Only run if AOI has been selected
    if(input$Forest == "") {NULL} else {
      
      ## read in national forest-specific rasters
      bloss <- raster(paste0("data/spatial/nf_limits/bloss_", input$Forest, ".tif"))
      sb <- raster(paste0("data/spatial/nf_limits/sb_",input$Forest, ".tif")) %>%
        cut(breaks = c(-0.5,0.5))

      rec <- raster(paste0("data/Spatial/nf_limits/rec_", input$Forest, ".tif"))
      wui <- raster(paste0("data/Spatial/nf_limits/wui_", input$Forest, ".tif"))
      cwd <- raster(paste0("data/spatial/nf_limits/cwd_", input$Forest, ".tif"))
      hs <- raster(paste0("data/spatial/nf_limits/hs_", input$Forest, ".tif"))
      spow <- raster(paste0("data/spatial/nf_limits/spow_", input$Forest, ".tif"))
      fisher <- raster(paste0("data/spatial/nf_limits/fisher_", input$Forest, ".tif"))

      ## Limit the range of biomass loss considered based on user-defined threshold
      blmin <- input$Need/100
      minmask <- cut(bloss, breaks = c(-0.01,blmin,1)) - 1 ## cut returns values of 1 & 2, -1 ensures 0 and 1 raster

      ## Run raster calculation based on user-defined weights
      pr <- (bloss + #More biomass loss increases priority
               hs*input$HSZ2 + #Zone 2 of high-severity increases priority
               spow*input$CASPO + #SPOW pacs can either increase or decrease priority
               fisher*input$Fisher + #Fisher core areas can either increase or decrease priority
               wui*input$WUI + #being in the WUI increases priority
               rec*input$Rec - #being in a rec area increases priority
               ## scale non-binary rasters with max of 1
               cwd/cellStats(cwd, stat='max')*input$cwd) * #high cwd decreases priority 
        sb * minmask #mask out areas of mechanical constraints & below need threshold


      ## Scale to have a max of 1; may want to do after mask step below
      ## Max will be equal to the number of input layers adjusted for weights
      maxp <- 1 + input$HSZ2 + input$CASPO + input$Fisher + input$WUI + input$Rec + input$cwd
      pr01 <- pr / maxp

      ## Limit to AOI (this seems to be the slow step so do it last)
      #### Really only necessary when AOI differs from the national forest datasets (i.e. when AOI is at the district level)
      #### May be able to save a little processing time here
      aoi_shp <- filter(forest, FORESTNAME == input$Forest) %>%
        as_Spatial()
      pr_aoi <- crop(pr01, aoi_shp, snap = "in") %>%
        ## mask out areas beyond AOI; slower than crop so helps to do this step-wise
        mask(mask = aoi_shp)

      ## Create a not treatable layer for AOI
      #### This step may not longer be needed with new user controls of map
      # nt <- crop(sb, aoi_shp, snap = "in") %>%
      #   mask(mask = aoi_shp)


      ## Reclassify to three classes based on quantile thirds above the minimum threshold
      ## occasionally have problems of breaks not being unique. add a bit to upper quantiles
      q3 <- quantile(pr_aoi[pr_aoi > blmin], probs = c(0, 0.33, 0.67, 1)) + c(-0.001, 0.000, 0.001, 0.001)
      breaks <- c(cellStats(pr_aoi, stat="min")-0.001, q3)

      #### Get an error of breaks are not unique for the Sierra with 0.5 need threshold, -1 CWD weight and others at 0 weight
      #### Having a negative weight expands the possible range of raw priority values
      #### Need to think more about this and fix
      # breaks <- c(cellStats(pr_aoi, stat="min"), #minimum raw priority score
      #             q3[1], #need threshold
      #             q3[2], #upper bound of low priority
      #             q3[3], #upper bound of moderate priority
      #             q3[4]) #maximum score (should not exceed 1)
      
      ## Reclassify into areas of no need and approximate thirds of the remaining
      pr3 <- cut(pr_aoi, breaks = breaks) %>%
        subs(data.frame(ID = c(1,2,3,4), Priority = c("No Need","Low", "Moderate", "High")))

      ## Return priority raster
      priority$raster <- pr3
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#     
#     ## Redefine priority function/layer
#     # priority <- reactive({pr_aoi})
#     
#     ## Everything below was for saving a default map. Obsolete with newer user-led map download.
#     ##https://datacarpentry.org/r-raster-vector-geospatial/02-raster-plot/index.html
#     ##https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# 
#     # ## make a background raster
#     # aoi_r <- raster(extent(aoi_shp), res = res(pr3))
#     # #### Slow step
#     # aoi_r <- rasterize(aoi_shp, aoi_r)
#     # aoi_df <- as.data.frame(aoi_r, xy = T) %>%
#     #   mutate(Priority = ifelse(!is.na(layer_OBJECTID), "Not Treatable", NA),
#     #          Priority = factor(Priority, levels = c("High", "Moderate", "Low", "No Need", "Not Treatable"))) %>%
#     #   filter(!is.na(Priority))
#     # 
#     # ## make not treatable dataframe
#     # nt_df <- as.data.frame(nt, xy = T) %>%
#     #   mutate(Priority = ifelse(scenb == 1, NA, "Not Treatable"),
#     #          Priority = factor(Priority, levels = c("High", "Moderate", "Low", "No Need", "Not Treatable"))) %>%
#     #   filter(!is.na(Priority))
#     # 
#     # ## Set up dataframe from raster
#     # #### This is a slow step
#     # pr3_df <- as.data.frame(pr3, xy = T) %>%
#     #   rename(Priority = Priority_Priority) %>%
#     #   mutate(Priority = factor(Priority, levels = c("High", "Moderate", "Low", "No Need", "Not Treatable"))) %>%
#     #   filter(!is.na(Priority))
#     # 
#     # ## Set up palette and replace last level as mask
#     # pal <- sequential_hcl(5, palette = "Inferno")
#     # 
#     # p <- ggplot() +
#     #   geom_raster(data = aoi_df, aes(x = x, y = y, fill = Priority)) +
#     #   geom_raster(data = pr3_df, aes(x = x, y = y, fill = Priority)) +
#     #   geom_raster(data = nt_df, aes(x = x, y = y, fill = Priority)) +
#     #   scale_fill_manual(values = c("High" = pal[1], "Moderate" = pal[2], "Low" = pal[3],
#     #                                "No Need" = "grey90", "Not Treatable" = "grey60"),
#     #                     breaks = c("High", "Moderate", "Low", "No Need", "Not Treatable")) +
#     #   theme_bw() +
#     #   theme(axis.title = element_blank()) + 
#     #   coord_quickmap()
#     # 
#     # ## Prompt user to pick a directory to save to
#     # wd <- choose.dir(default = "Computer", caption = "Select output directory")
#     # setwd(wd)
#     # 
#     # ## Create an output directory to save into
#     # new.dir <- "ReforestPriority"
#     # ## If one alread exists, add a number until it doesn't
#     # x <- 1
#     # repeat{
#     #   if(dir.exists(new.dir)) {
#     #   new.dir <- paste0("ReforestPriority",x)
#     #   x <- x + 1
#     #   } else
#     #     break
#     #   }
#     # dir.create(new.dir)
#     # 
#     # ## Add products to directory
#     # ggsave(paste0(new.dir,"/scratch_priority.png"), plot = p)
#     # # png(paste0(new.dir,"/scratch_priority.png"))
#     # # plot(pr_aoi)
#     # # dev.off()
#     # 
#     # writeRaster(pr_aoi, paste0(new.dir,"/priority.tif"))
#   }
#    
# })

