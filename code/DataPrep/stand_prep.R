## Purpose: Prepare and summarize stand data to be displayed in the shiny app
## Author: Zack Steel
## Created: 2019-05-16

stand_prep <- function() {
  library(tidyverse)
  library(sf)
  
  ## general data with treatment indicator
  gen <- read.csv("data/Stand/GeneralPlot.csv") %>%
    dplyr::select(plot = Plot.Name, forest = Forest, loc = Location, treated = Treated) %>%
    filter(!is.na(plot)) %>%
    arrange(plot)
  ## cover data
  cover <- read.csv("data/Stand/CoverData.csv") %>%
    dplyr::select(plot = PlotName, live_cc = LiveOverstory, dead_cc = DeadOverstory, 
                  shrub_c = ShrubCover, litter = Litter, woody_debris = WoodyDebris) %>%
    filter(!is.na(plot)) %>%
    arrange(plot)
  ## Tree data
  trees <- read.csv("data/Stand/TreeData.csv") %>%
    dplyr::select(plot = PlotName, tree = TreeNumber, species = Species, status = Status, 
                  dbh = DBH, live_crown = LiveCrownPercent, 
                  insect = InsectEvidence, insect_lev = InsectLevel) %>%
    ## idicator of live or not (including infested trees that have a green crown)
    mutate(live = ifelse(status %in% c("L", "I"), 1, 0),
           ## Calculate BA
           ba = pi * (dbh/2)^2 / 10000, # returns square meters
           ## Set some size classes
           class = case_when(dbh < 20 ~ 1,
                             dbh >= 20 & dbh < 40 ~ 2,
                             dbh >= 40 & dbh < 60 ~ 3,
                             dbh >= 60 & dbh < 80 ~ 4,
                             dbh >= 80 & dbh < 100 ~ 5,
                             dbh >= 100 & dbh < 120 ~ 6,
                             dbh >= 120 ~ 7)) 
  
  ## Summarise tree data by plot for dead and alive
  ## First calculate plot area as a proportion of a hectare
  plot_area <- pi*12.6^2 / 10000 #12.6m radius; returns hectares
  
  liveclass <- group_by(trees, plot) %>%
    filter(live == 1) %>%
    select(plot, class) %>%
    count(class) %>%
    mutate( tpha = round(n / plot_area, 0),
            class = paste0("class_live",class)) %>%
    select(-n) %>%
    spread(class, tpha, fill = 0) %>%
    ungroup() %>%
    mutate(tpha_live = rowSums(.[2:8]))
  
  liveplot <- group_by(trees, plot) %>%
    filter(live == 1) %>%
    add_count(class) %>%
    summarise(ba_live = sum(ba),
              dbh_mn_live = round(mean(dbh), 0),
              dbh_max_live = round(max(dbh), 0)) %>%
    ## Put ba on per hectare basis
    mutate(ba_live = round(ba_live / plot_area, 2)) %>%
    merge(liveclass, all.x = T)
  
  deadclass <- group_by(trees, plot) %>%
    filter(live == 0) %>%
    select(plot, class) %>%
    count(class) %>%
    mutate( tpha = round(n / plot_area, 0),
            class = paste0("class_dead",class)) %>%
    select(-n) %>% #needed or spread duplicates rows
    spread(class, tpha, fill = 0) %>%
    select(-class_deadNA) %>%
    ungroup() %>%
    mutate(tpha_dead = rowSums(.[2:8]))
  
  deadplot <- group_by(trees, plot) %>%
    filter(live == 0) %>%
    add_count(class) %>%
    summarise(ba_dead = sum(ba),
              dbh_mn_dead = round(mean(dbh), 0),
              dbh_max_dead = round(max(dbh), 0)) %>%
    ## Put ba on per hectare basis
    mutate(ba_dead = round(ba_dead / plot_area, 2)) %>%
    merge(deadclass, all.x = T)
  
  plot_sum <- merge(liveplot, deadplot, all = T) %>%
    filter(!is.na(plot)) %>%
    replace(., is.na(.), 0) %>%
    arrange(plot)
  
  
  ## Stand location data
  d <- read.csv("data/Stand/Plot_Locations2.csv") %>%
    ## just those completed
    # filter(Completed. == "Y") %>%
    dplyr::select(plot = Plots2017, forest = Forest, loc = Location, 
                  lat = Latitude, long = Longitude, utm_n = UTM_N_Corr, utm_e = UTM_E_Corr,
                  elev = Elevation1) %>%
    mutate(plot = as.factor(plot)) %>%
    ## A lot of points don't appear to have been surveyed
    filter(plot %in% gen$plot) %>%
    arrange(plot)
  
  ## Convert to spatial
  pts <- st_as_sf(d, coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  ## AOI perimeters
  district <- st_read("app_data", "SN_districts") %>%
    dplyr::select(aoi)
  
  ## Just those with points including yosemite
  yose <- st_read("C:/Users/zlsteel/Documents/geodata/NPSBoundary", "nps_boundary") %>%
    filter(UNIT_NAME == "Yosemite") %>%
    mutate(aoi = "Yosemite") %>%
    dplyr::select(aoi) %>%
    st_transform(crs = st_crs(district))
  
  keep <- st_intersects(district, pts) %>%
    apply(1, any)
  
  aois <- district[keep,] %>%
    rbind(yose) 
  
  ## Add aoi identifier to data
  d <- st_intersection(pts, district) %>%
    as.data.frame() %>%
    select(plot, forest, aoi) %>%
    merge(d, all = T) %>%
    mutate(aoi = ifelse(is.na(aoi), as.character(forest), 
                        as.character(aoi)))
  
  ## Put it all together
  d2 <- merge(d, gen[,c("plot", "treated")]) %>%
    merge(cover, all.x = T) %>%
    merge(plot_sum) %>%
    mutate(treated = ifelse(treated == "Y", "Yes", "No"))
  
  ## Save for later use
  write.csv(d2, "data/Stand/stand_prepped.csv", row.names = F)
  st_write(aois, "data/Stand/stand_aois.shp")
  st_write(aois, "app_data/stand_aois.shp")
}