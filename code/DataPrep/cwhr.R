## Purpose: Get CWHR data in usable format
## Project: reforest tool
## Author: Zack Steel

cwhr <- function() {
  library(tidyverse)
  library(raster)
  library(sf)
  library(fasterize)
  library(stars)
  
  ## read in shapefiles
  gb <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_GreatBasin.gdb",
                layer = "EVMid_R05_GreatBasin")
  ni <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_NorthInterior.gdb",
                layer = "EVMid_R05_NorthInterior")
  ns <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_NorthSierra.gdb",
                layer = "EVMid_R05_NorthSierra")
  ss <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_SouthSierra.gdb",
                layer = "EVMid_R05_SouthSierra")
  cv <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_CentralValley.gdb",
                layer = "EVMid_R05_CentralValley")
  
  ## Combine and get IDs
  cwhr <- do.call(rbind, list(gb, ni, ns, ss)) %>%
    dplyr::select(code = CWHR_TYPE)
  
  xwalk <- read.csv("data/Reference/cwhr_xwalk.csv") %>%
    dplyr::select(type = CWHR.Description, code = CWHR.Code)
  
  ## drop a couple redundant ones
  drop <- c("Desert Riparian (modified)", "Desert Wash (modified)",
            "Juniper (modified)", "Montane Riparian (modified)",
            "Sagebrush (modified)", "Valley Foothill Riparian (modified)")
  
  xwalk <- filter(xwalk, !(type %in% drop))
  
  xwalk2 <- data.frame(code = cwhr$code) %>%
    unique() %>%
    mutate(ID = as.integer(code)) %>%
    merge(xwalk, all.x = T) %>%
    unique() %>%
    arrange(ID)
  
  ## Bring in raster template
  bloss <- raster("data/Spatial/biomassloss.tif") #biomass loss
  
  r <- merge(cwhr, xwalk2, by = "code") %>%
    fasterize(bloss, field = "ID")
  
  ## create rat, add categories
  r <- ratify(r)
  types <- filter(xwalk2, ID %in% levels(r)[[1]]$ID)
  levels(r)[[1]] <- cbind(levels(r)[[1]], type = types$type)
  
  r2 <- projectRaster(r, crs = crs(bloss))
  
  ## Save for later
  writeRaster(r2, filename = "data/Spatial/cwhr.tif")
  
  #### Test w/ prioritization
  p <- raster("scratch/prioritization.tif")
  
  ## For each priority level, count up cwhr pixels
  for(i in 1:4) {
    
    temp_p <- subs(p, data.frame(ID = i, to = 1))
    temp_c <- crop(r2, extent(temp_p)) %>%
      mask(temp_p)
  }
}
