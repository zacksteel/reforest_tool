## Purpose: Prep land classification layers (i.e. WUI and Developed Rec areas)
## Author: Zack Steel
## Created: 3/4/19

LandClassPrep <- function() {
  library(tidyverse)
  library(raster)
  library(sf)
  
  ## Only areas within forests of interest
  fn <- c("Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
          "Stanislaus", "Inyo", "Sequoia", "Sierra")
  forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    filter(FORESTNAME %in% fn) 
  
  ## also read in biomass layer for creating matching rasters
  r <- raster("data/Spatial/biomassloss.tif")
  
  ## Read in rec area and WUI shapes
  rec <- st_read("data/Spatial", "DevelopedRecAreas") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84")
  wui <- st_read("data/Spatial", "WUI") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84")
  
  ## Limit to analysis area & convert to rasters
  rec_sn <- st_intersection(rec, forest) %>%
    mutate(indicator = 1) 
  wui_sn <- st_intersection(wui, forest) %>%
    mutate(indicator = 1) 
  
  rec_r <- rasterize(rec_sn, r, field = "indicator", background = 0)
  wui_r <- rasterize(wui_sn, r, field = "indicator", background = 0)
  
  ## Save for later use
  writeRaster(rec_r, "Data/Spatial/RecAreas.tif")
  writeRaster(wui_r, "Data/Spatial/WUI.tif")
}