## Purpose: Prepare data layers of reforestation feasibility, both for visualizing (feature) and calcultion (raster)
## Author: Zack Steel
## Date: 1/7/19
## Upstream: BLossPrep.R
## Downstream: app.R

FeasPrep <- function() {
  library(tidyverse)
  library(raster)
  library(sf)
  
  ## Resampling mask layer to match biomass layer
  ## scenario B layer is much finer than 1ha pixels, if processing allows, maybe go down to 30m eventually
  r <- raster("data/Spatial/scenb_wgs")
  bloss <- raster("data/spatial/biomassloss.tif")
  
  r2 <- resample(r, bloss) %>%
    round(0) 
  
  ## Save for use in app
  writeRaster(r2, "Data/Spatial/scenb.tif")

}