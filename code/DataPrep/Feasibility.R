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
  bloss <- raster("data/Spatial/biomassloss.tif")
  
  r2 <- resample(r, bloss) %>%
    round(0) 
  
  ## prep scenario D as well
  rd <- raster("data/spatial/scend_wgs") / 10 # converts 10 to 1

  rd2 <- resample(rd, bloss) %>%
    round(0)
  
  ## Create a Forest Service or not layer
  rcl <- matrix(c(0, 1, 1, NA, NA, 0), ncol = 3, byrow = T)
  fs <- reclassify(r2, rcl, right = NA)
  
  ## Save for use in app
  writeRaster(r2, "data/Spatial/scenb.tif")
  writeRaster(rd2, "data/Spatial/scend.tif")
  writeRaster(fs, "data/Spatial/FS_area.tif")

}