## Purpose: Prep high-severity core layer
## Author: Zack Steel
## Created: 2019-04-29
## Downstream: NF_Limits.R

hs_core200 <- function() 
  {
  library(tidyverse)
  library(raster)
  library(sf)
  
  ## Just considering areas within Sierra Nevada national forests
  fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
          "Stanislaus", "Inyo", "Sequoia", "Sierra")
  forest <- st_read("data/Spatial", "SN_NFs")
  
  ## also read in biomass layer for creating matching rasters
  r <- raster("data/Spatial/biomassloss.tif")
  
  ## Read in high-severity layer; already filtered to high-severity areas between 2012 and 2016 inclusive
  hs200 <- st_read("data/Spatial", "High_Sev12_17") %>%
    ## Pulling out the zone 2 and 3 areas according to the Tamm Review (aka 200m core areas); slow step ~30min
    st_buffer(-200) 
  
  hs200 <- st_transform(hs200, crs = "+proj=longlat +datum=WGS84")
  
  ## Limit to analysis area & convert to rasters
  hs_sn <- st_intersection(hs200, forest) %>%
    mutate(indicator = 1) 
  hs_r <- rasterize(hs_sn, r, field = "indicator", background = 0)
  
  ## Save for later use
  writeRaster(hs_r, "data/Spatial/hs200.tif")
}