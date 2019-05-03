## Purpose: Prep sensitive species layers - SPOW PACs and Fisher core habitat
## Author: Zack Steel
## Created: 2019-04-29
## Downstream: NF_Limits.R

Sensitive_Spp <- function() 
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
  
  ## Read in SPOW and Fisher shapes & project
  spow <- st_read("data/Spatial", "SPOW_PACs") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84")
  fisher <- st_read("data/spatial", "fisherCore") %>%
    st_buffer(0) %>%
    st_transform(crs = "+proj=longlat +datum=WGS84")
  
  ## Limit to analysis area & convert to rasters
  spow_sn <- st_intersection(spow, forest) %>%
    mutate(indicator = 1) 
  fisher_sn <- st_intersection(fisher, forest) %>%
    mutate(indicator = 1) 
  
  spow_r <- rasterize(spow_sn, r, field = "indicator", background = 0)
  fisher_r <- rasterize(fisher_sn, r, field = "indicator", background = 0)
  
  ## Save for later use
  writeRaster(spow_r, "data/Spatial/spow_pacs.tif")
  writeRaster(fisher_r, "data/Spatial/fisher_cores.tif")
  
}