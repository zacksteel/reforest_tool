## Purpose: Pre-process biomass loss layer
## Author: Zack Steel
## Created: 3/4/19
## Downstream: app.R

BLossPrep <- function(path) #path to raster layer
{
  
  library(sf)
  library(tidyverse)
  library(raster)
  
  ## Read in biomass loss layer
  path <- "J:/My Drive/Projects/ReForestTool/GIS/DataLayers/Biomass/CABiomass_loss12_16_wgs.tif"
  bloss <- raster(path) 
    ## real slow for some reason (~5 min); do in Arc first
    # projectRaster(crs = "+proj=longlat +datum=WGS84")
  
  ## Limit biomass loss to the Sierra Nevada & environs
  eswalk <- read.csv("data/Reference/EcoSectionXwalk.csv")
  es <- st_read("data/Spatial", "CAEcoSections") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    dplyr::select(ID = OBJECTID, es_code = ECOREGION_) %>%
    merge(eswalk) %>%
    filter(es_code %in% c("341D", "M261D", "M261E", "M261F"))
  
  bloss_sn <- crop(bloss, es) %>%
    mask(es)
  
  ## Write for use in app; overwriting seems to mess things up. Delete manually if needed.
  writeRaster(bloss_sn, "data/Spatial/biomassloss.tif", format = "GTiff")
  
}