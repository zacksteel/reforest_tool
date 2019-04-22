## Purpose: Pre-process climate water defecit layer
## Author: Zack Steel
## Created: 2019-04-22
## Downstream: app.R

cwd_prep <- function() 
{
  
  library(sf)
  library(tidyverse)
  library(raster)
  
  ## Read in biomass loss layer
  ## Pre-projected in Arc to WGS1984
  cwd <- raster("data/spatial/cwd1981_2010_ave_wgs84.tif") 
  
  ## Limit biomass loss to the Sierra Nevada & environs
  eswalk <- read.csv("data/Reference/EcoSectionXwalk.csv")
  es <- st_read("data/Spatial", "CAEcoSections") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
    # st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    dplyr::select(ID = OBJECTID, es_code = ECOREGION_) %>%
    merge(eswalk) %>%
    filter(es_code %in% c("341D", "M261D", "M261E", "M261F"))
  
  cwd_sn <- crop(cwd, es) %>%
    mask(es)
  
  ## Align and resample according to biomass loss layer
  bloss <- raster("data/spatial/biomassloss.tif")
  cwd_sn2 <- resample(cwd_sn, bloss)
  
  ## Write for use in app; overwriting seems to mess things up. Delete manually if needed.
  writeRaster(cwd_sn2, "data/Spatial/cwd_sn.tif", format = "GTiff")
  
}