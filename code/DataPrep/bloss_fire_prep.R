## Purpose: Create an integrated change layer - combinging LEMMA biomass and fire severity
## Author: Zack Steel
## Upstream: Bloss_prep.R
## Downstream: NF_Limits.R

bloss_fire_prep <- function()
{
  library(raster)
  library(tidyverse)
  library(sf)
  library(fasterize)
  
  ## bring in prepped biomass loss layer
  bloss <- raster("data/Spatial/biomassloss.tif")
  ## bring in fire severity layer
  sev <- st_read("data/Spatial/Sev12_16.shp") %>%
    st_transform(crs = st_crs(bloss)) %>%
    ## define change by class medians
    mutate(change = case_when(BURNSEV == 1 ~ 0, # 0%
                              BURNSEV == 2 ~ 0.05, # 0% < change < 10%
                              BURNSEV == 3 ~ 0.17, # 10% <= change < 25%
                              BURNSEV == 4 ~ 0.37, # 25% <= change < 50%
                              BURNSEV == 5 ~ 0.62, # 50 <= change < 75%
                              BURNSEV == 6 ~ 0.82, # 75 <= change < 90%
                              BURNSEV == 7 ~ 0.95)) # >= 90%
  sev_r <- fasterize(sf = sev, 
                     raster = bloss,
                     field = "change")
  
  ## Average the two raster. This is slow (~10 min).
  mn_chg <- overlay(bloss, sev_r, fun = function(x) mean(x, na.rm=T))
  ## prioritize severity data
  # chg <- overlay(x = bloss, y = sev_r, fun = function(x,y) {
  #   ifelse(is.na(y), x, y)
  # })
  # chg <- round(chg, 2)
  
  ## Save
  writeRaster(chg, "data/Spatial/integrated_loss.tif", format = "GTiff")
}
