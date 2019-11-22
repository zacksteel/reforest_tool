## Purpose: Prep veg layer for summarizing prioritizatin results
## Project: Reforestation tool
## Author: Zack Steel
## Upstream: BlossPrep.R
## Downstream: app.R

evt_prep <- function() {
  
  library(raster)
  library(sf)
  library(tidyverse)
  
  ## Bring in existing veg layer (as of 2012)
  evt <- raster("data/Spatial/evt_wgs84")
  
  ## Get raster atribute table
  rat <- levels(evt)[[1]]
  
  ## Bring in biomass layer template
  bloss <- raster("data/Spatial/biomassloss.tif")
  
  ## rescale evt using nearest neighbor
  evt2 <- crop(evt, bloss) %>%
    resample(bloss, method = "ngb") %>% # This is slow ~15min
    mask(bloss)
  
  ## cut down on rat
  rat2 <- dplyr::select(rat, ID, type = EVT_GP_N, order = EVT_ORDER) %>%
    filter(ID %in% unique(evt2)) %>%
    mutate(treed = ifelse(order == "Tree-dominated", 1, 0),
           type2 = ifelse(treed == 1, as.character(type), "Non-forest"),
           ## New unique IDs
           ID2 = as.integer(as.factor(type2)))
  
  ## reclassify
  rcl <- dplyr::select(rat2, ID, ID2)
  evt3 <- subs(evt2, rcl, by = "ID", which = "ID2")
  
  ## create rat, add categories
  rat3 <- dplyr::select(rat2, ID = ID2, type2) %>%
    unique()
  
  r <- ratify(evt3)
  
  levels(r)[[1]] <- cbind(levels(r)[[1]], type = rat3$type2)
  
  ## Save for later
  writeRaster(r, filename = "app_data/evt12.grd", overwrite = T)
}
