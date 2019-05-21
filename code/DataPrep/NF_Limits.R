## Purpose: Limit data raster to national forests in order to reduce app processing time
## Author: Zack Steel
## Created: 2019-04-24
## Upstream: Forest_Prep & specific raster prep files
## Downstream: app.R

NF_Limits <- function() {
  
  library(tidyverse)
  library(sf)
  library(raster)
  
  ## Read in Sierra-wide rasters
  bloss <- raster("data/Spatial/biomassloss.tif") #biomass loss
  sb <- raster("data/Spatial/scenb.tif") #treatment feasibility
  rec <- raster("data/Spatial/RecAreas.tif") 
  wui <- raster("data/Spatial/WUI.tif")
  cwd <- raster("data/Spatial/cwd_sn.tif") #cimate water deficit
  hs <- raster("data/Spatial/hs200.tif") #zone 2 & 3 or high-severity areas 2012-2017
  spow <- raster("data/Spatial/spow_pacs.tif") #spotted owl PACs
  fisher <- raster("data/Spatial/fisher_cores.tif") # fisher core areas
  fs <- raster("data/Spatial/FS_area.tif") # Forest service land
  
  ## Read in Sierra Nevada national forest shape
  forest_sf <- st_read("data/Spatial", "SN_NFs")
  
  ## Make a list of rasters to work through 
  r_l <- list(bloss, sb, rec, wui, cwd, hs, spow, fisher, fs)
  names(r_l) <- c("bloss", "sb", "rec", "wui", "cwd", "hs", "spow", "fisher", "fs")
  
  ## combinations for each national forest and dataset
  d <- expand.grid(forest = unique(forest_sf$FORESTNAME),
                   layer = names(r_l))
  
  ## Set up function to subset a datalayer by a forest
  f1 <- function(forest, layer) {
    ## If file already exists, skip
    fn <- paste0("app_data/nf_limits/",
                 layer, "_", forest, ".tif")
    if(file.exists(fn)) {print(paste0(fn, " exists, moving on"))} else
    {
      ## Pull out a forest polygon
      f <- forest_sf[forest_sf$FORESTNAME == forest,]
      ## Pull out raster of interest
      r <- r_l[[layer]]
      ## Crop and Mask
      r2 <- crop(r, f) %>%
        mask(mask = f)
      ## Save
      writeRaster(r2, filename = paste0("app_data/nf_limits/",
                                        layer, "_", forest, ".tif")) 
    }
  }
  
  ## Loop through each combination & run function
  for(i in 1:nrow(d)) {
    f1(forest = d[i,"forest"],
       layer = d[i,"layer"])
  }
  
  
}