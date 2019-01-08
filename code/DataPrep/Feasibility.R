## Purpose: Prepare data layers of reforestation feasibility, both for visualizing (feature) and calcultion (raster)
## Author: Zack Steel
## Date: 1/7/19

FeasPrep <- function() {
  library(tidyverse)
  library(raster)
  library(sf)
  
  ## Mask by forests for faster on the fly calculations/display
  r <- raster("data/Spatial/scenb_wgs")
  
  fn <- c("Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
          "Stanislaus", "Inyo", "Sequoia", "Sierra")
  forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    filter(FORESTNAME %in% fn) 
  
  for (fid in fn) {
    print(paste0("running ", fid))
    f <- filter(forest, FORESTNAME == fid)
    
    rp <- mask(r, f)
    trash <- crop(r, f)
  }
}