## Purpose: Do some house-cleaning of the forest shapefile before main app
## Author: Zack Steel
## Created: 2019-04-24

ForestPrep <- function() {
  library(sf)
  
  fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
          "Stanislaus", "Inyo", "Sequoia", "Sierra")
  
  forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    filter(FORESTNAME %in% fn) %>%
    dplyr::select(FORESTNAME, geometry)
  
  write_sf(forest, "data/Spatial/SN_NFs.shp")
}