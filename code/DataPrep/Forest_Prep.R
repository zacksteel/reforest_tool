## Purpose: Do some house-cleaning of the forest shapefile before main app
## Author: Zack Steel
## Created: 2019-04-24

ForestPrep <- function() {
  library(sf)
  library(tm)
  
  fn <- c("", "Lassen", "Plumas", "Tahoe", "Lake Tahoe Basin", "Eldorado", 
          "Stanislaus", "Inyo", "Sequoia", "Sierra")
  
  forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    filter(FORESTNAME %in% fn) %>%
    dplyr::select(FORESTNAME, FORESTNUMB, geometry)
  
  ## same for ranger districts
  rd <- st_read("data/Spatial/S_USA.RangerDistrict", "S_USA.RangerDistrict") %>%
    filter(REGION == "05" & FORESTNUMB %in% forest$FORESTNUMB) %>%
    mutate(district = as.character(DISTRICTNA),
           FORESTNAME = as.character(FORESTNAME),
           district = removeWords(district, " Ranger District"),
           FORESTNAME = removeWords(FORESTNAME, " National Forest"),
           aoi = paste0(FORESTNAME, " - ", district)) %>%
    st_buffer(0) %>% ## fixes problems with ring self-intersection
    st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    dplyr::select(FORESTNAME, district, aoi, geometry)
  
  write_sf(forest, "data/Spatial/SN_NFs.shp")
  write_sf(rd, "data/spatial/SN_districts.shp")
}