## Purpose: Pre-process mortality spatial data
## Author: Zack Steel
## Created: 11/13/18
## Downstream: app.R

MortPrep <- function(TPA_threshold = 25) {
  library(sf)
  library(tidyverse)
  library(raster)
  
  # library(leaflet)
  # library(cowplot)
  # library(rmapshaper)
  # library(maptools)
  # library(ggmap)
  # library(ggspatial)
  
  ## Limit ADS Mortality layer(s) to the Sierra Nevada & environs
  eswalk <- read.csv("data/Reference/EcoSectionXwalk.csv")
  es <- st_read("data/Spatial", "CAEcoSections") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    dplyr::select(ID = OBJECTID, es_code = ECOREGION_) %>%
    merge(eswalk) %>%
    filter(es_code %in% c("341D", "M261D", "M261E", "M261F"))
  
  mort <- st_read("data/Spatial", "ADSMort15") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84")
    # st_transform(crs = st_crs(sb))
  
  ## which mortality polygons are within the SN and environs?
  temp <- st_intersects(mort, es)
  sn.mort <- mort[lengths(temp) > 0,]
  
  sm <- dplyr::select(sn.mort, TPA1) %>%
    mutate(need = ifelse(TPA1 >= TPA_threshold, 1, 0))
  
  ## Set some levels for visualizing and mock-up priorities
  # sm <- dplyr::select(sn.mort, TPA1) %>%
  #   mutate(class = ifelse(TPA1 < 1, 1, NA),
  #          class = ifelse(TPA1 >= 1 & TPA1 < 10, 2, class),
  #          class = ifelse(TPA1 >= 10 & TPA1 < 25, 3, class),
  #          class = ifelse(TPA1 >= 25 & TPA1 < 100, 4, class),
  #          class = ifelse(TPA1 >= 100 & TPA1 < 200, 5, class),
  #          class = ifelse(TPA1 >= 200, 5, class),
  #          priority = ifelse(class %in% c(1,2), 1, NA),
  #          priority = ifelse(class %in% c(3), 2, priority),
  #          priority = ifelse(class %in% c(4,5), 3, priority),
  #          priority = as.factor(priority))
  
  ## summarize/dissolve by need
  sm.simple <- dplyr::select(sm, need) %>%
    group_by(need) %>%
    summarise_all(first)
  
  ## Save simplified version for mapping
  write_sf(sm.simple, "data/Spatial/mort15_simple.shp")
  
  ## convert to raster and save
  ## Use scenario B layer as template
  sb <- raster("data/Spatial/scenb_wgs") 
  
  r <- raster()
  extent(r) <- extent(sm.simple)

  #### pretty slow, big raster 
  rp <- rasterize(sm.simple, sb, 'need', fun = "max")
  writeRaster(rp, "data/Spatial/mort15_simple.tif", format = "GTiff")
  
  ## Add forest attribute
  # forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
  #   st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  #   dplyr::select(FORESTNAME)
  
  ## clip/intersect for each forest
  # for (i in 1:length(forest$FORESTNAME)) {
  #   ## pull out area of interest
  #   forest1 <- forest[i,] 
  #   sm.for <- st_intersection(sm, st_buffer(forest1, 0)) ## buffer operation necessary lassen with a hole
  #   
  #   ## set up basemap
  #   # bbox <- setNames(st_bbox(forest1), c("left", "bottom", "right", "top"))
  #   # map <- get_openstreetmap(bbox = bbox)
  #   # #setting zoom to 9 gives us a bit of padding around the bounding box
  #   # basemap <- get_stamenmap(maptype = "terrain", bbox = bbox)
  #   
  #   # basemap %>%
  #   p <- ggplot() +
  #     geom_sf(data=sm.for, aes(fill = priority), col = NA) + 
  #     scale_fill_manual(values = c("#CC9933", "#996600", "#993300"),
  #                       name = "Priority Level",
  #                       breaks = c(1,2,3),
  #                       labels = c("Low", "Moderate", "High")) +
  #     geom_sf(data=forest1, col = "darkgreen", fill = NA) +
  #     theme_bw() +
  #     annotation_scale(location = "bl", width_hint = 0.3) +
  #     annotation_north_arrow(location = "bl", which_north = "true", 
  #                            height = unit(0.3, "in"), width = unit(0.3, "in"),
  #                            pad_x = unit(0.3, "in"), pad_y = unit(0.28, "in"),
  #                            style = north_arrow_fancy_orienteering)
  #   
  #   save_plot(p, filename = paste0("maps/", forest1$FORESTNAME, ".png"), base_height = 6)
  #   
  #   ## Also save individual shapefiles
  #   write_sf(sm.for, paste0("data/Outputs/Priorities_Spatial/", forest1$FORESTNAME, ".shp"))
  # }
}



# 
# sm <- st_join(sm, forest) %>%
#   ## summarize/dissolve by class
#   dplyr::select(class) %>%
#   group_by(class) %>%
#   summarise_all(first)

## Convert to raster
# r <- raster()
# extent(r) <- extent(sn.mort)
# rp <- rasterize(sn.mort, r, 'TPA1', fun = "max")

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = es,
#               color = "black",
#               fill = T,
#               weight = 2,
#               opacity = 1,
#               fillOpacity = 0.2) %>%
#   # addRasterImage(x = rp)
#   addPolygons(data = trash2,
#               color = "red",
#               opacity = 0.1,
#               weight = 0.5,
#               fill = T,
#               fillColor = heat.colors(6, alpha = NULL),
#               fillOpacity = 0.8,
#               label = trash2$class)
              #~colorFactor(c("yellow", "yellow", "orange", "orange", "red", "red"),
               #                        sm$class))

# leaflet() %>%
#   addTiles() %>%
#   # addPolygons(data = aoi(),
#   #             color = "blue",
#   #             fill = F,
#   #             opacity = 0.8,
#   #             weight = 5) %>%
#   # addPolygons(data = forest,
#   #             color = "black",
#   #             fillColor = "green",
#   #             fill = T,
#   #             weight = 2,
#   #             opacity = 1,
#   #             fillOpacity = 0.2,
#   #             label = forest$FORESTNAME) %>%
#   addPolygons(data = trash,
#               color = "grey",
#               opacity = 0.5,
#               fill = T,
#               weight = 2,
#               fillColor = "black",
#               # fillColor = c("#CC9933", "#996600", "#993300"),
#               fillOpacity = 0.5,
#               label = trash$priority)