## Purpose: Pre-process mortality spatial data
## Author: Zack Steel
## Created: 11/13/18
## Downstream: app.R

library(sf)
library(leaflet)
library(tidyverse)
library(raster)
library(rmapshaper)
library(maptools)
library(sp)

## Limit ADS Mortality layer(s) to the Sierra Nevada & environs
eswalk <- read.csv("data/Reference/EcoSectionXwalk.csv")
es <- st_read("data/Spatial", "CAEcoSections") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(ID = OBJECTID, es_code = ECOREGION_) %>%
  merge(eswalk) %>%
  filter(es_code %in% c("341D", "M261D", "M261E", "M261F"))
mort <- st_read("data/Spatial", "ADSMort15") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

## which mortality polygons are within the SN and environs?
temp <- st_intersects(mort, es)
sn.mort <- mort[lengths(temp) > 0,]

## Set some levels for visualizing and mock-up priorities
sm <- dplyr::select(sn.mort, TPA1) %>%
  mutate(class = ifelse(TPA1 < 1, 1, NA),
         class = ifelse(TPA1 >= 1 & TPA1 < 10, 2, class),
         class = ifelse(TPA1 >= 10 & TPA1 < 25, 3, class),
         class = ifelse(TPA1 >= 25 & TPA1 < 100, 4, class),
         class = ifelse(TPA1 >= 100 & TPA1 < 200, 5, class),
         class = ifelse(TPA1 >= 200, 5, class)) %>%
  dplyr::select(class) %>%
  ## summarize/dissolve by class
  group_by(class) %>%
  summarise_all(first)

## Add forest attribute
forest <- st_read("data/Spatial", "Ca_NFBoundaries") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(FORESTNAME)

## clip/intersect for each forest
for (i in 1:length(forest$FORESTNAME)) {
  forest1 <- forest[i,]
  sm.for <- st_intersection(sm, forest1)
}

# 
# sm <- st_join(sm, forest) %>%
#   ## summarize/dissolve by class
#   dplyr::select(class) %>%
#   group_by(class) %>%
#   summarise_all(first)


## Save simplified version for mapping
write_sf(sm, "data/Spatial/mort15_simple.shp")

## Convert to raster
# r <- raster()
# extent(r) <- extent(sn.mort)
# rp <- rasterize(sn.mort, r, 'TPA1', fun = "max")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = es,
              color = "black",
              fill = T,
              weight = 2,
              opacity = 1,
              fillOpacity = 0.2) %>%
  # addRasterImage(x = rp)
  addPolygons(data = trash2,
              color = "red",
              opacity = 0.1,
              weight = 0.5,
              fill = T,
              fillColor = heat.colors(6, alpha = NULL),
              fillOpacity = 0.8,
              label = trash2$class)
              #~colorFactor(c("yellow", "yellow", "orange", "orange", "red", "red"),
               #                        sm$class))