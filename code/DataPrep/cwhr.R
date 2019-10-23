## Purpose: Get CWHR data in usable format
## Project: reforest tool
## Author: Zack Steel

cwhr <- function() {
  library(tidyverse)
  library(raster)
  library(sf)
  library(fasterize)
  library(stars)
  
  ## read in shapefiles
  gb <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_GreatBasin.gdb",
                layer = "EVMid_R05_GreatBasin")
  ni <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_NorthInterior.gdb",
                layer = "EVMid_R05_NorthInterior")
  ns <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_NorthSierra.gdb",
                layer = "EVMid_R05_NorthSierra")
  ss <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_SouthSierra.gdb",
                layer = "EVMid_R05_SouthSierra")
  cv <- st_read(dsn = "G:/My Drive/Projects/ReForestTool/GIS/DataLayers/S_USA.EVMid_R05_CentralValley.gdb",
                layer = "EVMid_R05_CentralValley")
  
  ## Combine and get IDs
  cwhr <- do.call(rbind, list(gb, ni, ns, ss)) %>%
    dplyr::select(code = CWHR_TYPE)
  
  xwalk <- read.csv("data/Reference/cwhr_xwalk.csv") %>%
    dplyr::select(type = CWHR.Description, code = CWHR.Code, treed) 
  
  ## drop a couple redundant ones
  drop <- c("Desert Riparian (modified)", "Desert Wash (modified)",
            "Juniper (modified)", "Montane Riparian (modified)",
            "Sagebrush (modified)", "Valley Foothill Riparian (modified)")
  
  xwalk <- filter(xwalk, !(type %in% drop)) 
  
  xwalk2 <- data.frame(code = cwhr$code) %>%
    unique() %>%
    mutate(ID = as.integer(code)) %>%
    merge(xwalk, all.x = T) %>%
    unique() %>%
    arrange(ID)
  
  ## Bring in raster template
  bloss <- raster("data/Spatial/biomassloss.tif") #biomass loss
  
  r <- merge(cwhr, xwalk2, by = "code") %>%
    fasterize(bloss, field = "ID") %>%
    projectRaster(crs = crs(bloss), method = "ngb") #nearest neighbor (ngb) needed for categorical values
  
  ## create rat, add categories
  r <- ratify(r)
  types <- filter(xwalk2, ID %in% levels(r)[[1]]$ID) %>%
    mutate(type = as.character(type),
           type2 = ifelse(treed == 1, type, "Non-forest"),
           type2 = ifelse(is.na(type2), "Non-forest", type2))
  levels(r)[[1]] <- cbind(levels(r)[[1]], type = types$type2)
  
  ## Save for later
  writeRaster(r, filename = "data/Spatial/cwhr.grd", overwrite = T)
  
  #### Test w/ prioritization
  #### https://www.r-exercises.com/2018/01/10/spatial-data-analysis-introduction-to-raster-processing-part-3/
  p <- raster("scratch/prioritization.tif")

  r2 <- crop(r, p) %>%
    raster::resample(p, metho = "ngb")
  
  ptab <- data.frame(prioritization = c(1,2,3,4), 
                     p_lab = c("Lower mortality",
                                        "3rd Priority", "2nd Priority", "1st Priority"))
  
  ct <- crosstab(p, r2, long = T) %>%
    merge(levels(r)[[1]], by.x = "layer", by.y = "ID") %>%
    merge(ptab) %>%
    group_by(type, p_lab) %>%
    summarize(freq = sum(Freq)) %>%
    nest(data = c(p_lab, freq)) %>%
    mutate(type_tot = map_dbl(data, ~{sum(.x$freq)})) %>%
    unnest(cols = c(data)) %>%
    arrange(desc(type_tot), desc(freq)) %>%
    ungroup() 
  
  filter(ct, p_lab != "Lower mortality") %>%
    mutate(type = fct_relevel(type, as.character(unique(ct$type)))) %>%
    ggplot(aes(x = type, y = freq, fill = p_lab)) +
    geom_bar(position = position_dodge(preserve = "single"), 
             stat = "identity", color = "grey30") +
    scale_fill_manual(name = NULL, values = c("yellow", "orange", "red")) +
    ylab("Hectares") + xlab(NULL) +
    scale_y_continuous(trans='log10') +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

}
