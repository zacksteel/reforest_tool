## Purpose: Prepare regen data from plot surveys for shiny app
## Author: Zack Steel
## Downstream: app.R

regen_prep <- function() {
  library(tidyverse)
  
  d <- read.csv("data/Stand/Regen2017.csv")
  stand <- read.csv("app_data/stand_prepped.csv") %>%
    dplyr::select(plot, forest, aoi, loc, treated)
  
  ## Species x-walk
  sdf <- data.frame(code = c("PIPO", "QUKE", "CADE", "ABCO", "PILA", "QUCH", 
                             "PIJE", "SALIX", "PSME", "CONU"),
                    species = c("Ponderosa pine", "Black oak", "Incense cedar",
                                "White fir", "Sugar pine", "Canyon live oak",
                                "Jeffrey pine", "Willow sp.", "Douglas fir",
                                "Dogwood"))
  
  ## First calculate plot area as a proportion of a hectare
  plot_area <- pi*12.6^2 / 10000 #12.6m radius; returns hectares
  
  ## Just interested in live regen, combining seedling and sapling
  d2 <- dplyr::select(d, plot = Plot.Name, code = SaplingSpeciesCode, status = Status,
               count = Count) %>%
    filter(status == "L") %>%
    filter(code %in% c("PIPO", "PILA", "CADE", "ABCO", "QUKE", "QUCH")) %>%
    merge(sdf) %>%
    group_by(plot, code, species) %>%
    summarise(spha = round(sum(count) / plot_area, 0)) %>%
    merge(stand)
  
  write.csv(d2,"app_data/regen_prepped.csv", row.names = F)
}