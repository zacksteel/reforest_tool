## Purpose: Master code for data prep
## Author: Zack Steel
## Date: 1-7-19

## Prep reforestation need layer(s)
# source("code/DataPrep/MortPrep.R")
# MortPrep(TPA_threshold = 25)
source("code/DataPrep/BLossPrep.R")
BLossPrep(path = "J:/My Drive/Projects/ReForestTool/GIS/DataLayers/Biomass/CABiomass_loss12_16.tif")

## Prep default feasibility layer (currently just Scenario B of mechanical opportunity)
## May also want to add UsuitableForestLandAreas layer, maybe make these mask layers optional
source("code/DataPrep/Feasibility.R")
FeasPrep()

## Prep binary land classification layers
source("code/DataPrep/LandClass.R")
LandClassPrep()

## Prep climate water deficit layer
source("code/DataPrep/CWD_Prep.R")