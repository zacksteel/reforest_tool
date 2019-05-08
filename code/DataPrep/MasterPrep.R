## Purpose: Master code for data prep
## Author: Zack Steel
## Date: 1-7-19


# source("code/DataPrep/MortPrep.R")
# MortPrep(TPA_threshold = 25)

## Limit National forests considered and project;
## Simplify district shapefiles for improved app speed
source("code/DataPrep/ForestPrep.R")
ForestPrep()

## Prep reforestation need layer(s)
source("code/DataPrep/BLossPrep.R")
BLossPrep(path = "J:/My Drive/Projects/ReForestTool/GIS/DataLayers/Biomass/CABiomass_loss12_16.tif")

## Prep default feasibility layer (currently just Scenario B of mechanical opportunity)
## May also want to add UsuitableForestLandAreas layer, maybe make these mask layers optional
source("code/DataPrep/Feasibility.R")
FeasPrep()

## Prep binary land classification layers
source("code/DataPrep/LandClass.R")
LandClassPrep()

## Prep high-severity core layer
source("code/DataPrep/hs_core200.R")
hs_core200()

## Prep sensitive species layers
source("code/DataPrep/Sensitive_Spp.R")
Sensitive_Spp()

## Prep climate water deficit layer
source("code/DataPrep/CWD_Prep.R")

## Limit each raster layer to each national forest to avoid on-the-fly calculations
source("code/Data/Prep/NF_Limits.R")
NF_Limits()