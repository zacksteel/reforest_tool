## Purpose: Master code for data prep
## Author: Zack Steel
## Date: 1-7-19

## Prep reforestation need layer(s)
source("code/DataPrep/MortPrep.R")
MortPrep(TPA_threshold = 25)

## Prep default feasibility layer (currently just Scenario B of mechanical opportunity)
source("code/DataPrep/Feasibility.R")
FeasPrep()

## Prep Other layers