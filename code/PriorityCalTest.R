## Purpose: Test prioritization calculations
## Author: Zack Steel
## Upstream: app.R
## Downstream: 

library(raster)
library(tidyverse)
library(sf)
library(stringr)

## Prioritization calculation starts at about line 630 of the app.R

## variables to take the place of reactive functions
forest <- "Eldorado"
dist <- "Eldorado - Georgetown"
mech <- "Moderate constraints"
need <- 50

## district shape
district <- st_read("app_data", "SN_districts")
aoi_dist <- filter(district, aoi == dist)

## read in national forest-specific rasters
# bloss <- raster(paste0("app_data/NF_Limits/bloss_", forest, ".tif"))
bloss <- raster(paste0("app_data/NF_Limits/intloss_", forest, ".tif"))
ra <- if(mech == "Moderate constraints") {
  raster(paste0("app_data/NF_Limits/sb_", forest, ".tif")) } else {
    if(mech == "Fewer constraints") {
      raster(paste0("app_data/NF_Limits/sd_", forest, ".tif"))
    }
  }

rec <- raster(paste0("app_data/NF_Limits/rec_", forest, ".tif"))
wui <- raster(paste0("app_data/NF_Limits/wui_", forest, ".tif"))
cwd <- raster(paste0("app_data/NF_Limits/cwd_", forest, ".tif"))
hs <- raster(paste0("app_data/NF_Limits/hs_", forest, ".tif"))
spow <- raster(paste0("app_data/NF_Limits/spow_", forest, ".tif"))
fisher <- raster(paste0("app_data/NF_Limits/fisher_", forest, ".tif"))

## Limit the range of biomass loss considered based on user-defined threshold
blmin <- need/100
minmask <- cut(bloss, breaks = c(-0.01,blmin,1)) - 1 ## cut returns values of 1 & 2, -1 ensures 0 and 1 raster


#### More variables for user input 
hzs_in <- 0
caspo_in <- 0
fisher_in <- 0
wui_in <- 0
rec_in <- 0
cwd_in <- 0

## Run raster calculation based on user-defined weights
pr <- (bloss + #More biomass loss increases priority
         hs*hzs_in + #Zone 2 of high-severity increases priority
         spow*caspo_in + #SPOW pacs can either increase or decrease priority
         fisher*fisher_in + #Fisher core areas can either increase or decrease priority
         wui*wui_in + #being in the WUI increases priority
         rec*rec_in + #being in a rec area increases priority
         ## scale non-binary rasters with max of 1
         cwd/maxValue(cwd)*cwd_in) 
  # cwd/cellStats(cwd, stat='max')*input$cwd) * #high cwd decreases priority 
  # ra * minmask #mask out areas of mechanical constraints & below need threshold

## Limit to AOI
pr_aoi <- crop(pr, aoi_dist, snap = "in") %>%
  mask(mask = aoi_dist)

## Scale to have a min of 0 and max of 1; also adjust need treshold
# pr_aoi2 <- pr_aoi / maxValue(pr_aoi) #maxp
pr_aoi2 <- (pr_aoi - minValue(pr_aoi)) / (maxValue(pr_aoi) - minValue(pr_aoi))
# need_adj <- (blmin - minValue(pr_aoi)) / (maxValue(pr_aoi) - minValue(pr_aoi))

## mask out areas of mechanical constraints & below need threshold
## ra and minmaks have broader extents, only calculating intersection as desired, suppressing warning
pr_aoi3 <- suppressWarnings(pr_aoi2 * ra * minmask)

## find adjusted minimum value
need_adj <- min(pr_aoi3[pr_aoi3 > 0])

#### moved the normalizing step below the next mask step



#### Rather than setting breaks by quantile, which becomes more problematic with the severity data because we more often have a lot of the same values, 

## Reclassify to three classes based on quantile thirds above the minimum threshold
## occasionally have problems of breaks not being unique. add a bit to upper quantiles
q3 <- quantile(pr_aoi3[pr_aoi3 >= need_adj], probs = c(0, 0.33, 0.67, 1)) %>%
  as.numeric() #+ c(-0.001, -0.0005, 0.0005, 0.001)
# q3 <- quantile(pr_aoi, probs = c(0, 0.33, 0.67, 1)) + c(-0.001, -0.0005, 0.0005, 0.001)
breaks <- c(cellStats(pr_aoi3, stat="min")-0.001, q3)

## Reclassify into areas of no need and approximate thirds of the remaining
# pr3 <- cut(pr_aoi, breaks = breaks) %>%
#   subs(data.frame(ID = c(1,2,3,4), Priority = c("Lower mortality",
#                                                 "3rd Priority", "2nd Priority", "1st Priority")))

#### Cut throws an error when breaks are not unique, how does reclassify handle this?
rcl <- matrix(c(breaks[1], breaks[2], 1,
                breaks[2], breaks[3], 2,
                breaks[3], breaks[4], 3,
                breaks[4], breaks[5], 4),
              ncol = 3, byrow = T)
pr3 <- reclassify(pr_aoi3, rcl, right = NA) %>%
  subs(data.frame(ID = c(1,2,3,4), Priority = c("Lower mortality",
                                                "3rd Priority", "2nd Priority", "1st Priority")))



## Testing veg plot

#### Test w/ prioritization
#### https://www.r-exercises.com/2018/01/10/spatial-data-analysis-introduction-to-raster-processing-part-3/
p <- raster("scratch/prioritization.tif")
r <- raster("app_data/evt12.grd")

r2 <- crop(r, p) %>%
  mask(p, method = "ngb")

ptab <- data.frame(prioritization = c(1,2,3,4), 
                   p_lab = c("Lower mortality",
                             "3rd Priority", "2nd Priority", "1st Priority"))

ct <- crosstab(p, r2, long = T) %>%
  merge(levels(r)[[1]], by.x = "ID2", by.y = "ID") %>%
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

