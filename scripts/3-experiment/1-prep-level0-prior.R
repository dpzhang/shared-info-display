library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(tibble)
library(rlist)
library(chron)
library(tmap)
library(sf)
library(spdep)
library(sp)
library(purrr)
library(parallel)
library(knitr)
library(urca)
library(brms)
library(rjson)
library(tidyr)
library(stringr)
source("../1-taxiTrips/utils/model-data.R")
source('./utils/level0-prior.R')
targetCA = c(8, 28, 32)
# Chicago community shape file
chicagoCA = sf::st_read("../1-taxiTrips/data/chicagoCA-shape/chicagoCA.shp") %>%
  select(area_numbe, community, perimeter, shape_len, geometry) %>% 
  mutate(area_numbe = as.numeric(area_numbe)) %>% 
  arrange(area_numbe) %>% 
  mutate(centroid_lat = geometry %>% map_dbl(~st_centroid(.x)[[2]]),
         centroid_lon = geometry %>% map_dbl(~st_centroid(.x)[[1]]))

# Construct spatial weights: 
ca.neighbors = chicagoCA %>% 
  spdep::poly2nb(queen = F) %>%   
  spdep::include.self(.)

alltripsDF = read.csv("../1-taxiTrips/data/taxiTrips/rawTrips.csv") %>%
  mutate(starttimestamp = chron::times(starttimestamp),
         endtimestamp = chron::times(endtimestamp))
tripsDF = read.csv("../1-taxiTrips/data/taxiTrips/finalTrips.csv") %>%
  mutate(starttimestamp = chron::times(starttimestamp),
         endtimestamp = chron::times(endtimestamp))

# Loading necessary pre-processed objects
traceAggrDFLst = readRDS("../1-taxiTrips/data/scriptOutput/trace.rds")
maxWaitTimeDFLst = readRDS("../1-taxiTrips/data/scriptOutput/maxWaitTime.rds")

modelDF = read.csv("../1-taxiTrips/data/modelData/cleanModelData.csv") %>%
  rename(pickup = pickups)
mod = readRDS("../2-counterfactual/model/fitted-model.rds")

scenarioOrder = c("2014-07-22", "2014-05-13", "2015-05-21", "2014-06-05", 
                  "2015-06-25", "2014-11-07", "2015-06-30", "2015-10-26", 
                  "2015-10-22", "2015-10-14", "2014-09-26", "2014-12-05", 
                  "2015-08-28", "2014-05-22", "2014-10-24", "2014-09-29")

robustOrder = c("2014-07-22", "2015-10-26", "2014-10-24", "2014-09-26", 
                "2015-10-14", "2015-08-28", "2014-05-13", "2015-06-25", 
                "2014-12-05", "2015-10-22", "2014-06-05", "2014-09-29", 
                "2014-11-07", "2014-05-22", "2015-05-21", "2015-06-30")

# Compute level-0 Prior for the main experiment
driverPriorLst = compute_driverPrior_data_allScenarios(scenarioOrder)
saveRDS(driverPriorLst, file = "./data/level0Prior.Rdata")