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
source("./utils/model-data.R")
# Chicago community shape file
chicagoCA = sf::st_read("./data/chicagoCA-shape/chicagoCA.shp") %>%
  select(area_numbe, community, perimeter, shape_len, geometry) %>% 
  mutate(area_numbe = as.numeric(area_numbe)) %>% 
  arrange(area_numbe) %>% 
  # Create latitude and longitude of the polygon centroid
  mutate(centroid_lat = geometry %>% map_dbl(~st_centroid(.x)[[2]]),
         centroid_lon = geometry %>% map_dbl(~st_centroid(.x)[[1]]))
# Construct spatial weights: 
ca.neighbors = chicagoCA %>% 
  spdep::poly2nb(queen = F) %>% 
  spdep::include.self(.)   


################################################################################
# Due to file size, the actual datasets are not included. Viewers need download
# the data from the Chicago Data Portal and load the raw trips and the processed
# trips to the environments.
################################################################################
alltripsDF = read.csv("./data/taxiTrips/{rawTrips}.csv")
tripsDF = read.csv("./data/taxiTrips/{processedTrips}.csv") %>%
  mutate(starttimestamp = chron::times(starttimestamp),
         endtimestamp = chron::times(endtimestamp))
################################################################################

# Identifying Trace 
find_prevLoc_singleDay(32, "2014-04-30") %>% 
  display_geoMap(plotTitle = "Dropoff CAs Before 9AM Loop Trips on 2014-04-29")


traceAggrDFLst = list("8"  = find_prevLoc(8),
                      "28" = find_prevLoc(28),
                      "32" = find_prevLoc(32))
saveRDS(traceAggrDFLst, "./data/scriptOutput/trace.rds")

display_tracePlot(32)

# Compute the maximum idle time between consecutive trips
maxWaitTimeDFLst = list(
  "8" = find_maxWaitTime(8),
  "28" = find_maxWaitTime(28),
  "32" = find_maxWaitTime(32))
saveRDS(maxWaitTimeDFLst, "./data/scriptOutput/maxWaitTime.rds")

# Note: the code chunk below takes hours to run. 
modelDF = generate_modelData(c(8, 28, 32), "conditional")
cleanDF = modelDF %>% 
  group_by(nextLoc) %>%
  filter(!flow %in% boxplot.stats(flow)$out) %>%
  filter(!pickups %in% boxplot.stats(pickups)$out) %>%
  ungroup
write.csv(cleanDF, "./data/modelData/modelData.csv", row.names = F)