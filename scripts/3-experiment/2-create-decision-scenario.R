library(dplyr)
library(magrittr)
library(tibble)
library(rlist)
library(purrr)
library(knitr)
library(urca)
library(brms)
library(rjson)
library(stringr)
source("./utils/data-gen.R")
targetCA = c(8, 28, 32)
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
 
start_time <- Sys.time()
experiment_stage1DataLst = generate_stage1_data(scenarioOrder,'experiment', 2333)
end_time <- Sys.time()
end_time - start_time
saveRDS(experiment_stage1DataLst, file = "./data/interfaceData/experiment/stage1InterfaceData-experiment.Rdata")


start_time <- Sys.time()
robustTrial_stage1DataLst = generate_stage1_data(robustOrder, 'robustTrial', 2333)
end_time <- Sys.time()
end_time - start_time
saveRDS(robustTrial_stage1DataLst, file = "./data/interfaceData/robustTrial/stage1InterfaceData-robustTrial.Rdata")