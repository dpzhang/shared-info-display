library(rjson)
library(dplyr)
library(magrittr)
library(tidyverse)
library(purrr)
library(stringr)
library(emdist)
library(rlist)
library(infer)
library(brms)
library(tidybayes)
library(parallel)
library(stringr)
source('./utils/process-individual.R')
targetCA = c(8, 28, 32)
modelDF = read.csv('../1-taxiTrips/data/modelData/modelData.csv')
mod = readRDS("../2-counterfactual/model/fitted.rds")
scenarioOrder = c("2014-07-22", "2014-05-13", "2015-05-21", "2014-06-05", 
                  "2015-06-25", "2014-11-07", "2015-06-30", "2015-10-26", 
                  "2015-10-22", "2015-10-14", "2014-09-26", "2014-12-05", 
                  "2015-08-28", "2014-05-22", "2014-10-24", "2014-09-29")
robustOrder = c("2014-07-22", "2015-10-26", "2014-10-24", "2014-09-26", 
                "2015-10-14", "2015-08-28", "2014-05-13", "2015-06-25", 
                "2014-12-05", "2015-10-22", "2014-06-05", "2014-09-29", 
                "2014-11-07", "2014-05-22", "2015-05-21", "2015-06-30")

# Individual-level Processing: Main Experiment
start_time <- Sys.time()
experiment_stage1DF = process_JSON_data('../4-database/queriedData/stage1Experiment.json', 1) %>%
  integrate_level1_networkData('experiment') %>%
  compute_AE
end_time <- Sys.time()
end_time - start_time
write.csv(experiment_stage1DF, '../3-experiment/data/collectedData/experiment/stage1Data-experiment.csv', row.names = F)

# Individual-level Processing: Robust Trial
start_time <- Sys.time()
robustTrial_stage1DF = process_JSON_data('../4-database/queriedData/stage1RobustTrial.json', 1) %>%
  integrate_level1_networkData('robustTrial') %>%
  filter(west_guess > 0 & north_guess > 0 & east_guess > 0) %>%
  compute_AE
end_time <- Sys.time()
end_time - start_time
write.csv(robustTrial_stage1DF, '../3-experiment/data/collectedData/robustTrial/stage1Data-robustTrial.csv', row.names = F)

# Individual-level Processing: Robust Comp
robustComp_stage1DF = sample_experiment_stage1(53)
write.csv(robustComp_stage1DF, '../3-experiment/data/collectedData/robustComp/stage1Data-robustComp.csv', row.names = F)