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
experiment_stage2DF = process_JSON_data('../4-database/queriedData/stage2Experiment.json', 2) %>%
  integrate_level2_networkData('experiment') %>%
  compute_AE
end_time <- Sys.time()
end_time - start_time
write.csv(experiment_stage2DF, '../3-experiment/data/collectedData/experiment/stage2Data-experiment.csv', row.names = F)

# Individual-level Processing: Robust Trial
start_time <- Sys.time()
robustTrial_stage2DF = process_JSON_data('../4-database/queriedData/stage2RobustTrial.json', 2) %>%
  integrate_level2_networkData('robustTrial') %>%
  compute_AE
end_time <- Sys.time()
end_time - start_time
write.csv(robustTrial_stage2DF, '../3-experiment/data/collectedData/robustTrial/stage2Data-robustTrial.csv', row.names = F)

# Individual-level Processing: Robust Composition
start_time <- Sys.time()
robustComp_stage2DF = process_JSON_data('../4-database/queriedData/stage2RobustComp.json', 2) %>%
  integrate_level2_networkData('robustComp') %>%
  compute_AE
end_time <- Sys.time()
end_time - start_time
write.csv(robustComp_stage2DF, '../3-experiment/data/collectedData/robustComp/stage2Data-robustComp.csv', row.names = F)
