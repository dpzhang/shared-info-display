library(dplyr)
library(magrittr)
library(tibble)
library(rlist)
library(purrr)
library(parallel)
library(knitr)
library(urca)
library(brms)
library(tidybayes)
library(rjson)
library(tidyr)
library(stringr)
source('./utils/level2-realized.R')
targetCA = c(8, 28, 32)
modelDF = read.csv("../1-taxiTrips/data/modelData/cleanModelData.csv") %>% rename(pickup = pickups)
mod = readRDS("../2-counterfactual/model/fitted.rds")

scenarioOrder = c("2014-07-22", "2014-05-13", "2015-05-21", "2014-06-05", 
                  "2015-06-25", "2014-11-07", "2015-06-30", "2015-10-26", 
                  "2015-10-22", "2015-10-14", "2014-09-26", "2014-12-05", 
                  "2015-08-28", "2014-05-22", "2014-10-24", "2014-09-29")

robustOrder = c("2014-07-22", "2015-10-26", "2014-10-24", "2014-09-26", 
                "2015-10-14", "2015-08-28", "2014-05-13", "2015-06-25", 
                "2014-12-05", "2015-10-22", "2014-06-05", "2014-09-29", 
                "2014-11-07", "2014-05-22", "2015-05-21", "2015-06-30")

# Now we take a look at the real composition for level-2s
produce_samplingInstruction(scenarioOrder, 'experiment')
produce_samplingInstruction(robustOrder, 'robustTrial')
produce_samplingInstruction(scenarioOrder, 'robustComp')

# Main Experiment
# On 8 cores, it took 2.8 hours to finish
start.time <- Sys.time()
experiment_level2Realized = simulate_level2_realizedOutcome(trialOrderVec = scenarioOrder,
                                                            directory = 'experiment',
                                                            numSim = 1000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(experiment_level2Realized, file = "./data/interfaceData/experiment/stage2Realized-experiment.Rdata")
generate_stage2_realized_json(experiment_level2Realized, 'experiment')

# Robust Trial
# On 8 cores, it took 3.2 hours to finish
start.time <- Sys.time()
robustTrial_level2Realized = simulate_level2_realizedOutcome(trialOrderVec = robustOrder,
                                                             directory = 'robustTrial',
                                                             numSim = 1000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(robustTrial_level2Realized, file = "./data/interfaceData/robustTrial/stage2Realized-robustTrial.Rdata")
generate_stage2_realized_json(robustTrial_level2Realized, 'robustTrial')

# Robust Composition
start.time <- Sys.time()
robustComp_level2Realized = simulate_level2_realizedOutcome(trialOrderVec = scenarioOrder,
                                                            directory = 'robustComp',
                                                            numSim = 1000)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(robustComp_level2Realized, file = "./data/interfaceData/robustComp/stage2Realized-robustComp.Rdata")
generate_stage2_realized_json(robustComp_level2Realized, 'robustComp')