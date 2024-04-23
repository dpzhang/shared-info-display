library(rjson)
library(dplyr)
library(magrittr)
library(tidyverse)
library(purrr)
library(stringr)
library(emdist)
library(rlist)
library(infer)
library(tidyr)
library(brms)
library(tidybayes)
library(Rsolnp)
library(parallel)
library(stringr)
source('./utils/process-aggregate.R')
source('./utils/maxWelfare.R')
scenarioOrder = c("2014-07-22", "2014-05-13", "2015-05-21", "2014-06-05", 
                  "2015-06-25", "2014-11-07", "2015-06-30", "2015-10-26", 
                  "2015-10-22", "2015-10-14", "2014-09-26", "2014-12-05", 
                  "2015-08-28", "2014-05-22", "2014-10-24", "2014-09-29")
robustOrder = c("2014-07-22", "2015-10-26", "2014-10-24", "2014-09-26", 
                "2015-10-14", "2015-08-28", "2014-05-13", "2015-06-25", 
                "2014-12-05", "2015-10-22", "2014-06-05", "2014-09-29", 
                "2014-11-07", "2014-05-22", "2015-05-21", "2015-06-30")
targetCA = c(8, 28, 32)
modelDF = read.csv('../1-taxiTrips/data/modelData/modelData.csv')
mod = readRDS("../2-counterfactual/model/fitted.rds")
(experiment_instructionDF = produce_fullLevel_samplingInstruction(scenarioOrder, 'experiment'))
(robustTrial_instructionDF = produce_fullLevel_samplingInstruction(robustOrder, 'robustTrial'))
(robustComp_instructionDF = produce_fullLevel_samplingInstruction(scenarioOrder, 'robustComp'))

# Data Processing: Experiment
experiment_aggregateDF = generate_aggregate_data(500, scenarioOrder, 'experiment')


# Data Processing: Robust Trial
robustTrial_aggregateDF = generate_aggregate_data(500, robustOrder, 'robustTrial')


# Data Processing: Robust Composition
robustComp_aggregateDF = generate_aggregate_data(500, scenarioOrder, 'robustComp')
