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
library(ggh4x)
library(ggpubr)
library(grid)
source('../5-process/utils/process-aggregate.R')
scenarioOrder = c("2014-07-22", "2014-05-13", "2015-05-21", "2014-06-05", 
                  "2015-06-25", "2014-11-07", "2015-06-30", "2015-10-26", 
                  "2015-10-22", "2015-10-14", "2014-09-26", "2014-12-05", 
                  "2015-08-28", "2014-05-22", "2014-10-24", "2014-09-29")
robustOrder = c("2014-07-22", "2015-10-26", "2014-10-24", "2014-09-26", 
                "2015-10-14", "2015-08-28", "2014-05-13", "2015-06-25", 
                "2014-12-05", "2015-10-22", "2014-06-05", "2014-09-29", 
                "2014-11-07", "2014-05-22", "2015-05-21", "2015-06-30")
targetCA = c(8, 28, 32)
condition_colors = c('#4E79A7', '#F28E2B', '#E15759', '#76B7B2')
modelDF = read.csv('../1-taxiTrips/data/modelData/cleanModelData.csv')
mod = readRDS("../2-counterfactual/model/fitted-model.rds")
(experiment_instructionDF = produce_fullLevel_samplingInstruction(scenarioOrder, 'experiment'))
(robustTrial_instructionDF = produce_fullLevel_samplingInstruction(robustOrder, 'robustTrial'))
(robustComp_instructionDF = produce_fullLevel_samplingInstruction(scenarioOrder, 'robustComp'))

simulate_single_scenario = function(simulationID, samplingArgsDF){
  set.seed(simulationID)

  trialNum = samplingArgsDF$trial
  
  level0Size = samplingArgsDF$level0
  level1Size = samplingArgsDF$level1
  level2Size = samplingArgsDF$level2
  
  level0Flow = sample_level0Flow_for4Conditions(samplingArgsDF)
  level1Flow = sample_higherLevelFlow(1, samplingArgsDF) %>% bind_rows
  level2Flow = sample_higherLevelFlow(2, samplingArgsDF) %>% bind_rows
  
  mergedFlowDF = level1Flow %>% 
    left_join(level2Flow, 
              by = c('trial', 'ca', 'tripdate', 'display', 'feedback')) %>%
    left_join(level0Flow, 
              by = c('trial', 'ca', 'tripdate', 'display', 'feedback')) %>%
    mutate(flow = level0Flow + level1Flow + level2Flow) %>%
    mutate(sim = simulationID) %>% 
    select(trial, sim, display, feedback, ca, flow, level1Flow, level2Flow, level0Flow)
  
  simPredDF = mergedFlowDF %>% 
    add_epred_draws(mod) %>% 
    group_by(trial, sim, display, feedback, ca, flow, .row) %>%
    summarise(predPickup = mean(.epred) %>% round(0), .groups = 'drop') %>%
    select(-.row) %>%
    left_join(mergedFlowDF,  by = c("trial", "sim", "display", "feedback", "ca", "flow")) %>% 
    rename(simFlow = flow)
  
  modelBoundsDF = get_modelInputRange()
  
  simOutcomeDF = simPredDF %>% 
    left_join(modelBoundsDF, by = 'ca') %>%
    mutate(predPickup = ifelse(simFlow > flow_max, predPickup_max, predPickup)) %>%
    mutate(predPickup = ifelse(simFlow < predPickup, simFlow, predPickup)) %>%
    select(trial, sim, display, feedback, ca, simFlow, level1Flow, level2Flow, level0Flow, predPickup)
  
  pickupProbDF = simOutcomeDF %>% 
    mutate(pickupProb = predPickup / simFlow) %>% 
    select(ca, pickupProb) %>%
    group_by(ca) %>% 
    median_qi()
  north_prob = pickupProb %>% filter(ca == 8) %>% pull(pickupProb)
  west_prob = pickupProb %>% filter(ca == 28) %>% pull(pickupProb)
  east_prob = pickupProb %>% filter(ca == 32) %>% pull(pickupProb)
  
  simulationDF = simOutcomeDF %>%
    mutate(ca = case_when(ca == 8 ~ 'north',
                          ca == 28 ~ 'west',
                          ca == 32 ~ 'east')) %>%
    pivot_wider(id_cols = c(trial, sim, display, feedback),
                names_from = ca,
                values_from = c(level1Flow, level2Flow, level0Flow)) %>%
    mutate(level1Pickup_north = round(level1Flow_north * north_prob, 0),
           level2Pickup_north = round(level2Flow_north * north_prob, 0),
           level0Pickup_north = round(level0Flow_north * north_prob, 0),
           
           level1Pickup_west = round(level1Flow_west * west_prob, 0),
           level2Pickup_west = round(level2Flow_west * west_prob, 0),
           level0Pickup_west = round(level0Flow_west * west_prob, 0),
           
           level1Pickup_east = round(level1Flow_east * east_prob, 0),
           level2Pickup_east = round(level2Flow_east * east_prob, 0),
           level0Pickup_east = round(level0Flow_east * east_prob, 0)
    ) %>% 
    mutate(level1Flow = level1Flow_north + level1Flow_west + level1Flow_east,
           level2Flow = level2Flow_north + level2Flow_west + level2Flow_east,
           level0Flow = level0Flow_north + level0Flow_west + level0Flow_east,
           
           level1Pickup = level1Pickup_north + level1Pickup_west + level1Pickup_east,
           level2Pickup = level2Pickup_north + level2Pickup_west + level2Pickup_east,
           level0Pickup = level0Pickup_north + level0Pickup_west + level0Pickup_east) %>% 
    select(trial, sim, display, feedback, level1Flow, level2Flow, level0Flow, level1Pickup, level2Pickup, level0Pickup) %>%
    mutate(level1Prob = level1Pickup / level1Flow,
           level2Prob = level2Pickup / level2Flow,
           level0Prob = level0Pickup / level0Flow) %>% 
    select(trial, sim, display, feedback, ends_with('Prob'), ends_with('Flow'))
  
  return(simulationDF)
}

experiment_levelOutcome = simulateN(500, scenarioOrder, 'experiment') %>% mutate(collection = 'experiment')
robustTrial_levelOutcome = simulateN(500, robustOrder, 'robustTrial') %>% mutate(collection = 'robustTrial')
robustComp_levelOutcome = simulateN(500, scenarioOrder, 'robustComp') %>% mutate(collection = 'robustComp')

combinedDF = rbind(experiment_levelOutcome, robustTrial_levelOutcome, robustComp_levelOutcome)
combinedDF = combinedDF %>% 
  filter(trial != 0) %>%
  mutate(display = ifelse(display == 0, 'Static', 'NetHOPs'),
         feedback = ifelse(feedback == 0, 'Bandit', 'Full'),
         interface = paste0(display, '+', feedback)) %>%
  mutate(interface = factor(interface, 
                            levels = c('Static+Bandit', 'Static+Full', 
                                       'NetHOPs+Bandit', 'NetHOPs+Full'))) %>% 
  mutate(collection = case_when(collection == 'experiment'~ 'Main Experiment',
                                collection == 'robustTrial' ~ 'Robust Trial',
                                collection == 'robustComp' ~ 'Robust Comp.') %>% 
           factor(levels = c('Main Experiment', 'Robust Trial', 'Robust Comp.'))) %>%
  select(collection, trial, sim, display, feedback, interface, ends_with('Prob')) %>%
  pivot_longer(!c(collection, trial, sim, display, feedback, interface), names_to = 'level', values_to = 'prob') %>% 
  mutate(level = case_when(level == 'level1Prob' ~ 'Level-1',
                           level == 'level2Prob' ~ 'Level-2',
                           level == 'level0Prob' ~ 'Level-0') %>% 
           factor(levels = c('Level-0', 'Level-1', 'Level-2')))

prob_interfaceEffect = 
  interfaceEffectDF %>%
  filter(level != 'Level-0') %>%
  ggplot(aes(x = interface, y = prob)) + 
  geom_pointinterval(aes(ymin = .lower, ymax = .upper, 
                         color = interface), 
                     fatten_point = 2,
                     size = 2) + 
  scale_color_manual(values = condition_colors) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, title = "(A) Prop. of Pickups from System Outcome") + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(color = 'white'),
        text = element_text(size = 10),
        strip.background = element_rect(fill="#F1F1F1")) +
  guides(color = guide_legend(override.aes = list(size=3))) + 
  facet_grid(cols = vars(collection),
             rows = vars(level),
             switch = 'y',
             scales = 'free',
             drop = T) + 
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(.4, .7), breaks = seq(.4, .7, by = 0.1), labels = scales::percent),
                               scale_y_continuous(limits = c(.55, .9), breaks = seq(.55, .9, by = 0.1), labels = scales::percent)))

prob_learningEffect = 
  combinedDF %>% 
  filter(level != 'Level-0') %>%
  ggplot(aes(x = trial, y = prob)) + 
  stat_lineribbon(aes(color = interface, fill = interface), .width = 0.95, alpha = 0.2) + 
  geom_line(aes(x = trial, y = median, color = interface), 
            linewidth = 0.7, 
            data = combinedDF %>% 
              filter(level != 'Level-0') %>%
              group_by(collection, level, interface, trial) %>% 
              summarise(median = median(prob), .groups = 'drop')
  ) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  scale_x_continuous(breaks = seq(1, 15, by = 2)) +
  labs(x = NULL, y = NULL, title = "(B). Prop. of Pickups from System Outcome by Trial") +
  facet_grid(rows = vars(level), 
             cols = vars(collection),
             switch = 'y',
             scales = 'free') +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill="#F1F1F1")) +
  guides(color = guide_legend(override.aes = list(size=3))) + 
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(.4, .7), breaks = seq(.4, .7, by = 0.1), labels = scales::percent),
                               scale_y_continuous(limits = c(.55, .9), breaks = seq(.55, .9, by = 0.1), labels = scales::percent)))

(probPlot = ggarrange(prob_interfaceEffect, prob_learningEffect, ncol = 2, 
                      widths = c(1, 2), common.legend = T, legend = 'bottom') %>%
    annotate_figure(left = textGrob("Prop. Pickup", rot = 90, vjust = .4, gp = gpar(cex = 1))))