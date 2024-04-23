library(tidyverse)    
library(brms)     
library(tidybayes)    
library(broom)        
library(broom.mixed)  
library(emmeans)      
library(patchwork)    
library(ggokabeito)   
library(gghalves)     
library(ggbeeswarm) 
library(magrittr)
library(rlist)
library(ggh4x)
library(ggpubr)
library(grid)
source('./utils/models.R')
source('./utils/model-effects.R')
collection_colors = c('#003f5c', '#58508d', '#bc5090')
condition_colors = c('#4E79A7', '#F28E2B', '#E15759', '#76B7B2')
contrast_color = '#bc5090'
experiment_models = create_modelData('experiment', summarize500 = T) %>%
  fit_all_models('experiment')
robustTrial_models = create_modelData('robustTrial', summarize500 = T) %>%
  fit_all_models('robustTrial')
robustComp_models = create_modelData('robustComp', summarize500 = T) %>%
  fit_all_models('robustComp')
modelVault = list('experiment' = experiment_models, 
                  'robustTrial' = robustTrial_models,
                  'robustComp' = robustComp_models)

# Load the data
trialData = readRDS('./data/modelData/individual.rds') %>% 
  filter(trial!=0)
aggData = readRDS('./data/modelData/aggregate.rds') %>% 
  filter(trial!=0)

# Compute global effect
conditionEffectDF = compute_allModels_effects('condition')
write.csv(conditionEffectDF, './data/analysis/effects/interval/condition.csv', row.names = F)

# Compute treatment effects
interfaceEffectDF = compute_allModels_effects('interface') %>% format_output
write.csv(interfaceEffectDF, './data/analysis/effects/interval/interface.csv', row.names = F)

# Compute learning effects
learningEffectDF = compute_allModels_effects('learning') %>% format_output
write.csv(learningEffectDF, './data/analysis/effects/interval/learning.csv', row.names = F)

drawsDF = draw_all_models(1000)
write.csv(drawsDF, './data/analysis/effects/draws/draws-1k.csv', row.names = F)

aggregate_interfaceEffect = interfaceEffectDF %>% 
  filter(response %in% c('ratio', 'ds')) %>% 
  mutate(response = case_when(response == 'ds' ~ 'Distribution Shift',
                              response == 'ratio' ~ 'Welfare Ratio') %>%
           factor(levels = c('Welfare Ratio', 'Distribution Shift'))) %>% 
  ggplot(aes(x = interface, y = .epred)) + 
  geom_pointinterval(aes(ymin = .lower, ymax = .upper, 
                         color = interface), 
                     fatten_point = 2) + 
  scale_color_manual(values = condition_colors) + 
  labs(x = NULL, y = NULL, title = "(A). Expected Aggregate-level Outcomes") + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(color = 'white'),
        text = element_text(size = 10),
        strip.background = element_rect(fill="#F1F1F1")) +
  guides(color = guide_legend(override.aes = list(size=3))) + 
  facet_grid(cols = vars(collection),
             rows = vars(response),
             switch = 'y',
             scales = 'free',
             drop = T) +
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(0.75, 0.96), breaks = seq(0.75, 0.96, by = 0.05), labels = scales::percent),
                               scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)))
  )


aggregate_learningEffect = 
  drawsDF %>% 
  filter(response %in% c('ratio', 'ds')) %>% 
  mutate(response = case_when(response == 'ds' ~ 'Distribution Shift',
                              response == 'ratio' ~ 'Welfare Ratio') %>%
           factor(levels = c('Welfare Ratio', 'Distribution Shift'))) %>% 
  ggplot(aes(x = trial, y = .epred)) + 
  stat_lineribbon(aes(color = interface, fill = interface), .width = 0.95, alpha = 0.2) + 
  geom_line(aes(x = trial, y = median, color = interface), 
            linewidth = 0.7, 
            data = drawsDF %>% 
              filter(response %in% c('ds', 'ratio')) %>% 
              mutate(response = case_when(response == 'ds' ~ 'Distribution Shift',
                                          response == 'ratio' ~ 'Welfare Ratio') %>%
                       factor(levels = c('Welfare Ratio', 'Distribution Shift'))) %>%
              group_by(collection, response, interface, trial) %>% 
              summarise(median = median(.epred), .groups = 'drop')
  ) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  labs(x = NULL, y = NULL, title = "(B). Expected Aggregate-level Outcomes by Trial") +
  scale_x_continuous(breaks = seq(1, 15, by = 2)) +
  #scale_x_continuous(labels = scales::percent) + 
  facet_grid(rows = vars(response), 
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
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(0.75, 0.96), breaks = seq(0.75, 0.96, by = 0.05), labels = scales::percent),
                               scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)))
  )

(aggPlot = ggarrange(aggregate_interfaceEffect, aggregate_learningEffect, ncol = 2, 
                     widths = c(1, 2), common.legend = T, legend = 'bottom') %>%
    annotate_figure(left = textGrob("Ratio (%)                     DS (EMD)", rot = 90, vjust = .4, gp = gpar(cex = 1))))

# Best Response Interface Effect
br_interfaceEffect = 
  interfaceEffectDF %>% 
  filter(response == 'br') %>%
  ggplot(aes(x = interface, y = .epred)) + 
  geom_pointinterval(aes(ymin = .lower, ymax = .upper, 
                         color = interface), 
                     fatten_point = 2) + 
  scale_color_manual(values = condition_colors) + 
  labs(x = NULL, y = NULL, title = "(A). Expected BR Rate") + 
  scale_y_continuous(
    # limits = c(0.5, 1), breaks = seq(0, 1, by = 0.1),
    labels = scales::percent) + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(color = 'white'),
        text = element_text(size = 10),
        strip.background = element_rect(fill="#F1F1F1")) +
  guides(color = guide_legend(override.aes = list(size=3))) + 
  facet_grid(rows = vars(level),
             cols = vars(collection),
             switch = 'y',
             scales = 'free',
             drop = T) +
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = scales::percent),
                               scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = scales::percent)))


br_learningEffect = 
  drawsDF %>% 
  filter(response == 'br') %>%
  ggplot(aes(x = trial, y = .epred)) + 
  stat_lineribbon(aes(color = interface, fill = interface), .width = 0.95, alpha = 0.2) + 
  geom_line(aes(x = trial, y = median, color = interface), 
            linewidth = 0.7, 
            data = drawsDF %>% 
              filter(response == 'br') %>% 
              group_by(collection, level, interface, trial) %>% 
              summarise(median = median(.epred), .groups = 'drop')
  ) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  labs(x = NULL, y = NULL, title = "(B). Expected Best Response Rate by Trial") +
  scale_x_continuous(breaks = seq(1, 15, by = 2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
  #scale_x_continuous(labels = scales::percent) + 
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
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = scales::percent),
                               scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = scales::percent)))

(brPlot = ggarrange(br_interfaceEffect, br_learningEffect, ncol = 2, 
                    widths = c(1, 2), common.legend = T, legend = 'bottom') %>%
    annotate_figure(left = textGrob("Pr(BR)", rot = 90, vjust = .4, gp = gpar(cex = 1))))

ae_interfaceEffect = 
  interfaceEffectDF %>% 
  filter(response == 'ae') %>%
  ggplot(aes(x = interface, y = .epred)) + 
  geom_pointinterval(aes(ymin = .lower, ymax = .upper, 
                         color = interface), 
                     fatten_point = 2) + 
  scale_color_manual(values = condition_colors) + 
  labs(x = NULL, y = NULL, title = "(A). Expected Anticipation Error") + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(color = 'white'),
        text = element_text(size = 11),
        strip.background = element_rect(fill="#F1F1F1")) +
  guides(color = guide_legend(override.aes = list(size=3))) + 
  facet_grid(rows = vars(level),
             cols = vars(collection),
             switch = 'y',
             scales = 'free',
             drop = T) +
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1)),
                               scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10))))

ae_learningEffect = 
  drawsDF %>% 
  filter(response == 'ae') %>%
  ggplot(aes(x = trial, y = .epred)) + 
  stat_lineribbon(aes(color = interface, fill = interface), .width = 0.95, alpha = 0.2) + 
  geom_line(aes(x = trial, y = median, color = interface), 
            linewidth = 0.7, 
            data = drawsDF %>% 
              filter(response == 'ae') %>% 
              group_by(collection, level, interface, trial) %>% 
              summarise(median = median(.epred), .groups = 'drop')
  ) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  labs(x = NULL, y = NULL, title = "(B). Expected Anticipation Error by Trial") +
  scale_x_continuous(breaks = seq(1, 15, by = 2)) +
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
  facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1)),
                               scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10))))

(aePlot = ggarrange(ae_interfaceEffect, ae_learningEffect, ncol = 2, 
                    widths = c(1, 2), common.legend = T, legend = 'bottom') %>%
    annotate_figure(left = textGrob("AE (EMD)", rot = 90, vjust = .4, gp = gpar(cex = 1))))


aggDF = aggData %>% 
  separate(simFlow, c('sim_west', 'sim_north', 'sim_east'), convert = T) %>% 
  separate(predFlow, c('pred_west', 'pred_north', 'pred_east'), convert = T) %>% 
  mutate(diff_west = sim_west - pred_west,
         diff_north = sim_north - pred_north,
         diff_east = sim_east - pred_east) %>% 
  mutate(diff_prop_west = diff_west / totalFlow,
         diff_prop_north = diff_north / totalFlow,
         diff_prop_east = diff_east/ totalFlow)


diffDF = aggDF %>% 
  select(collection, trial, sim, display, feedback, diff_prop_west, diff_prop_north, diff_prop_east) %>% 
  pivot_longer(!c(collection, trial, sim, display, feedback), names_to = 'district', values_to = 'diffProp') %>% 
  mutate(district = str_remove(district, 'diff_prop_')) %>% 
  mutate(interface = paste0(display, '+', feedback)) %>% 
  mutate(trial = as.factor(trial))

diff_feedback = diffDF %>% 
  group_by(collection, trial, district, feedback) %>% 
  median_qi(diffProp, .width = 0.95) %>% 
  select(collection, trial, district, feedback, diffProp, .lower, .upper)

diff_feedback_format = diff_feedback %>%
  mutate(feedback = ifelse(feedback == 0, 'Bandit', 'Full') %>% factor(levels = c('Bandit', 'Full'))) %>%
  mutate(collection = case_when(collection == 'experiment'~ 'Main Experiment',
                                collection == 'robustTrial' ~ 'Robust Trial',
                                collection == 'robustComp' ~ 'Robust Comp.') %>% 
           factor(levels = c('Main Experiment', 'Robust Trial', 'Robust Comp.'))) %>%
  mutate(district = str_to_title(district) %>% 
           str_c('District', sep = ' ') %>% 
           factor(levels = c('West District', 'North District', 'East District'))) %>%
  mutate(trial = as.numeric(trial))

feedbackEffect_plot = 
  diff_feedback_format %>% 
  ggplot(aes(x = trial, y = diffProp, color = factor(feedback), group = interaction(feedback, district))) + 
  geom_errorbar(aes(ymin = .lower, ymax = .upper), 
                width = 0.3,
                position = position_dodge(0.5)) + 
  geom_line(position = position_dodge(0.5)) + 
  geom_point(position = position_dodge(0.5)) + 
  geom_hline(yintercept=0, color = contrast_color) +
  scale_color_manual(values = condition_colors) +
  scale_x_continuous(breaks = seq(1, 15, by = 2)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_grid(rows = vars(collection), 
             cols = vars(district),
             switch = 'y',
             drop = T) + 
  labs(x = 'Trial', y = 'Diff. in Proportion',
       title = 'Feedback Effect to Flows by Districts in Proportion Difference') + 
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 11),
        strip.background = element_rect(fill="#F1F1F1")) +
  guides(color = guide_legend(override.aes = list(size=2)))