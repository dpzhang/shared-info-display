library(dplyr)
library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(rlist)
library(brms)
library(tidybayes)
library(bayesplot)
library(ggplot2)
library(ggpubr)
library(infer)
library(bayesplot)
source('./utils/models.R')
source('./utils/models-diagnostics.R')
collection_colors = c('#003f5c', '#58508d', '#bc5090')
condition_colors = c('#003f5c', '#58508d', '#ff6361', '#ffa600')
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

# BR Level-1 Diagnoistics
ggarrange(
  get_individualMod_rhat(modelVault, 'brLevel1_cond', 'Level-1 Pr(BR): Rhats'),
  get_individualMod_neff(modelVault, 'brLevel1_cond', 'Level-1 Pr(BR): Neff Ratio'),
  get_individualMod_rhat(modelVault, 'brLevel2_cond', 'Level-2 Pr(BR): Rhats'),
  get_individualMod_neff(modelVault, 'brLevel2_cond', 'Level-2 Pr(BR): Neff Ratio'),
  ncol = 2, nrow = 2
)

ggarrange(
  get_posterior_predictive_brModels(modelVault, 'experiment', 'brLevel1_cond', 'Level-1 Pr(BR): Main Experiment'),
  get_posterior_predictive_brModels(modelVault, 'robustTrial', 'brLevel1_cond', 'Level-1 Pr(BR): Robust Trial Order'),
  get_posterior_predictive_brModels(modelVault, 'robustComp', 'brLevel1_cond', 'Level-1 Pr(BR): Robust Composition'),
  get_posterior_predictive_brModels(modelVault, 'experiment', 'brLevel2_cond', 'Level-2 Pr(BR): Main Experiment'),
  get_posterior_predictive_brModels(modelVault, 'robustTrial', 'brLevel2_cond', 'Level-2 Pr(BR): Robust Trial Order'),
  get_posterior_predictive_brModels(modelVault, 'robustComp', 'brLevel2_cond', 'Level-2 Pr(BR): Robust Composition'),
  ncol = 3, nrow = 2
)

ggarrange(
  plot_posterior(modelVault, 'brLevel1_cond', 'Level-1 Pr(BR): Posterior'),
  plot_posterior(modelVault, 'brLevel2_cond', 'Level-2 Pr(BR): Posterior'),
  nrow = 2
)

ggarrange(
  get_individualMod_rhat(modelVault, 'aeLevel1_cond', 'Level-1 AE: Rhats'),
  get_individualMod_neff(modelVault, 'aeLevel1_cond', 'Level-1 AE: Neff Ratio'),
  get_individualMod_rhat(modelVault, 'aeLevel2_cond', 'Level-2 AE: Rhats'),
  get_individualMod_neff(modelVault, 'aeLevel2_cond', 'Level-2 AE: Neff Ratio'),
  ncol = 2, nrow = 2
)

ggarrange(
  get_posterior_predictive_EMD(modelVault, 'experiment', 'aeLevel1_cond', 'Level-1 AE: Main Experiment'),
  get_posterior_predictive_EMD(modelVault, 'robustTrial', 'aeLevel1_cond', 'Level-1 AE: Robust Trial Order'),
  get_posterior_predictive_EMD(modelVault, 'robustComp', 'aeLevel1_cond', 'Level-1 AE: Robust Composition'), 
  get_posterior_predictive_EMD(modelVault, 'experiment', 'aeLevel2_cond', 'Level-2 AE: Main Experiment'),
  get_posterior_predictive_EMD(modelVault, 'robustTrial', 'aeLevel2_cond', 'Level-2 AE: Robust Trial Order'),
  get_posterior_predictive_EMD(modelVault, 'robustComp', 'aeLevel2_cond', 'Level-2 AE: Robust Composition'),
  ncol = 3, nrow = 2, common.legend = T, legend = 'bottom'
)

ggarrange(
  plot_posterior(modelVault, 'aeLevel1_cond', 'Level-1 AE: Posterior'),
  plot_posterior(modelVault, 'aeLevel2_cond', 'Level-2 AE: Posterior'),
  nrow = 2
)

# DS Diagnoistics
ggarrange(
  get_aggregateMod_rhat(modelVault, 'ds_cond', 'DS: Rhats'),
  get_aggregateMod_neff(modelVault, 'ds_cond', 'DS: Neff Ratio'),
  nrow = 2
)

ggarrange(
  get_posterior_predictive_EMD(modelVault, 'experiment', 'ds_cond', 'DS: Main Experiment'),
  get_posterior_predictive_EMD(modelVault, 'robustTrial', 'ds_cond', 'DS: Robust Trial Order'),
  get_posterior_predictive_EMD(modelVault, 'robustComp', 'ds_cond', 'DS: Robust Composition'),
  ncol = 3, common.legend = T, legend = 'bottom'
)

ggarrange(
  mcmc_plot(modelVault$experiment$ds_cond, pars = 'b_') + theme_minimal() + labs(title = 'DS: Main Experiment'),
  mcmc_plot(modelVault$robustTrial$ds_cond, pars = 'b_') + theme_minimal() + labs(title = 'DS: Robust Trial Order'),
  mcmc_plot(modelVault$robustComp$ds_cond, pars = 'b_') + theme_minimal() + labs(title = 'DS: Robust Composition'),
  ncol = 3
)

# Ratio Diagnoistics
ggarrange(
  get_aggregateMod_rhat(modelVault, 'ratio_cond', 'Welfare Ratio: Rhats'),
  get_aggregateMod_neff(modelVault, 'ratio_cond', 'Welfare Ratio: Neff Ratio'),
  nrow = 2
)

ggarrange(
  get_posterior_predictive_ratio(modelVault, 'experiment', 'ratio_cond', 'Welfare Ratio: Main Experiment'),
  get_posterior_predictive_ratio(modelVault, 'robustTrial', 'ratio_cond', 'Welfare Ratio: Robust Trial Order'),
  get_posterior_predictive_ratio(modelVault, 'robustComp', 'ratio_cond', 'Welfare Ratio: Robust Composition'),
  ncol = 3, common.legend = T, legend = 'bottom'
)

ggarrange(
  mcmc_plot(modelVault$experiment$ratio_cond, pars = targetEffects) + theme_minimal() + labs(title = 'Ratop: Main Experiment'),
  mcmc_plot(modelVault$robustTrial$ratio_cond, pars = targetEffects) + theme_minimal() + labs(title = 'Ratop: Robust Trial Order'),
  mcmc_plot(modelVault$robustComp$ratio_cond, pars = targetEffects) + theme_minimal() + labs(title = 'Ratio: Robust Composition'),
  ncol = 3
)

targetEffects = c('b_Intercept', 'b_trial', 'b_display1', 'b_feedback1', 'b_trial:display1', 'b_trial:feedback1', 'b_display1:feedback1', 'b_trial:display1:feedback1')