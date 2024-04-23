get_individualMod_rhat = function(vault, modelDesc, mainTitle){
  experiment = vault$experiment[[modelDesc]] %>% 
    rhat %>% 
    mcmc_rhat_hist() + 
    yaxis_text(hjust = 1) +
    labs(title ='Main Experiment') +
    theme_minimal()
  
  robustTrial = vault$robustTrial[[modelDesc]] %>% 
    rhat %>% 
    mcmc_rhat_hist() + 
    yaxis_text(hjust = 1) +
    labs(title = 'Robust Trial Order') +
    theme_minimal()
  
  robustComp = vault$robustComp[[modelDesc]] %>% 
    rhat %>% 
    mcmc_rhat_hist() + 
    yaxis_text(hjust = 1) + 
    labs(title = 'Robust Composition') +
    theme_minimal()
  
  outputPlot = ggarrange(
    experiment,
    robustTrial,
    robustComp,
    nrow = 1,
    common.legend = T,
    legend = 'bottom'
  ) %>%
    annotate_figure(top = text_grob(mainTitle, 
                                    face = "bold", size = 14)) 
  return(outputPlot)
}

get_aggregateMod_rhat = function(vault, modelDesc, mainTitle){
  experiment = mcmc_plot(vault$experiment[[modelDesc]], type = 'rhat') + 
    labs(title = 'Main Experiment') +
    yaxis_text(hjust = 1) +
    theme_minimal()
  
  robustTrial = mcmc_plot(vault$robustTrial[[modelDesc]], type = 'rhat') + 
    labs(title = 'Robust Trial Order') +
    yaxis_text(hjust = 1) +
    theme_minimal()
  
  robustComp = mcmc_plot(vault$robustComp[[modelDesc]], type = 'rhat') + 
    labs(title = 'Robust Composition') +
    yaxis_text(hjust = 1) +
    theme_minimal()
  
  outputPlot = ggarrange(
    experiment,
    robustTrial,
    robustComp,
    nrow = 1,
    common.legend = T,
    legend = 'bottom'
  ) %>%
    annotate_figure(top = text_grob(mainTitle, 
                                    face = "bold", size = 14))
  return(outputPlot)
}

get_individualMod_neff = function(vault, modelDesc, mainTitle){
  experiment = vault$experiment[[modelDesc]] %>% 
    neff_ratio() %>% 
    mcmc_neff_hist() + 
    yaxis_text(hjust = 1) +
    labs(title ='Main Experiment') +
    theme_minimal()
  
  robustTrial = vault$robustTrial[[modelDesc]] %>% 
    neff_ratio() %>% 
    mcmc_neff_hist() + 
    yaxis_text(hjust = 1) +
    labs(title = 'Robust Trial Order')+
    theme_minimal()
  
  robustComp = vault$robustComp[[modelDesc]] %>% 
    neff_ratio() %>% 
    mcmc_neff_hist() + 
    yaxis_text(hjust = 1) + 
    labs(title = 'Robust Composition')+
    theme_minimal()
  
  outputPlot = ggarrange(
    experiment,
    robustTrial,
    robustComp,
    nrow = 1,
    common.legend = T,
    legend = 'bottom'
  ) %>%
    annotate_figure(top = text_grob(mainTitle, 
                                    face = "bold", size = 14))
  return(outputPlot)
}

get_aggregateMod_neff = function(vault, modelDesc, mainTitle){
  experiment = mcmc_plot(vault$experiment[[modelDesc]], type = 'neff') + 
    labs(title = 'Main Experiment') +
    yaxis_text(hjust = 1)+
    theme_minimal()
  
  robustTrial = mcmc_plot(vault$robustTrial[[modelDesc]], type = 'neff') + 
    labs(title = 'Robust Trial Order') +
    yaxis_text(hjust = 1)+
    theme_minimal()
  
  robustComp = mcmc_plot(vault$robustComp[[modelDesc]], type = 'neff') + 
    labs(title = 'Robust Composition') +
    yaxis_text(hjust = 1)+
    theme_minimal()
  
  outputPlot = ggarrange(
    experiment,
    robustTrial,
    robustComp,
    nrow = 1,
    common.legend = T,
    legend = 'bottom'
  ) %>%
    annotate_figure(top = text_grob(mainTitle, 
                                    face = "bold", size = 14))
  return(outputPlot)
}

plot_brModel_classificationError_percent = function(brMod){
  postPredict_summaryDF = brMod$data %>%
    add_epred_draws(brMod, ndraws = 1000) %>%
    mutate(.epred = ifelse(.epred >= 0.5, 1, 0)) %>%
    group_by(trial, condition, .draw, .epred) %>%
    summarise(countBR = n(), .groups = 'drop') %>%
    group_by(trial, condition, .draw) %>%
    summarise(.epred, propBR = countBR / sum(countBR), .groups = 'drop')
  
  ppPredData = postPredict_summaryDF %>% 
    group_by(trial, condition, .epred) %>%
    summarise(median = median(propBR), 
              lower = quantile(propBR, 0.025),
              upper = quantile(propBR, 0.975),
              .groups = 'drop') %>%
    rename(br = .epred) %>%
    mutate(br = as.factor(br))
  
  brData = brMod$data %>%
    group_by(br, condition, trial) %>%
    summarise(truth = n(), .groups = 'drop') %>%
    group_by(condition, trial) %>% 
    summarise(br, truth = truth / sum(truth), .groups = 'drop')
  
  brPlotDF = ppPredData %>% 
    left_join(brData, by = c('condition', 'trial', 'br'))
  
  brPlot = brPlotDF %>% 
    mutate(conditionText = case_when(condition == '00' ~ 'Static+Bandit',
                                     condition == '01' ~ 'Static+Full',
                                     condition == '10' ~ 'NetHOPs+Bandit',
                                     condition == '11' ~ 'NetHOPs+Full') %>%
             factor(levels = c('Static+Bandit', 'Static+Full', 'NetHOPs+Bandit', 'NetHOPs+Full'))) %>%
    ggplot() +
    geom_bar(aes(x = br, y = truth), stat = 'identity', 
             color = "#665191", fill = "#665191", alpha = 0.2) + 
    geom_errorbar(aes(x = br, y = truth, ymin=lower, ymax=upper), 
                  width = 0.2, color = '#003f5c') +
    geom_point(aes(x = br, y = median), color = '#ff6361') +
    facet_grid(rows = vars(conditionText), cols = vars(trial)) +
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Best Response (BR)', y = 'Proportion') +
    theme_linedraw()
  
  return(brPlot)
}

compute_classificationError = function(brMod){
  errorDF = brMod$data %>%
    add_epred_draws(brMod, ndraws = 1000) %>%
    mutate(.epred = ifelse(.epred >= 0.5, 1, 0),
           br = br %>% as.character() %>% as.numeric()) %>%
    mutate(eval = br == .epred) %>%
    group_by(.draw) %>%
    summarise(accuracy = sum(eval) / n(), .groups = 'drop')
  
  accuracy_bootstrap = errorDF %>% 
    specify(response = accuracy) %>% 
    generate(reps = 1e4, type = "bootstrap") %>%
    calculate(stat = "mean")
  
  accuracy_bootstrap_ci = accuracy_bootstrap %>% 
    get_confidence_interval(level = 0.95, type = "percentile") 
  
  errorPlot = visualize(accuracy_bootstrap) + 
    shade_confidence_interval(endpoints = accuracy_bootstrap_ci) + 
    theme_linedraw() + 
    labs(x = 'Classification Accuracy', y = 'Count', 
         title = 'Simulated Distribution of Accuracy Rate by Bootstraping') +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.01))
  
  summaryDF = accuracy_bootstrap %>% data.frame
  summaryStats = data.frame(lower = quantile(summaryDF$stat, 0.025),
                            median = median(summaryDF$stat), 
                            upper = quantile(summaryDF$stat, 0.975),
                            row.names = NULL)
  
  outputLst = list('plot' = errorPlot,
                   'stats' = summaryStats)
  
  return(outputLst)
}

get_posterior_predictive_brModels = function(vault, stage, modelDesc, subTitle){
  outputPlot = pp_check(vault[[stage]][[modelDesc]], type = 'error_binned', ndraws = 4) + 
    labs(title = subTitle) +
    theme_minimal()
  return(outputPlot)
}

get_posterior_predictive_EMD = function(vault, stage, modelDesc, mainTitle){
  pp_dens_overlay = pp_check(vault[[stage]][[modelDesc]], 
                             type = 'dens_overlay', 
                             ndraws = 100) + 
    scale_x_continuous(limits = c(-10, 10)) + 
    yaxis_text(hjust = 1) +
    theme_minimal() + 
    labs(title = mainTitle)
  
  return(pp_dens_overlay)
}

get_posterior_predictive_ratio = function(vault, stage, modelDesc, mainTitle){
  pp_dens_overlay = pp_check(vault[[stage]][[modelDesc]], 
                             type = 'dens_overlay', 
                             ndraws = 100) + 
    yaxis_text(hjust = 1) + 
    theme_minimal() + 
    labs(title = mainTitle)
  
  return(pp_dens_overlay)
}

plot_posterior = function(vault, modelDesc, mainTitle){
  post_experiment = mcmc_plot(vault$experiment[[modelDesc]]) + labs(title = 'Main Experiment') + theme_minimal()
  post_robustComp = mcmc_plot(vault$robustComp[[modelDesc]]) + labs(title = 'Robust Composition')  + theme_minimal()
  post_robustTrial = mcmc_plot(vault$experiment[[modelDesc]]) + labs(title = 'Robust Trial Order')  + theme_minimal()
  
  outputPlot = ggarrange(post_experiment, post_robustComp, post_robustTrial,
                         ncol = 3) %>%
    annotate_figure(top = text_grob(mainTitle, 
                                    face = "bold", size = 14))
  return(outputPlot)
}
