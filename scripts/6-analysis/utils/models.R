query_brData = function(trialDF, levelK, directory){
  brModelDF = trialDF %>%
    filter(level == levelK & collection == directory & trial != 0) %>%
    mutate(totalFlow = west_flow_realized + north_flow_realized + east_flow_realized) %>%
    select(userBR, trial, condition, id, visError, totalFlow, level, display, feedback) %>%
    mutate(userBR = as.factor(userBR),
           trial = as.numeric(trial),
           condition = as.factor(condition),
           id = as.factor(id),
           visError = as.numeric(visError) %>% log,
           totalFlow = as.numeric(totalFlow),
           display = as.factor(display),
           feedback = as.factor(feedback)) %>%
    rename(br = userBR)
  
  return(brModelDF)
}

query_aeData = function(trialDF, levelK, directory){
  aeModelDF = trialDF %>% 
    filter(level == levelK & collection == directory & trial != 0) %>%
    mutate(totalFlow = west_flow_realized + north_flow_realized + east_flow_realized,
           ae = case_when(ae == 0 ~ nth(sort(ae), 2) / 2, T ~ ae)) %>%
    mutate(logAE = log(ae)) %>%
    select(logAE, ae, trial, condition, id, visError, totalFlow, display, feedback) %>%
    mutate(logAE = as.numeric(logAE),
           ae = as.numeric(ae),
           trial = as.numeric(trial),
           condition = as.factor(condition),
           id = as.factor(id),
           visError = as.numeric(visError) %>% log,
           totalFlow = as.numeric(totalFlow),
           display = as.factor(display),
           feedback = as.factor(feedback))
  
  return(aeModelDF)
}

query_dsData = function(aggDF, directory, summarize500 = T){
  dsModelDF = aggDF %>% 
    filter(collection == directory & trial != 0) %>%
    select(ds, trial, condition, predPickup, maxPickup, sim, totalFlow, simFlow, predFlow, maxFlow, display, feedback) %>%
    mutate(ds = case_when( ds == 0 ~ nth(sort(ds), 2) / 2, T ~ ds)) %>%
    mutate(ds = as.numeric(ds),
           trial = as.numeric(trial),
           condition = as.factor(condition),
           visRatio = as.numeric(predPickup / maxPickup),
           sim = as.factor(sim),
           display = as.factor(display),
           feedback = as.factor(feedback)) %>%
    mutate(logDS = ds %>% log %>% as.numeric()) %>%
    separate(predFlow, c('predWest', 'predNorth', 'predEast')) %>%
    separate(simFlow, c('simWest', 'simNorth', 'simEast')) %>%
    separate(maxFlow, c('maxWest', 'maxNorth', 'maxEast')) %>%
    mutate_at(vars(matches('pred')|matches('sim')|matches('max')), as.numeric) %>%
    select(logDS, ds, trial, condition, display, feedback, 
           visRatio, sim, totalFlow, 
           simWest, simNorth, simEast, 
           predWest, predNorth, predEast,
           maxWest, maxNorth, maxEast)
  
  if (summarize500){
    dsModelDF = dsModelDF %>% 
      group_by(trial, display, feedback, condition) %>% 
      summarise(logDS = ds %>% mean %>% log,
                logSize = totalFlow %>% mean %>% log, 
                visRatio = visRatio %>% mean,
                .groups = 'drop')  
  }
  
  return(dsModelDF)
}

query_ratioData = function(aggDF, directory, summarize500 = T){
  ratioModelDF = aggDF %>%
    filter(collection == directory & trial != 0) %>% 
    select(ratio, trial, condition, predPickup, maxPickup, totalFlow, maxProb, sim, simPickup, maxPickup, display, feedback) %>%
    mutate(
      ratio = as.numeric(ratio),
      trial = as.numeric(trial),
      condition = as.factor(condition),
      visRatio = as.numeric(predPickup / maxPickup),
      totalFlow = as.numeric(totalFlow),
      maxProb = as.numeric(maxProb),
      sim = as.factor(sim),
      simPickup = as.numeric(simPickup),
      maxPickup = as.numeric(maxPickup),
      display = as.factor(display),
      feedback = as.factor(feedback)
      )
  
  if (summarize500){
    ratioModelDF = ratioModelDF %>% 
      group_by(trial, display, feedback, condition) %>%
      summarise(ratio = ratio %>% mean,
                maxProb = maxProb %>% mean,
                visRatio = visRatio %>% mean,
                .groups = 'drop')  
  }else{
    ratioModelDF %<>% 
      mutate(ratio = case_when(ratio == 1 ~ (1 + nth(sort(ratio, decreasing = T), 2)) / 2, 
                               T ~ ratio))
  }
  
  return(ratioModelDF)
}

create_modelData = function(directory = c('experiment', 'robustTrial', 'robustComp'), summarize500 = T){
  trialDF = readRDS('./data/modelData/individual.rds')
  aggDF = readRDS('./data/modelData/aggregate.rds')
  brLevel1DF = query_brData(trialDF, 1, directory)
  brLevel2DF = query_brData(trialDF, 2, directory)
  aeLevel1DF = query_aeData(trialDF, 1, directory)
  aeLevel2DF = query_aeData(trialDF, 2, directory)
  dsDF = query_dsData(aggDF, directory, summarize500)
  ratioDF = query_ratioData(aggDF, directory, summarize500)
  modelDataLst = list("brLevel1" = brLevel1DF,
                      "brLevel2" = brLevel2DF,
                      "aeLevel1" = aeLevel1DF,
                      "aeLevel2" = aeLevel2DF,
                      "ds" = dsDF,
                      "ratio" = ratioDF) 
  return(modelDataLst)
}

fit_br_model = function(modelDataLst, 
                        levelK = c(1, 2), 
                        directory = c('experiment', 'robustTrial', 'robustComp')){
  
  message(str_glue('=> Fitting BR Models: Level-{levelK}s | prereg: {directory}'))
  
  brms_formula = br ~ trial * display * feedback + (trial | id)
  savePath = str_glue('./data/modelFits/{directory}/brLevel{levelK}')
  
  modelData = modelDataLst[[str_glue('brLevel{levelK}')]]
  fit = brm(
    brms_formula,
    family = bernoulli(link = "logit"),
    data = modelData,
    prior = c(
      prior(normal(1, 5), class = "Intercept"),
      prior(normal(0, 2),  class = "b"),
      prior(normal(0, 2), class = "sd"),
      prior(lkj(2), class = "cor")
    ),
    chains = 4,
    cores = getOption("mc.cores", 4),
    iter = 10000,
    warmup = 5000,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 12),
    file = savePath,
    seed = 666
  )
  
  return(fit)
}


fit_ae_model = function(modelDataLst, 
                        levelK = c(1, 2), 
                        directory = c('experiment', 'robustTrial', 'robustComp')){
  
  message(str_glue('=> Fitting AE Models: Level-{levelK}s | prereg: {directory}'))
  
  brms_formula = logAE ~ trial * display * feedback + (trial | id)
  savePath = str_glue('./data/modelFits/{directory}/aeLevel{levelK}')
  
  modelData = modelDataLst[[str_glue('aeLevel{levelK}')]]
  fit = brm(
    brms_formula,
    family = student(),
    data = modelData,
    prior = c(
      prior(normal(1, 5), class = "Intercept"),
      prior(normal(0, 1),  class = "b"),
      prior(normal(0, 1), class = "sigma"),
      prior(normal(0, 1), class = "sd"),
      prior(gamma(2, 0.1), class = "nu"),
      prior(lkj(2), class = "cor")
    ),
    chains = 4,
    cores = getOption("mc.cores", 4),
    iter = 20000,
    warmup = 10000,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 13),
    file = savePath,
    seed = 666
  )
  
  return(fit)
}

fit_ds_model = function(modelDataLst, 
                        directory = c('experiment', 'robustTrial', 'robustComp')){
  
  message(str_glue('=> Fitting DS Models: prereg: {directory}'))
  brms_formula = logDS ~ trial * display * feedback
  savePath = str_glue('./data/modelFits/{directory}/dsMod')
  
  modelData = modelDataLst$ds
  fit = brm(
    brms_formula,
    family = student(),
    data = modelData,
    prior = c(
      prior(normal(2, 5), class = "Intercept"),
      prior(normal(0, 5), class = "b"),
      prior(normal(0, 1), class = "sigma"),
      prior(gamma(2, 0.1), class = "nu")
    ),
    chains = 4,
    cores = getOption("mc.cores", 4),
    iter = 10000,
    warmup = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    file = savePath,
    seed = 666
  )
  
  return(fit)
}

fit_ratio_model = function(modelDataLst, 
                           directory = c('experiment', 'robustTrial', 'robustComp')){
  
  message(str_glue('=> Fitting ratio Models: prereg: {directory}'))
  
  brms_formula =  bf(ratio ~ trial * display * feedback,
                     phi ~ trial * display * feedback + maxProb)
  savePath = str_glue('./data/modelFits/{directory}/ratioMod')
  
  modelData = modelDataLst$ratio
  fit = brm(
    brms_formula,
    family = Beta(link = 'logit', link_phi = 'log'),
    data = modelData,
    prior = c(
      prior(normal(2, 5), class = "Intercept"),
      prior(normal(0, 1), class = "b"),
      prior(normal(0, 50), class = 'Intercept', dpar = "phi"),
      prior(normal(0, 50), class = 'b', coef = 'maxProb', dpar = "phi"),
      prior(normal(0, 1), class = "b", dpar = "phi")
    ),
    chains = 4,
    cores = getOption("mc.cores", 4),
    iter = 10000,
    warmup = 5000,
    control = list(adapt_delta = 0.99, max_treedepth = 12),
    file = savePath,
    seed = 666
  )
  
  return(fit)
}

fit_individual_models = function(modelDataLst, 
                                 directory = c('experiment', 'robustTrial', 'robustComp')){
  message(str_glue("Collection: {directory}"))
  brLevel1Mod = fit_br_model(modelDataLst, 1, directory)
  brLevel2Mod = fit_br_model(modelDataLst, 2, directory)

  aeLevel1Mod = fit_ae_model(modelDataLst, 1, directory)
  aeLevel2Mod = fit_ae_model(modelDataLst, 2, directory)
  output_modelLst = list('brLevel1' = brLevel1Mod,
                         'brLevel2' = brLevel2Mod,
                         'aeLevel1' = aeLevel1Mod,
                         'aeLevel2' = aeLevel2Mod)
  
  return(output_modelLst)
}


fit_aggregate_models = function(modelDataLst, 
                                directory = c('experiment', 'robustTrial', 'robustComp')){
  message(str_glue("Collection: {directory}"))
  dsMod = fit_ds_model(modelDataLst, directory)
  ratioMod = fit_ratio_model(modelDataLst, directory)
  output_modelLst = list('dsMod' = dsMod,
                         'ratioMod' = ratioMod)
  
  return(output_modelLst)
}

fit_all_models = function(modelLst, directory = c('experiment', 'robustTrial', 'robustComp'),
                          sim500){
  message("Fitting individual-level Models...")
  individualModels = fit_individual_models(modelLst, directory)
  message("Fitting aggregate-level models...")
  aggregateModels = fit_aggregate_models(modelLst, directory)
  
  fullLst = append(individualModels, aggregateModels)
  return(fullLst)
}