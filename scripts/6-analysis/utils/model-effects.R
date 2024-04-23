construct_dummyData = function(collectionArm, ratio = F){
  newData = expand.grid(display = 0:1,
                        feedback = 0:1,
                        trial = 1:15)
  
  if (ratio){
    ratioMod = modelVault[[collectionArm]][['ratioMod']]
    maxProb = ratioMod$data %>% select(trial, maxProb) %>% distinct()
    output = newData %>% 
      left_join(maxProb, by = 'trial')
    return(output)
  }else{
    return(newData)
  }
}

draw_epreds = function(model, collectionArm, modelName, numDraws = 500){
  ratioFlag = ifelse(modelName == "ratioMod", T, F)
  newData = construct_dummyData(collectionArm = collectionArm, 
                                ratio = ratioFlag)
  
  tidy_epreds = model %>% 
    epred_draws(newdata = newData, 
                ndraws = numDraws, 
                re_formula = NA) %>%
    ungroup() %>%
    select(-.row)
  
  tidy_epreds %<>% 
    mutate(display = case_when(display == 0 ~ 'Static',
                               display == 1 ~ 'NetHOPs'),
           feedback = case_when(feedback == 0 ~ 'Bandit',
                                feedback == 1 ~ 'Full'))
  
  return(tidy_epreds)
}

compute_interval_single_condition = function(epredDF, effectName){
  output = epredDF %>% 
    select(.epred, all_of(effectName)) %>%
    rename('effect' = effectName) %>%
    group_by(effect) %>% 
    median_hdci %>%
    mutate(condition = effectName, type = 'condition') %>%
    select(type, condition, effect, .lower, .epred, .upper)
  return(output)
}

format_condition_effect = function(epredDF, 
                                       response = c('br', 'ae', 'ds', 'ratio'), 
                                       level = c('Level-1', 'Level-2', 'none')){
  outputDF = rbind(
    compute_interval_single_condition(epredDF, 'display'),
    compute_interval_single_condition(epredDF, 'feedback')
  ) %>% 
    mutate(response = response,
           level = level) %>% 
    select(type, response, level, condition, effect, .lower, .epred, .upper)
  
  if (response %in% c('ae', 'ds')){
    outputDF %<>% 
      mutate_if(is.numeric, exp)
  }
  
  return(outputDF)
}

compute_condition_effect = function(collectionArm = c('experiment', 'robustTrial', 'robustComp'),
                                 modelName = c('brLevel1', 'brLevel2',
                                               'aeLevel1', 'aeLevel2',
                                               'dsMod', 'ratioMod'),
                                 response = c('br', 'ae', 'ds', 'ratio'), 
                                 level = c('Level-1', 'Level-2', 'NA')){
  
  collectionLst = list('experiment' = 'Main Experiment',
                       'robustTrial' = 'Robust Trial',
                       'robustComp' = 'Robust Comp.')
  # Get the model
  model = modelVault[[collectionArm]][[modelName]]
  
  # Compute epreds
  epredDF = draw_epreds(model, collectionArm, modelName)
  
  outputDF = epredDF %>% 
    format_condition_effect(response, level) %>%
    mutate(collection = collectionLst[[collectionArm]]) %>% 
    select(collection, type, response, level, condition, effect, .lower, .epred, .upper)
  
  return(outputDF)
}


compute_allModels_effects = function(effectType = c('condition', 'interface', 'learning')){
  collectionArm = rep(c('experiment', 'robustTrial', 'robustComp'), each = 6)
  modelName = rep(c('brLevel1', 'brLevel2', 'aeLevel1', 'aeLevel2', 'dsMod', 'ratioMod'), times = 3)
  response = rep(c('br', 'br', 'ae', 'ae', 'ds', 'ratio'), times = 3)
  level = rep(c('Level-1', 'Level-2', 'Level-1', 'Level-2', 'NA', 'NA'), times = 3)
  
  argsLst = data.frame(collectionArm, modelName, response, level) %>% 
    filter( !(collectionArm == 'robustComp' & level == 'Level-1' )) %>%
    group_split(row_number(), .keep = F) 
    
  outputLst = list()
  for (args in argsLst){
    print(args)
    if (effectType == 'condition'){
      effectDF = compute_condition_effect(args$collectionArm, args$modelName, 
                                       args$response, args$level)  
    }else if (effectType == 'interface'){
      effectDF = compute_interface_effect(args$collectionArm, args$modelName, 
                                    args$response, args$level)
    }else{
      effectDF = compute_learning_effect(args$collectionArm, args$modelName, 
                                         args$response, args$level) %>% 
        mutate(trial = as.numeric(trial))
    }
     
    outputLst %<>% list.append(effectDF)
  }
  
  outputDF = outputLst %>% bind_rows()
  return(outputDF)
}

format_interface_effect = function(epredDF, 
                          response = c('br', 'ae', 'ds', 'ratio'), 
                          level = c('Level-1', 'Level-2', 'none')){
  
  outputDF = epredDF %>% 
    mutate(interface = paste0(display, '+', feedback)) %>%
    select(-display, -feedback) %>%
    select(.epred, interface) %>%
    group_by(interface) %>% 
    median_hdci %>%
    select(interface, .epred, .lower, .epred, .upper) %>% 
    mutate(response = response,
           level = level) %>% 
    select(response, level, interface, .lower, .epred, .upper)
  
  if (response %in% c('ae', 'ds')){
    outputDF %<>% 
      mutate_if(is.numeric, exp)
  }
  
  return(outputDF)
}

compute_interface_effect = function(collectionArm = c('experiment', 'robustTrial', 'robustComp'),
                              modelName = c('brLevel1', 'brLevel2', 
                                            'aeLevel1', 'aeLevel2',
                                            'dsMod', 'ratioMod'),
                              response = c('br', 'ae', 'ds', 'ratio'),
                              level = c('Level-1', 'Level-2', 'NA')) {
  # Get the model
  model = modelVault[[collectionArm]][[modelName]]
  collectionLst = list('experiment' = 'Main Experiment',
                       'robustTrial' = 'Robust Trial',
                       'robustComp' = 'Robust Comp.')
  
  # Compute epreds
  epredDF = draw_epreds(model, collectionArm, modelName)
  
  outputDF = epredDF %>%
    format_interface_effect(response, level) %>%
    mutate(collection = collectionLst[[collectionArm]]) %>%
    select(collection, response, level, interface, .lower, .epred, .upper)
  
  return(outputDF)
}

format_learning_effect = function(epredDF, 
                               response = c('br', 'ae', 'ds', 'ratio'), 
                               level = c('Level-1', 'Level-2', 'none')){
  
  outputDF = epredDF %>% 
    mutate(interface = paste0(display, '+', feedback)) %>%
    select(-display, -feedback) %>%
    select(.epred, interface, trial) %>%
    group_by(interface, trial) %>% 
    median_hdci %>%
    select(interface, trial, .epred, .lower, .epred, .upper) %>% 
    mutate(response = response,
           level = level) %>% 
    select(response, level, trial, interface, .lower, .epred, .upper)
  
  if (response %in% c('ae', 'ds')){
    outputDF %<>% 
      mutate(trial = as.factor(trial)) %>%
      mutate_if(is.numeric, exp)
  }
  
  return(outputDF)
}


compute_learning_effect = function(collectionArm = c('experiment', 'robustTrial', 'robustComp'),
                                    modelName = c('brLevel1', 'brLevel2', 
                                                  'aeLevel1', 'aeLevel2',
                                                  'dsMod', 'ratioMod'),
                                    response = c('br', 'ae', 'ds', 'ratio'),
                                    level = c('Level-1', 'Level-2', 'NA')) {
  # Get the model
  model = modelVault[[collectionArm]][[modelName]]
  collectionLst = list('experiment' = 'Main Experiment',
                       'robustTrial' = 'Robust Trial',
                       'robustComp' = 'Robust Comp.')
  
  # Compute epreds
  epredDF = draw_epreds(model, collectionArm, modelName)
  
  outputDF = epredDF %>%
    format_learning_effect(response, level) %>%
    mutate(collection = collectionLst[[collectionArm]]) %>%
    select(collection, response, level, interface, trial, .lower, .epred, .upper)
  
  return(outputDF)
}

format_output = function(inputDF){
  outputDF = inputDF %>%
    mutate(collection = factor(collection, levels = c('Main Experiment', 'Robust Trial', 'Robust Comp.')),
           level = factor(level, levels = c('Level-1', 'Level-2', NA)),
           interface = factor(interface, 
                              levels = c('Static+Bandit', 'Static+Full', 
                                         'NetHOPs+Bandit', 'NetHOPs+Full')))
  
  return(outputDF)
}