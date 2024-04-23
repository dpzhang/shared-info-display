process_strategy = function(filePath){
  rawData = fromJSON(file = filePath)[["__collections__"]]
  fullResponse = rawData %>% lapply(length) %>% `==`(18)  
  
  strategyDF = rawData[fullResponse] %>% 
    lapply(function(x) data.frame(
      display = x$groups$display,
      feedback = x$groups$feedback,
      strategy = x$strategy$strategy)) %>%
    bind_rows(.id = 'id')
  
  return(strategyDF)
}

process_JSON_data = function(filePath, expLevel){
  rawData = fromJSON(file = filePath)[["__collections__"]]
  
  numRecorded = rawData %>% length
  print(paste("Total number of participants recorded:", numRecorded))
  
  fullResponse = rawData %>% lapply(length) %>% `==`(18)  
  responseDF = rawData[fullResponse]
  numComplete = responseDF %>% length
  print(paste("Total number of participants with complete response:", numComplete))
  
  userData = responseDF %>% 
    mclapply(process_singleUserData, mc.cores = 8) %>%
    bind_rows(.id = 'id') %>% 
    mutate(level = expLevel)

  return(userData)
}

get_modelInputRange = function(model){
  rangeDF = model$data %>% 
    group_by(ca) %>% 
    summarise(max = max(flow),
              min = min(flow), .groups = 'drop') %>%
    pivot_longer(cols = -ca,
                 names_to = 'type',
                 values_to = 'flow') %>%
    add_epred_draws(model) %>% 
    group_by(ca, flow, type) %>% 
    summarise(predPickup = mean(.epred) %>% round(0), .groups = 'drop') %>%
    mutate(predProb = predPickup / flow) %>%
    pivot_wider(id_cols = ca,
                names_from = type,
                values_from = c(flow, predPickup, predProb))
  
  return(rangeDF)
}

process_singleUserData = function(userData){
  treatmentInfo = userData$groups %>% unlist %>% t %>% as.data.frame
  strategyDesc = userData$strategy %>% unlist %>% t %>% as.data.frame 
  userStrategy = strategyDesc$strategy
  treatmentDF = cbind(treatmentInfo, strategyDesc) %>% 
    select(-strategy) %>%
    mutate_at(vars(ends_with('Time')), as.numeric) %>%
    mutate(completionTime = (endTime - startTime) / (60 * 1000)) %>%
    select(-startTime, -endTime) %>%
    mutate(feedback = ifelse(feedback == 'results', 0, 1),
           display = ifelse(display == 'static', 0, 1))
  
  trialNames = names(userData)[!names(userData) %in% c('groups', 'strategy')]
  trialData = userData[trialNames]
  trialDF = trialData %>% 
    lapply(process_singleUserTrialData) %>%
    bind_rows() %>%
    arrange(trial)
  
  outputDF = cbind(treatmentDF, trialDF)
  return(outputDF)
}

process_singleUserTrialData = function(trialData){
  rawTrialData = trialData %>% 
    unlist %>% 
    t %>% 
    as.data.frame %>%
    mutate_at(vars(ends_with('Finish') | ends_with('Start')), as.numeric) %>%
    mutate(feedbackTime = (feedbackFinish - feedbackStart) / (60 * 1000), 
           taskTime = (taskFinish - taskStart) / (60 * 1000))
  
  outputData = rawTrialData %>% 
    select(round, west, north, east, decision, pickup, currentReward, taskTime, feedbackTime) %>%
    mutate(across(!decision & !pickup, as.numeric)) %>%
    rename(trial = round, west_guess = west, north_guess = north, east_guess = east) %>%
    mutate(currentReward = round(currentReward, 2))
  
  
  return(outputData)
}

integrate_level1_networkData = function(outputDF,
                                        directory = c('experiment', 'robustTrial', 'robustComp')){
  
  stage1InterfaceDataLst = readRDS(str_glue('../3-experiment/data/interfaceData/{directory}/stage1InterfaceData-{directory}.Rdata'))
  predFlowDF = stage1InterfaceDataLst %>% 
    lapply(function(x) x$staticPred %>% 
             select(ca,meanFlow) %>%
             mutate(ca = case_when(ca == 8 ~ 'north_flow_pred',
                                   ca == 28 ~ 'west_flow_pred',
                                   ca == 32 ~ 'east_flow_pred')) %>%
             t %>%
             data.frame %>%
             janitor::row_to_names(row_number = 1) %>%
             `rownames<-`( NULL )) %>%
    bind_rows(.id = 'id') %>%
    mutate(id = as.numeric(id) - 1) %>%
    rename(trial = id)
  
  predProbDF = stage1InterfaceDataLst %>% 
    lapply(function(x) x$staticPred %>% 
             select(ca, meanProb) %>%
             mutate(ca = case_when(ca == 8 ~ 'north_prob_pred',
                                   ca == 28 ~ 'west_prob_pred',
                                   ca == 32 ~ 'east_prob_pred')) %>%
             t %>%
             data.frame %>%
             janitor::row_to_names(row_number = 1) %>%
             `rownames<-`( NULL )) %>%
    bind_rows(.id = 'id') %>%
    mutate(id = as.numeric(id) - 1) %>%
    rename(trial = id) %>%
    select(-trial) %>%
    mutate(br1=names(.)[max.col(.)]) %>%
    tibble::rownames_to_column(var = 'trial') %>%
    mutate(trial = as.numeric(trial)-1) %>%
    mutate(br1 = str_extract(br1, "^[^_]*")) %>%
    mutate(br2 = case_when( (north_prob_pred == west_prob_pred) & (br1 == 'north') ~ 'west',
                            (north_prob_pred == west_prob_pred) & (br1 == 'west') ~ 'north')) %>%
    mutate(br2 = ifelse(is.na(br2), 'none', br2))
  
  predDF = predFlowDF %>% left_join(predProbDF, by = "trial")
  
  realizedFlowDF = stage1InterfaceDataLst %>% 
    lapply(function(x) x$realized %>% 
             select(ca,realizedFlow) %>%
             mutate(ca = case_when(ca == 8 ~ 'north_flow_realized',
                                   ca == 28 ~ 'west_flow_realized',
                                   ca == 32 ~ 'east_flow_realized')) %>%
             t %>%
             data.frame %>%
             janitor::row_to_names(row_number = 1) %>%
             `rownames<-`( NULL )) %>%
    bind_rows(.id = 'id') %>%
    mutate(id = as.numeric(id) - 1) %>%
    rename(trial = id)
  
  realizedProbDF = stage1InterfaceDataLst %>% 
    lapply(function(x) x$realized %>% 
             select(ca,realizedProb) %>%
             mutate(ca = case_when(ca == 8 ~ 'north_prob_realized',
                                   ca == 28 ~ 'west_prob_realized',
                                   ca == 32 ~ 'east_prob_realized')) %>%
             t %>%
             data.frame %>%
             janitor::row_to_names(row_number = 1) %>%
             `rownames<-`( NULL )) %>%
    bind_rows(.id = 'id') %>%
    mutate(id = as.numeric(id) - 1) %>%
    rename(trial = id)
  
  realizedDF = realizedFlowDF %>% left_join(realizedProbDF, by = "trial")
  
    
  fullDF = outputDF %>% 
    left_join(predDF, by = 'trial') %>%
    left_join(realizedDF, by = 'trial') %>%
    mutate(userBR = ifelse(decision == br1 | decision == br2, 1, 0))
  
  return(fullDF)
}

integrate_level2_networkData = function(outputDF, directory = c('experiment', 'robustTrial', 'robustComp')){
  stage1DataLst = readRDS(str_glue('../3-experiment/data/interfaceData/{directory}/stage1InterfaceData-{directory}.Rdata'))
  stage2DataDF = readRDS(str_glue('../3-experiment/data/interfaceData/{directory}/stage2Realized-{directory}.Rdata')) %>%
    bind_rows
  
  predFlowDF = stage1DataLst %>% 
    lapply(function(x) x$staticPred %>% 
             select(ca,meanFlow) %>%
             mutate(ca = case_when(ca == 8 ~ 'north_flow_pred',
                                   ca == 28 ~ 'west_flow_pred',
                                   ca == 32 ~ 'east_flow_pred')) %>%
             t %>%
             data.frame %>%
             janitor::row_to_names(row_number = 1) %>%
             `rownames<-`( NULL )) %>%
    bind_rows(.id = 'id') %>%
    mutate(id = as.numeric(id) - 1) %>%
    rename(trial = id)
  
  predProbDF = stage1DataLst %>% 
    lapply(function(x) x$staticPred %>% 
             select(ca, meanProb) %>%
             mutate(ca = case_when(ca == 8 ~ 'north_prob_pred',
                                   ca == 28 ~ 'west_prob_pred',
                                   ca == 32 ~ 'east_prob_pred')) %>%
             t %>%
             data.frame %>%
             janitor::row_to_names(row_number = 1) %>%
             `rownames<-`( NULL )) %>%
    bind_rows(.id = 'id') %>%
    mutate(id = as.numeric(id) - 1) %>%
    rename(trial = id)
  
  predDF = predFlowDF %>% left_join(predProbDF, by = "trial")
  realizedFlowDF = stage2DataDF %>% 
    select(trialNum, display, feedback, id, realizedFlow) %>%
    mutate(display = ifelse(display == 'static', 0, 1),
           feedback = ifelse(feedback == 'results', 0, 1),
           id = case_when(id == 8 ~ 'north_flow_realized',
                          id == 28 ~ 'west_flow_realized',
                          id == 32 ~ 'east_flow_realized')) %>%
    pivot_wider(names_from = id, values_from = realizedFlow) 
  
  
  realizedProbDF_long = stage2DataDF %>% 
    select(trialNum, display, feedback, id, realizedProb) %>%
    mutate(display = ifelse(display == 'static', 0, 1),
           feedback = ifelse(feedback == 'results', 0, 1),
           id = case_when(id == 8 ~ 'north_prob_realized',
                          id == 28 ~ 'west_prob_realized',
                          id == 32 ~ 'east_prob_realized'))
  
  realizedProbDF = realizedProbDF_long %>%
    pivot_wider(names_from = id, values_from = realizedProb)
  
  realizedDF = realizedFlowDF %>% 
    left_join(realizedProbDF, by = c('trialNum', 'display', 'feedback')) %>%
    rename(trial = trialNum)
  
  brDF = stage2DataDF %>%
    rename(ca = id, flow = realizedFlow) %>%
    add_epred_draws(mod) %>%
    group_by(trialNum, display, feedback, ca, flow, realizedProb) %>%
    summarise(realizedPickup = mean(.epred) %>% round(0), .groups = 'drop') %>%
    mutate(ca = as.numeric(ca)) %>%
    left_join(get_modelInputRange(mod), by = 'ca') %>%
    rename(id = ca, realizedFlow = flow) %>%
    mutate(realizedPickup = ifelse(realizedFlow > flow_max, predPickup_max, realizedPickup)) %>%
    mutate(realizedProb = realizedPickup/realizedFlow) %>%
    mutate(realizedProb = ifelse(realizedProb > 1, 1, realizedProb)) %>%
    select(trialNum, display, feedback, realizedProb, id) %>%
    group_by(trialNum, display, feedback) %>%
    filter(realizedProb == max(realizedProb)) %>%
    ungroup %>%
    mutate(display = ifelse(display == 'static', 0, 1),
           feedback = ifelse(feedback == 'results', 0, 1),
           id  = case_when(id == 8 ~ 'north',
                           id == 28 ~ 'west',
                           id == 32 ~ 'east')) %>%
    rename(br = id) %>%
    select(trialNum, display, feedback, br) %>%
    pivot_wider(id_cols = c('trialNum', 'display', 'feedback'), names_from = br, values_from = br) %>%
    setNames(c('trial', 'display', 'feedback', 'br1', 'br2')) %>%
    mutate(br1 = ifelse(is.na(br1), 'none', br1),
           br2 = ifelse(is.na(br2), 'none', br2))

  fullDF = outputDF %>% 
    left_join(predDF, by = 'trial') %>%
    left_join(realizedDF, by = c('trial', 'display', 'feedback')) %>%
    left_join(brDF, by = c('trial', 'display', 'feedback')) %>%
    mutate(userBR = ifelse(decision == br1 | decision == br2, 1, 0))
  
  return(fullDF)
}

compute_EMD = function(userGuess, benchMark){
  guessDist = userGuess %>% 
    str_split(',') %>%
    unlist %>% 
    as.numeric
  guessMat = data.frame(x = c(rep(1, times = guessDist[1]), 
                              rep(2, times = guessDist[2]),
                              rep(3, times = guessDist[3])),
                        y = c(1:guessDist[1], 1:guessDist[2], 1:guessDist[3])) %>%
    as.matrix
  
  
  benchDist = benchMark %>%
    str_split(',') %>%
    unlist %>% 
    as.numeric
  benchMat = data.frame(x = c(rep(1, times = benchDist[1]), 
                              rep(2, times = benchDist[2]),
                              rep(3, times = benchDist[3])),
                        y = c(1:benchDist[1], 1:benchDist[2], 1:benchDist[3])) %>%
    as.matrix
  
  diff = emdw(A = guessMat, wA = 1, 
              B = benchMat, wB = 1, dist = 'manhattan', max.iter = 1e6)
  return(diff)
}


compute_AE = function(processedDF){
  templateDF = processedDF %>%
    select(id, level, collection, display, feedback, completionTime, trial, 
           decision, userBR, pickup, currentReward, taskTime, feedbackTime,
           west_guess, north_guess, east_guess, 
           taskTime, feedbackTime,
           west_flow_pred, north_flow_pred, east_flow_pred,
           west_prob_pred, north_prob_pred, east_prob_pred,
           west_flow_realized, north_flow_realized, east_flow_realized,
           west_prob_realized, north_prob_realized, east_prob_realized,
           br1, br2) %>%
    mutate(userGuess = paste0(west_guess, ',', north_guess, ',', east_guess),
           predFlow = paste0(west_flow_pred, ',', north_flow_pred, ',', east_flow_pred),
           realizedFlow = paste0(west_flow_realized, ',', north_flow_realized, ',', east_flow_realized))
  
  aeDF = templateDF %>%
    select(id, trial, userGuess, realizedFlow) %>%
    group_split(row_number()) %>%
    mclapply(function(argsDF) {
      AE = compute_EMD(userGuess = argsDF$userGuess, benchMark = argsDF$realizedFlow)
      outputDF = data.frame(id = argsDF$id,
                            trial = argsDF$trial,
                            ae = AE)
      return(outputDF)
      }
      , mc.cores = 8) %>%
    bind_rows

  outputDF = templateDF %>%
    left_join(aeDF, by = c('id', 'trial'))

  return(outputDF)
}

sample_experiment_stage1 = function(sampleSize){
  experiment_stage1DF = read.csv('../3-experiment/data/collectedData/experiment/stage1Data-experiment.csv')
  
  set.seed(666)
  sampledIDs = experiment_stage1DF %>% 
    group_split(display, feedback) %>% 
    lapply(function(conditionDF) conditionDF %>% pull(id) %>% unique) %>%
    lapply(sample, 53, F) %>%
    unlist
  
  robustComp_stage1DF = experiment_stage1DF %>% 
    filter(id %in% sampledIDs)
  
  return(robustComp_stage1DF)
}
