remove_outlier_participants = function(df, threshold = 5){
  outputDF = df %>% 
    group_by(id) %>% 
    filter(all(taskTime > 5/60)) %>%
    ungroup
  return(outputDF)
}

load_individual = function(directory = c('experiment', 'robustTrial', 'robustComp')){
  stage1DF = read.csv(str_glue('../3-experiment/data/collectedData/{directory}/stage1Data-{directory}.csv')) %>%
    remove_outlier_participants
  stage2DF = read.csv(str_glue('../3-experiment/data/collectedData/{directory}/stage2Data-{directory}.csv')) %>%
    remove_outlier_participants
  
  cleanData = stage1DF %>% rbind(stage2DF)
  
  write.csv(cleanData, str_glue('./data/experiment/trialData/{directory}.csv'), row.names = F)
  return(cleanData)
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
              B = benchMat, wB = 1, dist = 'euclidean', max.iter = 1e6)
  return(diff)
}

compute_DS = function(processedSimDF){
  dsDF = processedSimDF %>%
    group_split(row_number()) %>%
    mclapply(function(argDF) {
      DS = compute_EMD(argDF$simFlow, argDF$predFlow)
      outputDF = argDF %>%
        select(-"row_number()") %>%
        mutate(ds = DS)
      return(outputDF)
    },
    mc.cores = 8) %>%
    bind_rows  
  
  return(dsDF)
}

produce_fullLevel_samplingInstruction = function(orderVec, 
                                                 directory = c('experiment', 'robustTrial', 'robustComp')){
  level0Comp = ifelse(directory == 'robustComp', 0.15, 0.3)
  level1Comp = ifelse(directory == 'robustComp', 0.35, 0.4)
  
  templateDF = data.frame(tripdate = orderVec)
  
  raw_samplingSizeDF = modelDF %>%
    filter(tripdate %in% orderVec) %>%
    group_by(tripdate) %>%
    summarise(totalFlow = sum(flow), .groups = 'drop') %>%
    mutate(level0 = round(totalFlow * level0Comp, 0)) %>%
    mutate(level1 = round(totalFlow * level1Comp, 0)) %>%
    mutate(level2 = round(totalFlow - level0 - level1))
  
  samplingSizeDF = templateDF %>% left_join(raw_samplingSizeDF, by = 'tripdate') %>%
    mutate(trial = seq(0, 15, by = 1)) %>%
    select(tripdate, trial, totalFlow, level0, level1, level2) %>%
    mutate(level0Comp = level0/totalFlow, level1Comp = level1/totalFlow, level2Comp = level2/totalFlow) %>%
    mutate_at(c("level0Comp", "level1Comp", "level2Comp"), round, 2) %>%
    mutate(stage = directory)
  
  
  return(samplingSizeDF)
}

sample_level0Flow = function(samplingArgsDF){
  trialNum = samplingArgsDF$trial
  directory = samplingArgsDF$stage
  priorLst = readRDS(str_glue('../3-experiment/data/level0Prior/level0Prior.Rdata'))
  targetDate = samplingArgsDF$tripdate
  level0Size = samplingArgsDF$level0
  
  identifier = priorLst %>% 
    lapply(function(priorDF) {
      priorDF %>% 
        bind_rows %>%
        filter(tripdate == targetDate)
    }) %>% 
    lapply(nrow) %>% 
    unlist
  targetIndex = which(identifier != 0)
  
  sampledLevel0 = priorLst %>%
    extract2(targetIndex) %>%
    lapply(function(priorDF) {
      if (nrow(priorDF) == 0){
        priorDF = data.frame(taxiid = NA,
                             prevDropoff = NA,
                             nextPickup = targetCA,
                             num = 0,
                             weight = 1/length(targetCA))
      }
      return(priorDF)
    }) %>% 
    sample(size = level0Size, replace = F) %>% 
    bind_rows(.id = "identifier") %>%
    group_by(identifier) %>%
    sample_n(size = 1, replace = F, weight = weight)
  
  level0Flow = sampledLevel0 %>% 
    group_by(nextPickup) %>% 
    summarise(level0Flow = n(), .groups = 'drop') %>%
    mutate(trial = targetIndex-1,
           tripdate = targetDate) %>%
    select(trial, tripdate, nextPickup, level0Flow) %>%
    rename(ca = nextPickup)
  
  return(level0Flow)
}

sample_level0Flow_for4Conditions = function(samplingArgsDF){
  cond00 = sample_level0Flow(samplingArgsDF) %>% mutate(display = 0, feedback = 0)
  cond01 = sample_level0Flow(samplingArgsDF) %>% mutate(display = 0, feedback = 1)
  cond10 = sample_level0Flow(samplingArgsDF) %>% mutate(display = 1, feedback = 0)
  cond11 = sample_level0Flow(samplingArgsDF) %>% mutate(display = 1, feedback = 1)
  
  level0FlowOutput = rbind(cond00, cond01, cond10, cond11)
  return(level0FlowOutput)
}

sample_higherLevelFlow = function(levelK, samplingArgsDF){
  targetDate = samplingArgsDF$tripdate
  trialNum = samplingArgsDF$trial
  directory = samplingArgsDF$stage
  expDF = load_individual(directory)
  levelSize = samplingArgsDF %>% pull(paste0('level', levelK))
  
  decisionLst = expDF %>% 
    filter(level == levelK & trial == trialNum) %>%
    select(display, feedback, decision) %>%
    group_split(display, feedback) %>%
    lapply(function(conditionDF){
      flowDF = conditionDF %>% 
        group_by(display, feedback, decision) %>%
        summarise(count = n(), .groups = 'drop') %>%
        sample_n(size = levelSize, weight = count, replace = T) %>%
        group_by(display, feedback, decision) %>%
        summarise(flow = n(), .groups = 'drop') %>%
        mutate(trial = trialNum, tripdate = targetDate) %>%
        select(trial, tripdate, display, feedback, decision, flow) %>%
        rename(ca = decision) %>%
        mutate(ca = case_when(ca == 'west'~28,
                              ca == 'north'~8,
                              ca == 'east'~32))
      
      templateDF = data.frame(trial = trialNum,
                              tripdate = targetDate,
                              display = unique(flowDF$display),
                              feedback = unique(flowDF$feedback),
                              ca = targetCA,
                              placeHolder = 0)
      
      outputDF = flowDF %>%
        right_join(templateDF, by = c('ca', 'trial', 'tripdate', 'display', 'feedback')) %>%
        mutate(flow = ifelse(is.na(flow), placeHolder, flow)) %>%
        select(-placeHolder) %>%
        setNames(c('trial', 'tripdate', 'display', 'feedback', 'ca', paste0("level", levelK, "Flow")))
      
      return(outputDF)
    })
  
  return(decisionLst)
}

get_modelInputRange = function(){
  rangeDF = mod$data %>% 
    group_by(ca) %>% 
    summarise(max = max(flow),
              min = min(flow), .groups = 'drop') %>%
    pivot_longer(cols = -ca,
                 names_to = 'type',
                 values_to = 'flow') %>%
    add_epred_draws(mod) %>% 
    group_by(ca, flow, type) %>% 
    summarise(predPickup = mean(.epred) %>% round(0), .groups = 'drop') %>%
    mutate(predProb = predPickup / flow) %>%
    pivot_wider(id_cols = ca,
                names_from = type,
                values_from = c(flow, predPickup, predProb))
  
  return(rangeDF)
}

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
    select(trial, sim, display, feedback, ca, flow)
  
  simPredDF = mergedFlowDF %>% 
    add_epred_draws(mod) %>% 
    group_by(trial, sim, display, feedback, ca, flow, .row) %>%
    summarise(predPickup = mean(.epred) %>% round(0), .groups = 'drop') %>%
    select(-.row) %>%
    rename(simFlow = flow)
  
  modelBoundsDF = get_modelInputRange()
  
  simOutcomeDF = simPredDF %>% 
    left_join(modelBoundsDF, by = 'ca') %>%
    mutate(predPickup = ifelse(simFlow > flow_max, predPickup_max, predPickup)) %>%
    mutate(predPickup = ifelse(simFlow < predPickup, simFlow, predPickup)) %>%
    select(trial, sim, display, feedback, ca, simFlow, predPickup)
  
  simulationDF = simOutcomeDF %>% 
    mutate(ca = case_when(ca == 8 ~ 'north',
                          ca == 28 ~ 'west',
                          ca == 32 ~ 'east')) %>%
    pivot_wider(id_cols = c(trial, sim, display, feedback),
                names_from = ca,
                values_from = c(simFlow, predPickup)) %>%
    mutate(simFlow = paste(simFlow_west, simFlow_north, simFlow_east, sep = ','),
           simPickup = predPickup_west + predPickup_north + predPickup_east,
           totalFlow = simFlow_west + simFlow_north + simFlow_east) %>%
    select(-starts_with("simFlow_"), -starts_with("predPickup_")) %>%
    mutate(simProb = simPickup/totalFlow)
    
  return(simulationDF)
}

simulate_allScenarios_once = function(curSimID, samplingInstruction){
  simulatedAllDF = samplingInstruction %>% 
    group_split(rowNum = row_number()) %>%
    lapply(function(argsDF) simulate_single_scenario(curSimID, argsDF)) %>%
    bind_rows
  return(simulatedAllDF)
}

simulateN = function(numSim, 
                     orderVec, directory = c('experiment', 'robustTrial', 'robustComp')){
  samplingInstruction = produce_fullLevel_samplingInstruction(orderVec, directory)
  
  simulationDF = as.list(1:numSim) %>% 
    mclapply(function(simulationID) simulate_allScenarios_once(simulationID, samplingInstruction),
             mc.cores = 8) %>%
    bind_rows
  
  return(simulationDF)
}

process_simulations = function(nSimulationDF, directory = c('experiment', 'robustTrial', 'robustComp')){
  predData = readRDS(str_glue('../3-experiment/data/interfaceData/{directory}/stage1InterfaceData-{directory}.Rdata')) %>%
    lapply(function(trialLst) trialLst[['staticPred']]) %>%
    bind_rows(.id = 'trial') %>% 
    mutate(trial = as.numeric(trial)-1) %>%
    mutate(decision = case_when(ca == 8 ~ 'north',
                                ca == 28 ~ 'west',
                                ca == 32 ~ 'east')) %>%
    pivot_wider(id_cols = trial,
                names_from = decision,
                values_from = c(meanFlow, meanPickup)) %>%
    mutate(predFlow = paste(meanFlow_west, meanFlow_north, meanFlow_east, sep = ','),
           totalFlow = meanFlow_west + meanFlow_north + meanFlow_east,
           predPickup = meanPickup_west + meanPickup_north + meanPickup_east) %>%
    mutate(predProb = predPickup / totalFlow) %>%
    select(trial, predFlow, totalFlow, predPickup, predProb)
  
  simPredDF = nSimulationDF %>% 
    left_join(predData, by = c('trial', 'totalFlow')) %>%
    select(trial, sim, display, feedback, totalFlow, 
           simFlow, simPickup, simProb, 
           predFlow, predPickup, predProb)
  
  return(simPredDF)
}

generate_aggregate_data = function(numSim, orderVec, directory = c('experiment', 'robustTrial', 'robustComp')){
  message('Processing data: \nSimulating 500 flows and compute ds...')
  start.time <- Sys.time()
  simulationDF = simulateN(numSim, orderVec, directory) %>%
    process_simulations(directory) %>%
    compute_DS
  write.csv(simulationDF, str_glue('./data/process/simulation/simulation500-{directory}.csv'), row.names = F)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message('Finished 500 simulations, and it takes: ', time.taken, ' hrs')
  
  message('Optimizing 16 scenarios based on the flow range of the simulated data...')
  start.time <- Sys.time()
  optimalDF = optimizeAll(simulationDF, directory)
  write.csv(optimalDF, str_glue('./data/process/maxWelfare/optimalWelfare-{directory}.csv'), row.names = F)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message('Finished 16 optimizations, and it takes: ', time.taken, ' hrs')
  
  aggregateDF = simulationDF %>% 
    left_join(optimalDF, by = c('trial','totalFlow')) %>%
    rename(maxFlow = optimalFlow,
           maxPickup = maximalPickup) %>%
    mutate(maxProb = maxPickup / totalFlow,
           welfareRatio = simPickup / maxPickup) %>%
    select(trial, sim, display, feedback, totalFlow, ds,
           simFlow, simPickup, simProb,
           predFlow, predPickup, predProb,
           maxFlow, maxPickup, maxProb,
           convergence, nfuneval, lagrange, welfareRatio)
  
  sanityCheck = aggregateDF %>%
    mutate(wrong = welfareRatio > 1) %>%
    filter(wrong == T) %>%
    nrow
  message('Sanity check: number of invalid row(s) is: ', sanityCheck)
  
  write.csv(aggregateDF, str_glue('./data/experiment/simulationData/aggregate-{directory}.csv'), row.names = F)
  
  return(aggregateDF)
}