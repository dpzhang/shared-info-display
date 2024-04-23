get_level_comp = function(modelDF, trialOrderVec, 
                            experimentStage = c('experiment', 
                                                'robustTrial', 
                                                'robustComp')){
  minTotalFlow = modelDF %>%
    filter(tripdate %in% trialOrderVec) %>% 
    group_by(tripdate) %>%
    summarise(totalFlow = sum(flow), .groups = 'drop') %>%
    pull(totalFlow) %>% 
    range %>%
    min
  
  set.seed(2)
  if (experimentStage %in% c('experiment', 'robustTrial')){
    compTable = rpois(minTotalFlow, 1.5) %>% table 
  }else{
    compTable = rpois(minTotalFlow, 3) %>% table 
  }
  
  comp = (compTable[1:3] / sum(compTable[1:3])) %>% round(1)
  
  roundedCompLevel2 = round(comp[1:2]/sum(comp[1:2]), 1)
  
  level2PerspectiveDF = as.data.frame(roundedCompLevel2) %>%
    setNames(c('level', 'level2WorldView'))
  
  return(level2PerspectiveDF)
}


produce_samplingInstruction = function(trialOrderVec, 
                                       directory = c('experiment', 'robustTrial', 'robustComp')){
  level0Comp = ifelse(directory == "robustComp", 0.2, 0.4)
  templateDF = data.frame(tripdate = trialOrderVec)
  
  raw_samplingSizeDF = modelDF %>%
    filter(tripdate %in% trialOrderVec) %>%
    group_by(tripdate) %>%
    summarise(totalFlow = sum(flow), .groups = 'drop') %>%
    mutate(level0 = round(totalFlow * level0Comp, 0)) %>%
    mutate(level1 = totalFlow - level0) 
  
  samplingSizeDF = templateDF %>% left_join(raw_samplingSizeDF, by = 'tripdate') %>%
    mutate(trial = seq(0, 15, by = 1)) %>%
    select(tripdate, trial, totalFlow, level0, level1) %>%
    mutate(level0Comp = round(level0 / totalFlow, 2),
           level1Comp = round(level1/totalFlow, 2))
  
  return(samplingSizeDF)
}

sampleLevel0Flow = function(level0PriorLst, targetDate, trialNum, level0Size){
  identifier = level0PriorLst %>% 
    lapply(function(priorDF) {
      priorDF %>% 
        bind_rows %>%
        filter(tripdate == targetDate)
    }) %>% 
    lapply(nrow) %>% 
    unlist
  targetIndex = which(identifier != 0)
  
  level0DecisionsDF = level0PriorLst %>% 
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
  
  level0Flows = level0DecisionsDF %>%
    group_by(nextPickup) %>% 
    summarise(level0Flow = n(), .groups = 'drop') %>%
    rename(ca = nextPickup) %>% 
    mutate(tripdate = targetDate,
           trialNum = trialNum) %>%
    select(tripdate, trialNum, ca, level0Flow)
  
  return(level0Flows)
}

sample_level0Flow_for4Conditions = function(level0PriorLst, targetDate, trialNum, level0Size){
  cond00 = sampleLevel0Flow(level0PriorLst, targetDate, trialNum, level0Size) %>% 
    mutate(display = 'static', feedback = 'results')
  cond01 = sampleLevel0Flow(level0PriorLst, targetDate, trialNum, level0Size) %>% 
    mutate(display = 'static', feedback = 'display')
  cond10 = sampleLevel0Flow(level0PriorLst, targetDate, trialNum, level0Size) %>% 
    mutate(display = 'NetHOPs', feedback = 'results')
  cond11 = sampleLevel0Flow(level0PriorLst, targetDate, trialNum, level0Size) %>% 
    mutate(display = 'NetHOPs', feedback = 'display')
  
  level0FlowOutput = rbind(cond00, cond01, cond10, cond11)
  return(level0FlowOutput)
}

sample_level1Decision = function(df, tripdate, trialNum,
                                 display, feedback, 
                                 level1SampleSize){
  # prep
  templateDF = data.frame(tripdate = tripdate, trialNum = trialNum, 
                          ca = targetCA)
  displayCode = ifelse(display == 'static', 0, 1)
  feedbackCode = ifelse(feedback == 'results', 0, 1)
  
  conditionDF = df %>% 
    filter(display == displayCode 
           & feedback == feedbackCode 
           & trial == trialNum) %>%
    select(decision) %>%
    table() %>% 
    as.data.frame() %>%
    setNames(c('ca', 'decision')) %>%
    mutate(ca = case_when(ca == 'north' ~ 8,
                          ca == 'west' ~ 28,
                          ca == 'east' ~ 32)) %>%
    sample_n(size = level1SampleSize, replace = T, weight = decision) %>%
    group_by(ca) %>% 
    summarise(level1Flow = n(), .groups = 'drop')
  
  outputDF = templateDF %>% left_join(conditionDF, by = "ca") %>%
    mutate(level1Flow = replace_na(level1Flow, 0)) %>%
    mutate(display = display, feedback = feedback) %>%
    select(tripdate, trialNum, display, feedback, ca, level1Flow)
  
  return(outputDF)
}


sampleLevel1Flow = function(level1DecisionDF, tripdate, trialNum, level1Size){
  
  # Condition: static + results
  condition1 = sample_level1Decision(df = level1DecisionDF, 
                                     tripdate = tripdate, trialNum = trialNum,
                                     display = "static", feedback = 'results',
                                     level1SampleSize = level1Size)
  
  # Condition 2: static + display
  condition2 = sample_level1Decision(df = level1DecisionDF,
                                     tripdate = tripdate, trialNum = trialNum,
                                     display = "static", feedback = 'display',
                                     level1SampleSize = level1Size)
  
  # Condition 3: NetHOPs + results
  condition3 = sample_level1Decision(df = level1DecisionDF,
                                     tripdate = tripdate, trialNum = trialNum,
                                     display = "NetHOPs", feedback = 'results',
                                     level1SampleSize = level1Size)
  
  # Condition 4: NetHOPs + display
  condition4 = sample_level1Decision(df = level1DecisionDF, 
                                     tripdate = tripdate, trialNum = trialNum,
                                     display = "NetHOPs", feedback = 'display',
                                     level1SampleSize = level1Size)
  
  output = list(condition1, condition2, condition3, condition4)
  return(output)
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

produce_level2Realized = function(tripdate, trialNum, level0Size, level1Size,
                                  directory = c('experiment', 'robustTrial', 'robustComp')
                                  ){
  level0PriorLst = readRDS(str_glue('./data/level0Prior/{directory}-level0Prior.Rdata'))
  level1DataDF = read.csv(str_glue('./data/collectedData/{directory}/stage1Data-{directory}.csv')) %>%
    group_by(id) %>% 
    filter(!any(taskTime < 5/60)) %>% 
    ungroup
  
  level0DF = sample_level0Flow_for4Conditions(level0PriorLst, tripdate, trialNum, level0Size)
  level1DF = sampleLevel1Flow(level1DataDF, tripdate, trialNum, level1Size)  
  boundsDF = get_modelInputRange(mod)
  
  mergedFlowLst = level1DF %>%
    lapply(function(conditionDF) conditionDF %>% 
             left_join(level0DF, by = c('tripdate', 'trialNum', 'ca', 'display', 'feedback')) %>%
             mutate(flow = level0Flow + level1Flow))
  
  predFlowLst = mergedFlowLst %>% 
    lapply(function(conditionDF){
      rawPredDF = conditionDF %>% 
        add_epred_draws(mod) %>%
        group_by(tripdate, trialNum, display, feedback, ca, level0Flow, level1Flow, flow) %>% 
        summarise(realizedPickup = mean(.epred) %>% round(0), .groups = 'drop') %>%
        rename(realizedFlow = flow, id = ca)
      resolveDF = rawPredDF %>% 
        left_join(boundsDF, by = c('id' = 'ca')) %>%
        mutate(realizedPickup = ifelse(realizedFlow > flow_max, predPickup_max, realizedPickup)) %>%
        mutate(realizedProb = round(realizedPickup / realizedFlow, 2)) %>%
        mutate(realizedProb = ifelse(realizedProb > 1, 1, realizedProb)) %>%
        select(tripdate, trialNum, display, feedback, id, level0Flow, level1Flow, realizedFlow, realizedPickup, realizedProb)
      return(resolveDF)
    })
  
  return(predFlowLst)
}


generate_level2_realizedOutcome = function(trialOrderVec, 
                                           directory = c("experiment", "robustTrial", "robustComp"),
                                           simID){
  seedNum = ifelse(directory == 'robustComp', simID + 666, simID)
  set.seed(seedNum)
                   
  samplingSizeDF = produce_samplingInstruction(trialOrderVec, directory)
  
  argumentLst = samplingSizeDF %>%
    group_split(row_number()) 
  
  outputLst = argumentLst %>%
    lapply(function(argsDF)
      produce_level2Realized(argsDF$tripdate, argsDF$trial, 
                             argsDF$level0, argsDF$level1,
                             directory))
  
  outputDF = outputLst %>% 
    lapply(bind_rows) %>% 
    bind_rows %>%
    mutate(simID = simID)
  
  return(outputDF)
}

simulate_level2_realizedOutcome = function(trialOrderVec, 
                                           directory = c("experiment", "robustTrial", "robustComp"),
                                           numSim = 1000){
  simDF = as.list(1:numSim) %>% 
    mclapply(function(simulationID) 
      generate_level2_realizedOutcome(trialOrderVec,
                                      directory,
                                      simulationID),
      mc.cores = 8) %>%
    bind_rows
  
  referenceDF = produce_samplingInstruction(trialOrderVec, directory) %>% select(tripdate, totalFlow)
  
  processedDF = simDF %>% 
    group_by(tripdate, trialNum, display, feedback, id) %>%
    summarise(level0Flow = weighted.mean(level0Flow),
              level1Flow = weighted.mean(level1Flow),
              realizedFlow = weighted.mean(realizedFlow),
              realizedProb = weighted.mean(realizedProb),
              .groups = 'drop') %>%
    select(tripdate, trialNum, display, feedback, id, realizedFlow, realizedProb) %>%
    mutate(realizedFlow = round(realizedFlow, 0),
           realizedProb = round(realizedProb, 2)) %>%
    pivot_wider(names_from = 'id', 
                values_from = c('realizedFlow', 'realizedProb'), 
                names_sep = '-') %>%
    left_join(referenceDF, by = 'tripdate') %>% 
    arrange(trialNum) %>%
    mutate(`realizedFlow-28` = totalFlow - `realizedFlow-8` - `realizedFlow-32`) %>%
    select(-totalFlow) %>% 
    pivot_longer(!c(tripdate, trialNum, display, feedback),
                 names_to = c('.value', 'id'),
                 names_sep = "-")
  
  sanityCheck = processedDF %>% 
    group_by(tripdate, trialNum, display, feedback) %>%
    summarise(totalFlow = sum(realizedFlow), .groups = 'drop') %>%
    left_join(referenceDF, by = 'tripdate') %>%
    mutate(assess = totalFlow.x == totalFlow.y) %>%
    pull(assess) %>%
    unique %>% length
  print(ifelse(sanityCheck == 1, 
               "All Good, time to export!", 
               "Something is off as the total flow is not correct..."))
  
  exportLst = processedDF %>% 
    group_split(trialNum, display, feedback)
  
  return(exportLst)
}

generate_stage2_realized_json = function(conditionLst, 
                                         directory = c("experiment", "robustTrial", "robustComp")){
  conditionLst %>% 
    lapply(export_stage2_realized, directory)
  return(conditionLst)
}

export_stage2_realized = function(scenarioDF, 
                                  directory = c("experiment", "robustTrial", "robustComp")){
  display_cond = unique(scenarioDF$display)
  feedback_cond = unique(scenarioDF$feedback)
  trialNum = unique(scenarioDF$trialNum)
  
  outputDF = scenarioDF %>%
    select(id, realizedFlow, realizedProb) %>%
    mutate_all(as.numeric)
  
  jsonData = toJSON(asplit(outputDF, 1))
  
  savePath = str_glue('./output/{directory}/realizedFlow/stage2/{display_cond}_{feedback_cond}/')
  fileName = str_glue("dc{trialNum}-realized-flow.json")
  outputPath = paste0(savePath, fileName)
  write(jsonData, outputPath)
}