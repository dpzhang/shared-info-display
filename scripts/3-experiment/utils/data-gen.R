sample_hypothetical_flows = function(singleDayPrior, randomDay, seedNum = 2333){
  sampledFlow = singleDayPrior %>% 
    lapply(function(priorDF) {
      set.seed(seedNum)
      if (nrow(priorDF) == 0){
        priorDF = data.frame(taxiid = NA,
                             prevDropoff = NA,
                             nextPickup = targetCA,
                             num = 0,
                             weight = 1/length(targetCA))
      }
      priorDF %>% 
        sample_n(size = 10001, replace = T, weight = weight) %>% 
        mutate(sampleID = row_number())
    })
  
  sampledFlowDF = sampledFlow %>% 
    bind_rows %>%
    group_by(sampleID, nextPickup) %>%
    summarise(predFlow = n(), .groups = "drop") %>%
    filter(nextPickup %in% targetCA)
  
  flowRange = mod$data %>%
    group_by(ca) %>%
    summarise(min = range(flow) %>% min,
              max = range(flow) %>% max)
  
  sample8DF = sampledFlowDF %>% 
    filter(nextPickup == 8 & between(predFlow, 
                                     flowRange %>% filter(ca == 8) %>% pull(min),
                                     flowRange %>% filter(ca == 8) %>% pull(max)))
  sample28DF = sampledFlowDF %>%
    filter(nextPickup == 28 & between(predFlow, 
                                      flowRange %>% filter(ca == 28) %>% pull(min),
                                      flowRange %>% filter(ca == 28) %>% pull(max))) 
  sample32DF = sampledFlowDF %>% 
    filter(nextPickup == 32 & between(predFlow, 
                                      flowRange %>% filter(ca == 32) %>% pull(min),
                                      flowRange %>% filter(ca == 32) %>% pull(max)))
  
  validSamples = Reduce(intersect, list(sample8DF$sampleID, sample28DF$sampleID, sample32DF$sampleID))
  print(str_glue('Number of valid sample is: {length(validSamples)}'))
  needReplacement = length(validSamples) < 1001
  
  set.seed(seedNum)
  sampleUse = sample(validSamples, 1001, replace = needReplacement) %>% sort()
  
  validFlowDF = sampledFlowDF %>% 
    filter(sampleID %in% sampleUse) %>%
    setNames(c("sampleID", "ca", "flow")) %>%
    mutate(predPickup = predict(mod, newdata = data.frame(ca = ca, flow = flow)) %>% 
             data.frame %>% pull("Estimate") %>% round(0)) %>%
    mutate(prob = predPickup / flow) %>%
    rename(pickup = predPickup) %>%
    select(sampleID, ca, flow, pickup, prob)
  
  # HOPS pred data
  hopFramesDF = validFlowDF %>% 
    filter(row_number() <= 3000) %>%
    mutate(sampleID = paste0('frame-', rep(1:1000, each = 3))) %>%
    rename(predFlow = flow, predPickup = pickup, predProb = prob) %>%
    mutate(predProb = round(predProb, 2))
  
  # Static pred data
  staticDF = hopFramesDF %>%
    group_by(ca) %>%
    summarise(meanFlow = weighted.mean(predFlow) %>% round(0),
              meanPickup = weighted.mean(predPickup) %>% round(0), 
              .groups = "drop") %>%
    mutate(meanProb = round(meanPickup / meanFlow, 2))
  
  realizedFlowDF = modelDF %>% 
    filter(tripdate == randomDay) %>% 
    select(-tripdate) %>% 
    rename(ca = nextLoc, realizedFlow = flow, realizedPickup = pickup, realizedProb = prob) %>%
    mutate(realizedProb = round(realizedProb, 2))
  # Difference flow data
  staticFixDF = staticDF %>% 
    left_join(realizedFlowDF, by = 'ca') %>% 
    select('ca', 'meanFlow', 'meanPickup', 'meanProb', 'realizedFlow') %>%
    mutate(errorPercent = (realizedFlow - meanFlow) / realizedFlow)
  
  realizedSum = realizedFlowDF %>% pull(realizedFlow) %>% sum
  staticPredSum = staticDF %>% pull(meanFlow) %>% sum
  diff = realizedSum - staticPredSum
  if (realizedSum < staticPredSum){
    changeCA = staticFixDF %>% 
      filter(errorPercent > 0) %>% 
      filter(errorPercent == max(errorPercent)) %>% 
      pull(ca)
    
    staticDF = staticDF %>% 
      mutate(meanFlow = case_when(ca == changeCA ~ meanFlow - abs(diff),
                                  T ~ meanFlow))
  }else if (realizedSum > staticPredSum){
    changeCA = staticFixDF %>% 
      filter(errorPercent < 0) %>% 
      filter(errorPercent == min(errorPercent)) %>% 
      pull(ca)
    
    staticDF = staticDF %>% 
      mutate(meanFlow = case_when(ca == changeCA ~ meanFlow + abs(diff),
                                  T ~ meanFlow))
  }
  
  outputLst = list("hopsPred" = hopFramesDF, 
                   "staticPred" = staticDF,
                   "realized" = realizedFlowDF)
  return(outputLst)
}


export_hopsPred = function(hopsPred, trialNum, directory = c("experiment", "robustTrial", 'robustComp')){
  linksDF = hopsPred %>% 
    select(-predProb, -predPickup) %>%
    group_by(ca) %>%
    tidyr::pivot_wider(names_from = sampleID, values_from = predFlow) %>%
    ungroup %>%
    rowwise %>%
    mutate(maxFlow = max(c_across(starts_with('frame')))) %>%
    mutate(minFlow = min(c_across(starts_with('frame')))) %>%
    rename(id = ca) %>%
    select(id, minFlow, maxFlow, starts_with('frame')) %>%
    ungroup
  
  
  nodesDF = hopsPred %>%
    select(-predFlow, -predPickup) %>%
    group_by(ca) %>%
    tidyr::pivot_wider(names_from = sampleID, values_from =predProb) %>% 
    ungroup() %>%
    rename(id = ca) %>%
    rowwise %>%
    mutate(maxProb = max(c_across(starts_with('frame'))),
           minProb = min(c_across(starts_with('frame')))) %>%
    select(id, minProb, maxProb, starts_with('frame')) %>%
    ungroup
  
  jsonData = list(nodes = asplit(nodesDF, 1), 
                  links = asplit(linksDF, 1)) %>% toJSON
  
  #Export
  savePath = str_glue("./output/{directory}/decision-scenarios/HOPs/")  
  fileName = str_glue("dc{trialNum}-HOPs.json")
  outputPath = paste0(savePath, fileName)
  write(jsonData, outputPath)
}


export_staticPred = function(staticPred, trialNum, directory = c("experiment", "robustTrial", "robustComp")){
  outputDF = staticPred %>% 
    rename(id = ca) %>% 
    select(-meanPickup) %>%
    select(id, meanFlow, meanProb)
  
  jsonData = toJSON(asplit(outputDF, 1))
  savePath = str_glue("./output/{directory}/decision-scenarios/static/")
  fileName = str_glue("dc{trialNum}-static.json")
  outputPath = paste0(savePath, fileName)
  write(jsonData, outputPath)
}


export_realized = function(realized, trialNum, directory = c("experiment", "robustTrial", "robustComp")){
  outputDF = realized %>% 
    rename(id = ca) %>% 
    select(-realizedPickup) %>%
    select(id, realizedFlow, realizedProb)
  
  jsonData = toJSON(asplit(outputDF, 1))
  savePath = str_glue("./output/{directory}/realizedFlow/stage1/")
  fileName = str_glue("dc{trialNum}-realized-flow.json")
  outputPath = paste0(savePath, fileName)
  write(jsonData, outputPath)
}


export_allData_d3 = function(listContainer, trialNum, directory = c("experiment", "robustTrial", "robustComp")){
  export_staticPred(listContainer$staticPred, trialNum, directory)
  export_hopsPred(listContainer$hopsPred, trialNum, directory)
  export_realized(listContainer$realized, trialNum, directory)  
  return(listContainer)
}

generate_stage1_data = function(scenarioDatesVec, directory = c("experiment", "robustTrial", "robustComp"), seedNum){
  referenceDF = data.frame(tripdate = scenarioDatesVec, 
                           trialNum = seq(0, length(scenarioDatesVec) - 1, by = 1))
  
  priorObject = readRDS(str_glue('./data/level0Prior/{directory}-level0Prior.Rdata'))
  
  outputLst = priorObject %>% 
    lapply(function(dayPrior){
      curDate = dayPrior %>% bind_rows %>% pull(tripdate) %>% unique
      curTrial = referenceDF %>% filter(tripdate == curDate) %>% pull(trialNum)
      print(str_glue('{curTrial}: {curDate}'))
      expDisplayData = dayPrior %>% 
        sample_hypothetical_flows(curDate, seedNum) %>%
        export_allData_d3(curTrial, directory)
      return(expDisplayData)
    })
  return(outputLst)
}
