compute_sim_bounds = function(simulationDF){
  simBoundsDF = simulationDF %>%
    separate(simFlow, c('simFlow_west', 'simFlow_north', 'simFlow_east')) %>%
    mutate_at(vars(starts_with('simFlow')), as.numeric) %>%
    group_by(trial, totalFlow) %>% 
    summarise(minSimFlow_west = min(simFlow_west),
              maxSimFlow_west = max(simFlow_west),
              minSimFlow_north = min(simFlow_north),
              maxSimFlow_north = max(simFlow_north),
              minSimFlow_east = min(simFlow_east),
              maxSimFlow_east = max(simFlow_east),
              .groups = 'drop')
  
  return(simBoundsDF)
}

summarize_modelInputRange = function(model){
  rangeDF = model$data %>%
    mutate(prob = pickup / flow) %>%
    group_by(ca) %>%
    summarise(minFlow = min(flow), maxFlow = max(flow),
              .groups = 'drop')
  
  return(rangeDF)
}

compute_data_bounds = function(mod){
  dataBoundsDF = mod %>%
    summarize_modelInputRange %>%
    mutate(ca = case_when(ca == 8 ~ 'north',
                          ca == 28 ~ 'west',
                          ca == 32 ~ 'east')) %>% 
    rename(minDataFlow = minFlow,
           maxDataFlow = maxFlow) %>%
    select(ca, minDataFlow, maxDataFlow) %>%
    pivot_wider(names_from = ca,
                values_from = c(minDataFlow, maxDataFlow))
  
  return(dataBoundsDF)
}

compute_bounds = function(simulationDF, mod){
  simBoundsDF = compute_sim_bounds(simulationDF)
  dataBoundsDF = compute_data_bounds(mod)
  fullBoundData = cbind(simBoundsDF, dataBoundsDF)

  boundsDF = fullBoundData %>% 
    group_split(trial, totalFlow) %>%
    lapply(function(scenarioConditionDF) {
      scenarioConditionDF %>%
        mutate(lowerBound_west = min(minSimFlow_west, minDataFlow_west),
               upperBound_west = max(maxSimFlow_west, maxDataFlow_west),
               lowerBound_north = min(minSimFlow_north, minDataFlow_north),
               upperBound_north = max(maxSimFlow_north, maxDataFlow_north),
               lowerBound_east = min(minSimFlow_east, minDataFlow_east),
               upperBound_east = max(maxSimFlow_east, maxDataFlow_east)) %>%
        select(trial, totalFlow,
               lowerBound_west, upperBound_west,
               lowerBound_north, upperBound_north,
               lowerBound_east, upperBound_east)
    }) %>% 
    bind_rows %>%
    mutate_all(as.numeric)
  
  return(boundsDF)
}


optimize_scenario = function(argsDF, 
                             directory = c('experiment', 'robustTrial', 'robustComp')) {
  print(argsDF$trial)
  scenarioIndex = argsDF$trial+1
  totalFlow = argsDF$totalFlow
  lb_west = argsDF$lowerBound_west
  ub_west = argsDF$upperBound_west
  lb_north = argsDF$lowerBound_north
  ub_north = argsDF$upperBound_north
  lb_east = argsDF$lowerBound_east
  ub_east = argsDF$upperBound_east
  
  # Load the objective function: need objectiveFuncLst
  source(str_glue('./maxWelfare-objectives-{directory}.R'))
  objective_func = objectiveFuncLst[[scenarioIndex]]
  
  cl = makePSOCKcluster(4)
  opResult = gosolnp(
    pars = NULL,
    fixed = NULL,
    fun = objective_func,
    eqfun = equality_func,
    eqB = totalFlow,
    LB = c(lb_west, lb_north, lb_east),
    UB = c(ub_west, ub_north, ub_east),
    distr = c(1,1,1),
    n.restarts = 400,
    n.sim = 2000,
    cluster = cl,
    rseed = 6666)
  stopCluster(cl)
  
  resultsDF = data.frame(
    trial = argsDF$trial,
    totalFlow = argsDF$totalFlow,
    westFlow = round(opResult$pars[1], 0),
    northFlow = round(opResult$pars[2], 0),
    eastFlow = round(opResult$pars[3], 0),
    convergence = opResult$convergence,
    lagrange = opResult$lagrange,
    nfuneval = opResult$nfuneval
  ) %>%
    mutate(maximalPickup = round(totalFlow - objective_func(opResult$pars), 0),
           northFlow = totalFlow - westFlow - eastFlow) %>%
    mutate(optimalFlow = paste(westFlow, northFlow, eastFlow, sep = ',')) %>%
    select(trial, totalFlow, 
           optimalFlow, maximalPickup, 
           convergence, nfuneval, lagrange)
  
  return(resultsDF)
}

optimizeAll = function(simulationDF, directory = c('experiment', 'robustTrial', 'robustComp')){
  optArgsDF = compute_bounds(simulationDF, mod) %>%
    group_split(trial, totalFlow)
  
  optimalDF = optArgsDF %>% 
    lapply(function(argsDF) optimize_scenario(argsDF, directory)) %>%
    bind_rows
  
  return(optimalDF)
}

