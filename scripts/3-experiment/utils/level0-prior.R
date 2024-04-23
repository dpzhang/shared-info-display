identify_targetDrivers = function(randomDay){
  candidatesDF = targetCA %>% 
    lapply(function(caID) compute_dailyCAProb(caID, randomDay, "conditional", T)) %>%
    bind_rows
  
  return(candidatesDF)
}

identify_singleDriver_condPrior = function(targetDriverID, prevDropCA, randomDay){
  aggregate_dailyConsecutives = function(dayTripsDF){
    outputDF = dayTripsDF %>%
      mutate(nextPickup = lead(pickupcommunity)) %>%
      select(taxiid, dropoffcommunity, nextPickup) %>%
      setNames(c("taxiid", "prevDropoff", "nextPickup")) %>%
      na.omit %>%
      group_by(taxiid, prevDropoff, nextPickup) %>%
      summarise(num = n(), .groups = "drop")
    return(outputDF)
  }
  daysBack = ifelse(randomDay == "2014-01-13", 8, 10)
  priorDateRange = generate_dateRange(randomDay, daysBack)
  
  priorTrips = alltripsDF %>% 
    filter( (tripdate %in% priorDateRange) & (taxiid == targetDriverID) &
              starttimestamp >= "07:00:00" & endtimestamp <= "10:00:00") %>% 
    arrange(tripdate, endtimestamp)  
  
  possibleLoc = ca.neighbors[[prevDropCA]]
  templateDF = data.frame(taxiid = targetDriverID, 
                          prevDropoff = prevDropCA,
                          nextPickup = possibleLoc, 
                          num = 0, 
                          weight = 1/length(possibleLoc))
  if (nrow(priorTrips) == 0){
    consecutivePriorDF = templateDF
  }else{
    consecutivePriorDF = priorTrips %>%
      group_by(tripdate) %>% 
      group_map(~aggregate_dailyConsecutives(.x)) %>%
      bind_rows %>%
      group_by(taxiid, prevDropoff, nextPickup) %>%
      summarise(num = n(), .groups = "drop") %>%
      filter(prevDropoff == prevDropCA) %>%
      mutate(weight  = num / sum(num))
    
    if (nrow(consecutivePriorDF) == 0){
      consecutivePriorDF = templateDF
    }
  }
  return(consecutivePriorDF) 
}

identify_targetDrivers_condPrior = function(candidatesDF){
  priorLst = candidatesDF %>% 
    group_split(row_number()) %>%
    mclapply(function(driverDF) {
      identify_singleDriver_condPrior(driverDF$taxiid,
                                      driverDF$prevDrop,
                                      driverDF$tripdate) %>%
        filter(nextPickup %in% targetCA)
    }, mc.cores = 6)
  return(priorLst)
}

compute_driverPrior_data_allScenarios = function(dateVec){
  outputLst = list()
  for (i in 1:length(dateVec)){
    targetDate = dateVec[i]
    print(str_glue('{i}: {targetDate}'))
    priorSingleDayLst = identify_targetDrivers(targetDate) %>% 
      identify_targetDrivers_condPrior %>% 
      lapply(function(priorDF) priorDF %>% mutate(trial = i-1, tripdate = targetDate))
    outputLst %<>% list.append(priorSingleDayLst)
  }
  
  return(outputLst)
}