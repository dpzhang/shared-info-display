create_eda = function(tripsDF, CAID, plotTitle = c("AM Peak (9:00 - 10:00 AM)", "9:00 AM", "9:00 AM Loop")){
  if (CAID == 8){
    CA_name = "North Loop"
    counter = '(B).'
  }else if(CAID == 28){
    CA_name = "West Loop"
    counter = '(A).'
  }else if(CAID == 32){
    CA_name = "The Loop"
    counter = '(C).'
  }
  
  
  aggPickupDF =  tripsDF %>% 
    filter(starttimestamp == "09:00:00" 
           & pickupcommunity == CAID) %>%
    group_by(tripdate) %>%
    summarise(numTrips = n(), .groups = "drop") %>%
    mutate(tripdate = as.Date(tripdate, "%Y-%m-%d"))
  
  # create summary statistics
  summaryStatsDF = summary(aggPickupDF$numTrips) %>%
    unclass %>%
    as.data.frame %>%
    t %>%
    data.frame(row.names = "") %>%
    mutate(SD = sd(aggPickupDF$numTrips)) %>%
    setNames(c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "SD"))
  
  # Create distribution plots
  dailyPickups_boxplot = 
    aggPickupDF %>%
    ggplot(aes(x = "", y = numTrips)) +
    geom_boxplot(color = "#003f5c") + 
    labs(x = "", y = "Num. Pickups") + 
    theme_minimal()
  
  dailyPickups_histogram = 
    aggPickupDF %>%
    ggplot(aes(x = numTrips)) + 
    geom_histogram(bins = 15, color = "#ffa600", fill = "#003f5c") +
    labs(x = "Num. Pickups", y = "Freq.") + 
    theme_minimal()
  dailyPickups_distPlots = ggarrange(dailyPickups_boxplot, 
                                     dailyPickups_histogram) %>%
    annotate_figure(top = text_grob(paste(counter, CA_name),
                                    face = "bold", size = 14))
  
  # Create time series plots
  dailyPickups_tsPlot = aggPickupDF %>%
    ggplot(aes(x = tripdate, y = numTrips, group = 1)) +
    geom_point(color = "#ffa600") + 
    geom_line(color = "#003f5c") + 
    geom_vline(xintercept = as.Date("2015-07-01"),
               color = "#bc5090") +
    scale_x_date(date_breaks = "2 month", 
                 labels = scales::date_format("%Y-%m"),
                 limits = as.Date(c('2014-01-01','2015-12-30'))) + 
    labs(x = "Dates", 
         y = "Num. Pickups", 
         title = paste("Total", CA_name, plotTitle, "Pickups")) +
    theme_minimal()
  
  outputLst = list(
    "data" = aggPickupDF,
    "descriptives" = summaryStatsDF, 
    "distributions" = dailyPickups_distPlots,
    "ts" = dailyPickups_tsPlot,
    "distBox" = dailyPickups_boxplot,
    "distHist" = dailyPickups_histogram
  )
  return(outputLst)
}

display_geoMap = function(aggregateDF, statVar = "Count", plotTitle){
  # Load and process the shape files: `chicagoCA`, which contains 77 Chicago Community Areas
  chicagoCA %<>%
    left_join(aggregateDF, by = c("area_numbe" = "prevDropoffLoc"))
  
  
  chicagoCA %>% 
    tm_shape() +
    tm_polygons(col = statVar, style = "pretty", 
                palette = "Reds", n = 5, border.alpha = 1)  + 
    tm_compass(type = "4star", position = c("left", "bottom")) + 
    tm_layout(frame = F, main.title = plotTitle,
              legend.outside = T) +
    tm_text("area_numbe")
}

find_traceDF = function(pickupCA, randomDay){
  ###################################################
  # Description:
  # The goal of this function is to trace the previous dropoff location 
  # of drivers whom the data shows has pickuped in `pickupCA` at 9AM on `randomDay`
  # The function returns a data frame
  # Arg: 
  # pickupCA: integer of pickup community area ID
  # randomDay: str of day in "YYYY-MM-DD"
  ###################################################
  set.seed(2333)
  # Find all drivers who pickuped up at 9AM in `pickupCA` on `randomDay`
  pickupDF = tripsDF %>%
    filter(tripdate == randomDay 
           & starttimestamp == "09:00:00" 
           & pickupcommunity == pickupCA) %>%
    group_by(taxiid) %>% 
    sample_n(1) %>%
    ungroup
  
  driverHistoryDF = tripsDF %>%
    filter(tripdate == randomDay 
           & taxiid %in% pickupDF$taxiid 
           & starttimestamp < "09:00:00") %>%
    group_by(taxiid) %>% 
    arrange(endtimestamp) %>%
    filter(row_number()==n()) %>%
    ungroup()
  
  traceDF_full = pickupDF %>% 
    left_join(driverHistoryDF, 
              by = c("tripdate", "taxiid"),
              suffix = c("_next", "_prev")) 
  
  traceDF = traceDF_full %>%
    select(taxiid, 
           endtimestamp_prev,
           starttimestamp_next,
           dropoffcommunity_prev,
           pickupcommunity_next)
  
  traceWaitDF = traceDF %>%
    mutate(endtimestamp_prev = case_when(
      endtimestamp_prev > starttimestamp_next ~starttimestamp_next,
      T ~ endtimestamp_prev)) %>%
    mutate(timeDiff = starttimestamp_next - endtimestamp_prev) %>% 
    mutate(waiting = hours(timeDiff)  * 60 + minutes(timeDiff)) %>%
    select(taxiid, dropoffcommunity_prev, pickupcommunity_next, waiting) %>%
    setNames(c("taxiid", "prevDropoff", "nextPickup", "waiting"))
  
  return(traceWaitDF)
}

find_prevLoc_singleDay = function(pickupCA, randomDay){
  ###################################################
  # Description:
  # The goal of this function is to aggregate the number of drivers coming from 
  # unique communities that they dropoff before they pickup in `pickupCA` at 9AM on `randomDay`
  # The function returns a data frame
  # Arg: 
  # pickupCA: integer of pickup community area ID
  # randomDay: str of day in "YYYY-MM-DD"
  ################################################### 
  traceWaitDF = find_traceDF(pickupCA, randomDay)
  
  waitTimeOutliers = traceWaitDF %>% pull(waiting) %>% boxplot.stats() %>% extract2("out")
  
  prevDropLocDF = traceWaitDF %>%
    filter(!waiting %in% waitTimeOutliers) %>% 
    group_by(prevDropoff) %>%
    summarise(count = n(),
              .groups = "drop") %>%
    setNames(c("prevDropoffLoc", "Count")) %>%
    mutate(tripdate = randomDay) %>%
    select(tripdate, prevDropoffLoc, Count)
  
  return(prevDropLocDF)
}

find_prevLoc = function(pickupCA){
  ###################################################
  # Description:
  # The goal of this function is to loop `find_prevLoc_singleDay` on every
  # unique day in our filtered analysis dataframe. 
  # The function returns a data frame
  # Arg: 
  # pickupCA: integer of pickup community area ID
  ################################################### 
  
  # Identify all unique days from processed data
  alldays = tripsDF %>% pull(tripdate) %>% unique
  
  # Loop through all unique days
  summaryDF = alldays %>%
    mclapply(function(el_day) find_prevLoc_singleDay(pickupCA, el_day), 
             mc.cores = 6)
  
  outputDF = summaryDF %>% 
    bind_rows %>%
    group_by(prevDropoffLoc) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    mutate(CountPerDay = round(Count / length(alldays), 2)) %>%
    arrange(desc(CountPerDay)) 
  
  return(outputDF)
}

find_maxWaitTime = function(pickupCA){
  ###################################################
  # Description:
  # The function does 2 things:
  # 1. find all previous dropoff locations before 9 AM trips in `pickupCA` on
  #    a `randomDay` (by finding the `traceDF`)
  # 2. Group by `prevDropoffCA_vec`, remove the outlier of waiting time, and
  #    find the max waiting time before 9 AM `pickupCA` trip
  # Arg: 
  # traceAggrLst: a list of data frames, in which each data frame records
  #              the aggregation of previous dropoff locations before 9AM `pickupCA`
  #              trips
  # pickupCA: integer of pickup community area ID
  ###################################################
  alldays = tripsDF %>% pull(tripdate) %>% unique
  
  targetDropoffCA_vec = traceAggrDFLst %>% 
    extract2(as.character(pickupCA)) %>%
    filter(CountPerDay > 1) %>%
    na.omit() %>%
    pull(prevDropoffLoc)
  
  relevantTripsDF = alldays %>%
    mclapply(function(el_day) find_traceDF(pickupCA, el_day),
             mc.cores = 6) %>%
    bind_rows %>%
    filter(prevDropoff %in% targetDropoffCA_vec)
  
  cleanTripsDF = relevantTripsDF %>% 
    group_by(prevDropoff) %>%
    group_modify(~ {
      .x %>% 
        filter(!waiting %in% boxplot.stats(waiting)[['out']])
    }) 
  
  maxWaitDF = cleanTripsDF %>%
    group_by(prevDropoff) %>%
    summarise(maxWait = max(waiting), .groups = "drop")
  
  return(maxWaitDF)
}

display_tracePlot = function(pickupCA){
  pickupCA = as.character(pickupCA)
  CAnameLst = list("8" = "North Loop", "28" = "West Loop",
                   "32" = "Loop", "33" = "North Loop")
  
  tmap_arrange(
    traceAggrDFLst[[pickupCA]] %>% 
      display_geoMap(statVar = "Count", 
                     plotTitle = paste("Unique dropoff CAs before", CAnameLst[[pickupCA]],"pickups")),
    traceAggrDFLst[[pickupCA]] %>% 
      filter(CountPerDay > 1) %>%
      display_geoMap(statVar = "CountPerDay", 
                     plotTitle = paste("Reliable dropoff (>1 trips/day) CAs before", CAnameLst[[pickupCA]],"pickups"))
  )
}

minutesConverter = function(mins){
  quotient = mins %/% 60
  remainder = mins %% 60
  output = chron::times(paste0(quotient, ":", remainder,":00"))
  return(output)
}

generate_dateRange = function(targetDate, numDaysBack){
  uniqueDates = alltripsDF %>% pull(tripdate) %>% unique
  startIndex = which(uniqueDates == targetDate) - numDaysBack
  endIndex = which(uniqueDates == targetDate) - 1
  priorDateRange = uniqueDates[startIndex:endIndex]  
  
  return(priorDateRange)
}

aggregate_prior_targetDriver = function(targetDriverID, 
                                        prevDropCA, 
                                        randomDay, 
                                        priorType = c("conditional", "unconditional")){
  ###################################################
  # Description:
  # The function aims to extract priors of a driver by taxiid.
  # The goal is to find the all consecutive pickups of a driver from all trips
  # completed in a date range. From the number of counts of the consecutive trips
  # we can find the likely search decision by a driver.
  
  # Arg: 
  # targetDriverID: integer of driverID
  # priorDateRange: a vector of dates in str indicating all unique days we want
  #                 to query prior from 
  ###################################################
  # This helper function process all trips completed by a driver on a specific day
  # The goal is to connect consecutive trips and count the number of times this consecutive
  # pattern happens
  aggregate_dailyConsecutives = function(dayTripsDF){
    outputDF = dayTripsDF %>%
      mutate(nextPickup = lead(pickupcommunity)) %>%
      select(taxiid, dropoffcommunity, nextPickup) %>%
      setNames(c("taxiid", "prevDropoff", "nextPickup")) %>%
      na.omit %>%
      group_by(prevDropoff, nextPickup) %>%
      summarise(num = n(), .groups = "drop")
    return(outputDF)
  }
  ###################################################
  daysBack = ifelse(randomDay == "2014-01-13", 8, 10)
  priorDateRange = generate_dateRange(randomDay, daysBack)
  priorTrips = alltripsDF %>% 
    filter( (tripdate %in% priorDateRange) & (taxiid == targetDriverID) &
              starttimestamp >= "07:00:00" & endtimestamp <= "10:00:00") %>% 
    arrange(tripdate, endtimestamp)  
  
  if (nrow(priorTrips) == 0){
    possibleLoc = ca.neighbors[[prevDropCA]]
    set.seed(2333)
    inferred = sample(possibleLoc, 1)
  }else{
    # Prior can be conditional or unconditional
    # Conditional prior: based on the current location
    # Unconditional prior: current location does not matter
    if (priorType == "conditional"){
      consecutivePriorDF = priorTrips %>%
        group_by(tripdate) %>% 
        group_map(~aggregate_dailyConsecutives(.x)) %>%
        bind_rows %>%
        group_by(prevDropoff, nextPickup) %>%
        summarise(num = n(), .groups = "drop") %>%
        filter(prevDropoff == prevDropCA) 
      
      if (nrow(consecutivePriorDF) == 0) {
        set.seed(2333)
        maxPickupDF = priorTrips %>%
          group_by(pickupcommunity) %>%
          summarise(num = n(), .groups = "drop") %>%
          filter(num == max(num)) %>%
          sample_n(1)
        inferred = maxPickupDF %>% pull(pickupcommunity)
      }else{
        set.seed(2333)
        inferred = consecutivePriorDF %>%
          filter(num == max(num)) %>%
          sample_n(1) %>%
          pull(prevDropoff)
      }
    }
    if (priorType == "unconditional"){ 
      set.seed(2333)
      maxPickupDF = priorTrips %>%
        group_by(pickupcommunity) %>%
        summarise(num = n(), .groups = "drop") %>%
        filter(num == max(num)) %>%
        sample_n(1)
      inferred = maxPickupDF %>% pull(pickupcommunity)
    }
  }
  
  outputDF = data.frame(randomDay, targetDriverID, prevDropCA, inferred) %>%
    setNames(c("tripdate", "taxiid", "prevDrop", "inferred"))
  return(outputDF)
}

compute_dailyCAProb = function(pickupCA, randomDay, 
                               priorType = c("conditional", "unconditional"),
                               details = F){
  pickupDF = tripsDF %>% 
    filter(tripdate == randomDay 
           & pickupcommunity == pickupCA 
           & starttimestamp == "9:00:00")
  
  if(nrow(pickupDF) == 0){
    outputTemplate = data.frame(tripdate = randomDay, 
                                nextLoc = pickupCA, 
                                flow = NA, 
                                pickups = NA, 
                                prob = NA)
    return(outputTemplate)
  }else{
    prevDropDF = tripsDF %>%
      filter(taxiid %in% pickupDF$taxiid
             & tripdate == randomDay
             & starttimestamp < "9:00:00") %>%
      arrange(taxiid, endtimestamp) %>%
      group_by(taxiid) %>%
      filter(endtimestamp == max(endtimestamp)) %>%
      ungroup %>%
      select(tripdate, endtimestamp, taxiid, dropoffcommunity) %>%
      setNames(c("tripdate", "prevDropTime", "taxiid", "prevDrop"))
    
    waitTimeDF = prevDropDF %>%
      mutate(prevDropTimestamp = paste(tripdate, prevDropTime) %>% strptime("%Y-%m-%d %H:%M:%S"),
             bench = paste(tripdate, "09:00:00") %>% strptime("%Y-%m-%d %H:%M:%S")) %>%
      mutate(waitTime = difftime(bench, prevDropTimestamp, units = "min") %>% as.numeric) %>%
      mutate(waitTime = ifelse(waitTime < 0, 0, waitTime)) %>%
      select(tripdate, taxiid, prevDrop, waitTime)
    
    waitTimeThreshold = waitTimeDF %>% pull(waitTime) %>% boxplot.stats() %>% extract2("stats") %>% max
    filterArgDF = waitTimeDF %>% 
      filter(waitTime <= waitTimeThreshold) %>%
      group_by(tripdate, prevDrop) %>%
      summarise(maxWaitTime = max(waitTime), 
                numTrips = n(),
                .groups = "drop") %>%
      filter(numTrips > 1)
    if (nrow(filterArgDF) == 0){
      filterArgDF = traceAggrDFLst %>% 
        extract2(as.character(pickupCA)) %>% 
        filter(CountPerDay > 1) %>% 
        na.omit %>%
        mutate(tripdate = randomDay) %>%
        select(tripdate, prevDropoffLoc) %>%
        rename(prevDrop = prevDropoffLoc) %>%
        left_join(maxWaitTimeDFLst %>% extract2(as.character(pickupCA)),
                  by = c("prevDrop" =  "prevDropoff")) %>%
        mutate(minusTime = minutesConverter(maxWait)) %>%
        mutate(threshold = chron::times("09:00:00") - minusTime) %>%
        select(-minusTime, -maxWait) 
    }else{
      filterArgDF %<>% 
        mutate(minusTime = minutesConverter(maxWaitTime)) %>%
        mutate(threshold = chron::times("09:00:00") - minusTime) %>%
        select(-minusTime, -maxWaitTime) 
    }
    
    rawCandidatesDF = filterArgDF %>%
      group_split(prevDrop) %>%
      lapply(function(el){
        tripsDF %>% 
          filter(tripdate == el$tripdate 
                 & dropoffcommunity == el$prevDrop
                 & endtimestamp >= el$threshold 
                 & endtimestamp < "09:00:00")
      }) %>%
      bind_rows 
    
    set.seed(2333)
    candidatesDF = rawCandidatesDF %>%
      group_by(taxiid) %>%
      filter(endtimestamp == max(endtimestamp)) %>%
      filter(starttimestamp == max(starttimestamp)) %>%
      sample_n(1) %>%
      ungroup() %>%
      select(tripdate, taxiid, dropoffcommunity) %>%
      rename(prevDrop= dropoffcommunity)
    
    # Split candidates by types: 
    # successful candidates: those from the data who we can confirm get a pickup at 9AM in `pickupCA` 
    # unknown drivers.
    # To impose control, we need to filter out some pickups at `pickupCA` that 
    # does not match our candidate filter criteria
    # Find the candidates we identified who actually got pickups in `targetCA`
    candidates_success = pickupDF %>%
      filter(taxiid %in% candidatesDF$taxiid) %>%
      pull(taxiid)
    
    # Now create a dataframe that contains only successful candidates
    candidates_successDF = candidatesDF %>%
      filter(taxiid %in% candidates_success) %>%
      mutate(nextLoc = pickupCA,
             outcome = 1)
    
    # This leaves the rest of candidates that we unkown about the outcome
    candidates_unknownDF = candidatesDF %>%
      filter(!taxiid %in% candidates_success)
    
    # Special case: when there is no unknown candidates
    if (nrow(candidates_unknownDF) == 0){
      resultsDF = data.frame(tripdate = randomDay,
                             nextLoc = pickupCA,
                             flow = nrow(candidatesDF),
                             pickups = nrow(candidates_successDF)) %>%
        mutate(prob = pickups / flow)
      
      return(resultsDF)
    }else{
      # Among the unknown candidates, we can figure out who picked up in 
      # 9AM, these people should be removed from candidates because we are sure they
      # did not go to `pickupCA` for trips
      candidates_success_elsewhere = tripsDF %>%
        filter(tripdate == randomDay
               & taxiid %in% candidates_unknownDF$taxiid
               & starttimestamp == "9:00:00") %>%
        pull(taxiid) %>% 
        unique
      
      candidates_unknownDF %<>% filter(!taxiid %in% candidates_success_elsewhere)
      
      # For these unknown, need to infer their locations
      candidates_unknownInferDF = candidates_unknownDF %>%
        group_split(taxiid) %>%
        mclapply(function(driverDF) 
          aggregate_prior_targetDriver(driverDF$taxiid, 
                                       driverDF$prevDrop, 
                                       driverDF$tripdate,
                                       priorType),
          mc.cores = 6) %>%
        bind_rows
      
      candidates_failDF = candidates_unknownInferDF %>%
        filter(inferred == pickupCA) %>%
        rename(nextLoc = inferred) %>%
        mutate(outcome = 0)
      # Here we need to aggregate the results
      if (details){
        resultsDF = rbind(candidates_successDF, candidates_failDF)
      }else{
        resultsDF = rbind(candidates_successDF, candidates_failDF) %>%
          group_by(tripdate, nextLoc) %>%
          summarise(flow = n(),
                    pickups = sum(outcome), .groups = "drop") %>%
          mutate(prob = pickups / flow)  
      }
      return(resultsDF)
    }
  }
}

generate_modelData = function(CA_vec, 
                              priorType = c("conditional", "unconditional")){
  alldays = unique(tripsDF$tripdate)
  outputLst = list()
  
  start = Sys.time()
  for(dayEl in alldays){
    print(dayEl)
    dayOutputDF = CA_vec %>%
      lapply(function(caID) {
        print(caID)
        compute_dailyCAProb(caID, dayEl, priorType)}
      ) %>%
      bind_rows
    outputLst %<>% list.append(dayOutputDF)
  }
  end = Sys.time()
  print(end-start)
  
  outputDF = outputLst %>% bind_rows
  
  return(outputDF)
}

plot_results = function(df, varName = c("pickups", "prob"), wrap = T){
  yAxisLab = ifelse(varName == "pickups", "Pickups", "Prob(Pickup)")
  
  basePlot = cleanDF %>%
    mutate(targetCA = recode_factor(nextLoc, 
                                    `8` = "North Loop",
                                    `28` = "West Loop",
                                    `32` = "The Loop")) %>% 
    ggplot(aes_string(x = "flow", y = varName), color = targetCA, group = targetCA) + 
    geom_point(aes(color = targetCA)) +
    geom_smooth(aes(color = targetCA, fill = targetCA), formula = y~x, method = "lm") +
    scale_color_manual(values=c("#003f5c", "#bc5090", "#58508d")) + 
    scale_fill_manual(values=c("#003f5c", "#bc5090", "#58508d")) + 
    labs(x = "Flow", y = yAxisLab) + 
    theme_minimal() + 
    theme(legend.title = element_blank(),
          legend.position = "bottom")
  if (wrap){
    basePlot + facet_wrap(vars(targetCA), scale = "free")
  }else{
    basePlot
  }
}