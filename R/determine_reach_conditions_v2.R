determine_reach_conditions <- function(indicators, definedBenchmarks, assignments, defaultBenchmarks) {
  
### DATA PREP ------------------------------------------------------------------
  # Select single Indicator+ConditionCategoryNum combination for defaultBenchmarks.  
  # Code here prioritizes "3" categories when available.
  defaultBenchmarks <- defaultBenchmarks %>% arrange(Indicator, desc(ConditionCategoryNum)) %>%
    group_by(Indicator) %>%
    slice(1) %>% ungroup()
  
  # Convert indicators to dataframe
  indicatorsdf <- as.data.frame(indicators)
  
  # Combine default and custom benchmark groups
  benchmarks <- bind_rows(definedBenchmarks, defaultBenchmarks)
  
  # Attribute and benchmarks for selecting and pivoting
  indicAttrSelection <- c('EvaluationID')
  benchmarkNames <- unique(benchmarks$Indicator)
  
  # Simplify indicator table using above values
  indicatorSelect <- indicatorsdf %>% 
    select(all_of(c(indicAttrSelection, benchmarkNames)))
  
  # Convert indicatorSelect to long format to prep for joining 
  indicatorLong <- indicatorSelect %>% pivot_longer(-all_of(indicAttrSelection), 
                                                    names_to = "Indicator", 
                                                    values_to = "value")
  
  # Pivot values from "apply benchmark" table to long form. Only custom
  # benchmarks are induced here (no Defaults)
  assignmentsCustomLong <- assignments %>% 
    # clean up table to just evalID and benchmarks 
    select(any_of(c(indicAttrSelection, benchmarkNames))) %>% 
    pivot_longer(-all_of(indicAttrSelection), names_to = "Indicator", values_to = "BenchmarkGroup")
  
  # Fill out the benchmark group assignments. This gives a table where each
  # point+indicator combination has a BenchmarkGroup (either a custom one or Default)
  # This tells us which benchmark group (NOT actual mod/maj values yet) should 
  # be applied to the indicator of each point.
  assignmentsCompleteLong <- assignmentsCustomLong %>% 
    group_by(EvaluationID) %>%
    complete(Indicator = defaultBenchmarks$Indicator, fill = list(BenchmarkGroup = "Default")) 
  
  # Attach the actual numerical threshold values and operators (benchmarks) to assignmentsCompleteLong
  benchmarksAndAssignments <-  left_join(assignmentsCompleteLong, benchmarks, join_by("BenchmarkGroup", "Indicator"))
  
  # Join indicators and benchmark assignments
  IndicatorValuesBenchmarks <- inner_join(indicatorLong, benchmarksAndAssignments, join_by("EvaluationID", "Indicator"))
  
### CALCULATIONS ---------------------------------------------------------------
  # Code from BLM used to calculate default benchmarks
  IndicatorValuesBenchmarks$Condition <- NA
  
  
  for (i in 1:nrow(IndicatorValuesBenchmarks)) {
    
    #print (paste("Row ", i))
    IndicatorValuesBenchmarks_oneRow = IndicatorValuesBenchmarks[i,] 
    
    eval_stringMaj1 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToModerateRel1, IndicatorValuesBenchmarks_oneRow$MajorBenchmark1)
    eval_stringMin1 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MinimalToModerateRel1, IndicatorValuesBenchmarks_oneRow$ModerateBenchmark1)
    eval_stringMaj2 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToModerateRel2, IndicatorValuesBenchmarks_oneRow$MajorBenchmark2)
    eval_stringMin2 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MinimalToModerateRel2, IndicatorValuesBenchmarks_oneRow$ModerateBenchmark2)
    eval_stringMajMin1 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToMinimalRel1, IndicatorValuesBenchmarks_oneRow$MajorBenchmark1)
    eval_stringMajMin2 <- paste(IndicatorValuesBenchmarks_oneRow$value, IndicatorValuesBenchmarks_oneRow$MajorToMinimalRel2, IndicatorValuesBenchmarks_oneRow$MajorBenchmark2)
    
    
    #eval(parse(text = eval_stringMaj1))
    #eval(parse(text = eval_stringMin1))
    #eval(parse(text = eval_stringMaj2))
    #eval(parse(text = eval_stringMin2))
    IndicatorValuesBenchmarks[i,"Condition"] <-
      ifelse(IndicatorValuesBenchmarks_oneRow$ConditionCategoryNum==3,  
             ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Increases with stress"|IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases with stress",
                    ifelse(eval(parse(text = eval_stringMaj1)),"Major",
                           ifelse(eval(parse(text = eval_stringMin1)),"Minimal", "Moderate")
                    ),          
                    
                    ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases and increases with stress",
                           ifelse(eval(parse(text = eval_stringMaj1)) |eval(parse(text = eval_stringMaj2)) ,"Major",
                                  ifelse(eval(parse(text = eval_stringMin1)) & eval(parse(text = eval_stringMin2)),"Minimal","Moderate"))
                           ,NA)),
             # if statements for 2 categories         
             ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Increases with stress"|IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases with stress",
                    ifelse(eval(parse(text = eval_stringMajMin1)),"Major","Minimal"),
                    
                    
                    ifelse(IndicatorValuesBenchmarks_oneRow$IncreaserDecreaser=="Decreases and increases with stress",
                           ifelse(eval(parse(text = eval_stringMajMin1)) |eval(parse(text = eval_stringMajMin2)) ,"Major","Minimal")
                           ,NA))
      )
    
  }

### PREP FOR EXPORT ------------------------------------------------------------
  IndicatorValuesBenchmarksDetailed <- left_join(IndicatorValuesBenchmarks, indicators, by = "EvaluationID") %>%
    select(PointID, EvaluationID, Indicator, value, BenchmarkGroup,
           ModerateBenchmark1, MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2,
           Condition, BLM_AdminState, District, FieldOffice, Project,
           FieldEvalDate, ProtocolType, FieldStatus, PointSelectionType,
           OriginalDesign, OriginalStratum, HumanInfluence, BeaverFlowMod, BeaverSigns,
           WaterWithdrawals, SideChannels)
  
  # Convert to wide format to join with original indicators input
  reachConditionsWide <- IndicatorValuesBenchmarks %>% 
    select(EvaluationID, Indicator, value, Condition) %>% 
    pivot_wider(id_cols = !value, names_from = Indicator, values_from = Condition, names_glue = "{Indicator}{'Condition'}")
  
  # Join reach conditions with indicators (the original, spatial object) and NOT indicatorsdf
  reachConditionsWide <- left_join(indicators, reachConditionsWide, by = "EvaluationID")
  
  reachCondList <- list()
  reachCondList[['reachConditionsLong']] <- IndicatorValuesBenchmarksDetailed
  reachCondList[['reachConditionsWide']] <- reachConditionsWide
  
  return(reachCondList)
}
