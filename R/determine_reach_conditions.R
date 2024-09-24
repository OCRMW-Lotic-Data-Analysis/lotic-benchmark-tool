determine_reach_conditions <- function(indicators, definedBenchmarks, assignments) {
  

### DATA PREP ------------------------------------------------------------------
  # Select single Indicator+ConditionCategoryNum combination for defaultBenchmarks.  
  # Code here prioritizes "3" categories when available.
  # defaultBenchmarks <- defaultBenchmarks %>% arrange(Indicator, desc(ConditionCategoryNum)) %>%
  #   group_by(Indicator) %>%
  #   slice(1) %>% ungroup()
  
  # Convert indicators to dataframe
  indicatorsdf <- as.data.frame(indicators)
  
  # Combine default and custom benchmark groups
  benchmarks <- definedBenchmarks
  
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
  # benchmarks are included here (no Defaults)
  assignmentsCompleteLong <- assignments %>% 
    # clean up table to just evalID and benchmarks 
    select(any_of(c(indicAttrSelection, benchmarkNames))) %>% 
    pivot_longer(-all_of(indicAttrSelection), names_to = "Indicator", values_to = "BenchmarkGroup") %>%
    filter(BenchmarkGroup != "Default")
  
  # Fill out the benchmark group assignments. This gives a table where each
  # point+indicator combination has a BenchmarkGroup (either a custom one or Default)
  # This tells us which benchmark group (NOT actual mod/maj values yet) should 
  # be applied to the indicator of each point.
  # assignmentsCompleteLong <- assignmentsCustomLong %>%
  #   group_by(EvaluationID) %>%
  #   complete(Indicator = defaultBenchmarks$Indicator, fill = list(BenchmarkGroup = "Default"))
  
  # Attach the actual numerical threshold values and operators (benchmarks) to assignmentsCompleteLong
  benchmarksAndAssignments <-  left_join(assignmentsCompleteLong, benchmarks, join_by("BenchmarkGroup", "Indicator"))
 
  # Join indicators and benchmark assignments
  IndicatorValuesBenchmarks <- inner_join(indicatorLong, benchmarksAndAssignments, join_by("EvaluationID", "Indicator"))

  
  
### CALCULATIONS ---------------------------------------------------------------
  # Code from BLM used to calculate default benchmarks
  IndicatorValuesBenchmarks$Condition <- NA
  if (nrow(IndicatorValuesBenchmarks) > 0){
 
  for (i in 1:nrow(IndicatorValuesBenchmarks)) {
    
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
  }

### PREP FOR EXPORT ------------------------------------------------------------

  # Filter the defaultConditions to the points selected by the user. 
  # defaultConditions is loaded when app is started.
  defaultCondSel <- defaultConditions %>% filter(EvaluationID %in% indicators$EvaluationID) %>%
    select(EvaluationID, Indicator, Value, Condition, BenchmarkGroup,
           ModerateBenchmark1, MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2) %>% rename(value = Value)
  
  # Clean up IndicatorValuesBenchmarks for prep for join with defaultCondSel 
  IndicatorValuesBenchmarksSel <- IndicatorValuesBenchmarks %>%
    select(EvaluationID, Indicator, value, Condition, BenchmarkGroup,
           ModerateBenchmark1, MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2) %>%
    type.convert(as.is = FALSE) # converts numbers stored as characters to numeric data type.  Needed for row_bind below.

  # Logic to handle if someone leaves all assignments as "default" instead of
  # selecting a custom benchmark group.  
  if (nrow(IndicatorValuesBenchmarksSel) > 0){
    condtionsJoined <- bind_rows(  # order here is super important.  It ensures custom benchmarks are prioritized over default.
      IndicatorValuesBenchmarksSel %>% group_by(EvaluationID, Indicator),
      defaultCondSel %>% group_by(EvaluationID, Indicator)
    ) %>%
      slice(1)
  } else if (nrow(IndicatorValuesBenchmarksSel) == 0){ 
      condtionsJoined <- defaultCondSel %>% 
        group_by(EvaluationID, Indicator) %>%
        slice(1)
      }
  
   # Prep the "long" format of the data
   IndicatorValuesBenchmarksDetailed <- left_join(condtionsJoined, indicators, by = "EvaluationID") %>%
    select(PointID, EvaluationID, Indicator, value, Condition, BenchmarkGroup,
           ModerateBenchmark1, MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2,
           BLM_AdminState, District, FieldOffice, Project,
           FieldEvalDate, ProtocolType, FieldStatus, PointSelectionType,
           OriginalDesign, OriginalStratum, HumanInfluence, BeaverFlowMod, BeaverSigns,
           WaterWithdrawals, SideChannels)
  

  # Convert to wide format to join with original indicators input
  reachConditionsWide <- condtionsJoined %>% 
    select(EvaluationID, Indicator, value, Condition) %>% 
    pivot_wider(id_cols = !value, names_from = Indicator, values_from = Condition, names_glue = "{Indicator}{'Condition'}")
  
  
  # Workaround.  There are some indicators (e.g. TotalNitrogen_Modeled) that are not in the standard Indicators data.
  # Here, we prep those columns to be joined with reachConditionsWide.
  valuesNotInIndicators <- condtionsJoined %>% filter(!(Indicator %in% names(indicators))) %>%
    select(EvaluationID, Indicator, value) %>%
    pivot_wider(id_cols = EvaluationID, names_from = Indicator, values_from = value)
  
  # Join reach conditions with missing conditions/values like TotalNitrogen_Modeled
  reachConditionsWide <- left_join(reachConditionsWide, valuesNotInIndicators, by = "EvaluationID")
  
  # Join reach conditions with indicators 
  reachConditionsWide <- left_join(indicators, reachConditionsWide, by = "EvaluationID")
  
  # Put data in final format, a list.
  reachCondList <- list()
  reachCondList[['reachConditionsLong']] <- IndicatorValuesBenchmarksDetailed
  reachCondList[['reachConditionsWide']] <- reachConditionsWide
  
  return(reachCondList)
}
