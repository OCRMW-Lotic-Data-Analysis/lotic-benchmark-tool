determine_reach_conditions <- function(indicators, definedBenchmarks, assignments, defaultBenchmarks) {
  
  # Select single Indicator+ConditionCategoryNum combination for defaultBenchmarks.  
  # Code here prioritizes "3" categories when available.
  defaultBenchmarks <- defaultBenchmarks %>% arrange(Indicator, desc(ConditionCategoryNum)) %>%
    group_by(Indicator) %>%
    slice(1)
  
  # Convert indicators to dataframe
  indicatorsdf <- as.data.frame(indicators)
  
  # Combine default and custom benchmark groups
  benchmarks <- bind_rows(definedBenchmarks, defaultBenchmarks)
  
  # Attribute and benchmarks for selecting and pivoting
  indicAttrSelection <- c('EvaluationID')
  benchmarkNames <- unique(benchmarks$Indicator)
  
  # Simplify indicator table using above values
  indicatorSelect <- indicatorsdf %>% 
    select(all_of((c(indicAttrSelection, benchmarkNames))))
  
  # Convert indicatorSelect to long format to prep for joining 
  indicatorLong <- indicatorSelect %>% pivot_longer(-all_of(indicAttrSelection), 
                                                    names_to = "Indicator", 
                                                    values_to = "value")
  
  # Pivot values from "apply benchmark" table to long form.  This tells us which 
  # benchmark group (NOT min/mod/maj values yet) should be applied to 
  # the indicator of each point.
  assignmentsLong <- assignments %>% pivot_longer(-EvaluationID, names_to = "Indicator", values_to = "bmGroup")
  
  # Attach the actual numerical threshold values and operators (benchmarks) to assignmentsLong
  benchmarksAndAssignments <-  left_join(assignmentsLong, benchmarks, join_by("bmGroup", "Indicator"))
  
  # Join indicators and benchmark assignments
  IndicatorValuesBenchmarks <- inner_join(indicatorLong, benchmarksAndAssignments, join_by("EvaluationID", "Indicator"))
  
  
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
  
  # Convert to wide format to join with original indicators input
  reachConditionsWide <- IndicatorValuesBenchmarks %>% 
    select(EvaluationID, Indicator, value, Condition) %>% 
    pivot_wider(id_cols = !value, names_from = Indicator, values_from = Condition, names_glue = "{Indicator}{'Condition'}")
  
  # Join reach conditions with indicators (the original, spatial object) and NOT indicatorsdf
  reachConditions <- left_join(indicators, reachConditionsWide, by = "EvaluationID")
  
  
  return(reachConditions)
}
