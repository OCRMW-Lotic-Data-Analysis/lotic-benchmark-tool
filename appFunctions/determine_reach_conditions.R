determine_reach_conditions <- function(indicators, benchmarks) {
  
  indicators <- as.data.frame(indicators)
  
  # Attribute and benchmarks for selecting and pivoting
  attributeSelection <- c('EvaluationID','BLM_AdminState','EcoregionStreamSize','ProtocolType','StreamOrder','SampledMidLatitude','SampledMidLongitude')
  benchmarkNames <- benchmarks$Indicator
  
  # Simplify indicator table using above values
  indicatorSelect <- indicators %>% 
    select(all_of((c(attributeSelection, benchmarkNames))))
  
  # Convert indicators to long format and join with benchmarks
  indicatorLong <- indicatorSelect %>% pivot_longer(-all_of(attributeSelection), 
                                      names_to = "Indicator", 
                                      values_to = "value")
  
  IndicatorValuesBenchmarks <- inner_join(indicatorLong, benchmarks, by = "Indicator", relationship = "many-to-many")
  
  
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

  return(IndicatorValuesBenchmarks)
}
