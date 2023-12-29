condition_summary_table <- function(reachConditions, selectedBenchmarks){

reachConditions <- st_drop_geometry(reachConditions)

#selectedBenchmarks <- c("PctBankCoveredStableMIM", "PctBankOverheadCover")
#selectedBenchmarks <- selectedBenchmarks()
selectedBenchmarkConditions <- selectedBenchmarks %>% paste0("Condition")

bmCondSummary <- reachConditions %>% select(all_of(selectedBenchmarkConditions)) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "condition") %>%
  group_by(Indicator, condition) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = condition, values_from = count) %>%
  # Remove "Condition" suffix to get normal indicator name
  mutate(Indicator = str_replace(Indicator, "Condition", "")) %>%
  relocate(Indicator, Minimal, Moderate, Major)


bmStatSummary <- reachConditions %>% select(all_of(selectedBenchmarks)) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "value") %>%
  group_by(Indicator) %>%
  summarise(Min = min(value),
            Max = max(value),
            Mean = round(mean(value), digits = 2))


bmSummary <- full_join(bmCondSummary, bmStatSummary, by = "Indicator")  
  
return(bmSummary)
}

