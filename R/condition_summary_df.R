condition_summary_df <- function(reachConditions, selectedBenchmarks){

# Blank template of bmCondSummary table.  Used to prevent errors when Min, Mod, or Maj is missing.
bmSummaryBlank <- tibble(Indicator = character(), Minimal = numeric(), Moderate = numeric(), Major = numeric())

# Convert to normal dataframe
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
  # bind to blank template
  bind_rows(bmSummaryBlank) %>%
  select(Indicator, Minimal, Moderate, Major)


bmStatSummary <- reachConditions %>% select(all_of(selectedBenchmarks)) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "value") %>%
  group_by(Indicator) %>%
  summarise(Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE),
            Mean = round(mean(value, na.rm = TRUE), digits = 2))


bmSummary <- full_join(bmCondSummary, bmStatSummary, by = "Indicator")  
  
return(bmSummary)
}

