library(here)
library(readr)
library(dplyr)

# This script combines "A_NationalDefaultConditions" (National) and "A_DefaultConditions" (State)
# default conditions.  We prefer to use a state default condition over a national one so if 
# a condition exists in both default files for a specific EvaluationID, only the state is kept.


nat <- read_csv(here("pre-processing", "defaultConditions", "A_NationalDefaultConditions_2024-02-10.csv"))
st <- read_csv(here("pre-processing", "defaultConditions", "A_DefaultConditions_2024-02-10.csv"))


merged <- bind_rows(st, nat) %>% group_by(EvaluationID, Indicator) %>% slice_head()

write_csv(merged, here("pre-processing", "defaultConditions", "stateNatDefaultCondJoined.csv"))
