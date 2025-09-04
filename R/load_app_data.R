# Raw indicator data
indicatorData_raw <- readr::read_csv("./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv", show_col_types = FALSE) %>%
  dplyr::mutate(FieldEvalDate = as.Date(FieldEvalDate, tryFormats = c("%m/%d/%Y"))) %>%
  sf::st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269) %>%
  sf::st_transform(crs = 4326)

# Load default conditions
defaultConditions <- vroom::vroom("./appData/stateNatDefaultCondJoined.csv", show_col_types = FALSE)


indicatorMetadata <- readr::read_csv("./appData/indicator_metadata.csv", col_types = readr::cols(.default = "c")) %>% tidyr::drop_na() # only include fully complete indicator metadata
blankCustomBMForm <- readr::read_csv("./appData/blankCustomBMForm.csv", col_types = readr::cols(.default = "c", 
                                                                                  ModerateBenchmark1 = "n", 
                                                                                  MajorBenchmark1 = "n",
                                                                                  ModerateBenchmark2 = "n",
                                                                                  MajorBenchmark2 = "n",
                                                                                  ConditionCategoryNum = "n"))
