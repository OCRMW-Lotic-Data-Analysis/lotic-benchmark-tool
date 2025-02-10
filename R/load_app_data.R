# Function to load indicator data that does some prep and validation 
load_indicator_data <- function(name, path) {
  ext <- tools::file_ext(name)
  dat <- switch(ext,
                csv = vroom::vroom(path, delim = ",", show_col_types = FALSE) %>% 
                  st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269) %>%
                  st_transform(crs = 4326),
                validate("Invalid file; Please upload a .csv"))
}

# Load  indicator data
indicatorData_raw <- reactive({
  load_indicator_data("BLM_Natl_AIM_Lotic_Indicators_Hub.csv", "./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv")
})

# Load default conditions
defaultConditions <- readr::read_csv("./appData/A_DefaultConditions.csv", show_col_types = FALSE)


indicatorMetadata <- readr::read_csv("./appData/indicator_metadata.csv", col_types = readr::cols(.default = "c")) %>% tidyr::drop_na() # only include fully complete indicator metadata
blankCustomBMForm <- readr::read_csv("./appData/blankCustomBMForm.csv", col_types = readr::cols(.default = "c", 
                                                                                  ModerateBenchmark1 = "n", 
                                                                                  MajorBenchmark1 = "n",
                                                                                  ModerateBenchmark2 = "n",
                                                                                  MajorBenchmark2 = "n",
                                                                                  ConditionCategoryNum = "n"))
