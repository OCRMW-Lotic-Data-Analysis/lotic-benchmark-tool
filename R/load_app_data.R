# Function to load indicator data that does some prep and validation 
load_indicator_data <- function(name, path) {
  ext <- tools::file_ext(name)
  dat <- switch(ext,
                csv = vroom::vroom(path, delim = ",", show_col_types = FALSE) %>% 
                  st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269),
                validate("Invalid file; Please upload a .csv"))
}

# Load  indicator data
indicatorData_raw <- reactive({
  load_indicator_data("BLM_Natl_AIM_Lotic_Indicators_Hub.csv", "./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv")
})

# Load default conditions
defaultConditions <- readr::read_csv("./appData/A_DefaultConditions.csv", show_col_types = FALSE) %>% 
  dplyr::select(PointID, EvaluationID, Indicator, 
         Value, BenchmarkGroup, ModerateBenchmark1, 
         MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2, 
         Condition, BLM_AdminState)
