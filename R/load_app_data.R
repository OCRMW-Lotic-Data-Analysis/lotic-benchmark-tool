

load_indicator_data <- function(name, path) {
  ext <- tools::file_ext(name)
  dat <- switch(ext,
                csv = vroom::vroom(path, delim = ",", show_col_types = FALSE) %>% 
                  st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269),
                validate("Invalid file; Please upload a .csv"))
}

indicatorData_raw <- reactive({
  load_indicator_data("BLM_Natl_AIM_Lotic_Indicators_Hub.csv", "./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv")
})

defaultConditions <- read_csv("./appData/A_DefaultConditions.csv", show_col_types = FALSE) %>% 
  select(PointID, EvaluationID, Indicator, 
         Value, BenchmarkGroup, ModerateBenchmark1, 
         MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2, 
         Condition, BLM_AdminState)
