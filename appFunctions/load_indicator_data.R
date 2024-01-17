
load_indicator_data <- function(name, path) {
ext <- tools::file_ext(name)
dat <- switch(ext,
              csv = vroom::vroom(path, delim = ",", show_col_types = FALSE) %>% 
                st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269),
              validate("Invalid file; Please upload a .csv"))
}
