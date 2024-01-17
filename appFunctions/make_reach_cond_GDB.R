# Create fileGBD export and pass location to 'file'
make_reach_cond_GDB <- function(reachConditions, file) {
  data <- reachConditions # I assume this is a reactive object
  # create a temp folder for shp files
  temp_gdb <- tempdir()
  # write shp files
  st_write(obj = data, 
           dsn = file.path(temp_gdb, "reachConditions.gdb"),
           layer = "reachConditions",
           driver = "OpenFileGDB",
           append = FALSE)
  # zip all the shp files
  zip_file <- file.path(temp_gdb, "reachConditionstemp.zip")
  gdb_files <- list.files(temp_gdb,
                          "reachConditions.gdb",
                          full.names = TRUE)

  zip::zip(zip_file, files = "reachConditions.gdb", root = temp_gdb)
  
  # copy the zip file to the file argument
  file.copy(zip_file, file)
  # remove all the files created
  file.remove(zip_file, gdb_files)
}
