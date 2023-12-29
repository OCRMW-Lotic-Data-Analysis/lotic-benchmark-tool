library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(gridlayout)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(shinyWidgets)
library(rhandsontable)
library(htmltools)
library(zip)

# Load appFunctions
source("./appFunctions/determine_reach_conditions.R")

# Default "Fish Bearing" example benchmarks from original tool.
#defaultBenchmarks <- read.csv("./sample_data/fishbearing_bm_group.csv", colClasses = "character")
defaultBenchmarks <- read.csv("./appData/default_benchmark_and_operators.csv", colClasses = "character")

#shinyuieditor::launch_editor(app_loc = "benchmark_dashboard/")
ui <- page_navbar(
  title = "Lotic AIM Indicator Benchmark Tool",
  selected = "1. Select Indicators",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  # 1. Select Indicators
  nav_panel(
    title = "1. Select Indicators",
    grid_container(
      layout = c(
        "selectIndicatorSidebar indicatorMap  ",
        "indicatorTable         indicatorTable"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.4fr",
        "1.6fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "selectIndicatorSidebar",
        card_body(
          fileInput("upload", "Upload Indicators", accept = c(".csv"))
        )
      ),
      grid_card(
        area = "indicatorMap",
        full_screen = TRUE,
        card_body(
          leafletOutput(outputId = "indicatorMap"))
      ),
      grid_card(
        area = "indicatorTable",
        full_screen = TRUE,
        card_body(
          DTOutput(
            outputId = "indicatorTable",
            height = "auto",
            fill = TRUE
          )
        )
      )
    )
  ),
  # 2. Define Benchmarks
  nav_panel(
    title = "2. Define Benchmarks",
    grid_container(
      layout = c(
        "benchmark_sidebar benchmark_table",
        "benchmark_sidebar benchmark_table"
      ),
      row_sizes = c(
        "365px",
        "1fr"
      ),
      col_sizes = c(
        "0.4fr",
        "1.6fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "benchmark_sidebar",
        card_header("Benchmark Groups"),
        card_body(
          pickerInput(inputId = "selectBenchmarks",
                                label = "Select Benchmarks", 
                                choices = unique(defaultBenchmarks$Indicator),
                                options = list(
                                  `actions-box` = TRUE), 
                                multiple = TRUE
                              ),
          radioButtons(
            inputId = "categoryNumSelector",
            label = "Number of Categories",
            choices = c("2" = 2, "3" = 3),
            selected = 3,
            inline = TRUE
            ),
          actionButton(
            inputId = "myButton",
            label = "Download Bechmark Config Template"
          ),
          actionButton(
            inputId = "myButton",
            label = "Upload Benchmark Config"
          )
        )
      ),
      grid_card(
        area = "benchmark_table",
        card_body(
          rHandsontableOutput("benchmark_hot"),
          DTOutput('benchmark_dt')
        )
      )
    )
  ),
  # 3. Reach Conditions
  nav_panel(
    title = "3. Reach Conditions",
    grid_container(
      layout = c(
        "reach_cond_selectors   reach_cond_map        ",
        "reach_conditions_table reach_conditions_table"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.4fr",
        "1.6fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "reach_conditions_table",
        full_screen = TRUE,
        card_body(
          DTOutput(
            outputId = "reachConditionTable",
            height = "auto",
            fill = TRUE
          )
        )
      ),
      grid_card(
        area = "reach_cond_selectors",
        full_screen = TRUE,
        card_body(
          selectInput(
            inputId = "reachCondMapSelect",
            label = "Select Indicator for Map",
            choices = NULL
          ),
          downloadButton(
            outputId = "reachCondDLcsv",
            label = "Download CSV"
          ),
          downloadButton(
            outputId = "reachCondDLgdb",
            label = "Download ESRI FileGDB"
          )
        )
      ),
      grid_card(
        area = "reach_cond_map",
        full_screen = TRUE,
        leafletOutput(outputId = "reachCondMap")
      )
    )
  )
)


server <- function(input, output, session) {
  #bs_themer()
# 0. Global Items ------------------------------------------------------------
  
# 1. Select Indicators -------------------------------------------------------
    
# Uploaded indicator data (pre-filtered by user)
  indicatorData <- reactive({
    req(input$upload)
    vroom::vroom(input$upload$datapath, delim = ",") %>%
      st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269)
  })
  
  # Map showing initial indicators loaded into app
  output$indicatorMap <- renderLeaflet({
    labels <- paste(
      "<strong>", indicatorData()$PointID,
      "</strong><br>", indicatorData()$StreamName) %>%
      lapply(htmltools::HTML)
    
    pal <- colorFactor(c("navy", "red", "red"), domain = c("Targeted", "RandomGRTS", "RandomSystematic"))
    indicatorData() %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 5,
        color = ~pal(PointSelectionType),
        stroke = FALSE, fillOpacity = 1,
        label = ~labels) %>%
      addProviderTiles("Esri.WorldTopoMap",
                       group = "Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addLayersControl(
        baseGroups = c("Esri.WorldTopoMap",
                       "Esri.WorldImagery"),
        # position it on the topleft
        position = "topleft") %>%
      addLegend(pal = pal, values = ~PointSelectionType, opacity = 1)
    })
  
  # Table showing initial indicators loaded into app
  output$indicatorTable <- renderDT({
    indicatorData()},
    extensions = 'Scroller',
    options = list(
      dom = 't',  #hide search box
      deferRender = FALSE,
      scrollY = 350,
      scrollCollapse = TRUE,
      scroller = TRUE,
      scrollX = TRUE)
    )
  
# 2. Define Benchmarks -------------------------------------------------------
  
  # Benchmarks to include for uploaded points
  selectedBenchmarks <- reactive({
    input$selectBenchmarks
    })
  
  # Editable benchmark table
  output$benchmark_hot <- renderRHandsontable({
    # if (selectedBenchmarks() == ""){
    #   return(NULL)
    # }
    
    rhandsontable(
      data = defaultBenchmarks %>% filter(Indicator %in% selectedBenchmarks())
      # data = data.frame(Benchmarks = selectedBenchmarks(),
      #                   Major = 76,
      #                   Operator = "<",
      #                   Minor = 24)
    )
  })
  
  
  output$benchmark_dt <- renderDT({
    defaultBenchmarks %>% filter(Indicator %in% selectedBenchmarks())},
    editable = list(target = 'row', 
                    disable = list(columns = c(0, 3, 4, 5))),
    rownames = FALSE,
    selection = 'none',
    options = list(dom = 't')  #hide search box
    )
  
  
  # Save edited benchmark table for calculations
  definedBenchmarks <- reactive({
    hot_to_r(input$benchmark_hot)
  })
  
# 3. Reach Conditions --------------------------------------------------------
  
  #Use selectedBenchmarks to update dropdown options for plotting reach conditions
  observe({
    updateSelectInput(session, "reachCondMapSelect",
                      choices = selectedBenchmarks(),
                      selected = ""
                      )
    })
  
  
  # Map showing indicators colored by selected reach condition variable
  output$reachCondMap <- renderLeaflet({
    #mapVar <- input$reachCondMapSelect
    
    # labels <- paste0(
    #   "<strong>", indicatorData()$PointID, "</strong><br>",
    #   indicatorData()$StreamName, "<br>",
    #   mapVar, ": ", selectedBenchmarks()[[mapVar]]) %>%
    #   lapply(htmltools::HTML)
    
    # pal <- colorFactor(
    #   palette = "viridis",
    #   domain = as.numeric(selectedBenchmarks()$Condition))
    
    pal <- colorFactor(
         palette = "viridis",
         domain = as.numeric(selectedBenchmarks()$Condition))
    
    indicatorData() %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 5,
        color = ~pal,
        stroke = FALSE, 
        fillOpacity = 1,
        #label = ~labels
        ) %>%
      addProviderTiles("Esri.WorldTopoMap",
                       group = "Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addLayersControl(
        baseGroups = c("Esri.WorldTopoMap",
                       "Esri.WorldImagery"),
        # position it on the topleft
        position = "topleft") #%>%
      #addLegend(pal = pal, values = ~as.numeric(selectedBenchmarks()[[mapVar]]), opacity = 1)
  })
  
  # Table showing reach conditions after benchmark evaluation
  # output$reachConditionTable <- renderDT({determine_reach_conditions(indicators =  indicatorData(), benchmarks = definedBenchmarks())},
  #   options = list(lengthChange = FALSE,
  #                  dom = 't' #hide search box
  #                  )
  #   )
  
  # Reach conditions (Min, Mod, Max) for each indicator
  reachConditions <- reactive({determine_reach_conditions(indicators =  indicatorData(), 
                                                          benchmarks = definedBenchmarks(), 
                                                          categoryNum = input$categoryNumSelector)})

  
  # Prep data for CSV export.  Currently only outputs pivoted reachConditions table.  Having issues joining with other table within this reactive.
  indicatorCSV <- reactive({
    
    indicatorDataNoGeom <- st_drop_geometry(indicatorData())
    reachConditionsWide <- reachConditions() %>% 
      pivot_wider(id_cols = !value, names_from = Indicator, values_from = Condition, names_glue = "{Indicator}{'Condition'}")
    
    left_join(indicatorDataNoGeom, reachConditionsWide, by = "EvaluationID")
  })
  
  # Download CSV button 
  output$reachCondDLcsv <- downloadHandler(
    filename = "reachConditions.csv",
    content = function(file) {
      write.csv(indicatorCSV(), file)
    }
  )
  
  # Prep data for Shapefile export.  Currently only outputs pivoted reachConditions table.  Having issues joining with other table within this reactive.
  indicatorSHP <- reactive({
    
    reachConditionsWide <- reachConditions() %>% 
      pivot_wider(id_cols = !value, names_from = Indicator, values_from = Condition, names_glue = "{Indicator}{'Condition'}")
    
    indicatorsWithConditions <- left_join(indicatorData(), reachConditionsWide, by = "EvaluationID")
    return(indicatorsWithConditions)
  })
  
  # # Download shapefile button 
  # output$reachCondDLshp <- downloadHandler(
  #   
  #   filename = function() {
  #     paste("reachConditions", ".gdb", sep="")
  #   },
  #   content = function(file) {
  #     st_write(obj =indicatorSHP(), 
  #              dsn = file,
  #              layer = "reachConditions",
  #              driver = "OpenFileGDB")
  #   }
  # )
  
  output$reachCondDLgdb <- downloadHandler(
    filename = "reachConditions.zip",
    content = function(file) {
      data <- indicatorSHP() # I assume this is a reactive object
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
      # # the following zip method works for me in linux but substitute with whatever method working in your OS 
      # zip_command <- paste("zip -j", 
      #                      zip_file, 
      #                      paste(shp_files, collapse = " "))
      zip::zip(zip_file, files = "reachConditions.gdb", root = temp_gdb)
      
      # copy the zip file to the file argument
      file.copy(zip_file, file)
      # remove all the files created
      file.remove(zip_file, gdb_files)
    }
  )
  
  # Reach Conditions Table (mostly a placeholder for now)
  output$reachConditionTable <- renderDT({reachConditions()},)
}

shinyApp(ui, server)