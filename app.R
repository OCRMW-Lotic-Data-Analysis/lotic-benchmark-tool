library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(bslib)
library(gridlayout)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(shinyWidgets)
library(rhandsontable)
library(reactable)
library(htmltools)
library(zip)

# Load appFunctions
source("./appFunctions/determine_reach_conditions.R")
source("./appFunctions/condition_summary_table.R")
source("./appFunctions/conditionsBoxplot.R")

# Default "Fish Bearing" example benchmarks from original tool.
#defaultBenchmarks <- read.csv("./sample_data/fishbearing_bm_group.csv", colClasses = "character")
defaultBenchmarks <- read.csv("./appData/default_benchmark_and_operators.csv", colClasses = "character")

#shinyuieditor::launch_editor(app_loc = "benchmark_dashboard/"

### UI -------------------------------------------------------------------------
ui <- page_navbar(
  title = "Lotic AIM Indicator Benchmark Tool",
  selected = "1. Select Indicators",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
  # 1. Select Indicators ----
  nav_panel(
    title = "1. Select Indicators",
    page_sidebar(
        sidebar = sidebar(fileInput("upload", "Upload Indicators", accept = c(".csv")),
                          selectInput(
                            inputId = "indicatorMapSelect",
                            label = "Select Indicator for Map",
                            choices = c("PointSelectionType", "EcoregionStreamSize","BeaverFlowMod",
                                        "BeaverSigns", "StreamOrder", "Project",
                                        "District", "FieldOffice")
                            ),
                          width = "300px"
                          ),
      navset_card_tab(
        nav_panel("Map",
                  leafletOutput(outputId = "indicatorMap"),
                  top = 80, right = 300,
                                
                  ),
        nav_panel("Table",
                  DTOutput(outputId = "indicatorTable",height = "auto",fill = TRUE)
                  )
        )
      ),
  ),
  # 2. Define Benchmarks ----
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
  # 3. Reach Conditions ----
  nav_panel(
    title = "3. Reach Conditions",
    page_sidebar(
      sidebar = sidebar(
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
          ),
        width = "300px"
        ),
      navset_card_tab(
        nav_panel("Map", 
                  leafletOutput(outputId = "reachCondMap")),
        nav_panel("Table",
                  DTOutput(outputId = "reachConditionTable",height = "auto", fill = TRUE)
        )
      )
    ),
  ),
  
  # 4. Summary ----
  nav_panel(
    title = "4. Summary",
    navset_card_tab(
      nav_panel(title = "Table", DTOutput("bmSummaryTable")),
      nav_panel(title = "Box Plots", 
                layout_sidebar(
                  sidebar = sidebar(selectInput(
                    inputId = "bmSummaryBoxplotsSelect",
                    label = "Select Indicator to Plot",
                    choices = NULL),
                    width = "300px",
                    open = "always"),
                  plotlyOutput("bmSummaryBoxplots")
                  )
                )
      )
    )
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {
  #bs_themer()
# 0. Global Items ------------------------------------------------------------
  
# 1. Select Indicators -------------------------------------------------------
    
# Uploaded indicator data (pre-filtered by user)
  indicatorData <- reactive({
    req(input$upload)
    vroom::vroom(input$upload$datapath, delim = ",", show_col_types = FALSE) %>%
      st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269)
  })
  
  # Map showing initial indicators loaded into app
  output$indicatorMap <- renderLeaflet({
    
    # Label to display when hovering over point
    labels <- paste(
      "<strong>", indicatorData()$PointID,
      "</strong><br>", indicatorData()$StreamName) %>%
      lapply(htmltools::HTML)
    
    # Indicators to map
    mapVals <- indicatorData()[[input$indicatorMapSelect]]
    
    # Make a palette
    pal <- colorFactor(palette = "Set2", levels = unique(mapVals))
    
    # Map
    indicatorData() %>%
      st_transform(crs = 4326) %>%
      leaflet(
        options = leafletOptions(
          attributionControl=FALSE)
        ) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 7,
        color = "black",
        fillColor = ~pal(mapVals),
        stroke = TRUE, 
        weight = 1,
        fillOpacity = 1,
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
      addLegend(pal = pal, 
                values = mapVals, 
                opacity = 1,
                title = input$indicatorMapSelect) 
    })
  
  # Table showing initial indicators loaded into app
  # output$indicatorTable <- renderDT({
  #   indicatorData()},
  #   extensions = 'Scroller',
  #   options = list(
  #     dom = 't',  #hide search box
  #     deferRender = FALSE,
  #     scrollY = 350,
  #     scrollCollapse = TRUE,
  #     scroller = TRUE,
  #     scrollX = TRUE)
  #   )
  
  #Simple table - doesnt autofit data though.  Above version does but isn't perfect.
  output$indicatorTable <- renderDT({
    indicatorData()},)
  
# 2. Define Benchmarks -------------------------------------------------------
  
  # Editable benchmark table
  output$benchmark_hot <- renderRHandsontable({
    # if (selectedBenchmarks() == ""){
    #   return(NULL)
    # }
    
    rhandsontable(
      data = defaultBenchmarks %>% filter(Indicator %in% input$selectBenchmarks &
                                          ConditionCategoryNum == input$categoryNumSelector)
      )
  })
  
  
  output$benchmark_dt <- renderDT({
    defaultBenchmarks %>% filter(Indicator %in% input$selectBenchmarks)},
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
  
  # Reach conditions (Min, Mod, Max) for each indicator
  reachConditions <- reactive({determine_reach_conditions(indicators =  indicatorData(), 
                                                          benchmarks = definedBenchmarks(), 
                                                          categoryNum = input$categoryNumSelector)
    })
  
  #Use selectedBenchmarks to update dropdown options for plotting reach conditions
  observe({
    updateSelectInput(session, "reachCondMapSelect",
                      choices = definedBenchmarks()$Indicator,
                      selected = definedBenchmarks()$Indicator[1]
                      )
    })
  
  
  # Map showing indicators colored by selected reach condition variable
  output$reachCondMap <- renderLeaflet({
    
    # Pull variable to plot from input select.  Input just shows benchmark name
    # to make select cleaner but "Condition" is appended to select col with value.
    mapVar <- paste0(input$reachCondMapSelect, "Condition")
    
    # Label that shows up on hover
    labels <- paste0(
      "<strong>", reachConditions()$PointID, "</strong><br>",
      reachConditions()$StreamName, "<br>",
      mapVar, ": ", reachConditions()[[mapVar]]) %>%
      lapply(htmltools::HTML)
    
    # Create a new grouping variable.  Ensures legend item order is correct.
    ord <- factor(c("Major", "Moderate", "Minimal"), levels = c("Major", "Moderate", "Minimal"))
    
    
    # Palet
    pal <- colorFactor(c("#f03b20", "#feb24c", "#ffeda0"), 
                       levels = ord,
                       ordered = TRUE)
    
    # Map
    reachConditions() %>%
      st_transform(crs = 4326) %>%
      leaflet(
        options = leafletOptions(
          attributionControl=FALSE)
      ) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 7,
        color = "black",
        fillColor = ~pal(reachConditions()[[mapVar]]),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = ~labels
      ) %>%
      addProviderTiles("Esri.WorldTopoMap",
                       group = "Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addLayersControl(
        baseGroups = c("Esri.WorldTopoMap",
                       "Esri.WorldImagery"),
        # position it on the topleft
        position = "topleft") %>%
      addLegend(pal = pal, 
                values = ord,
                opacity = 1, 
                title = mapVar)
    
  })

  # Download CSV button 
  output$reachCondDLcsv <- downloadHandler(
    filename = "reachConditions.csv",
    content = function(file) {
      write.csv(st_drop_geometry(reachConditions()), file) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )
  
  # Download ESRI FileGDB button (currently works locally but not on Shinyapps.io)
  output$reachCondDLgdb <- downloadHandler(
    filename = "reachConditions.zip",
    content = function(file) {
      data <- reachConditions() # I assume this is a reactive object
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
  
# 4. Condition Summary ---------------------------------------------------------
  output$bmSummaryTable <- renderDT({
    condition_summary_table(reachConditions(), definedBenchmarks()$Indicator)},)
  
  observe({
    updateSelectInput(session, "bmSummaryBoxplotsSelect",
                      choices = definedBenchmarks()$Indicator,
                      selected = definedBenchmarks()$Indicator[1]
                      )
    })
  
  
  output$bmSummaryBoxplots <- renderPlotly({
    conditionsBoxplot(reachConditions(), input$bmSummaryBoxplotsSelect)
  })
}

shinyApp(ui, server)