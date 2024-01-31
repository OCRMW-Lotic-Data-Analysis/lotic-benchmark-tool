library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(bslib)
library(plotly)
library(ggiraph)
library(DT)
library(leaflet)
library(sf)
library(shinyWidgets)
library(rhandsontable)
library(reactable)
library(reactable.extras)
library(htmltools)
library(zip)

source("./appFunctions/load_indicator_data.R")


ui <- page_navbar(
  title = "Lotic AIM Indicator Benchmark Tool",
  selected = "2. Define Benchmarks",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
  
  
  # 2. Define Benchmarks ----
  nav_panel(
    title = "2. Define Benchmarks",
    page_sidebar(
      #Sidebar
      sidebar = sidebar(
        width = "300px",
        h5("Create Benchmark Groups"),
        textInput("bmGroupNameinput", label = "Group Name"),
        pickerInput(inputId = "selectBenchmarks3",
                    label = "Three Condition Categories",
                    choices = "",    # choices are updated based on input of benchmarkValues()
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        pickerInput(inputId = "selectBenchmarks2",
                    label = "Two Condition Categories",
                    choices = "",    # choices are updated based on input of benchmarkValues()
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        br(),
        
        actionButton("saveNewBMGroup", label = "Save New Benchmark Group", style="color: #000000; background-color: #DEFFDE"),
        actionButton("deleteBMGroup", label = "Delete Selected Benchmark Group", style="color: #000000; background-color: #FFDEDE"),
        hr(),
        actionButton("editBMGroup", label = "Edit Selected Benchmark Group"),
        
        downloadButton(
          outputId = "benchmarkConfigDLcsv",
          label = "Download Current Benchmark Configuration"
        ),
        fileInput("benchmarkUpload", 
                  "Upload Benchmark Config", 
                  accept = c(".csv"))),
      # Main Panel
      rHandsontableOutput("defineBenchmark_hot"),
      dataTableOutput("benchmarkGroupsTable"),
      #verbatimTextOutput("value")
      )
    ),
  
  # 3. Apply Benchmarks ----
  nav_panel(
    title = "3 Apply Benchmarks",
    page_sidebar(
      #Sidebar
      sidebar = sidebar(
        width = "300px",
        h5("Apply Benchmarks")
        ),
      # Main Panel
      rHandsontableOutput("applyBenchmarks_hot"),
      #verbatimTextOutput("groupnames"),
      #rHandsontableOutput("benchmarkGroupTEST")
      )
    )
  
  
)

server <- function(input, output, session) {
  
  
  
  
  
  
  
  
  
  
  indicatorData <- reactive({
  
  read_csv("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/lotic-benchmark-tool/sample_data/wy_landerPts.csv", show_col_types = FALSE) %>%
      st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269)

  })
  
  
  benchmarkValues <-reactiveVal(read.csv("./appData/default_benchmark_and_operators.csv", colClasses = "character"))
  
  
  
  
  
  
  
  
  
  # 2. Define Benchmarks -------------------------------------------------------
  
  
  # Update benchmark selectors based on benchmarkValues().  This could come from
  # manual entry or upload.
  observe({
    updateSelectInput(session, "selectBenchmarks3",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator))
    
    updateSelectInput(session, "selectBenchmarks2",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator))
  })
  
  # Create mutually exclusive selectors for 2 and 3 category benchmarks.
  # For example: if pH is selected for 3 Category, it will be removed
  # from the dropdown selector for 2 Category.
  observeEvent(
    input$selectBenchmarks3,
    {
      updatePickerInput(
        session = session,
        inputId = "selectBenchmarks2",
        choices = setdiff(filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator), 
                          input$selectBenchmarks3),
        selected = input$selectBenchmarks2
      )
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$selectBenchmarks2,
    {
      
      updatePickerInput(
        session = session,
        inputId = "selectBenchmarks3",
        choices = setdiff(filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator), 
                          input$selectBenchmarks2),
        selected = input$selectBenchmarks3
      )
    },
    ignoreNULL = FALSE
  )
  
  # Editable benchmark table.  This is where users enter major/moderate thresholds
  output$defineBenchmark_hot <- renderRHandsontable({
    # if (input$selectBenchmarks3 == "" & input$selectBenchmarks2 == ""){
    #   return(NULL)
    # }
    cond2 <- dplyr::filter(benchmarkValues(), Indicator %in% input$selectBenchmarks3 & ConditionCategoryNum == 3)
    cond3 <- dplyr::filter(benchmarkValues(), Indicator %in% input$selectBenchmarks2 & ConditionCategoryNum == 2)
    
    dat <- bind_rows(cond2, cond3) %>% arrange(Indicator)
    
    rhandsontable(data = dat) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) 
    
  })
  
  # Download benchmarks as currently displayed in 'defineBenchmark_hot' table 
  output$benchmarkConfigDLcsv <- downloadHandler(
    filename = "benchmarkConfig.csv",
    content = function(file) {
      write.csv(definedBenchmarks(), file, na = "", row.names = FALSE) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )
  
  observeEvent(input$benchmarkUpload,{
    ext <- tools::file_ext(input$benchmarkUpload$name)
    bmul <- switch(ext,
                   csv = vroom::vroom(input$benchmarkUpload$datapath, delim = ",", col_types = cols(.default = "c"), show_col_types = FALSE),
                   validate("Invalid file; Please upload a .csv")
    )
    benchmarkValues(bmul)
    
    updateSelectInput(session, "selectBenchmarks3",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator),
                      selected = filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator))
    
    updateSelectInput(session, "selectBenchmarks2",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator),
                      selected = filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator))
    
    #print(benchmarkValues()) 
  })
  
  # Save the currently displayed values of the 'defineBenchmark_hot' table
  definedBenchmarks <- reactive({
    hot_to_r(input$defineBenchmark_hot)
    })
  
  # Initialize empty container for saved benchmark groups
  benchmarkGroupDF <- reactiveValues()
  benchmarkGroupWideSum <- reactiveValues()
  
  # Save edited benchmark table.  Also resets text/picker inputs and clears table.
  observeEvent(input$saveNewBMGroup,{
    req(input$bmGroupNameinput)
    # Get new benchmark group data into data frame
    newGroupData <- definedBenchmarks() %>% tibble::add_column(bmgroup = input$bmGroupNameinput)
    
    # Merge previously saved groups with newly entered group (long form)
    benchmarkGroupDF$df <- bind_rows(benchmarkGroupDF$df, newGroupData)
    
    # Merge previously saved groups with newly entered group (wide form summary table)
    benchmarkGroupWideSum$df <- benchmarkGroupDF$df %>% 
      select(bmgroup, Indicator, ModerateBenchmark1, ConditionCategoryNum) %>% 
      pivot_wider(names_from = Indicator, values_from = ModerateBenchmark1)
    
    # Reset all inputs to blank to prepare for next benchmark group
    updateTextInput(session, "bmGroupNameinput", value = "")
    updatePickerInput(session,"selectBenchmarks3", selected = character(0))
    updatePickerInput(session,"selectBenchmarks2", selected = character(0))
    #print(names(benchmarkGroupDF))
    }
    )
  
  
  # Delete the benchmark group based on the selected row in table.
  observeEvent(input$deleteBMGroup,{

    if (!is.null(input$benchmarkGroupsTable_rows_selected)) {
      # Get groupName(s) you want to delete into vector of strings
      groupNames <- benchmarkGroupWideSum$df %>% slice(input$benchmarkGroupsTable_rows_selected) %>% pull(bmgroup)
  
      # Actually remove the data from benchmarkGroupWideSum and benchmarkGroupDF reactive value
      benchmarkGroupWideSum$df <- benchmarkGroupWideSum$df %>% subset(!(bmgroup %in% groupNames))
      benchmarkGroupDF$df <- benchmarkGroupDF$df %>% subset(!(bmgroup %in% groupNames))
    }
  })
  
  
  # Edit previously saved benchmark group. 
  # almost works. opens table to edit but doesnt allow for saving and selectInputs are not correct. 
  observeEvent(input$editBMGroup,{
    groupNames <- benchmarkGroupWideSum$df %>% slice(input$benchmarkGroupsTable_rows_selected) %>% pull(bmgroup)
    bmedit <- benchmarkGroupDF$df %>% subset(bmgroup %in% groupNames)
    #print(groupNames)
    #print(bmedit)
    benchmarkValues(bmedit)
    
    updateSelectInput(session, "selectBenchmarks3",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator),
                      selected = filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator))
    
    updateSelectInput(session, "selectBenchmarks2",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator),
                      selected = filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator))
    
    #print(benchmarkValues()) 
  })
  
  # Summary table of defined benchmark groups.  
  output$benchmarkGroupsTable <- renderDataTable(
    benchmarkGroupWideSum$df,
    selection = "single"
    )
  
  
  
  # 3. Apply Benchmarks --------------------------------------------------------
  
  # Selected which benchmark groups to apply to each pointID/indicator combo.
  output$applyBenchmarks_hot <- renderRHandsontable({
    # Get the names of all possible benchmarks (will likley need to tweak this)
    bmVars <- unique(benchmarkValues()$Indicator)
    
    # Unique benchmark group names (unique b/c it's pulling from long-form table)
    bmGroups <- reactiveValuesToList(benchmarkGroupDF)[['df']] %>% pull(bmgroup) %>% unique()
    #print(bmGroups)
    
    # Strip indicator table to basic info and set benchmark group to "Default".
    # "Default" setting will be used to apply BLM's pre-defined, default conditions.
    applyBechmarkDat <- indicatorData() %>% st_drop_geometry() %>% select(c(PointID, StreamName, EvaluationID))
    applyBechmarkDat[bmVars] <- "Default"
    
    # Actual table
    rhandsontable(applyBechmarkDat,
                  rowHeaders = FALSE) %>%
    hot_table(highlightRow = TRUE, contextMenu = FALSE) %>%
    hot_cols(fixedColumnsLeft = 3) %>%
    hot_col(1:3, readOnly = TRUE) %>%
    hot_col(col = bmVars,
            type = "dropdown",
            # Including "Default" below is key. If 'bmGroups' only has 1 value, the dropdown doesn't work.
            source = c(bmGroups, "Default"),
            strict = FALSE,
            # Cells with "Default" selected are greyed out a bit.  Makes it easier to see where custom values are used.
            renderer = "
             function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (value == 'Default') {
             td.style.color = 'lightgrey';
             }
             Handsontable.renderers.DropdownRenderer.apply(this, arguments);
             }"
            )
    })
  
  #output$groupnames <- renderText({ bmGroups })
  #output$benchmarkGroupTEST <- renderRHandsontable({
  #    rhandsontable(data = benchmarkGroupDF$df)
  #})
  
}

shinyApp(ui, server)