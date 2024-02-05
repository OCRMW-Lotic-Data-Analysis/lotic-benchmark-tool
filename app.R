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

# Load appFunctions
source("./appFunctions/determine_reach_conditions_v2.R")
source("./appFunctions/condition_summary_table.R")
source("./appFunctions/conditions_boxplot.R")
source("./appFunctions/load_indicator_data.R")
source("./appFunctions/leaflet_maps.R")
source("./appFunctions/make_reach_cond_GDB.R")

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
      sidebar = sidebar(
        selectInput("startingDataType", "Indicator Data Source",
                    choices = c("Upload" = "upload", 
                                "Filter from all indicators" = "filter",
                                "Lander Sample Data" = "sampleData")),
        
        conditionalPanel(
          condition = "input.startingDataType == 'upload'",
          fileInput("indicatorUpload", "Upload Indicators", accept = c(".csv"))),
        
        
        # Map indicator select that only displays when data is loaded.  Logic in server.
        uiOutput("indicatorMapSelect"),
        
        
        conditionalPanel(condition = "input.startingDataType == 'filter'",
                         accordion_panel(
                           "  Filters", icon = bsicons::bs_icon("sliders"),
                           pickerInput(inputId = "adminState",
                                       label = "Admin State",
                                       choices = NULL,
                                       options = list(
                                         `actions-box` = TRUE),
                                       multiple = TRUE
                           )
                         )),
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
    page_sidebar(
      #Sidebar
      sidebar = sidebar(
        width = "300px",
        h5("Create Benchmark Groups"),
        textInput("bmGroupNameinput", label = "Group Name"),
        pickerInput(inputId = "selectBenchmarks3",
                    label = "Three Condition Categories",
                    choices = "",    # choices are updated based on input of defaultBenchmarkVals()
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        pickerInput(inputId = "selectBenchmarks2",
                    label = "Two Condition Categories",
                    choices = "",    # choices are updated based on input of defaultBenchmarkVals()
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
  ),
  
  # 4. Reach Conditions ----
  nav_panel(
    title = "4. Reach Conditions",
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
  
  # 5. Summary ----
  nav_panel(
    title = "5. Summary",
    navset_card_tab(
      nav_panel(title = "Table", DTOutput("bmSummaryTable")),
      nav_panel(title = "Box Plots", 
                layout_sidebar(
                  sidebar = sidebar(
                    selectInput(
                      inputId = "bmSummaryBoxplotsSelect",
                      label = "Select Indicator to Plot",
                      choices = NULL),
                    checkboxInput(
                      inputId = "showDensity",
                      label = "Include Density Plot", 
                      value = TRUE),
                    width = "300px",
                    open = "always"),
                  girafeOutput("bmSummaryBoxplots")
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
  
  #  Uploaded indicator data (pre-filtered by user) or filter from all available indicators
  
  indicatorData <- reactive({
    if (input$startingDataType == "upload") {
      req(input$indicatorUpload)
      #dat <- load_indicator_data(input$indicatorUpload$name, input$indicatorUpload$datapath)
      load_indicator_data(input$indicatorUpload$name, input$indicatorUpload$datapath)
    } else if (input$startingDataType == "filter") {
      #dat <- load_indicator_data("BLM_Natl_AIM_Lotic_Indicators_Hub.csv", "./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv")
      load_indicator_data("BLM_Natl_AIM_Lotic_Indicators_Hub.csv", "./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv")
    } else if (input$startingDataType == "sampleData") {
      load_indicator_data("wy_landerPts.csv", "./appData/wy_landerPts.csv")
    }
  })
  
  ### Experimental filtering 
  observeEvent(
    input$startingDataType == "filter",
    {
      updatePickerInput(
        session = session,
        inputId = "adminState",
        choices = unique(indicatorData()$BLM_AdminState),
        selected = NULL
      )
    })
  
  event_trigger <- reactive({
    list(input$adminState)
  })
  
  observeEvent(ignoreInit = TRUE, event_trigger(),
               {
                 asdf <- indicatorData() %>% filter(BLM_AdminState %in% input$adminState)
                 updatePickerInput(
                   session = session,
                   inputId = "adminState",
                   choices = unique(asdf$BLM_AdminState,
                                    selected = unique(asdf$BLM_AdminState))
                   
                 )
               })
  ## End experimenting filters
  
  
  # Map showing initial indicators loaded into app
  
  # Only once data is loaded (uploaded or local version of ALL), display the map
  # attribute selector.
  output[["indicatorMapSelect"]] <- renderUI({
    req(indicatorData())
    
    selectInput(
      inputId = "indicatorMapSelect",
      label = "Select Indicator for Map",
      choices = c("PointSelectionType", "EcoregionStreamSize","BeaverFlowMod",
                  "BeaverSigns", "StreamOrder", "Project",
                  "District", "FieldOffice")
    )
    
  })
  
  output$indicatorMap <- renderLeaflet({
    
    indicator_leaflet_map(indicatorData(), input$indicatorMapSelect)
    
  })
  
  #Simple table - doesnt autofit data though.  Above version does but isn't perfect.
  output$indicatorTable <- renderDT({
    indicatorData()},)
  
  
# 2. Define Benchmarks -------------------------------------------------------
  
  # Load default benchmarks
  defaultBenchmarkVals <-reactiveVal(read.csv("./appData/default_benchmark_and_operators.csv", colClasses = "character") %>% 
                                       mutate(bmGroup = "Default"))
  
  # Update benchmark selectors based on defaultBenchmarkVals().  This could come from
  # manual entry or upload.
  observe({
    updateSelectInput(session, "selectBenchmarks3",
                      choices = filter(defaultBenchmarkVals(), ConditionCategoryNum == 3) %>% pull(Indicator))
    
    updateSelectInput(session, "selectBenchmarks2",
                      choices = filter(defaultBenchmarkVals(), ConditionCategoryNum == 2) %>% pull(Indicator))
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
        choices = setdiff(filter(defaultBenchmarkVals(), ConditionCategoryNum == 2) %>% pull(Indicator), 
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
        choices = setdiff(filter(defaultBenchmarkVals(), ConditionCategoryNum == 3) %>% pull(Indicator), 
                          input$selectBenchmarks2),
        selected = input$selectBenchmarks3
      )
    },
    ignoreNULL = FALSE
  )
  
  # Editable benchmark table.  This is where users enter major/moderate thresholds
  output$defineBenchmark_hot <- renderRHandsontable({
    
    # Wait to display table until benchmark categories are selected.
    req(isTruthy(input$selectBenchmarks3 != "") || isTruthy(input$selectBenchmarks2 != ""))
    
    # Filter to selected benchmarks
    cond2 <- dplyr::filter(defaultBenchmarkVals(), Indicator %in% input$selectBenchmarks3 & ConditionCategoryNum == 3)
    cond3 <- dplyr::filter(defaultBenchmarkVals(), Indicator %in% input$selectBenchmarks2 & ConditionCategoryNum == 2)
    
    # Merge above filtered data.  Since this is no longer the defaultBenchmarkVal, remove column with "Default" group.
    # Column removed as opposed to just removing values bc column is added later.
    dat <- bind_rows(cond2, cond3) %>% arrange(Indicator) %>%
      select(-bmGroup)
    
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
  
  # Upload benchmarks (NOT CURRENTLY WORKING)
  # observeEvent(input$benchmarkUpload,{
  #   ext <- tools::file_ext(input$benchmarkUpload$name)
  #   bmul <- switch(ext,
  #                  csv = vroom::vroom(input$benchmarkUpload$datapath, delim = ",", col_types = cols(.default = "c"), show_col_types = FALSE),
  #                  validate("Invalid file; Please upload a .csv")
  #   )
  #   defaultBenchmarkVals(bmul)
  #   
  #   updateSelectInput(session, "selectBenchmarks3",
  #                     choices = filter(defaultBenchmarkVals(), ConditionCategoryNum == 3) %>% pull(Indicator),
  #                     selected = filter(defaultBenchmarkVals(), ConditionCategoryNum == 3) %>% pull(Indicator))
  #   
  #   updateSelectInput(session, "selectBenchmarks2",
  #                     choices = filter(defaultBenchmarkVals(), ConditionCategoryNum == 2) %>% pull(Indicator),
  #                     selected = filter(defaultBenchmarkVals(), ConditionCategoryNum == 2) %>% pull(Indicator))
  #   
  #   #print(defaultBenchmarkVals()) 
  # })
  
  # Save the currently displayed values of the 'defineBenchmark_hot' table
  definedBenchmarks <- reactive({
    hot_to_r(input$defineBenchmark_hot)
  })
  
  # Initialize empty container for saved benchmark groups
  benchmarkGroupDF <- reactiveValues()
  benchmarkGroupWideSum <- reactiveValues()
  
  # SAVE edited benchmark table.  Also resets text/picker inputs and clears table.
  observeEvent(input$saveNewBMGroup,{
    req(input$bmGroupNameinput)
    # Get new benchmark group data into data frame
    newGroupData <- definedBenchmarks() %>% tibble::add_column(bmGroup = input$bmGroupNameinput)
    
    # Merge previously saved groups with newly entered group (long form)
    benchmarkGroupDF$df <- bind_rows(benchmarkGroupDF$df, newGroupData)
    
    # Merge previously saved groups with newly entered group (wide form summary table)
    benchmarkGroupWideSum$df <- benchmarkGroupDF$df %>% 
      select(bmGroup, Indicator, ModerateBenchmark1, ConditionCategoryNum) %>% 
      pivot_wider(names_from = Indicator, values_from = ModerateBenchmark1)
    
    # Reset all inputs to blank to prepare for next benchmark group
    updateTextInput(session, "bmGroupNameinput", value = "")
    updatePickerInput(session,"selectBenchmarks3", selected = character(0))
    updatePickerInput(session,"selectBenchmarks2", selected = character(0))
    #print(names(benchmarkGroupDF))
  })
  
  # DELETE the benchmark group based on the selected row in table.
  observeEvent(input$deleteBMGroup,{
    
    if (!is.null(input$benchmarkGroupsTable_rows_selected)) {
      # Get groupName(s) you want to delete into vector of strings
      groupNames <- benchmarkGroupWideSum$df %>% slice(input$benchmarkGroupsTable_rows_selected) %>% pull(bmGroup)
      
      # Actually remove the data from benchmarkGroupWideSum and benchmarkGroupDF reactive value
      benchmarkGroupWideSum$df <- benchmarkGroupWideSum$df %>% subset(!(bmGroup %in% groupNames))
      benchmarkGroupDF$df <- benchmarkGroupDF$df %>% subset(!(bmGroup %in% groupNames))
    }
  })
  
  # EDIT previously saved benchmark group. 
  # almost works. opens table to edit but doesnt allow for saving and selectInputs are not correct. 
  # observeEvent(input$editBMGroup,{
  #   groupNames <- benchmarkGroupWideSum$df %>% slice(input$benchmarkGroupsTable_rows_selected) %>% pull(bmGroup)
  #   bmedit <- benchmarkGroupDF$df %>% subset(bmGroup %in% groupNames)
  #   #print(groupNames)
  #   #print(bmedit)
  #   defaultBenchmarkVals(bmedit)
  #   
  #   updateSelectInput(session, "selectBenchmarks3",
  #                     choices = filter(defaultBenchmarkVals(), ConditionCategoryNum == 3) %>% pull(Indicator),
  #                     selected = filter(defaultBenchmarkVals(), ConditionCategoryNum == 3) %>% pull(Indicator))
  #   
  #   updateSelectInput(session, "selectBenchmarks2",
  #                     choices = filter(defaultBenchmarkVals(), ConditionCategoryNum == 2) %>% pull(Indicator),
  #                     selected = filter(defaultBenchmarkVals(), ConditionCategoryNum == 2) %>% pull(Indicator))
  #   
  #   #print(defaultBenchmarkVals()) 
  # })
  
  # Summary table of defined benchmark groups.  
  output$benchmarkGroupsTable <- renderDataTable(
    benchmarkGroupDF$df,
    selection = "single"
  )
  
# 3. Apply Benchmarks --------------------------------------------------------
  
  # Selected which benchmark groups to apply to each pointID/indicator combo.
  output$applyBenchmarks_hot <- renderRHandsontable({
    req(benchmarkGroupDF$df)
    
    # Get the names of all possible benchmarks (will likley need to tweak this)
    bmVars <- unique(defaultBenchmarkVals()$Indicator)
    
    # Unique benchmark group names (unique b/c it's pulling from long-form table)
    bmGroups <- reactiveValuesToList(benchmarkGroupDF)[['df']] %>% pull(bmGroup) %>% unique()
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
              allowInvalid = FALSE,
              # Cells with "Default" selected are greyed out a bit.  Makes it easier to see where custom values are used.
              renderer = "
             function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (value == 'Default') {
             td.style.color = 'lightgrey';
             } else if (value != 'Default') {
             td.style.background = '#C1FFC1';
             }
             Handsontable.renderers.DropdownRenderer.apply(this, arguments);
             }"
      )
  })
  
  assignedBenchmarks <- reactive({
    hot_to_r(input$applyBenchmarks_hot)
  })
  
  #output$groupnames <- renderText({ bmGroups })
  #output$benchmarkGroupTEST <- renderRHandsontable({
  #    rhandsontable(data = benchmarkGroupDF$df)
  #})
  
# 4. Reach Conditions --------------------------------------------------------
  
  # Reach conditions (Min, Mod, Max) for each indicator
  reachConditions <- reactive({determine_reach_conditions(indicators =  indicatorData(),
                                                          definedBenchmarks = benchmarkGroupDF$df,
                                                          defaultBenchmarks = defaultBenchmarkVals(),
                                                          assignments = assignedBenchmarks())
  })
  
  #Use selectedBenchmarks to update dropdown options for plotting reach conditions
  observe({
    updateSelectInput(session, "reachCondMapSelect",
                      #choices = definedBenchmarks()$Indicator,
                      #selected = definedBenchmarks()$Indicator[1]
                      choices = benchmarkGroupDF$df$Indicator,
                      selected = benchmarkGroupDF$df$Indicator[1]
    )
  })
  
  
  # Map showing indicators colored by selected reach condition variable
  output$reachCondMap <- renderLeaflet({
    
    reachCond_leaflet_map(reachConditions(), input$reachCondMapSelect)
    
  })
  
  # Download CSV button 
  output$reachCondDLcsv <- downloadHandler(
    filename = "reachConditions.csv",
    content = function(file) {
      write.csv(st_drop_geometry(reachConditions()), file, row.names = FALSE) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )
  
  # Download ESRI FileGDB button (currently works locally but not on Shinyapps.io)
  output$reachCondDLgdb <- downloadHandler(
    filename = "reachConditions.zip",
    content = function(file) {
      make_reach_cond_GDB(reachConditions(), file)}
  )
  
  
  
  # Reach Conditions Table (mostly a placeholder for now)
  output$reachConditionTable <- renderDT({reachConditions()},)
  
# 5. Condition Summary ---------------------------------------------------------
  output$bmSummaryTable <- renderDT({
    condition_summary_table(reachConditions(), benchmarkGroupDF$df$Indicator)},
    extensions = 'Buttons',
    options = list(
      paging =FALSE,
      searching = FALSE,
      dom = 'tB',
      buttons = list( 
        list(extend = 'csv',   filename =  "benchmarkSummaryTable"),
        list(extend = 'excel', filename =  "benchmarkSummaryTable"))
    )
  )
  
  observe({
    updateSelectInput(session, "bmSummaryBoxplotsSelect",
                      choices = benchmarkGroupDF$df$Indicator,
                      selected = benchmarkGroupDF$df$Indicator[1]
    )
  })
  
  
  # output$bmSummaryBoxplots <- renderPlotly({
  #   conditions_boxplot(reachConditions(), input$bmSummaryBoxplotsSelect)
  # })
  
  output$bmSummaryBoxplots <- renderGirafe({
    conditions_boxplot(reachConditions(), input$bmSummaryBoxplotsSelect, input$showDensity)
  })
}

shinyApp(ui, server)