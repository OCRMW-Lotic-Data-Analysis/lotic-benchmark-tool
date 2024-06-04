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
        h5("Filter and Select"),
        accordion(
           accordion_panel(
             "Geographic Filters",
             icon = bsicons::bs_icon("globe-americas"),
             pickerInput(inputId = "adminState_filter",
                         label = "Admin State",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE,
                         options = pickerOptions(maxOptions = 1)),
             pickerInput(inputId = "project_filter",
                         label = "Project",
                         choices = c("test1", "test2"),
                         options = list(
                           `actions-box` = TRUE),
                         multiple = TRUE)
             ),
           accordion_panel(
             "Attribute Filters",
             icon = bsicons::bs_icon("list-nested"),
             pickerInput(inputId = "pointType_filter",
                         label = "Point Type",
                         choices = "",
                         options = list(
                           `actions-box` = TRUE),
                         multiple = TRUE),
             pickerInput(inputId = "protocol_filter",
                         label = "Protocol",
                         choices = "",
                         options = list(
                           `actions-box` = TRUE),
                         multiple = TRUE),
             dateRangeInput(inputId = 'dateRange_filter',
                            label = 'Date range',
                            start = NA, 
                            end = NA)
             )
           )
         ,
        width = "320px"
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
        #hr(),
        #actionButton("editBMGroup", label = "Edit Selected Benchmark Group"),
        
        #downloadButton(
        #  outputId = "benchmarkConfigDLcsv",
        #  label = "Download Current Benchmark Configuration"
        #),
        #fileInput("benchmarkUpload", 
        #          "Upload Benchmark Config", 
        #          accept = c(".csv"))
      ),
      # Main Panel
      layout_columns(
        card(rHandsontableOutput("defineBenchmark_hot"),
             class = "border-0 p-0"),
        card(reactableOutput("benchmarkGroupsTable"),
             class = "border-0 p-0"),
        col_widths = c(12),
        row_heights = c(2,1)
      )
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
      nav_panel(title = "Table", #DTOutput("bmSummaryTable")
                reactableOutput("bmSummaryTable")),
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
  
  # filter from all available indicators
  indicatorData_raw <- reactive({
    load_indicator_data("BLM_Natl_AIM_Lotic_Indicators_Hub.csv", "./appData/BLM_Natl_AIM_Lotic_Indicators_Hub.csv")
    })
  
  # Update indicator selectors based on indicatorData_raw().  This could come from
  # manual entry or upload.
  observe({
    updatePickerInput(session, "pointType_filter",
                      choices = na.omit(unique(indicatorData_raw()$PointSelectionType)))
    updatePickerInput(session, "protocol_filter",
                      choices = na.omit(unique(indicatorData_raw()$ProtocolType)))
  })
  
 # Indicator Filtering
  
  # Start by setting default pickerinput values
  observeEvent(
    input$startingDataType == "filter",
    {
      updatePickerInput(
        session = session,
        inputId = "adminState_filter",
        choices = c("", unique(indicatorData_raw()$BLM_AdminState)),
        selected = NULL
      )
    })
  
  observeEvent(
    input$startingDataType == "filter",
    {
      updatePickerInput(
        session = session,
        inputId = "project_filter",
        choices = unique(indicatorData_raw()$Project),
        selected = NULL
      )
    })
  
  
  observeEvent(
    input$adminState_filter, {
      if (input$adminState_filter == "None") {
        updatePickerInput(
          session = session,
          inputId = "project_filter",
          choices = unique(indicatorData_raw()$Project),
          selected = NULL
        )
      } else {
        
        updatePickerInput(
          session = session,
          inputId = "project_filter",
          choices = indicatorData_raw() %>% filter(BLM_AdminState == input$adminState_filter) %>% pull(Project) %>% unique(),
          selected = NULL
        )
      }
    }
  )
  
  
  # Logic for applying all user-selected filters
  
  indicatorData_active <- reactive({
    filtered_data <- indicatorData_raw()
    filtered_data
    
    # Admin State
    if (is.null(input$adminState_filter)){
      filtered_data
    } else if (input$adminState_filter != "") {
       filtered_data <- filtered_data %>% filter(BLM_AdminState == input$adminState_filter)
    }
    
    # Project
    if (!is.null(input$project_filter)) {
      filtered_data <- filtered_data %>% filter(Project %in% input$project_filter)
    }
    
    # Point Type (targeted or random)
    if (!is.null(input$pointType_filter)) {
      filtered_data <- filtered_data %>% filter(PointSelectionType %in% input$pointType_filter)
    }
    
    # Protocl (waderable or boatable)
    if (!is.null(input$protocol_filter)) {
      filtered_data <- filtered_data %>% filter(ProtocolType %in% input$protocol_filter)
    }
    
    # Field Eval date range
    if (all(!is.na(input$dateRange_filter))) {
      filtered_data <- filtered_data %>% filter(as.Date(FieldEvalDate) >= input$dateRange_filter[1] & as.Date(FieldEvalDate) <= input$dateRange_filter[2]) 
    }
    
    filtered_data
    })
  
  
  # Reactive value to store selected points
  selected_points <- reactiveVal(data.frame())
   
  # Map showing initial indicators loaded into app
  output$indicatorMap <- renderLeaflet({
    indicator_leaflet_map(indicatorData_active())
  })
  
  # Observe click events on the map
  observeEvent(input$indicatorMap_marker_click, {
    click <- input$indicatorMap_marker_click
    selected <- selected_points()
    
    # Check if the clicked point is already selected
    if (click$id %in% selected$EvaluationID) {
      # Remove the point from selected points if it's already selected
      
      selected <- selected %>% filter(EvaluationID != click$id)
     
    } else {
      
      # Add the point to selected points if it's not already selected
      selected <- rbind(selected, indicatorData_active() %>% filter(EvaluationID == click$id))
      print(4)
    }
    
    # Update reactive value
    selected_points(selected)
    
    # Update the map with the new selection
    leafletProxy("indicatorMap", data = indicatorData_active()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        #~longitude , ~latitude, 
        color = ~ifelse(EvaluationID %in% selected$EvaluationID, "red", indicatorPalette(PointSelectionType)),
        layerId = ~EvaluationID,
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })
  
  #Simple table - doesnt autofit data though.  Above version does but isn't perfect.
  output$indicatorTable <- renderDT({
    indicatorData_active()},)
  
  
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
  
  # SAVE edited benchmark table.  Also resets text/picker inputs and clears table.
  observeEvent(input$saveNewBMGroup,{
    req(input$bmGroupNameinput)
    # Get new benchmark group data into data frame
    newGroupData <- definedBenchmarks() %>% tibble::add_column(bmGroup = input$bmGroupNameinput, .before = 1)
    
    # Merge previously saved groups with newly entered group (long form)
    benchmarkGroupDF$df <- bind_rows(benchmarkGroupDF$df, newGroupData)

    
    # Reset all inputs to blank to prepare for next benchmark group
    updateTextInput(session, "bmGroupNameinput", value = "")
    updatePickerInput(session,"selectBenchmarks3", selected = character(0))
    updatePickerInput(session,"selectBenchmarks2", selected = character(0))
    #print(names(benchmarkGroupDF))
  })
  
  # DELETE the benchmark group based on the selected row in table.
  observeEvent(input$deleteBMGroup,{
    
    selected <- reactive(getReactableState("benchmarkGroupsTable", "selected"))
    
    if (!is.null(selected)) {
      # Get groupName(s) you want to delete into vector of strings
      groupNames <- benchmarkGroupDF$df %>% slice(selected()) %>% pull(bmGroup)

  
      # Actually remove the data from benchmarkGroupDF reactive value
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
  # output$benchmarkGroupsTable <- renderDataTable(
  #   benchmarkGroupDF$df,
  #   selection = "single"
  # )
  output$benchmarkGroupsTable <- renderReactable({
    req(benchmarkGroupDF$df)
    reactable(benchmarkGroupDF$df,
              pagination = FALSE,
              groupBy = "bmGroup",
              selection = "multiple",
              onClick = "select",
              defaultColDef = colDef(minWidth = 220))
  })
  
# 3. Apply Benchmarks --------------------------------------------------------
  
  # Selected which benchmark groups to apply to each pointID/indicator combo.
  output$applyBenchmarks_hot <- renderRHandsontable({
    req(benchmarkGroupDF$df)
    apply_benchmarks_table(defaultBenchmarkVals(), benchmarkGroupDF, indicatorData_active())
  })
  
  assignedBenchmarks <- reactive({
    hot_to_r(input$applyBenchmarks_hot)
  })
  
  #output$groupnames <- renderText({ bmGroups })
  #output$benchmarkGroupTEST <- renderRHandsontable({
  #    rhandsontable(data = benchmarkGroupDF$df)
  #})
  
# 4. Reach Conditions --------------------------------------------------------
  
  # Calculate Reach conditions (Min, Mod, Max) for each indicator
  reachConditions <- reactive({determine_reach_conditions(indicators =  indicatorData_active(),
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
  # output$bmSummaryTable <- renderDT({
  #   condition_summary_table(reachConditions(), benchmarkGroupDF$df$Indicator)},
  #   extensions = 'Buttons',
  #   options = list(
  #     paging =FALSE,
  #     searching = FALSE,
  #     dom = 'tB',
  #     buttons = list( 
  #       list(extend = 'csv',   filename =  "benchmarkSummaryTable"),
  #       list(extend = 'excel', filename =  "benchmarkSummaryTable"))
  #   )
  # )
  
  output$bmSummaryTable <- renderReactable({
    bmSummary <- condition_summary_table(reachConditions(), benchmarkGroupDF$df$Indicator)
    reactable(bmSummary,
              fullWidth = FALSE,
              pagination = FALSE, 
              showPageInfo = FALSE,
              highlight = TRUE,
              rowStyle = list(cursor = "pointer"),
              columns = list(
                Indicator = colDef(width = 220),
                Minimal = colDef(na = "–", align = "center"),
                Moderate = colDef(na = "–", align = "center"),
                Major = colDef(na = "–", align = "center"),
                Min = colDef(na = "–", align = "center"),
                Max = colDef(na = "–", align = "center"),
                Mean = colDef(na = "–", align = "center")
              ),
              columnGroups = list(
                colGroup(name = "Number of Reaches", columns = c("Minimal", "Moderate", "Major")),
                colGroup(name = "Indicator Summary Statistics", columns = c("Min", "Max", "Mean"))
              ))
  })
  
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