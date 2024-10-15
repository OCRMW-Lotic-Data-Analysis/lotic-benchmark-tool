library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(bslib)
library(ggiraph)
library(DT)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyWidgets)
library(rhandsontable)
library(reactable)
library(htmltools)
library(zip)

### UI -------------------------------------------------------------------------
ui <- page_navbar(
  title = "Lotic AIM Custom Benchmark Tool",
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
                            start = as.Date("2013-06-01"), 
                            end = NULL),
             checkboxInput("onlyRecentVisits", 
                           label = "Only include most recent visit", 
                           value = FALSE),
             )
           ),
        actionButton("clearSelection", label = "Clear Selection")
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
                    choices = "",    # choices are updated based on input of indicatorList()
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        pickerInput(inputId = "selectBenchmarks2",
                    label = "Two Condition Categories",
                    choices = "",    # choices are updated based on input of indicatorList()
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        br(),
        
        actionButton("saveNewBMGroup", 
                     label = shiny::tagList(bsicons::bs_icon("floppy"),"Save New Group"), 
                     style="color: #000000; background-color: #DEFFDE"),
        actionButton("deleteBMGroup", 
                     label = shiny::tagList(bsicons::bs_icon("trash"),"Delete Selected Group"), 
                     style="color: #000000; background-color: #FFDEDE"),
        hr(),
        downloadButton(outputId = "exportBMGroupsCSV", 
                       label = "Export Saved Groups"),
        fileInput("uploadBMGroup", 
                     label = "Upload Benchmark Groups"),
        actionButton("loadSampleBMGroup", label = shiny::tagList(
          bsicons::bs_icon("arrow-counterclockwise"),
          "Load Sample Benchmark Group")),
        #hr(),
        #actionButton("editBMGroup", label = "Edit Selected Benchmark Group"),
      ),
      # Main Panel
      layout_columns(
        card(rHandsontableOutput("defineBenchmark_hot"),
             class = "border-0 p-0"),
        navset_card_tab(
          nav_panel("Default Conditions",
                    card(reactableOutput("defaultConditionsTable"), class = "border-0 p-0")),
          nav_panel("Benchmark Groups",
                    card(reactableOutput("benchmarkGroupsTable"), class = "border-0 p-0"))
          )
        ,
        col_widths = c(12),
        row_heights = c(1,1)
      )
    )
  ),
  
  # 3. Apply Benchmarks ----
  nav_panel(
    title = "3 Apply Benchmarks",
    page_sidebar(
      #Sidebar
      sidebar = sidebar(
        open = FALSE,
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
        hr(),
        p("Download 'Full Output Table'"),
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
        nav_panel("Full Output Table",
                  DTOutput(outputId = "reachConditionTable",height = "auto", fill = TRUE)),
        nav_panel("Review Applied Benchmarks",
                  reactableOutput(outputId = "reviewAppliedBenchmarks"))
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
                  card(max_height = 500,
                       girafeOutput("bmSummaryBoxplots"))
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
  # Indicator Filtering
  
  # Update indicator selectors based on indicatorData_raw().  This could come from
  # manual entry or upload.
  observe({
    updatePickerInput(
      session = session, 
      inputId = "pointType_filter",
      choices = na.omit(unique(indicatorData_raw()$PointSelectionType)))
    
    updatePickerInput(
      session = session, 
      inputId = "protocol_filter",
      choices = na.omit(unique(indicatorData_raw()$ProtocolType)))
    
    updatePickerInput(
      session = session,
      inputId = "adminState_filter",
      choices = c("", unique(indicatorData_raw()$BLM_AdminState) %>% sort()), # "" to force blank entry in dropdown
      selected = NULL)
    
    updatePickerInput(
      session = session,
      inputId = "project_filter",
      choices = unique(indicatorData_raw()$Project) %>% sort(),
      selected = NULL)
    })
  
  # If an admin state is selected, only show projects within that state.
  observeEvent(
    input$adminState_filter, {
      if (input$adminState_filter == "") {
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
          choices = indicatorData_raw() %>% filter(BLM_AdminState == input$adminState_filter) %>% pull(Project) %>% unique() %>% sort(),
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
    
    # Include only most recent visits  onlyRecentVisits
    if (input$onlyRecentVisits == TRUE){
      filtered_data <- filtered_data %>% group_by(PointID) %>% filter(FieldEvalDate == max(as.Date(FieldEvalDate))) %>% ungroup()
    }
    
    filtered_data
    })
  
  # Clear selection button
  observeEvent(input$clearSelection, {
    selected_points(NULL)
    leafletProxy("indicatorMap") %>%
      clearGroup("selectedPts")
  })
  
  # Reactive value to store selected points
  selected_points <- reactiveVal()
  
  ### Leaflet map initialization and proxies
  
  # Initialized leaflet map.  No data added yet.
  output$indicatorMap <- renderLeaflet({
    indicator_leaflet_map()
  })
  
  # Add filtered data to map (i.e indicatorData_active() data)
  observeEvent(indicatorData_active(), {
    indicator_leaflet_activeData_proxy(mapId = "indicatorMap", data = indicatorData_active())
  })
  
  # When indicatorData_active() changes via filters, selected points are preserved on map
  observeEvent(indicatorData_active(), {
    if (!is.null(selected_points())) {
      indicator_leaflet_selection_proxy(mapId = "indicatorMap", data = selected_points())
    }
  })
  
  # INDIVIDUAL POINT SELECTION - When point is clicked, select/unselect it
  observeEvent(input$indicatorMap_marker_click, {
    # Retrieve click ID.  From an unselected point, this will return the 'PointID'.
    # From a selected point, this will return the 'selectionID'. Two different IDs are needed because
    # if two points share the same 'layerId' in leaflet the second one will just replace the original.
    # Here, selected points are drawn "on top" of 'allPts'.
    clickID <- input$indicatorMap_marker_click$id
    clickGroup <- input$indicatorMap_marker_click$group

    selected <- selected_points()
    
    # Logic to change selection status.  All original points are in the "allPts" 'group'.
    # All selected points are in the "selectedPts" 'group'.  Groups are defined in the leaflet and
    # leaflet proxy 'addCircleMarkers' function.

    # Get PointID from point.
    if (clickGroup == "allPts") {  # allPts = unselected
      clickPtID <- str_split_i(clickID, "_", i = 1) 
    } else if (clickGroup == "selectedPts") {
      clickPtID <- selected %>% filter(selectionID == clickID) %>% pull(PointID)
    }
    
    # Check if the point is already selected.  If selected, unselect it. If not selected, select it.
    if (clickPtID %in% selected$PointID) {
      # Remove the point from selected_points
      new_selection <- selected %>% filter(PointID != clickPtID)
    } else {
      # Add the point to selected_points
      new_selection <- bind_rows(selected, indicatorData_active() %>% filter(PointID == clickPtID))
    }
    
    # Make new selectionID to act as layerId for selected points.
    new_selection$selectionID <- seq_len(nrow(new_selection))
    
    # Save the updated selection
    selected_points(new_selection) 
    
    # Update map with new selection
    indicator_leaflet_selection_proxy(mapId = "indicatorMap", data = selected_points())
  })
  
  # POLYON SELECTION - Select all points within a drawn polygon
  observeEvent(input$indicatorMap_draw_new_feature, {
    selected <- selected_points()
    
    # Extract polygon feature 
    feature <- input$indicatorMap_draw_new_feature
    coords <- feature$geometry$coordinates[[1]]
    selectionPoly <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))
    
    # Find points within the polygon
    pointsInPoly <- st_filter(indicatorData_active(), selectionPoly)
    
    # Identify only the new points.  Do not include points already selected.
    new_points <- pointsInPoly %>% filter(!(EvaluationID %in% selected$EvaluationID))
    
    # Combined previous and new selection
    new_selection <- bind_rows(selected, indicatorData_active() %>% filter(EvaluationID %in% new_points$EvaluationID))
    
    # Make new selectionID to act as layerId for selected points.
    new_selection$selectionID <- seq_len(nrow(new_selection))
    
    # Save the updated selection
    selected_points(new_selection)
    
    # Update map with new selection
    indicator_leaflet_selection_proxy(mapId = "indicatorMap", data = selected_points())  # Update the map to reflect the color change
    
    # # Optional workaround to immediately remove the drawn feature once completed.  Toolbar may quickly flash.
    # leafletProxy(indicatorMap) %>%
    #   removeDrawToolbar(clearFeatures = TRUE) %>%
    #   addDrawToolbar(
    #     polylineOptions = FALSE,
    #     polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = 'red')),
    #     rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = 'red')),
    #     circleOptions = FALSE,
    #     markerOptions = FALSE,
    #     editOptions = editToolbarOptions(edit = FALSE)
    #   )
    
  })
  
  # Simple table showing selected indicators - doesnt autofit data though.
  output$indicatorTable <- renderDT({
    selected_points()},)
  
  
# 2. Define Benchmarks -------------------------------------------------------
  
  # Load default benchmarks
  #indicatorList <-reactiveVal(read.csv("./appData/default_benchmark_and_operators.csv", colClasses = "character"))
  indicatorList <- reactiveVal(read.csv("./appData/custom_indicator_list.csv", colClasses = "character") %>% arrange(Indicator))
  
  # Update benchmark selectors based on indicatorList().
  observe({
    updatePickerInput(session, "selectBenchmarks3",
                      choices = filter(indicatorList(), ConditionCategoryNum == 3) %>% pull(Indicator))
    
    updatePickerInput(session, "selectBenchmarks2",
                      choices = filter(indicatorList(), ConditionCategoryNum == 2) %>% pull(Indicator))
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
        choices = setdiff(filter(indicatorList(), ConditionCategoryNum == 2) %>% pull(Indicator), 
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
        choices = setdiff(filter(indicatorList(), ConditionCategoryNum == 3) %>% pull(Indicator), 
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
    cond2 <- dplyr::filter(indicatorList(), Indicator %in% input$selectBenchmarks3 & ConditionCategoryNum == 3)
    cond3 <- dplyr::filter(indicatorList(), Indicator %in% input$selectBenchmarks2 & ConditionCategoryNum == 2)
    
    # Merge above filtered data.  Since this is no longer the indicatorList, remove column with "Default" group.
    # Column removed as opposed to just removing values bc column is added later.
    dat <- bind_rows(cond2, cond3) %>% arrange(Indicator) 
    
    rhandsontable(data = dat) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_col(1, width = 200)
    
  })
  
  # Save the currently displayed values of the 'defineBenchmark_hot' table
  definedBenchmarks <- reactive({
    req(input$bmGroupNameinput)
    hot_to_r(input$defineBenchmark_hot)
  })
  
  # Initialize empty container for saved benchmark groups
  benchmarkGroupDF <- reactiveValues()
  
  # Load sample benchmark group
  observeEvent(input$loadSampleBMGroup,{
    newGroupData <- read_csv("./appData/sample_benchmark_group.csv", col_types = cols(.default = col_character()), show_col_types = FALSE) %>% 
      tibble::add_column(BenchmarkGroup = "example", .before = 1)
    # Merge previously saved groups with newly entered group (long form)
    benchmarkGroupDF$df <- bind_rows(benchmarkGroupDF$df, newGroupData)
  })
  
  
  
  # SAVE edited benchmark table.  Also resets text/picker inputs and clears table.
  observeEvent(input$saveNewBMGroup,{
    req(!is.null(input$bmGroupNameinput) & !is.null(input$defineBenchmark_hot))
    # Get new benchmark group data into data frame
    newGroupData <- definedBenchmarks() %>% tibble::add_column(BenchmarkGroup = input$bmGroupNameinput, .before = 1)
    
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
      groupNames <- benchmarkGroupDF$df %>% slice(selected()) %>% pull(BenchmarkGroup)
  
      # Actually remove the data from benchmarkGroupDF reactive value
      benchmarkGroupDF$df <- benchmarkGroupDF$df %>% subset(!(BenchmarkGroup %in% groupNames))
    }
  })
  
  # EXPORT all currently saved benchmark groups
  output$exportBMGroupsCSV <- downloadHandler(
    filename = "savedBenchmarkGroups.csv",
    content = function(file) {
      write.csv(st_drop_geometry(benchmarkGroupDF$df), file, row.names = FALSE) # need st_drop_geometry or it splits geom into two columns that overwrite data.
      #write.csv(st_drop_geometry(reachConditionsLong()), file, row.names = FALSE) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )
  
  # UPLOAD benchmarks groups(s)
  observeEvent(input$uploadBMGroup,{
    ext <- tools::file_ext(input$uploadBMGroup$name)
    newGroupData <- switch(ext,
                   csv = vroom::vroom(input$uploadBMGroup$datapath, delim = ",", col_types = cols(.default = "c"), show_col_types = FALSE),
                   validate("Invalid file; Please upload a .csv")
    )
    benchmarkGroupDF$df <- bind_rows(benchmarkGroupDF$df, newGroupData)
  })
  # # EDIT previously saved benchmark group. 
  # # almost works. opens table to edit but doesnt allow for saving and selectInputs are not correct. 
  #  observeEvent(input$editBMGroup,{
  #    selected <- reactive(getReactableState("benchmarkGroupsTable", "selected"))
  #    groupNames <- benchmarkGroupDF$df %>% slice(selected()) %>% pull(BenchmarkGroup)
  #    #groupNames <- benchmarkGroupDF$df %>% slice(input$benchmarkGroupsTable_rows_selected) %>% pull(BenchmarkGroup)
  #    bmedit <- benchmarkGroupDF$df %>% subset(BenchmarkGroup %in% groupNames)
  #    print(groupNames)
  #    print(bmedit)
  #    #indicatorList(bmedit)
  #    output$defineBenchmark_hot <- bmedit
  #    updateSelectInput(session, "selectBenchmarks3",
  #                      choices = filter(indicatorList(), ConditionCategoryNum == 3) %>% pull(Indicator),
  #                      selected = filter(indicatorList(), ConditionCategoryNum == 3) %>% pull(Indicator))
  #    
  #    updateSelectInput(session, "selectBenchmarks2",
  #                      choices = filter(indicatorList(), ConditionCategoryNum == 2) %>% pull(Indicator),
  #                      selected = filter(indicatorList(), ConditionCategoryNum == 2) %>% pull(Indicator))
  #    
  #    #print(indicatorList()) 
  #  })

  
  
   #Summary table of defined benchmark groups.  
   output$benchmarkGroupsTable <- renderDataTable(
     benchmarkGroupDF$df,
     selection = "single"
   )

  
  
  
  
  output$benchmarkGroupsTable <- renderReactable({
    req(nrow(benchmarkGroupDF$df) > 0)
    saved_benchmark_groups_table(benchmarkGroupDF$df)
  })
  
  output$defaultConditionsTable <- renderReactable({
    req(selected_points())
    defCond <- defaultConditions %>% dplyr::select(
        PointID, EvaluationID, Indicator, Value, BenchmarkGroup, ModerateBenchmark1,
        MajorBenchmark1, ModerateBenchmark2, MajorBenchmark2 ,Condition, BLM_AdminState
        ) %>%
      filter(EvaluationID %in% selected_points()$EvaluationID) %>%
      arrange(EvaluationID) %>%
      relocate(Condition, .after = Value)
    default_conditions_table(defCond)
  })
  
# 3. Apply Benchmarks --------------------------------------------------------
  
  # Selected which benchmark groups to apply to each pointID/indicator combo.
  output$applyBenchmarks_hot <- renderRHandsontable({
    req(benchmarkGroupDF$df)
    apply_benchmarks_table(benchmarkGroupDF, selected_points())
  })
  
  assignedBenchmarks <- reactive({
    hot_to_r(input$applyBenchmarks_hot)
  })
  
# 4. Reach Conditions --------------------------------------------------------
  
  # Calculate Reach conditions (Min, Mod, Maj) for each indicator
  reachConditionsdf <- reactive({
    req(selected_points())
    req(benchmarkGroupDF$df)
    req(assignedBenchmarks())
    determine_reach_conditions(indicators =  selected_points(),
                                                          definedBenchmarks = benchmarkGroupDF$df,
                                                          assignments = assignedBenchmarks())
    
  })

  reachConditionsWide <- reactive({reachConditionsdf()[['reachConditionsWide']]})
  reachConditionsLong <- reactive({reachConditionsdf()[['reachConditionsLong']]})
  
  #Use selectedBenchmarks to update dropdown options for plotting reach conditions
  observe({
    updateSelectInput(session, "reachCondMapSelect",
                      choices = benchmarkGroupDF$df$Indicator,
                      selected = benchmarkGroupDF$df$Indicator[1]
                      )
    })
  
  # MAP showing indicators colored by selected reach condition variable
  output$reachCondMap <- renderLeaflet({
    req(reachConditionsWide(), input$reachCondMapSelect)
    reachCond_leaflet_map(reachConditionsWide(), input$reachCondMapSelect)
    })
  
  # Download CSV button 
  output$reachCondDLcsv <- downloadHandler(
    filename = "reachConditions.csv",
    content = function(file) {
      write.csv(st_drop_geometry(reachConditionsWide()), file, row.names = FALSE) # need st_drop_geometry or it splits geom into two columns that overwrite data.
      #write.csv(st_drop_geometry(reachConditionsLong()), file, row.names = FALSE) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )
  
  # Download ESRI FileGDB button (currently works locally but not on Shinyapps.io)
  output$reachCondDLgdb <- downloadHandler(
    filename = "reachConditions.zip",
    content = function(file) {
      make_reach_cond_GDB(reachConditionsWide(), file)}
    )
  
  # TABLE - 'Full Ouput Table' 
  #output$reachConditionTable <- renderDT({reachConditionsWide()},)
  output$reachConditionTable <- renderDT({
    req(reachConditionsWide())
    reachConditionsWide()
    })
  
  # TABLE - review applied benchmarks
  output$reviewAppliedBenchmarks <- renderReactable({
    req(selected_points())
    review_applied_benchmarks_table(reachConditionsLong())
    })
  
# 5. Condition Summary ---------------------------------------------------------
  output$bmSummaryTable <- renderReactable({
    # Calculate summary data
    #bmSummary <- condition_summary_df(reachConditionsWide(), benchmarkGroupDF$df$Indicator)
    bmSummary <- condition_summary_df(reachConditionsWide(), unique(reachConditionsLong()$Indicator))

    # Render summary table
    condition_summary_table(bmSummary)
  })
  
  observe({
    updateSelectInput(session, "bmSummaryBoxplotsSelect",
                      choices = benchmarkGroupDF$df$Indicator,
                      selected = benchmarkGroupDF$df$Indicator[1]
    )
  })
  
  
  # output$bmSummaryBoxplots <- renderPlotly({
  #   conditions_boxplot(reachConditionsWide(), input$bmSummaryBoxplotsSelect)
  # })
  
  output$bmSummaryBoxplots <- renderGirafe({
    conditions_boxplot(reachConditionsWide(), input$bmSummaryBoxplotsSelect, input$showDensity)
  })
}

shinyApp(ui, server)