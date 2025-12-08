library(shiny)
library(readr)
library(ggplot2)
library(patchwork)
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
library(scales)
library(shinyjs)
library(ggdist)

### UI -------------------------------------------------------------------------
ui <- page_navbar(
  title = "Lotic AIM Custom Benchmark Tool",
  selected = "1. Select Site Visits",
  theme = bslib::bs_theme(font_scale = 0.9, bootswatch = "yeti"),
  navbar_options = navbar_options(collapsible = TRUE),

  # 1. Select Site Visits ----
  nav_panel(
    title = "1. Select Site Visits",
    page_sidebar(
      sidebar = sidebar(
        #p("Select site visits for analysis by filtering.  For more specific selection, click on individual site visits or use the polygon drawing tools"),
        value_box(
          title = span(
            "Selected Site Visits",
            tooltip(
              bsicons::bs_icon("question-circle", fill = "#f29305"),
              "Select site visits for analysis by filtering and/or manual selection.  For manual selection, click on individual site visits or use the polygon drawing tools.",
              placement = "right"
            )
          ),
          value = uiOutput("siteVisitSelectionCount"),
          showcase = NULL,
          full_screen = FALSE,
          fill = FALSE,
          height = 100L
        ),
        filter_UI("filters", indicatorData_raw),
        actionButton(inputId = "clear_selection", label = "Clear Selection"),

        width = "320px"
      ),
      navset_card_tab(
        nav_panel(
          title = "Map",
          map_UI("map"),
          #top = 80, right = 300
        ),
        nav_panel(
          title = "Table",
          DTOutput(outputId = "indicatorTable", height = "auto", fill = TRUE)
        )
      )
    )
  ),

  # 2. Define Benchmarks ----
  nav_panel(
    shinyjs::useShinyjs(),
    title = "2. Define Benchmarks",
    page_sidebar(
      #Sidebar
      sidebar = sidebar(
        width = "300px",
        h5("Create Benchmark Groups"),
        p("Define individual benchmarks and assign a group name."),
        actionButton(
          "addNewBM",
          label = shiny::tagList(
            bsicons::bs_icon("box-arrow-in-right"),
            "Add New Benchmark"
          ),
          style = "color: #000000; background-color: #deefff"
        ),
        textInput("bmGroupNameinput", label = "Group Name"),
        br(),
        actionButton(
          "saveNewBenchmarkGroup",
          label = shiny::tagList(bsicons::bs_icon("floppy"), "Save New Group"),
          style = "color: #000000; background-color: #DEFFDE"
        ),
        actionButton(
          "deleteBMGroup",
          label = shiny::tagList(
            bsicons::bs_icon("trash"),
            "Delete Selected Group"
          ),
          style = "color: #000000; background-color: #FFDEDE"
        ),
        hr(),
        downloadButton(
          outputId = "exportBMGroupsCSV",
          label = "Export Saved Groups"
        ),
        fileInput("uploadBMGroup", label = "Upload Benchmark Groups"),
        actionButton(
          "loadSampleBMGroup",
          label = shiny::tagList(
            bsicons::bs_icon("arrow-counterclockwise"),
            "Load Sample Benchmark Group"
          )
        ),
        #hr(),
        #actionButton("editBMGroup", label = "Edit Selected Benchmark Group"),
      ),
      # Main Panel
      card(
        rHandsontableOutput("workingBenchmarks_hot"),
        class = "border-0 p-0",
        fill = FALSE,
        height = "50%"
      ),
      navset_card_tab(
        nav_panel(
          "Default Conditions",
          card(
            reactableOutput("defaultConditionsTable"),
            class = "border-0 p-0"
          )
        ),
        nav_panel(
          "Benchmark Groups",
          card(reactableOutput("benchmarkGroupsTable"), class = "border-0 p-0")
        )
      )
    )
  ),

  # 3. Apply Benchmarks ----
  nav_panel(
    title = "3. Apply Benchmarks",
    # fluidRow() breaks the scroll bars.
    card(
      rHandsontableOutput("applyBenchmarks_hot"),
      height = "400px",
      class = "border-0 p-0"
    ),
    card(
      leafletOutput(outputId = "applyBenchmarksMap", height = "350px"),
      class = "border-0 p-0"
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
          outputId = "reachCondDLgpkg",
          label = "Download GeoPackage"
        ),
        width = "300px"
      ),
      navset_card_tab(
        nav_panel("Map", leafletOutput(outputId = "reachCondMap")),
        nav_panel(
          "Full Output Table",
          DTOutput(
            outputId = "reachConditionTable",
            height = "auto",
            fill = TRUE
          )
        ),
        nav_panel(
          "Review Applied Benchmarks",
          reactableOutput(outputId = "reviewAppliedBenchmarks")
        )
      )
    ),
  ),

  # 5. Summary ----
  nav_panel(
    title = "5. Summary",
    navset_card_tab(
      nav_panel(
        title = "Table",
        fluidPage(
          downloadLink(
            outputId = "downloadSummaryTable",
            label = tagList(
              icon("download"),
              "Download as CSV"
            ),
            # Use Bootstrap classes to make it look like a compact button
            class = "btn btn-primary btn-sm"
          ),
          reactableOutput("bmSummaryTable")
        )
      ),
      nav_panel(
        title = "Box Plots",
        layout_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "bmSummaryBoxplotsSelect",
              label = "Select Indicator to Plot",
              choices = NULL
            ),
            checkboxInput(
              inputId = "showDensity",
              label = "Include Density Plot",
              value = TRUE
            ),
            downloadLink(
              outputId = "downloadBoxplot",
              label = tagList(
                icon("download"),
                "Download as PNG"
              ),
              # Use Bootstrap classes to make it look like a compact button
              class = "btn btn-primary btn-sm"
            ),
            width = "300px",
            open = "always"
          ),
          layout_column_wrap(
            width = 1 / 2,
            card(
              min_height = 500,
              class = "border-0 p-0",

              #downloadButton("downloadBoxplot", "Download High-Res Plot"),
              girafeOutput("bmSummaryBoxplots")
            )
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_panel(
    title = tagList(
      "Help", 
      bsicons::bs_icon("question-circle")
    ),
    includeMarkdown("appData/help.md")
  )
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {
  # 1. Select Indicators ---------------------------------------------------------
  # Indicator Filtering
  # Module to filter by state, field office, etc.
  filteredData <- filter_server("filters", indicatorData_raw)

  # Clear selection button
  observeEvent(input$clearSelection, {
    selected_points(NULL)
    leafletProxy("indicatorMap") %>%
      clearGroup("selectedPts")
  })

  # Clear out "selected" points.  This value sent to map_server
  clearReactive <- reactiveVal()
  observe({
    clearReactive(input$clear_selection)
  })

  # Initialize map module.  Returns manually selected points if selected, else filtered points
  selected_points <- map_server(
    "map",
    data = filteredData,
    original_data = indicatorData_raw,
    clear_selection = clearReactive
  )

  # Simple table showing selected indicators - doesnt autofit data though.
  output$indicatorTable <- renderDT(
    {
      selected_points()
    },
  )

  output$siteVisitSelectionCount <- renderText({
    paste(nrow(selected_points()))
  })

  # 2. Define Benchmarks ---------------------------------------------------------

  ## 2.1 Define Single New Benchmark -------------------------------------------

  # Empty reactiveVal to capture/store single new custom benchmark (defineBenchmarkMod_server output) below
  newSingleBenchmark <- reactiveVal()

  # Modal on click.  Benchmark entry GUI
  observeEvent(input$addNewBM, {
    showModal(
      modalDialog(
        defineBenchmarkMod_UI(id = "defBM"),
        footer = tagList(
          actionButton("saveNewBM", "Save & Close"),
          modalButton("Cancel")
        )
      )
    )

    # Run the module server and capture its return
    moduleDataOutput <- defineBenchmarkMod_server(
      id = "defBM",
      metadata = indicatorMetadata,
      blankForm = blankCustomBMForm
    )

    # Capture output from defineBenchmarkMod module in reactiveVal.  This is what will be saved when user hits "save" in the modal
    observe({
      newSingleBenchmark(moduleDataOutput())
      # Check if benchmark values and inequality operators are logical.  Had to nest if statements due to NULL values.
      if (!is_empty(newSingleBenchmark())) {
        # needed because, for example, `nrow(NULL) > 1` returns logical(0).
        if (nrow(newSingleBenchmark()) == 1) {
          # Perform logical check of values.  'Save & Close' button only works if inputs are valid.
          if (saveValidator(newSingleBenchmark())) {
            shinyjs::enable("saveNewBM")
          } else {
            shinyjs::disable("saveNewBM")
          }
        }
      } #end logic check
    })
  }) # end modal on click

  ## 2.2 Merge new and existing single benchmarks ------------------------------

  # New benchmark group to store all newly defined individual benchmarks
  newBenchmarkGroup <- reactiveVal(blankCustomBMForm)

  # Save button for single benchmark.  Close modal and add the newly created newSingleBenchmark() data to table of all custom benchmarks
  observeEvent(input$saveNewBM, {
    newVals <- bind_rows(newBenchmarkGroup(), newSingleBenchmark()) %>%
      arrange(Indicator)
    newBenchmarkGroup(newVals)
    removeModal()
  })

  # Save the currently displayed values of the 'workingBenchmarks_hot' table
  workingBenchmarks_dat <- reactive({
    req(input$bmGroupNameinput)
    hot_to_r(input$workingBenchmarks_hot)
  })

  ## 2.x Manage benchmark groups  ----------------------------------------------

  # Initialize empty container for all saved benchmark groups
  allBenchmarkGroups <- reactiveValues()

  # Load sample benchmark group
  observeEvent(input$loadSampleBMGroup, {
    newGroupData <- read_csv(
      "./appData/sample_benchmark_group.csv",
      col_types = readr::cols(
        .default = "c",
        ModerateBenchmark1 = "n",
        MajorBenchmark1 = "n",
        ModerateBenchmark2 = "n",
        MajorBenchmark2 = "n",
        ConditionCategoryNum = "n"
      ),
      show_col_types = FALSE
    ) %>%
      tibble::add_column(BenchmarkGroup = "example", .before = 1)
    # Merge previously saved groups with newly entered group (long form)
    allBenchmarkGroups$df <- bind_rows(allBenchmarkGroups$df, newGroupData)
  })

  # SAVE edited benchmark table.  Also resets text/picker inputs and clears table.
  observeEvent(input$saveNewBenchmarkGroup, {
    req(
      !is.null(input$bmGroupNameinput) & !is.null(input$workingBenchmarks_hot)
    )
    # Get new benchmark group data into data frame
    newGroupData <- workingBenchmarks_dat() %>%
      tibble::add_column(BenchmarkGroup = input$bmGroupNameinput, .before = 1)

    # Merge previously saved groups with newly entered group (long form)
    allBenchmarkGroups$df <- bind_rows(allBenchmarkGroups$df, newGroupData)

    # Reset all inputs to blank to prepare for next benchmark group
    updateTextInput(session, "bmGroupNameinput", value = "")

    # Clear new benchmark group so it's not rendered in 'workingBenchmarks_hot'
    newBenchmarkGroup(blankCustomBMForm)

    #print(names(allBenchmarkGroups))
  })

  # DELETE the benchmark group based on the selected row in table.
  observeEvent(input$deleteBMGroup, {
    selected <- reactive(getReactableState("benchmarkGroupsTable", "selected"))

    if (!is.null(selected)) {
      # Get groupName(s) you want to delete into vector of strings
      groupNames <- allBenchmarkGroups$df %>%
        slice(selected()) %>%
        pull(BenchmarkGroup)

      # Actually remove the data from allBenchmarkGroups reactive value
      allBenchmarkGroups$df <- allBenchmarkGroups$df %>%
        subset(!(BenchmarkGroup %in% groupNames))
    }
  })

  # EXPORT all currently saved benchmark groups
  output$exportBMGroupsCSV <- downloadHandler(
    filename = "savedBenchmarkGroups.csv",
    content = function(file) {
      write.csv(
        st_drop_geometry(allBenchmarkGroups$df),
        file,
        row.names = FALSE
      ) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )

  # UPLOAD benchmarks groups(s)
  observeEvent(input$uploadBMGroup, {
    ext <- tools::file_ext(input$uploadBMGroup$name)
    newGroupData <- switch(
      ext,
      csv = vroom::vroom(
        input$uploadBMGroup$datapath,
        delim = ",",
        col_types = readr::cols(
          .default = "c",
          ModerateBenchmark1 = "n",
          MajorBenchmark1 = "n",
          ModerateBenchmark2 = "n",
          MajorBenchmark2 = "n",
          ConditionCategoryNum = "n"
        ),
        show_col_types = FALSE
      ),
      validate("Invalid file; Please upload a .csv")
    )
    allBenchmarkGroups$df <- bind_rows(allBenchmarkGroups$df, newGroupData)
  })

  # # EDIT previously saved benchmark group.
  # # almost works. opens table to edit but doesnt allow for saving and selectInputs are not correct.
  # observeEvent(input$editBMGroup,{
  #   selected <- reactive(getReactableState("benchmarkGroupsTable", "selected"))
  #   if (!is.null(selected)) {
  #     # Get groupName(s) you want to delete into vector of strings
  #     groupNames <- allBenchmarkGroups$df %>% slice(selected()) %>% pull(BenchmarkGroup)
  #
  #   }
  # })

  ## 2.x Tables ----------------------------------------------------------------
  # Table showing all indicator benchmarks for the current benchmark group
  output$workingBenchmarks_hot <- renderRHandsontable({
    req(nrow(newBenchmarkGroup()) > 0)
    rhandsontable(data = newBenchmarkGroup()) %>%
      hot_context_menu(allowColEdit = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_col(1, width = 200)
  })

  # Table of all currently saved benchmark groups.
  output$benchmarkGroupsTable <- renderReactable({
    req(nrow(allBenchmarkGroups$df) > 0)
    saved_benchmark_groups_table(allBenchmarkGroups$df)
  })

  # Table of default conditions for selected points
  output$defaultConditionsTable <- renderReactable({
    req(selected_points())
    defCond <- defaultConditions %>%
      dplyr::select(
        PointID,
        EvaluationID,
        Indicator,
        Value,
        BenchmarkGroup,
        ModerateBenchmark1,
        MajorBenchmark1,
        ModerateBenchmark2,
        MajorBenchmark2,
        Condition,
        BLM_AdminState
      ) %>%
      filter(EvaluationID %in% selected_points()$EvaluationID) %>%
      arrange(EvaluationID) %>%
      relocate(Condition, .after = Value)
    default_conditions_table(defCond)
  })

  # 3. Apply Benchmarks --------------------------------------------------------

  # Selected which benchmark groups to apply to each pointID/indicator combo.
  output$applyBenchmarks_hot <- renderRHandsontable({
    req(isTruthy(allBenchmarkGroups$df) & isTruthy(selected_points()))
    apply_benchmarks_table(allBenchmarkGroups, selected_points())
  })

  # Map and proxy
  output$applyBenchmarksMap <- renderLeaflet({
    req(selected_points())
    applyBenchmark_leaflet(data = selected_points())
  })

  observeEvent(input$applyBenchmarks_hot_select, {
    selectedEvalID <- assignedBenchmarks()[
      input$applyBenchmarks_hot_select$select$r,
      "EvaluationID"
    ]
    selectedPtInApplyBMTable <- selected_points() %>%
      filter(EvaluationID == selectedEvalID)

    applyBenchmark_leaflet_proxy("applyBenchmarksMap", selectedPtInApplyBMTable)
  })

  # Track current state of applyBenchmarks_hot table
  assignedBenchmarks <- reactive({
    hot_to_r(input$applyBenchmarks_hot)
  })

  # 4. Reach Conditions --------------------------------------------------------

  # Calculate Reach conditions (Min, Mod, Maj) for each indicator
  reachConditionsdf <- reactive({
    req(selected_points())
    req(allBenchmarkGroups$df)
    req(assignedBenchmarks())
    determine_reach_conditions(
      indicators = selected_points(),
      definedBenchmarks = allBenchmarkGroups$df,
      assignments = assignedBenchmarks()
    )
  })

  reachConditionsWide <- reactive({
    reachConditionsdf()[['reachConditionsWide']]
  })
  reachConditionsLong <- reactive({
    reachConditionsdf()[['reachConditionsLong']]
  })

  #Use selectedBenchmarks to update dropdown options for plotting reach conditions
  observe({
    updateSelectInput(
      session,
      "reachCondMapSelect",
      choices = unique(reachConditionsLong()$Indicator),
      selected = unique(reachConditionsLong()$Indicator)[1]
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
      write.csv(
        st_drop_geometry(reachConditionsWide()),
        file,
        row.names = FALSE
      ) # need st_drop_geometry or it splits geom into two columns that overwrite data.
    }
  )

  # Download Geopackage button
  output$reachCondDLgpkg <- downloadHandler(
    filename = "reachConditions.gpkg",
    content = function(file) {
      st_write(obj = reachConditionsWide(), dsn = file, layer = "reachCond")
    }
  )

  # TABLE - 'Full Ouput Table'
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

  # Table ---
  # Download as csv
  output$downloadSummaryTable <- downloadHandler(
    # Define the file name
    filename = function() {
      paste0("condition_summary_table.csv")
    },
    # Write the data to the file connection
    content = function(file) {
      readr::write_csv(
        condition_summary_df(
          reachConditionsWide(),
          unique(reachConditionsLong()$Indicator)
        ),
        file
      )
    }
  )

  # Render table
  output$bmSummaryTable <- renderReactable({
    # Calculate summary data
    bmSummary <- condition_summary_df(
      reachConditionsWide(),
      unique(reachConditionsLong()$Indicator)
    )

    # Render summary table
    condition_summary_table(bmSummary)
  })

  # Update boxplot indicator choices based on available indicators
  observe({
    updateSelectInput(
      session,
      "bmSummaryBoxplotsSelect",
      choices = unique(reachConditionsLong()$Indicator),
      selected = unique(reachConditionsLong()$Indicator)[1]
    )
  })

  # Boxplots ---
  # Download as PNG
  output$downloadBoxplot <- downloadHandler(
    # Define the file name
    filename = function() {
      paste0(input$bmSummaryBoxplotsSelect, "_boxplot", ".png")
    },
    # Write the data to the file connection
    content = function(file) {
      # We use readr::write_csv() for reliable CSV writing
      # The data is pulled from the reactive expression defined above
      ggsave(
        file,
        plot = conditions_boxplot(
          reachConditionsWide(),
          input$bmSummaryBoxplotsSelect,
          input$showDensity,
          plotOutput = "ggplot"
        ),
        device = "png",
        width = 5,
        height = 4
      )
    }
  )

  # Render plot in app
  output$bmSummaryBoxplots <- renderGirafe({
    #summaryBoxPlot()
    conditions_boxplot(
      reachConditionsWide(),
      input$bmSummaryBoxplotsSelect,
      input$showDensity,
      plotOutput = "ggiraph"
    )
  })
}

shinyApp(ui, server)
