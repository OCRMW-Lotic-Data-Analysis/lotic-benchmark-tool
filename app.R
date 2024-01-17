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
source("./appFunctions/determine_reach_conditions.R")
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
                      choices = c("Upload" = "upload", "Filter from all indicators" = "filter")),
          
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
      sidebar = sidebar(
        width = "300px",
        h5("Select Benchmarks"),
        pickerInput(inputId = "selectBenchmarks3",
                    label = "Three Condition Categories",
                    #choices = filter(benchmarkValues, ConditionCategoryNum == 3) %>% pull(Indicator),
                    choices = "",
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        pickerInput(inputId = "selectBenchmarks2",
                    label = "Two Condition Categories",
                    #choices = filter(benchmarkValues, ConditionCategoryNum == 2) %>% pull(Indicator),
                    choices = "",
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
        ),
        hr(),
        
        downloadButton(
          outputId = "benchmarkConfigDLcsv",
          label = "Download Current Benchmark Configuration"
        ),
        fileInput("benchmarkUpload", 
                  "Upload Benchmark Config", 
                  accept = c(".csv"))),
        rHandsontableOutput("benchmark_hot")
  )),

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
                  sidebar = sidebar(
                    selectInput(
                      inputId = "bmSummaryBoxplotsSelect",
                      label = "Select Indicator to Plot",
                      choices = NULL),
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
    
    if(is.null(indicatorData()))return()
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
  benchmarkValues <-reactiveVal(read.csv("./appData/default_benchmark_and_operators.csv", colClasses = "character"))
  
  observe({
    updateSelectInput(session, "selectBenchmarks3",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 3) %>% pull(Indicator))
                      
    updateSelectInput(session, "selectBenchmarks2",
                      choices = filter(benchmarkValues(), ConditionCategoryNum == 2) %>% pull(Indicator))
    })
  
  # Create mutually exclusive selectors for 2 and 3 category benchmarks.  This 
  # means that if, for example, pH is selected for 3 Category, it will be removed
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
  
  # Editable benchmark table
  output$benchmark_hot <- renderRHandsontable({
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
  
  # Download benchmarks as displayed in table 
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
    
   print(benchmarkValues()) 
  })

  # Save edited benchmark table for calculations
  definedBenchmarks <- reactive({
    hot_to_r(input$benchmark_hot)
  })
  
# 3. Reach Conditions --------------------------------------------------------
  
  # Reach conditions (Min, Mod, Max) for each indicator
  reachConditions <- reactive({determine_reach_conditions(indicators =  indicatorData(), 
                                                          benchmarks = definedBenchmarks())
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
  
# 4. Condition Summary ---------------------------------------------------------
  output$bmSummaryTable <- renderDT({
    condition_summary_table(reachConditions(), definedBenchmarks()$Indicator)},
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
                      choices = definedBenchmarks()$Indicator,
                      selected = definedBenchmarks()$Indicator[1]
                      )
    })
  
  
  # output$bmSummaryBoxplots <- renderPlotly({
  #   conditions_boxplot(reachConditions(), input$bmSummaryBoxplotsSelect)
  # })
  
  output$bmSummaryBoxplots <- renderGirafe({
    conditions_boxplot(reachConditions(), input$bmSummaryBoxplotsSelect)
  })
}

shinyApp(ui, server)