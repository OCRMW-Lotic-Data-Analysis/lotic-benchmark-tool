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

ui <- page_navbar(
  title = "Lotic AIM Indicator Benchmark Tool",
  selected = "2. Define Benchmarks",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
  
  
  # 2. Define Benchmarks ----
  nav_panel(
    title = "2. Define Benchmarks",
    page_sidebar(
      sidebar = sidebar(
        width = "300px",
        h5("Create Benchmark Groups"),
        textInput("bmGroupNameinput", label = "Group Name"),
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
        actionButton("saveNewBMGroup", label = "Save New Benchmark Group", style="color: #fff; background-color: #53C1BE"),
        hr(),
        actionButton("editBMGroup", label = "Edit Selected Benchmark Group"),
        actionButton("deleteBMGroup", label = "Delete Selected Benchmark Group"),
        
        
        downloadButton(
          outputId = "benchmarkConfigDLcsv",
          label = "Download Current Benchmark Configuration"
        ),
        fileInput("benchmarkUpload", 
                  "Upload Benchmark Config", 
                  accept = c(".csv"))),
      rHandsontableOutput("benchmark_hot"),
      dataTableOutput("benchmarkGroupsTable"),
      verbatimTextOutput("value")
    ))
  
  
)

server <- function(input, output, session) {
  
 
  
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
  
  # Initialize empty container for saved benchmark groups
  # cant delete items in 
  benchmarkGroupDFs <- reactiveValues()
  benchmarkGroupWideSum <- reactiveValues()
  
  observeEvent(input$saveNewBMGroup,{
    req(input$bmGroupNameinput)
    benchmarkGroupDFs[[input$bmGroupNameinput]] <- definedBenchmarks() %>% tibble::add_column(bmgroup = input$bmGroupNameinput)
    benchmarkGroupWideSum$df <- reactiveValuesToList(benchmarkGroupDFs) %>% 
      bind_rows() %>% 
      select(bmgroup, Indicator, ModerateBenchmark1, ConditionCategoryNum) %>% 
      pivot_wider(names_from = Indicator, values_from = ModerateBenchmark1)
    updateTextInput(session, "bmGroupNameinput", value = "")
    updatePickerInput(session,"selectBenchmarks3", selected = character(0))
    updatePickerInput(session,"selectBenchmarks2", selected = character(0))
    print(names(benchmarkGroupDFs))
  }
  )
  
  observeEvent(input$deleteBMGroup,{

    if (!is.null(input$benchmarkGroupsTable_rows_selected)) {
      # Get groupName(s) you want to delete into vector of strings
      groupNames <- benchmarkGroupWideSum$df %>% slice(input$benchmarkGroupsTable_rows_selected) %>% pull(bmgroup)
  
      # Actually remove the data from benchmarkGroupWideSum reactive value
      benchmarkGroupWideSum$df <- benchmarkGroupWideSum$df %>% subset(!(bmgroup %in% groupNames))
      
      # cant delete items in reactiveValues.  Mayabe make it is list in reactive?
      #https://stackoverflow.com/questions/39436713/r-shiny-reactivevalues-vs-reactive
    }
  })
  
  output$benchmarkGroupsTable <- renderDataTable({
    benchmarkGroupWideSum$df
  })
  
  #output$value <- renderPrint({ names(benchmarkGroupDFs) })
  output$value <- renderPrint({ paste(input$benchmarkGroupsTable_rows_selected) })
  
}

shinyApp(ui, server)