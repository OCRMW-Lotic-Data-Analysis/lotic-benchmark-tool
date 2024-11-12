library(shiny)
library(here)
library(DT)
library(htmltools)
library(bslib)
library(dplyr)

ui <- fluidPage(
  actionButton("addCustomBM", label = "Add Custom Benchmark"),
  hr(),
  
  
  ## TO GO INTO MODAL
  
  selectInput("indicator_def", "Select Indicator", choices = NULL),
  textOutput("indic_units"),
  textOutput("indic_range"),
  textOutput("indic_increaseDecrease"),
  radioButtons("categoryNum", "Number of Condition Categories", choices = list("3" = 3, "2" = 2), selected = 3, inline = TRUE),
  
  
  fluidRow(
    uiOutput("valuesAndOperators")
  ),
 
  actionButton("saveSingleBM", "Save"),
  
  ## END OF MODAL

  
  #uiOutput("defineCustomBM"),
  dataTableOutput("savedData")
)

server <- function(input, output, session) {
  indicatorMetadata <- read.csv("./indicator_metadata.csv", colClasses = "character")
  blankCustomBMForm <- read.csv("./custom_indicator_blank.csv", colClasses = "character")
  
  

  # Reactive blank custom BM form.
  customBMvals <- reactiveVal({blankCustomBMForm})
  
  # Update indicator selector from the metadata dictionary
  observe({
    updateSelectInput(session, "indicator_def",
                      choices = unique(indicatorMetadata$Indicator)
    )
  })
  
  
  
  # Function to make UI for benchmark value and operator inputs.  This is greatly
  # complicated because of pH which allows two separate benchmarsk to be set simultaneously.
  makeUI <- function(numCats, chosenIndicator, indicatorMetadata){
    
    # Get selected indicator
    selIndicator <- indicatorMetadata %>% filter(Indicator == chosenIndicator) %>% slice_head(n = 1)

    # Sub function to create vector of (in)equality operators dictated by the 
    # 'IncreaserDecreaser' value in indicator metadata.  User can only change 
    # whether operator is inclusive or not.
    setOperators <- function(indicator = chosenIndicator,
                             IncDec = selIndicator$IncreaserDecreaser,
                             pHtype = NULL){
    
      # The simple version, if indicator is not pH
      if (indicator != "pH"){
        operators <- switch(IncDec,
                       "Increases with stress" = c("<=", "<"),
                       "Decreases with stress" = c(">=", ">"))
      # Account for both acidic and alkaline benchmarks both of which must be defined.
      # pHtype here is used to help define the 'choices' in the selectInputs below.
      } else if (indicator == "pH") {
          if (pHtype == "acidic") {          #"Decreases with stress"
            operators <- c(">=", ">")       
          } else if (pHtype == "alkaline") { #"Increases with stress"
            operators <- c("<=", "<")
            }
        }
        
        return(operators)
    }
    
    # Huge logic test that decided which of 4 layouts to use.
    # Option 1) 3 Categories, not pH
    if (numCats == 3) {
      if (chosenIndicator != "pH") {
        uiLayout <- 
          tagList(
            fluidRow(
              column(width = 2,
                     h3("Minimal")),
              column(width = 1,
                     selectInput("MinimalToModerateRel1_3cat", "", choices = setOperators())),
              column(width = 2,
                     textInput("moderateBM_1", "Moderate")),
              column(width = 1,
                     selectInput("MajorToModerateRel1_3cat", "", choices = setOperators())),
              column(width = 2,
                     textInput("majorBM_1", "Major"))
              )
            )
      # Option 2) 3 Categories, is pH
      } else if (chosenIndicator == "pH") {
        uiLayout <- 
          tagList(
            p("pH currently requires two benchmarks to account for both alkaline and acidic conditions"),
            h4("Acidic Conditions:"),
            fluidRow(
              column(width = 2,
                     h3("Minimal")),
              column(width = 1,
                     selectInput("MinimalToModerateRel1_3cat", "", choices = setOperators(pHtype = "acidic"))),
              column(width = 2,
                     textInput("moderateBM_1", "Moderate")),
              column(width = 1,
                     selectInput("MajorToModerateRel1_3cat", "", choices = setOperators(pHtype = "acidic"))),
              column(width = 2,
                     textInput("majorBM_1", "Major"))
            ),
            h4(" Alkaline Conditions:"),
            fluidRow(
              column(width = 2,
                     h3("Minimal")),
              column(width = 1,
                     selectInput("MinimalToModerateRel2_3cat", "", choices = setOperators(pHtype = "alkaline"))),
              column(width = 2,
                     textInput("moderateBM_2", "Moderate")),
              column(width = 1,
                     selectInput("MajorToModerateRel2_3cat", "", choices = setOperators(pHtype = "alkaline"))),
              column(width = 2,
                     textInput("majorBM_2", "Major"))
              )
          )
      }
    # Option 3) 2 Categories, not pH
    } else if (numCats == 2) {
      if (chosenIndicator != "pH") {
        uiLayout <- 
          tagList(
            fluidRow(
              column(width = 2,
                     h3("Minimal")),
              column(width = 1,
                     selectInput("MinimalToMajorRel1_2cat", "", choices = setOperators())),
              column(width = 2,
                     textInput("majorBM_1", "Major"))
            )
          )
      # Option 4) 2 Categories, is pH
      } else if (chosenIndicator == "pH") {
        uiLayout <- 
          tagList(
            p("pH currently requires two benchmarks to account for both alkaline and acidic conditions"),
            h4("Acidic Conditions:"),
            fluidRow(
              column(width = 2,
                     h3("Minimal")),
              column(width = 1,
                     selectInput("MinimalToMajorRel1_2cat", "", choices = setOperators(pHtype = "acidic"))),
              column(width = 2,
                     textInput("majorBM_1", "Major"))
            ),
            h4(" Alkaline Conditions:"),
            fluidRow(
              column(width = 2,
                     h3("Minimal")),
              column(width = 1,
                     selectInput("MinimalToMajorRel2_2cat", "", choices = setOperators(pHtype = "alkaline"))),
              column(width = 2,
                     textInput("majorBM_2", "Major"))
            )
          )
      }
    }
    
    return(uiLayout)
  }
  
  # Render Custom BM UI
  output$valuesAndOperators <- renderUI({
    req(input$indicator_def)
    makeUI(input$categoryNum, input$indicator_def, indicatorMetadata)
  })



  # Display metatdata about selected indicator (e.g. units and value ranges)
  observeEvent(
    input$indicator_def,
    {
      # Metadata for selected indicator
      selIndicator <- indicatorMetadata %>% filter(Indicator == input$indicator_def) %>% slice_head(n = 1)
     
      output$indic_units <- renderText({ paste("Units: ", selIndicator$Units) })
      output$indic_range <- renderText({ paste("Range: ", selIndicator$ValueRange) })
      output$indic_increaseDecrease <- renderText({ paste(selIndicator$IncreaserDecreaser) })
      
    }, ignoreInit = TRUE)
    
  
  
  
  
  
  
    # Save new benchmark for single indicator
    observeEvent(
      input$saveSingleBM,
      { 
        dat <- reactiveValuesToList(input)
        print(dat)
        # newDat <- data.frame(
        #   Indicator = input$indicator_def,
        #   ConditionCategoryNum = input$categoryNum,
        #   ModerateBenchmark1 = input$moderateBM_1,
        #   MajorBenchmark1 = input$majorBM_1,
        #   IncreaserDecreaser = selIndicator$IncreaserDecrease,
        #   MajorToModerateRel1 = input$MajorToModerateRel1_3cat
        #   
        #   )
        # print(newDat)
        #customBMvals(bind_rows(customBMvals(), newDat))
        #print(customBMvals())
        }
      )

    
    
    
    
    
    
  observeEvent(input$addCustomBM, {
    ### This is the pop up board for input a new row
    showModal(
      modalDialog(
        "asdf"
        )
      )
  })
              
              
}

shinyApp(ui, server)



