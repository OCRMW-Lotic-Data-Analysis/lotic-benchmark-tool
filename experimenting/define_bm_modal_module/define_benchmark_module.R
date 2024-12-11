library(shiny)
library(here)
library(DT)
library(htmltools)
library(bslib)
library(dplyr)
library(stringr)
library(purrr)


# UI
defineBenchmarkMod_UI <- function(id){
  ns <- NS(id)
  fluidPage(
    # CSS for aligning Min/Mod/Maj labels with operator and value input boxes
    tags$style(
     ".min-p {
        text-align: right;
      }
      .modmax-p {
        text-align: right; 
        padding-top: 15px
      }"),
    selectInput(ns("Indicator"), "Select Indicator", choices = NULL),
    textOutput(ns("indic_units")),
    textOutput(ns("indic_range")),
    textOutput(ns("indic_increaseDecrease")),
    br(),
    radioButtons(ns("ConditionCategoryNum"), "Number of Condition Categories", choices = list("3" = 3, "2" = 2), selected = 3, inline = TRUE),
    uiOutput(ns("valuesAndOperators")),
    actionButton(ns("saveSingleBM"), "Save"),
    dataTableOutput(ns("tableOut"))
  )
}


# SERVER
defineBenchmarkMod_server <- function(id, metadata, blankForm){
  moduleServer(id, function(input, output, session) {



    # Get namespace of current module to pass to uiOutput/renderUI.
    ns <- session$ns
    
    # Update select inputs 
    observe({
      updateSelectInput(session, "Indicator",
                        choices = unique(metadata$Indicator)
      )
    })
    
    
    # Display metatdata about selected indicator (e.g. units and value ranges)
    observeEvent(
      input$Indicator,
      {
        # Metadata for selected indicator
        selIndicator <- metadata %>% filter(Indicator == input$Indicator) %>% slice_head(n = 1)
        
        output$indic_units <- renderText({ paste("Units: ", selIndicator$Units) })
        output$indic_range <- renderText({ paste("Range: ", selIndicator$RangeLower, " - ", selIndicator$RangeUpper) })
        output$indic_increaseDecrease <- renderText({ paste(selIndicator$IncreaserDecreaser) })
        
      }, ignoreInit = TRUE)
    
    # FUNCTION - MAKE NEW UI
    # Make UI for benchmark value and operator inputs.  This is greatly
    # complicated because of pH which allows two separate benchmarks to be set simultaneously.
    makeUI <- function(numCats, chosenIndicator, metadata){
      
      # Get selected indicator
      selIndicator <- metadata %>% filter(Indicator == chosenIndicator) %>% slice_head(n = 1)
      
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
      } # end setOperators()
    
      
      # Huge logic test that decided which of 4 layouts to use.
      # Option 1) 3 Categories, not pH
      if (numCats == 3) {
        if (chosenIndicator != "pH") {
          uiLayout <- 
            tagList(
              fluidRow(
                column(width = 2,
                       h4("Moderate", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MinimalToModerateRel1"), "", choices = setOperators())),
                column(width = 2,
                       textInput(ns("ModerateBenchmark1"), ""))),
              fluidRow(
                column(width = 2,
                       h4("Major", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MajorToModerateRel1"), "", choices = setOperators())),
                column(width = 2,
                       textInput(ns("MajorBenchmark1"), ""))
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
                       h4("Moderate", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MinimalToModerateRel1"), "", choices = setOperators(pHtype = "acidic"))),
                column(width = 2,
                       textInput(ns("ModerateBenchmark1"), ""))),
              fluidRow(
                column(width = 2,
                       h4("Major", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MajorToModerateRel1"), "", choices = setOperators(pHtype = "acidic"))),
                column(width = 2,
                       textInput(ns("MajorBenchmark1"), ""))
              ),
              h4(" Alkaline Conditions:"),
              fluidRow(
                column(width = 2,
                       h4("Moderate", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MinimalToModerateRel2"), "", choices = setOperators(pHtype = "alkaline"))),
                column(width = 2,
                       textInput(ns("ModerateBenchmark2"), ""))),
              fluidRow(
                column(width = 2,
                       h4("Major", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MajorToModerateRel2"), "", choices = setOperators(pHtype = "alkaline"))),
                column(width = 2,
                       textInput(ns("MajorBenchmark2"), ""))
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
                       h4("Major", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MajorToMinimalRel1"), "", choices = setOperators())),
                column(width = 2,
                       textInput(ns("MajorBenchmark1"), ""))
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
                       h4("Major", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MajorToMinimalRel1"), "", choices = setOperators(pHtype = "acidic"))),
                column(width = 2,
                       textInput(ns("MajorBenchmark1"), ""))
              ),
              h4(" Alkaline Conditions:"),
              fluidRow(
                column(width = 2,
                       h4("Major", class = "modmax-p")),
                column(width = 2,
                       selectInput(ns("MajorToMinimalRel2"), "", choices = setOperators(pHtype = "alkaline"))),
                column(width = 2,
                       textInput(ns("MajorBenchmark2"), ""))
              )
            )
        }
      } 
      
      # Return currently visible operator and mod/maj benchmark inputs. This ensures only 
      # the currently visible input values are saved to the benchmark table.  Switching between 'condition categories',
      # for example, will not reset the previously displayed input$... values and therefore they can mistakenly be saved to the table.  
      
      # Pull input labels from the tagList created above.
      selectedUILabels <- htmltools::tagQuery(uiLayout)$find("label")$selectedTags()
      visibleRenderUIInputVals <- map(1:length(selectedUILabels), ~selectedUILabels[[.x]][["attribs"]][["for"]]) 
      
      # Remove the appended namespace name (defBM) from the labels    
      visibleRenderUIInputVals <- visibleRenderUIInputVals %>% unlist() %>% str_remove(paste0(id,"-"))
      
      # Export ui labels and layout
      uiOut <- list(visibleRenderUIInputVals = visibleRenderUIInputVals,
                    uiLayout = uiLayout)
    
      #print(uiOut$visibleRenderUIInputVals)
      return(uiOut)
      
      
    } # end of makeUI() 
    
    # Run makeUI() when Indicator or # of Condition Categories changes.
    # Returns:  newUI()[["visibleRenderUIInputVals"]] and newUI()[["uiLayout"]]
    newUI <- reactive(makeUI(input$ConditionCategoryNum, input$Indicator, metadata))
    
    
    # Render Custom BM UI  
    output$valuesAndOperators <- renderUI({
      req(input$Indicator)
      newUI()[["uiLayout"]]
    }
    )
    
    
    # Save currently entered benchmark values.  Used for module return and possibly
    # graphical representation of values.
    currentlyEnteredValues <- reactive(
      {
        # Flip the operator for MajorToModerateRel1 and MajorToModerateRel2.  The
        # operature required in the table is oppisite from what make sense in the UI
        flipSymbol <- function(operator){
          if (!is.null(operator)) {
            switch(operator,
                   "<" = ">",
                   ">" = "<",
                   ">=" = "<=",
                   "<=" = ">=")
          }
        }

        # Pull all input$ values into list.  Using list because it handles NULL inputs better.
        newDat_allVals <- list(
          Indicator = input$Indicator,
          ConditionCategoryNum = input$ConditionCategoryNum,
          ModerateBenchmark1 = input$ModerateBenchmark1,
          MajorBenchmark1 = input$MajorBenchmark1,
          ModerateBenchmark2 = input$ModerateBenchmark2,
          MajorBenchmark2 = input$MajorBenchmark2,
          IncreaserDecreaser = metadata %>% filter(Indicator == input$Indicator & ConditionCategoryNum == input$ConditionCategoryNum) %>% pull(IncreaserDecreaser),
          MajorToModerateRel1 = flipSymbol(input$MajorToModerateRel1),  # flip the operator
          MajorToModerateRel2 = flipSymbol(input$MajorToModerateRel2),  # flip the operator
          MinimalToModerateRel1 = input$MinimalToModerateRel1,
          MinimalToModerateRel2 = input$MinimalToModerateRel2,
          MajorToMinimalRel1 = input$MajorToMinimalRel1,
          MajorToMinimalRel2 = input$MajorToMinimalRel2
        )


        # List to dataframe
        newDat <- newDat_allVals %>%
          .[c("Indicator","ConditionCategoryNum","IncreaserDecreaser", newUI()[["visibleRenderUIInputVals"]])] %>%
          bind_cols()
        
        newDat
        #print(newUI()[["visibleRenderUIInputVals"]])
      }
    )

   
  # Create reactiveVal for module return()
    newInidicatorOutput <- reactiveVal()
  
  # Save new benchmark for single indicator
    observeEvent(
      input$saveSingleBM,
      {newInidicatorOutput(currentlyEnteredValues())}
      )

    return(newInidicatorOutput)
    
    }) # end moduleServer
} # end defineBenchmarkMod_server




# APP
################################################################################
ui <- fluidPage(
  fluidRow(
    defineBenchmarkMod_UI(id = "defBM"),
    verbatimTextOutput("newIndicOut")
  )
)

server <- function(input, output, session) {
  # Load metadata and blank form
  indicatorMetadata <- read.csv("./indicator_metadata.csv", colClasses = "character")
  blankCustomBMForm <- read.csv("./custom_indicator_blank.csv", colClasses = "character")

  newInidicator <- defineBenchmarkMod_server(id = "defBM", metadata = indicatorMetadata, blankForm = blankCustomBMForm)
  #newInidicator
  output$newIndicOut <- renderPrint({newInidicator()})

}

shinyApp(ui = ui, server = server)
