### Supporting Functions -------------------------------------------------------
# Note, not all large functions can be pulled out due to namespacing

# Visualization plot showing the currently defined thresholds (ggplot2) 
bmDefVisual <- function(metadata, custBM){
  
  # ININTIAL VALUES --------------------------------------------------------
  # Simplify value names from input custBM dataframe
  indic <- custBM$Indicator
  incOrDec <- custBM$IncreaserDecreaser
  numCats <- custBM$ConditionCategoryNum
  maj1 <- custBM$MajorBenchmark1
  # Only assign value if needed.  Prevents warnings.
  if ("ModerateBenchmark1" %in% colnames(custBM)){
    mod1 <- custBM$ModerateBenchmark1
  }
  # Only assign values if they are needed for pH.  Prevents warnings.
  if ("ModerateBenchmark2" %in% colnames(custBM)){
    mod2 <- custBM$ModerateBenchmark2
  }
  if ("MajorBenchmark2" %in% colnames(custBM)){
    maj2 <- custBM$MajorBenchmark2
  }
  
  # Lower and Upper ranges for the plot
  rangeLow <- metadata %>% filter(Indicator == indic) %>% pull(RangeLower) %>% as.numeric()
  rangeUp <- metadata %>% filter(Indicator == indic) %>% pull(RangeUpper) %>% as.numeric()
  
  # Account for ranges with no true lower or upper end (i.e. -Inf or Inf)
  # 3 categories
  if (numCats == 3){
    if (incOrDec == "Increases with stress") {
      if (rangeUp == Inf){
        rangeUp <- maj1 * 1.2 # add 20% to create some padding
      }
      
      if (rangeLow == -Inf){
        rangeLow <- mod1 * 0.8 # subtract 20% to create some padding
      }
    }
    if (incOrDec == "Decreases with stress") {
      if (rangeUp == Inf){
        rangeUp <- mod1 * 1.2 # add 20% to create some padding
      }
      
      if (rangeLow == -Inf){
        rangeLow <- maj1 * 0.8 # subtract 20% to create some padding
      }
    }
  }
  # 2 categories
  if (numCats == 2){
    if (incOrDec == "Increases with stress") {
      if (rangeUp == Inf){
        rangeUp <- maj1 * 1.2 # add 20% to create some padding
      }
      
      if (rangeLow == -Inf){
        rangeLow <- maj1 * 0.8 # subtract 20% to create some padding
      }
    }
    if (incOrDec == "Decreases with stress") {
      if (rangeUp == Inf){
        rangeUp <- maj1 * 1.2 # add 20% to create some padding
        print(paste0("rangeUpInf: ", rangeUp))
      }
      
      if (rangeLow == -Inf){
        rangeLow <- maj1 * 0.8 # subtract 20% to create some padding
        print(paste0("rangeLowinf: ", rangeLow))
      }
    }
  }
    
  
  # ASSIGN GLOBAL MIN/MAX --------------------------------------------------
  ### Assign xmin and xmax values for Minimal, Moderate, and Major boxes to
  # be used in ggplot annotate() boxes.
  # 3 categorries
  if (numCats == 3){
    # NOT ph
    if (incOrDec == "Increases with stress") {
      min_xmin <- rangeLow
      min_xmax <- mod1
      
      mod_xmin <- mod1
      mod_xmax <- maj1
      
      maj_xmin <- maj1
      maj_xmax <- rangeUp
    }
    if (incOrDec == "Decreases with stress") {
      min_xmin <- mod1
      min_xmax <- rangeUp
      
      mod_xmin <- maj1
      mod_xmax <- mod1
      
      maj_xmin <- rangeLow
      maj_xmax <- maj1
    }
    # pH
    if (incOrDec == "Decreases and increases with stress") {
      # Acidic
      min_xmin_acid <- mod1
      min_xmax_acid <- rangeUp
      
      mod_xmin_acid <- maj1
      mod_xmax_acid <- mod1
      
      maj_xmin_acid <- rangeLow
      maj_xmax_acid <- maj1
      
      # Alkaline
      min_xmin_alk <- rangeLow
      min_xmax_alk <- mod2
      
      mod_xmin_alk <- mod2
      mod_xmax_alk <- maj2
      
      maj_xmin_alk <- maj2
      maj_xmax_alk <- rangeUp
    }
  }
  
  if (numCats == 2){
    # NOT ph
    if (incOrDec == "Increases with stress") {
      min_xmin <- rangeLow
      min_xmax <- maj1
      
      maj_xmin <- maj1
      maj_xmax <- rangeUp
    }
    if (incOrDec == "Decreases with stress") {
      min_xmin <- maj1
      min_xmax <- rangeUp
      
      maj_xmin <- rangeLow
      maj_xmax <- maj1
    }
    # pH
    if (incOrDec == "Decreases and increases with stress") {
      # Acidic
      min_xmin_acid <- maj1
      min_xmax_acid <- rangeUp
      
      maj_xmin_acid <- rangeLow
      maj_xmax_acid <- maj1
      
      # Alkaline
      min_xmin_alk <- rangeLow
      min_xmax_alk <- maj2
      
      maj_xmin_alk <- maj2
      maj_xmax_alk <- rangeUp
    }
  }
  
  # PLOT BUILDING ----------------------------------------------------------
  # Set up blank plot framework
  p <- ggplot() +
    coord_cartesian(
      ylim=c(0,1),
      clip = "off",
      expand = FALSE) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_blank(),
      plot.margin = margin(t = 10,  # Top margin
                           r = 10,  # Right margin
                           b = 10,  # Bottom margin
                           l = 10)) # Left margin) # Left margin
  
  
  ### Add annotations (colored areas) to plot
  # NOT ph
  if (indic != "pH"){
    if(numCats == 3){
      p <- p  +
        annotate("rect", xmin = mod_xmin, xmax = mod_xmax, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
        annotate("rect", xmin = min_xmin, xmax = min_xmax, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
        annotate("rect", xmin = maj_xmin, xmax = maj_xmax, ymin = 0, ymax = 1, fill = "#895a44", color = "black")
      
      p <- p + scale_x_continuous(limits = c(rangeLow,rangeUp),
                                  breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                  expand = c(0,0),
                                  sec.axis = dup_axis(breaks = c(mod1,maj1)))
      
    }
    if (numCats == 2){
      p <- p  +
        annotate("rect", xmin = min_xmin, xmax = min_xmax, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
        annotate("rect", xmin = maj_xmin, xmax = maj_xmax, ymin = 0, ymax = 1, fill = "#895a44", color = "black")
      
      p <- p + scale_x_continuous(limits = c(rangeLow,rangeUp),
                                  breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                  expand = c(0,0),
                                  sec.axis = dup_axis(breaks = c(maj1)))
    }
    
    
  }
  # pH
  if (indic == "pH"){
    if(numCats == 3){
      p_acid <- p  +
        annotate("rect", xmin = mod_xmin_acid, xmax = mod_xmax_acid, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
        annotate("rect", xmin = min_xmin_acid, xmax = min_xmax_acid, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
        annotate("rect", xmin = maj_xmin_acid, xmax = maj_xmax_acid, ymin = 0, ymax = 1, fill = "#895a44", color = "black")
      
      p_alk <- p  +
        annotate("rect", xmin = mod_xmin_alk, xmax = mod_xmax_alk, ymin = 0, ymax = 1, fill = "#e6e600", color = "black") +
        annotate("rect", xmin = min_xmin_alk, xmax = min_xmax_alk, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
        annotate("rect", xmin = maj_xmin_alk, xmax = maj_xmax_alk, ymin = 0, ymax = 1, fill = "#895a44", color = "black")
      
      # Need 2 plots for pH fork acid and alkaline
      p_acid <- p_acid + scale_x_continuous(limits = c(rangeLow,rangeUp),
                                            breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                            expand = c(0,0),
                                            sec.axis = dup_axis(breaks = c(mod1,maj1))) +
        labs(title = "Acidic")
      
      p_alk <- p_alk + scale_x_continuous(limits = c(rangeLow,rangeUp),
                                          breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                          expand = c(0,0),
                                          sec.axis = dup_axis(breaks = c(mod2,maj2))) +
        labs(title = "Alkaline")
      
    }
    if (numCats == 2){
      p_acid <- p  +
        annotate("rect", xmin = min_xmin_acid, xmax = min_xmax_acid, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
        annotate("rect", xmin = maj_xmin_acid, xmax = maj_xmax_acid, ymin = 0, ymax = 1, fill = "#895a44", color = "black")
      
      p_alk <- p  +
        annotate("rect", xmin = min_xmin_alk, xmax = min_xmax_alk, ymin = 0, ymax = 1, fill = "#00a9e6", color = "black") +
        annotate("rect", xmin = maj_xmin_alk, xmax = maj_xmax_alk, ymin = 0, ymax = 1, fill = "#895a44", color = "black")
      
      # Need 2 plots for pH fork acid and alkaline
      p_acid <- p_acid + scale_x_continuous(limits = c(rangeLow,rangeUp),
                                            breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                            expand = c(0,0),
                                            sec.axis = dup_axis(breaks = c(maj1))) +
        labs(title = "Acidic")
      
      p_alk <- p_alk + scale_x_continuous(limits = c(rangeLow,rangeUp),
                                          breaks = breaks_pretty(n = 10),  # helps with arbitrary input ranges
                                          expand = c(0,0),
                                          sec.axis = dup_axis(breaks = c(maj2))) +
        labs(title = "Alkaline")
    }
    
    # patchwork package used for stacked plots
    p <- p_acid / p_alk
  }
  
  # A "legend" for the plots.  Just another blank ggplot with annotations.
  if (numCats == 3){
    legend <- ggplot() +
      coord_cartesian(
        ylim=c(0,1),
        xlim=c(0,120),
        clip = "off",
        expand = FALSE) +
      theme_void() +
      annotate("rect", xmin = 12, xmax = 22, ymin = 0, ymax = 0.5, fill = "#00a9e6", color = "black") +
      annotate("text", x = 23, y = 0.25, label = "Minimal", hjust = 0) +
      annotate("rect", xmin = 50, xmax = 60, ymin = 0, ymax = 0.5, fill = "#e6e600", color = "black") +
      annotate("text", x = 61, y = 0.25, label = "Moderate", hjust = 0) +
      annotate("rect", xmin = 90, xmax = 100, ymin = 0, ymax = 0.5, fill = "#895a44", color = "black") +
      annotate("text", x = 101, y = 0.25, label = "Major", hjust = 0)
  } else if (numCats == 2){
    legend <- ggplot() +
      coord_cartesian(
        ylim=c(0,1),
        xlim=c(0,120),
        clip = "off",
        expand = FALSE) +
      theme_void() +
      annotate("rect", xmin = 30, xmax = 40, ymin = 0, ymax = 0.5, fill = "#00a9e6", color = "black") +
      annotate("text", x = 41, y = 0.25, label = "Minimal", hjust = 0) +
      annotate("rect", xmin = 70, xmax = 80, ymin = 0, ymax = 0.5, fill = "#895a44", color = "black") +
      annotate("text", x = 81, y = 0.25, label = "Major", hjust = 0)
  }
  
  # Add legend with patchwork
  p <- p/legend
  
  
  # Return plot
  p
  
}

# Check logic of input values.  Do inequalities make sense and match the indicators 'IncreaserDecreaser' category?
# "Save & Close" buttin will remain greyed out until values are valid.
saveValidator <- function(newData){
  if (newData$ConditionCategoryNum == 3){
    # Test "Decreases with stress" indicators
    if (newData$IncreaserDecreaser == "Decreases with stress"){
      formTest <- isTRUE(newData$ModerateBenchmark1 > newData$MajorBenchmark1)
    } 
    
    # Test "Increases with stress" indicators
    if (newData$IncreaserDecreaser == "Increases with stress"){
      formTest <- isTRUE(newData$ModerateBenchmark1 < newData$MajorBenchmark1)
    } 
    
    # Test "Decreases and increases with stress" indicators (pH)
    if (newData$IncreaserDecreaser == "Decreases and increases with stress"){
      formTest <- isTRUE((newData$ModerateBenchmark1 > newData$MajorBenchmark1) & (newData$ModerateBenchmark2 < newData$MajorBenchmark2))
    } 
  }
  
  
  if (newData$ConditionCategoryNum == 2){
    # Test "Decreases with stress" indicators
    if (newData$IncreaserDecreaser == "Decreases with stress"){
      formTest <- is.numeric(newData$MajorBenchmark1)
    } 
    
    # Test "Increases with stress" indicators
    if (newData$IncreaserDecreaser == "Increases with stress"){
      formTest <- is.numeric(newData$MajorBenchmark1)
    } 
    
    # Test "Decreases and increases with stress" indicators (pH)
    if (newData$IncreaserDecreaser == "Decreases and increases with stress"){
      formTest <- isTRUE( is.numeric(newData$MajorBenchmark1) & is.numeric(newData$MajorBenchmark2))
    } 
  }
  
  return(formTest)
}

### Module UI ------------------------------------------------------------------
# UI
defineBenchmarkMod_UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      # CSS for aligning Min/Mod/Maj labels with operator and value input boxes
      tags$style(
        ".min-p {
        text-align: right;
      }
      .modmax-p {
        text-align: right; 
        padding-top: 25px
      }"),
      selectInput(ns("Indicator"), "Select Indicator", choices = NULL),
      textOutput(ns("indic_units")),
      textOutput(ns("indic_range")),
      textOutput(ns("indic_increaseDecrease")),
      br(),
      radioButtons(ns("ConditionCategoryNum"), "Number of Condition Categories", choices = list("3" = 3, "2" = 2), selected = 3, inline = TRUE),
      uiOutput(ns("valuesAndOperators")),
      plotOutput(ns("bmVisualPlot"), inline = T),
      #actionButton(ns("saveSingleBM"), "Save")
    )
  )
}

### Module Server --------------------------------------------------------------
# SERVER
defineBenchmarkMod_server <- function(id, metadata, blankForm){
  moduleServer(id, function(input, output, session) {
    
    # Get namespace of current module to pass to uiOutput/renderUI.
    ns <- session$ns
    
    # Update select inputs 
    observe({
      updateSelectInput(session, "Indicator",
                        choices = unique(metadata$Indicator))
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
    
    # MAKE OPERATOR/VALUE UI ---------------------------------------------------
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
                              #"Increases with stress" = c("<=", "<"),
                              #"Decreases with stress" = c(">=", ">"))
                              "Increases with stress" = c(">=", ">"),
                              "Decreases with stress" = c("<=", "<"))
          # Account for both acidic and alkaline benchmarks both of which must be defined.
          # pHtype here is used to help define the 'choices' in the selectInputs below.
        } else if (indicator == "pH") {
          if (pHtype == "acidic") {          #"Decreases with stress"
            operators <- c("<=", "<")       
          } else if (pHtype == "alkaline") { #"Increases with stress"
            operators <- c(">=", ">")
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
                column(width = 3,
                       h5("Moderate", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MinimalToModerateRel1"), "", choices = setOperators())),
                column(width = 3,
                       textInput(ns("ModerateBenchmark1"), ""))),
              fluidRow(
                column(width = 3,
                       h5("Major", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MajorToModerateRel1"), "", choices = setOperators())),
                column(width = 3,
                       textInput(ns("MajorBenchmark1"), ""))
              )
            )
          # Option 2) 3 Categories, is pH
        } else if (chosenIndicator == "pH") {
          uiLayout <- 
            tagList(
              p("pH currently requires two benchmarks to account for both alkaline and acidic conditions"),
              h5("Acidic Conditions:"),
              fluidRow(
                column(width = 3,
                       h5("Moderate", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MinimalToModerateRel1"), "", choices = setOperators(pHtype = "acidic"))),
                column(width = 3,
                       textInput(ns("ModerateBenchmark1"), ""))),
              fluidRow(
                column(width = 3,
                       h5("Major", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MajorToModerateRel1"), "", choices = setOperators(pHtype = "acidic"))),
                column(width = 3,
                       textInput(ns("MajorBenchmark1"), ""))
              ),
              h5(" Alkaline Conditions:"),
              fluidRow(
                column(width = 3,
                       h5("Moderate", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MinimalToModerateRel2"), "", choices = setOperators(pHtype = "alkaline"))),
                column(width = 3,
                       textInput(ns("ModerateBenchmark2"), ""))),
              fluidRow(
                column(width = 3,
                       h5("Major", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MajorToModerateRel2"), "", choices = setOperators(pHtype = "alkaline"))),
                column(width = 3,
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
                column(width = 3,
                       h5("Major", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MajorToMinimalRel1"), "", choices = setOperators())),
                column(width = 3,
                       textInput(ns("MajorBenchmark1"), ""))
              )
            )
          # Option 4) 2 Categories, is pH
        } else if (chosenIndicator == "pH") {
          uiLayout <- 
            tagList(
              p("pH currently requires two benchmarks to account for both alkaline and acidic conditions"),
              h5("Acidic Conditions:"),
              fluidRow(
                column(width = 3,
                       h5("Major", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MajorToMinimalRel1"), "", choices = setOperators(pHtype = "acidic"))),
                column(width = 3,
                       textInput(ns("MajorBenchmark1"), ""))
              ),
              h5(" Alkaline Conditions:"),
              fluidRow(
                column(width = 3,
                       h5("Major", class = "modmax-p")),
                column(width = 3,
                       selectInput(ns("MajorToMinimalRel2"), "", choices = setOperators(pHtype = "alkaline"))),
                column(width = 3,
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
      
      return(uiOut)
      
      
    } # end of makeUI() 
    
    # Run makeUI() when Indicator or # of Condition Categories changes.
    # Returns:  newUI()[["visibleRenderUIInputVals"]] and newUI()[["uiLayout"]]
    newUI <- reactive({
      req(input$ConditionCategoryNum, input$Indicator)
      makeUI(input$ConditionCategoryNum, input$Indicator, metadata)
    })
    
    # Render Custom BM UI  
    output$valuesAndOperators <- renderUI({
      newUI()[["uiLayout"]]
    })
    
    # REALTIME SAVING OF ENTERED VALUES ------------------------------------------  
    # Save currently entered benchmark values.  Used for module return and possibly
    # graphical representation of values.
    currentlyEnteredValues <- reactive(
      {
        req(newUI())
        # Flip the operator for MajorToModerateRel1 and MajorToModerateRel2.  The
        # operator required in the table is oppisite from what make sense in the UI
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
          ConditionCategoryNum = as.numeric(input$ConditionCategoryNum),
          ModerateBenchmark1 = as.numeric(input$ModerateBenchmark1),
          MajorBenchmark1 = as.numeric(input$MajorBenchmark1),
          ModerateBenchmark2 = as.numeric(input$ModerateBenchmark2),
          MajorBenchmark2 = as.numeric(input$MajorBenchmark2),
          IncreaserDecreaser = metadata %>% filter(Indicator == input$Indicator) %>% pull(IncreaserDecreaser) %>% unique(),
          MajorToModerateRel1 = input$MajorToModerateRel1,  
          MajorToModerateRel2 = input$MajorToModerateRel2,  
          MinimalToModerateRel1 = flipSymbol(input$MinimalToModerateRel1),  # flip the operator to match BLM table
          MinimalToModerateRel2 = flipSymbol(input$MinimalToModerateRel2),  # flip the operator to match BLM table
          MajorToMinimalRel1 = input$MajorToMinimalRel1,
          MajorToMinimalRel2 = input$MajorToMinimalRel2
        )
        
        # List to dataframe
        newDat <- newDat_allVals %>%
          .[c("Indicator","ConditionCategoryNum","IncreaserDecreaser", newUI()[["visibleRenderUIInputVals"]] )] %>%
          bind_cols()
        newDat
      }
    )
    
    
    
    
    # Visualization of currently entered benchmark. Updates in realtime as values change.
    output$bmVisualPlot <- renderPlot({
      # Only plot if currentlyEnteredValues() has data and is COMPLETELY filled out
      if (nrow(currentlyEnteredValues()) > 0 & sum(is.na(currentlyEnteredValues())) == 0){
        
        bmDefVisual(metadata = metadata, custBM = newInidicatorOutput())
        
      } 
    }, # Set size of renderPlot() output.  Otherwise, a non-pH plot is very "tall"
    width = 425, 
    height = function(){
      if (input$Indicator != "pH") {
        return(125)
        
      }
      if (input$Indicator == "pH") {
        return(200)
      }
    }
    ) 
    
    newInidicatorOutput <- reactiveVal()
    observe({
      newInidicatorOutput(currentlyEnteredValues())
    })
    
    return(newInidicatorOutput)
    
  }) # end moduleServer
} # end defineBenchmarkMod_server