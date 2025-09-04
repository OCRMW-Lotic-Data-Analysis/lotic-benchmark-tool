# ---- FILTER MODULE -----------------------------------------------------------

# UI -----
filter_UI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    accordion(
      open = TRUE, 
      multiple = TRUE,
      accordion_panel(
        "Geographic Filters",
        icon = bsicons::bs_icon("globe-americas"),
        pickerInput(inputId = ns("adminState_filter"),
                    label = "Admin State",
                    choices = c("", unique(data$BLM_AdminState) %>% sort()),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1)),
        pickerInput(inputId = ns("project_filter"),
                    label = "Project",
                    choices = unique(data$Project) %>% sort(),
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE)
      ),
      accordion_panel(
        "Attribute Filters",
        icon = bsicons::bs_icon("list-nested"),
        pickerInput(inputId = ns("pointType_filter"),
                    label = "Point Type",
                    choices = na.omit(unique(data$PointSelectionType)),
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE),
        pickerInput(inputId = ns("protocol_filter"),
                    label = "Protocol",
                    choices = na.omit(unique(data$ProtocolType)),
                    selected = "Wadable",
                    options = list(
                      `actions-box` = TRUE),
                    multiple = FALSE),
        dateRangeInput(inputId = ns("dateRange_filter"),
                       label = 'Date range',
                       start = as.Date("2013-06-01"), 
                       end = NULL),
        checkboxInput(inputId = ns("onlyRecentVisits"), 
                      label = "Only include most recent visit", 
                      value = FALSE),
      )
    )
  )
  
}


# SERVER -----
filter_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Logic for applying all user-selected filters
    filtered_data <- reactive({
      filtered <- data
      
      # Admin State
      if (is.null(input$adminState_filter)){
        filtered
      } else if (input$adminState_filter != "") {
        filtered <- filtered %>% filter(BLM_AdminState == input$adminState_filter)
      }
      
      # Project
      if (!is.null(input$project_filter)) {
        filtered <- filtered %>% filter(Project %in% input$project_filter)
      }
      
      # Point Type (targeted or random)
      if (!is.null(input$pointType_filter)) {
        filtered <- filtered %>% filter(PointSelectionType %in% input$pointType_filter)
      }
      
      # Protocl (waderable or boatable)
      if (!is.null(input$protocol_filter)) {
        filtered <- filtered %>% filter(ProtocolType %in% input$protocol_filter)
      }
      
      # Field Eval date range
      if (all(!is.na(input$dateRange_filter))) {
        filtered <- filtered %>% filter(as.Date(FieldEvalDate) >= input$dateRange_filter[1] & as.Date(FieldEvalDate) <= input$dateRange_filter[2]) 
      }
      
      # Include only most recent visits  onlyRecentVisits
      if (input$onlyRecentVisits == TRUE){
        filtered <- filtered %>% group_by(PointID) %>% filter(as.Date(FieldEvalDate) == max(as.Date(FieldEvalDate))) %>% ungroup()
      }
      
      return(filtered)
    })
    
    
    # If an admin state is selected, only show projects within that state.
    observeEvent(
      input$adminState_filter, {
        if (input$adminState_filter == "") {
          updatePickerInput(
            session = session,
            inputId = "project_filter",
            choices = unique(data$Project),
            selected = NULL
          )
        } else {
          updatePickerInput(
            session = session,
            inputId = "project_filter",
            choices = data %>% filter(BLM_AdminState == input$adminState_filter) %>% pull(Project) %>% unique() %>% sort(),
            selected = NULL
          )
        }
      }
    )
    
    # Return filtered data for use by other modules
    return(filtered_data)
  })
}
