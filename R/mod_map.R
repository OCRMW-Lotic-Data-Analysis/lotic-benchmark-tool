library(leaflet)


# ---- MAP MODULE ----
map_UI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "600px")
}

map_server <- function(id, data, original_data, clear_selection) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store selected points (list of data frames)
    # Initialize with empty df with colnames (prevents crashing due to missing selection/filter)
    selected_points <- reactiveVal(original_data %>% slice(0) %>% mutate(selectionID = NA)) 

    # RENDER BLANK MAP
    output$map <- renderLeaflet({
      indicator_leaflet_map()
    })
    
    # ADD (FILTERED) DATA POINTS TO MAP
    observe({
      current_data <- data() %>% st_transform(crs = 4326)
      indicator_leaflet_activeData_proxy(mapId = "map", data = current_data)
    })
    
    ### POINT SELECTION ----------------------------------------------------------
    
    # INDIVIDUAL POINT SELECTION - When point is clicked, select/unselect it
    observeEvent(input$map_marker_click, {
      # Retrieve click ID.  From an unselected point, this will return the 'PointID'.
      # From a selected point, this will return the 'selectionID'. Two different IDs are needed because
      # if two points share the same 'layerId' in leaflet the second one will just replace the original.
      # Here, selected points are drawn "on top" of 'allPts'.
      clickID <- input$map_marker_click$id
      clickGroup <- input$map_marker_click$group
      
      current_data <- data() %>% st_transform(crs = 4326)
      
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
        new_selection <- bind_rows(selected, current_data %>% filter(PointID == clickPtID))
      }
      
      # Make new selectionID to act as layerId for selected points.
      new_selection$selectionID <- seq_len(nrow(new_selection))
      
      # Save the updated selection
      selected_points(new_selection) 
      
      # Update map with new selection
      indicator_leaflet_selection_proxy(mapId = "map", data = selected_points())
    })
    
    
    # POLYON SELECTION - Select all points within a drawn polygon
    observeEvent(input$map_draw_new_feature, {
      selected <- selected_points()
      current_data <- data() %>% st_transform(crs = 4326)

      # Extract polygon feature
      feature <- input$map_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      selectionPoly <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))

      # Find points within the polygon
      pointsInPoly <- st_filter(current_data, selectionPoly)

      # Identify only the new points.  Do not include points already selected.
      new_points <- pointsInPoly %>% filter(!(EvaluationID %in% selected$EvaluationID))

      # Combined previous and new selection
      new_selection <- bind_rows(selected, current_data %>% filter(EvaluationID %in% new_points$EvaluationID))

      # Make new selectionID to act as layerId for selected points.
      new_selection$selectionID <- seq_len(nrow(new_selection))

      # Save the updated selection
      selected_points(new_selection)

      # Update map with new selection
      indicator_leaflet_selection_proxy(mapId = "map", data = selected_points())  # Update the map to reflect the color change
    })
    # end point selection
    
    # Clear selected points from map 
    observeEvent(clear_selection(), {  # clear_selection() is an action button input outside of module
      selected <- selected_points()
      if (!is.null(selected)) {
        selected_points(selected %>% slice(0))
        indicator_leaflet_selection_proxy(mapId = "map", data = selected_points())
        }
    })
    
    # If points are selected, and user changes filter, reset manual selection
    observeEvent(data(), {
      selected_points(selected_points() %>% slice(0))
    })

    # If no points are manually selected, use filtered data for return
    workingDat <- reactive({
    if (nrow(selected_points()) > 0) {
      wdata <- selected_points()
    }
    else {
      wdata <- data()
    }
    return(wdata)
    })

    return(workingDat)
  })
}
