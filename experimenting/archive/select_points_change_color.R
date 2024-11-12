# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)

# Define UI for the Shiny app
ui <- fluidPage(
  leafletOutput("mymap")
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Sample data
  points <- data.frame(
    lat = c(37.7749, 34.0522, 40.7128),
    lng = c(-122.4194, -118.2437, -74.0060),
    label = c("San Francisco", "Los Angeles", "New York"),
    category = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  
  # Create a color palette based on categories
  palette <- colorFactor(palette = c("blue", "green", "purple"), levels = points$category)
  
  # Reactive value to store selected points
  selected_points <- reactiveVal(data.frame())
  
  # Render the initial Leaflet map
  output$mymap <- renderLeaflet({
    leaflet(points) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat, 
        color = ~palette(category),
        layerId = ~label,
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.8
      ) %>%
      addLegend(pal = palette, 
                values = points$category, 
                opacity = 1,
                title = "mappingVarInput")
  })
  
  # Observe click events on the map
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    selected <- selected_points()
    
    # Check if the clicked point is already selected
    if (click$id %in% selected$label) {
      # Remove the point from selected points if it's already selected
      selected <- selected %>% filter(label != click$id)
    } else {
      # Add the point to selected points if it's not already selected
      selected <- rbind(selected, points %>% filter(label == click$id))
    }
    
    # Update reactive value
    selected_points(selected)
    
    # Update the map with the new selection
    leafletProxy("mymap", data = points) %>%
      clearMarkers() %>%
      addCircleMarkers(
        color = ~ifelse(label %in% selected$label, "red", palette(category)),
        layerId = ~label,
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
