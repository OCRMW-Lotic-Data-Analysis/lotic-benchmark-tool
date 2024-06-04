# Map displaying loaded or filtered indicators at start of workflow.
# Set pallette.  Leaving outside of function to use in leafletproxy elsewhere.
indicatorPalette <- colorFactor(palette = c("#1e4ca2", "#59c2fa", "#7bf8cf"), levels = c("RandomGRTS","Targeted","RandomSystematic"))

# Map making function
indicator_leaflet_map <- function(indicatorData) {
  # Label to display when hovering over point
  labels <- paste(
    "<strong>", indicatorData$PointID,
    "</strong><br>", indicatorData$StreamName) %>%
    lapply(htmltools::HTML)
  
  # Map
  indicatorData %>%
    st_transform(crs = 4326) %>%
    leaflet(
      options = leafletOptions(
        attributionControl=FALSE)
    ) %>%
    addTiles() %>%
    addCircleMarkers(
      layerId = ~EvaluationID,
      radius = 3,
      color = "black",
      fillColor = ~indicatorPalette(PointSelectionType),
      stroke = TRUE, 
      weight = 1,
      fillOpacity = 1,
      label = ~labels) %>%
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Esri.WorldTopoMap") %>%
    addProviderTiles("Esri.WorldImagery",
                     group = "Esri.WorldImagery") %>%
    addLayersControl(
      baseGroups = c("Esri.WorldTopoMap",
                     "Esri.WorldImagery"),
      # position it on the topleft
      position = "topleft") %>%
    addLegend(pal = indicatorPalette, 
              values = ~PointSelectionType, 
              opacity = 1,
              title = "Point Selection Type")
}


# Map displaying reach conditions after benchmarks have been defined and applied
reachCond_leaflet_map <- function(reachConditions, mappingVarInput) {
# Pull variable to plot from input select.  Input just shows benchmark name
    # to make select cleaner but "Condition" is appended to select col with value.
    conditionVar <- paste0(mappingVarInput, "Condition")
    indicatorVar <- mappingVarInput
    
    
    # Label that shows up on hover
    labels <- paste0(
      "<strong>", reachConditions$PointID, "</strong><br>",
      reachConditions$StreamName, "<br>",
      indicatorVar,": ", reachConditions[[indicatorVar]], "<br>",
      "Condition: ", reachConditions[[conditionVar]]) %>%
      lapply(htmltools::HTML)
    
    # Create a new grouping variable.  Ensures legend item order is correct.
    ord <- factor(c("Major", "Moderate", "Minimal"), levels = c("Major", "Moderate", "Minimal"))
    
    
    # Palet
    pal <- colorFactor(c("#f03b20", "#feb24c", "#ffeda0"), 
                       levels = ord,
                       ordered = TRUE)
    
    # Map
    reachConditions %>%
      st_transform(crs = 4326) %>%
      leaflet(
        options = leafletOptions(
          attributionControl=FALSE)
      ) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 7,
        color = "black",
        fillColor = ~pal(reachConditions[[conditionVar]]),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        label = ~labels
      ) %>%
      addProviderTiles("Esri.WorldTopoMap",
                       group = "Esri.WorldTopoMap") %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Esri.WorldImagery") %>%
      addLayersControl(
        baseGroups = c("Esri.WorldTopoMap",
                       "Esri.WorldImagery"),
        # position it on the topleft
        position = "topleft") %>%
      addLegend(pal = pal, 
                values = ord,
                opacity = 1, 
                title = conditionVar)
}