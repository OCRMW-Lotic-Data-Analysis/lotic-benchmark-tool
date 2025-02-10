library(leaflet)

# Palettes and Labels -------------------------------------------------------

## Initial view of all indicators palette
indicatorPalette <- colorFactor(palette = c("#1e4ca2", "#59c2fa", "#7bf8cf"), levels = c("RandomGRTS","Targeted","RandomSystematic"))

# Label to display when hovering over point on idicator selection map
indicator_labels <- function(indicatorData){
  labels <- paste(
    "<strong>", indicatorData$PointID,
    "</strong><br>", indicatorData$StreamName,
    "<br>", as.Date(indicatorData$FieldEvalDate)) %>%
    lapply(htmltools::HTML)
}

# Reach Conditions (Min, Mod, Maj) palette
## Factors/levels - used in both legend and map symbology. Leaflet needs these both defined. 
reachCondFactors <- factor(c("Major", "Moderate", "Minimal"), levels = c("Major", "Moderate", "Minimal"))
reachCondPalette <- colorFactor(c("#895a44", "#e6e600", "#00a9e6"),
                                na.color = NA,
                                levels = reachCondFactors,
                                ordered = TRUE)

# Maps -------------------------------------------------------------------------

## Select Indicators -----------------------------------------------------------
## Initial map with just basemaps and general settings.
indicator_leaflet_map <- function() {

  leaflet(
    options = leafletOptions(
      attributionControl=FALSE,
      maxZoom = 16)
    ) %>%
    setView(-113, 41, zoom = 5) %>%
    addTiles() %>%
    addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
    addProviderTiles("Esri.WorldImagery"  , group = "Esri.WorldImagery") %>%
    addProviderTiles("Esri.WorldTopoMap"  , group = "Esri.WorldTopoMap") %>%
    addProviderTiles("USGS.USTopo"        , group = "USGS.USTopo") %>%
    addProviderTiles("USGS.USImagery"     , group = "USGS.USImagery") %>%
    addProviderTiles("USGS.USImageryTopo" , group = "USGS.USImageryTopo") %>%
    addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "Esri.WorldImagery",
                                    "Esri.WorldTopoMap", "USGS.USTopo",
                                  "USGS.USImagery", "USGS.USImageryTopo"), 
                   position = "topleft") %>%
    addDrawToolbar(
      singleFeature = TRUE,
      toolbar = toolbarOptions(
        actions = list(title = "Cancel", text = "Cancel"),
        finish = list(title = "Done", text = "Done"),
        undo = list(title = "Delete last vertex", text = "Undo"),
        buttons = list(polygon      = "Select by polygon",
                       rectangle    = "Select by rectangle")
      ),
      handlers =  handlersOptions(
        polygon = list(tooltipStart  = "Start drawing.  Click first point to complete shape")
      ),
      editOptions = editToolbarOptions(),
      polylineOptions = F, rectangleOptions = T, circleOptions = F,
      polygonOptions = T, markerOptions = F, circleMarkerOptions = F)
}

# Proxy map to render initial data and subsequent filtered data.
indicator_leaflet_activeData_proxy <- function(mapId, data){
  
  data <- data %>% st_transform(crs = 4326)

  leafletProxy(mapId, data = data) %>%
    clearMarkers() %>%
    clearControls() %>%  #removes any prior legend.  Allows addLegend below to work.
    addCircleMarkers(
      layerId = ~EvaluationID,
      group = "allPts",
      radius = 4,
      color = "white",
      fillColor = ~indicatorPalette(PointSelectionType),
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      label = ~indicator_labels(data)) %>%
      addLegend(pal = indicatorPalette,
                values = ~PointSelectionType,
                opacity = 1,
                title = "Point Selection Type")
}

# Proxy map to display the selected points with new symbology.
indicator_leaflet_selection_proxy <- function(mapId, data){
  
  data <- data %>% st_transform(crs = 4326)

  leafletProxy(mapId) %>%
    clearGroup("selectedPts") %>%
    addCircleMarkers(
      data = data,
      #fillColor = ~ifelse(EvaluationID %in% selected$EvaluationID, "red", indicatorPalette(PointSelectionType)),
      fillColor = "red",
      group = "selectedPts",
      layerId = ~selectionID,
      radius = 4,
      color = "white",  # point border/outline
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      label = ~indicator_labels(data)
    )
}

## Assign Benchmarks  ----------------------------------------------------------
# Map showing selected_points() on "Assign Benchmark" page
applyBenchmark_leaflet <- function(data){
  
  data <- data %>% st_transform(crs = 4326)
  
  leaflet(data = data,
    options = leafletOptions(
      attributionControl=FALSE,
      maxZoom = 16)
    ) %>%
    addTiles() %>%
    addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
    addProviderTiles("Esri.WorldImagery"  , group = "Esri.WorldImagery") %>%
    addProviderTiles("Esri.WorldTopoMap"  , group = "Esri.WorldTopoMap") %>%
    addProviderTiles("USGS.USTopo"        , group = "USGS.USTopo") %>%
    addProviderTiles("USGS.USImagery"     , group = "USGS.USImagery") %>%
    addProviderTiles("USGS.USImageryTopo" , group = "USGS.USImageryTopo") %>%
    addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "Esri.WorldImagery",
                                    "Esri.WorldTopoMap", "USGS.USTopo",
                                  "USGS.USImagery", "USGS.USImageryTopo"), 
                   position = "topleft") %>%
    addCircleMarkers(
      layerId = ~EvaluationID,
      group = "applyBenchmarks",
      radius = 4,
      color = "white",
      fillColor = ~indicatorPalette(PointSelectionType),
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      label = ~indicator_labels(data)) %>%
    addLegend(pal = indicatorPalette,
              values = ~PointSelectionType,
              opacity = 1,
              title = "Point Selection Type")

}

# Proxy map to display the selected point in applyBenchmarks_hot table
applyBenchmark_leaflet_proxy <- function(mapId, data){
  
  data <- data %>% st_transform(crs = 4326)
  
  leafletProxy(mapId) %>%
    clearGroup("selectedApplyBenchmarks") %>%
    addCircleMarkers(
      data = data,
      fillColor = "red",
      group = "selectedApplyBenchmarks",
      layerId = ~PointID,
      radius = 4,
      color = "white",  # point border/outline
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      label = ~indicator_labels(data)
    )
}


## Reach Conditions ------------------------------------------------------------
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
    
    
    # Map
    reachConditions %>%
      st_transform(crs = 4326) %>%
      leaflet(
        options = leafletOptions(
          attributionControl=FALSE,
          maxZoom = 16)
      ) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 6,
        color = "black",
        fillColor = ~reachCondPalette(reachConditions[[conditionVar]]),
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
      addLegend(pal = reachCondPalette, 
                values = reachCondFactors,
                opacity = 1, 
                title = conditionVar)
}