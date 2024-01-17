indicator_leaflet_map <- function(indicatorData, mappingVarInput) {

# Label to display when hovering over point
labels <- paste(
  "<strong>", indicatorData$PointID,
  "</strong><br>", indicatorData$StreamName) %>%
  lapply(htmltools::HTML)

# Indicators to map
mapVals <- indicatorData[[mappingVarInput]]

# Make a palette
pal <- colorFactor(palette = "Set2", levels = unique(mapVals))

# Map
indicatorData %>%
  st_transform(crs = 4326) %>%
  leaflet(
    options = leafletOptions(
      attributionControl=FALSE)
  ) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 7,
    color = "black",
    fillColor = ~pal(mapVals),
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
  addLegend(pal = pal, 
            values = mapVals, 
            opacity = 1,
            title = mappingVarInput)
}
