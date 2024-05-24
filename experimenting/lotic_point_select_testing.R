source("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/lotic-benchmark-tool/R/leaflet_maps.R")

pts <- vroom::vroom("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/lotic-benchmark-tool/appData/wy_landerPts.csv", delim = ",", show_col_types = FALSE) %>% 
  st_as_sf(coords = c("SampledMidLongitude", "SampledMidLatitude"), crs = 4269)


indicator_leaflet_map(pts, "PointSelectionType")




ui <- fluidPage(
  titlePanel("Leaflet Map"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId="selected_photos", label="Selected images", value = "", placeholder = NULL)
    ),
    mainPanel(
      leafletOutput("mymap"),
      tableOutput("checker")
    )
  )
)


server <- function(input, output, session) {
  #https://groups.google.com/g/shiny-discuss/c/LWk4ZYNhsSc
  points <- pts
  points$clicked <- FALSE
  RV <- reactiveValues(points = points)
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'white',
    library = 'ion',
    markerColor = "blue"
  )
  
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     #addTiles() %>%
  #     addProviderTiles("OpenStreetMap", group = "OSM") %>%
  #     addAwesomeMarkers(data = points, lng = ~X, lat = ~Y, layerId = ~id, icon = icons)
  # })
  
  output$mymap <- renderLeaflet({
    indicator_leaflet_map(points, "PointSelectionType") %>%
      addAwesomeMarkers(layerId = ~id, icon = icons)
  })
  
  myLeafletProxy <- leafletProxy(mapId = "mymap", session)
  
  observeEvent(input$mymap_marker_click,{
    clicked_point <- input$mymap_marker_click
    RV$points[points$id==clicked_point$id,]$clicked <- !(RV$points[points$id==clicked_point$id,]$clicked)
    
    updateTextInput(inputId = "selected_photos", value = paste(unlist(RV$points$id[which(RV$points$clicked)]), collapse = ", "))
    
    removeMarker(map = myLeafletProxy, layerId = clicked_point$id)
    addAwesomeMarkers(map = myLeafletProxy,
                      lng = clicked_point$lng,
                      lat = clicked_point$lat,
                      layerId = clicked_point$id,
                      icon = awesomeIcons(
                        icon = 'ios-close',
                        iconColor = 'white',
                        library = 'ion',
                        markerColor = ifelse(RV$points[clicked_point$id,]$clicked, yes = "red", no = "blue")
                      ))
  })
  
  output$checker <- renderTable({
    #glimpse(RV$points)
    })
}

shinyApp(ui, server)
