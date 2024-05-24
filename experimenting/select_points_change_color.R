library(shiny)
library(sf)
library(leaflet)
library(geojsonsf)

getData <- function(){
  poly <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[7.207031249999999,46.97463048970666],[7.18231201171875,46.89867745059795],[7.267456054687499,46.86864162233212],[7.392425537109376,46.85831292242506],[7.529754638671874,46.86864162233212],[7.678070068359375,46.9061837801476],[7.683563232421874,46.97556750833867],[7.592926025390624,47.03082254778662],[7.371826171874999,47.01584377790821],[7.207031249999999,46.97463048970666]]]}}]}'
  
  sf_poly <- geojson_sf(poly)
  points <- st_as_sf(st_sample(sf_poly, 20))
  points$id <- 1:nrow(points)
  coords <- st_coordinates(points)
  
  df <- data.frame(st_drop_geometry(points), coords)
  return(df)
}

ui <- fluidPage(
  titlePanel("Leaflet Map"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId="selected_photos", label="Selected images", value = "", placeholder = NULL)
    ),
    mainPanel(
      leafletOutput("mymap")
    )
  )
)

server <- function(input, output, session) {
  #https://groups.google.com/g/shiny-discuss/c/LWk4ZYNhsSc
  points <- getData()
  points$clicked <- FALSE
  RV <- reactiveValues(points = points)
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'white',
    library = 'ion',
    markerColor = "blue"
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      #addTiles() %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      addAwesomeMarkers(data = points, lng = ~X, lat = ~Y, layerId = ~id, icon = icons)
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
}

shinyApp(ui, server)
