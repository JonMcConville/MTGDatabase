library(shiny)
library(leaflet)

# Define locations
locations <- data.frame(
  city = c("San Francisco", "Los Angeles", "San Diego", "Monterey Bay", "Las Vegas"),
  lat = c(37.7749, 34.0522, 32.7157, 36.6002, 36.1699),
  lon = c(-122.4194, -118.2437, -117.1611, -121.8947, -115.1398)
)

# Define routes with estimated driving times
routes <- data.frame(
  from = c("San Francisco", "San Francisco", "Los Angeles", "Los Angeles", "San Diego"),
  to = c("Los Angeles", "Monterey Bay", "San Diego", "Las Vegas", "Las Vegas"),
  time = c("6 hrs", "2 hrs", "2 hrs", "4 hrs", "5 hrs"),
  lat1 = c(37.7749, 37.7749, 34.0522, 34.0522, 32.7157),
  lon1 = c(-122.4194, -122.4194, -118.2437, -118.2437, -117.1611),
  lat2 = c(34.0522, 36.6002, 32.7157, 36.1699, 36.1699),
  lon2 = c(-118.2437, -121.8947, -117.1611, -115.1398, -115.1398)
)

# UI
ui <- fluidPage(
  leafletOutput("map")
)

# Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = locations, ~lon, ~lat, popup = ~city) %>%
      addPolylines(data = routes, ~lon1, ~lat1, ~lon2, ~lat2, color = "blue") %>%
      addLabelOnlyMarkers(data = routes, ~((lon1 + lon2) / 2), ~((lat1 + lat2) / 2),
                          label = ~time, labelOptions = labelOptions(noHide = TRUE))
  })
}

# Run the app
shinyApp(ui, server)