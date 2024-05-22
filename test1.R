library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(htmltools)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Leaflet with ggplot"),
  dashboardSidebar(
    # Sidebar content
  ),
  dashboardBody(
    # Body content
    fluidRow(
      column(width = 6,
             position = "relative",
             leafletOutput("map"),
             div(style = "position: absolute; top: 0; left: 75%; width: 25%; height: 25%; transform: translate(-50%, 0);", plotlyOutput("plotInfo")),
             textOutput("markerInfo")
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Sample data for markers
  data <- data.frame(
    lat = c(51.505, 51.51, 51.515),
    lng = c(-0.09, -0.1, -0.11),
    name = c("Marker 1", "Marker 2", "Marker 3"),
    info = c("Info 1", "Info 2", "Info 3")
  )
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, label = ~name, options = markerOptions(title = ~info))
  })
  
  # Render the plot and information when a marker is clicked
  observeEvent(input$map_marker_click, {
    event <- input$map_marker_click
    if (!is.null(event)) {
      marker_name <- event$id
      
      # Update marker information text
      marker_info <- data[data$name == marker_name, "info"]
      output$markerInfo <- renderText({
        paste("Info:", marker_info)
      })
      
      # Create a ggplot based on marker clicked
      p <- ggplot(data[data$name == marker_name, ], aes(x = 1:10, y = runif(10))) +
        geom_point() +
        ggtitle(marker_name)
      
      # Render the ggplot graph
      output$plotInfo <- renderPlotly({
        ggplotly(p)
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)