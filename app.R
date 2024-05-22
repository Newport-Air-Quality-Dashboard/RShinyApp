library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(leaflet.extras)
library(htmltools)
library(plotly)
library(scales)
library(con2aqi)
library(reshape2)


# ---- Setup stuff ----
  #graph theme
  grafana_colors <- c("#F49D37", "#1388A2", "#FF5983", "#6F257F", "#51D6AB")
  dark_theme <- function() {
    theme_minimal() +
      theme(
        # Dark background
        panel.background = element_rect(fill = "#1F1F1F"),
        # Dark grid lines
        panel.grid.major = element_line(color = "#505050"),
        panel.grid.minor = element_blank(),
        # White axes
        axis.line = element_line(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        # White plot background
        plot.background = element_rect(fill = "#1F1F1F"),
        # White plot border
        panel.border = element_rect(color = "white", fill = NA),
        # White legend text
        legend.text = element_text(color = "white"),
        # White legend background
        legend.background = element_rect(fill = "#1F1F1F", color = NA),
        # White strip text
        strip.text = element_text(color = "white"),
        # White title text
        plot.title = element_text(color = "white")
      )
  }
  
  # Load data
  SensorDataDB <- read.csv("sensor_data_current.csv")
  SensorDataDBHistorical <- read.csv("sensor_data_historical.csv")
  
  SensorDataDB$AQI_2.5_10_Minute_Average <- con2aqi("pm25", SensorDataDB$pm2.5_10minute)
  SensorDataDBHistorical$AQI_2.5_10_Minute_Average <- con2aqi("pm25", SensorDataDBHistorical$pm2.5_10minute)
  SensorDataDB$Date <- as.POSIXct(SensorDataDB$last_seen, origin = "1970-01-01", tz = "UTC")
  SensorDataDBHistorical$Date <- as.POSIXct(SensorDataDBHistorical$last_seen, origin = "1970-01-01", tz = "UTC")
  
  
  # Notifications
  notifs <- dropdownMenu(type = "notifications",
                         messageItem(from = "Sales Dept", message = "Sales are steady this month."),
                         messageItem(from = "New User", message = "How do I register?", icon = icon("question"), time = "13:45"),
                         messageItem(from = "Support", message = "The new server is ready.", icon = icon("life-ring"), time = "2014-12-01"))
  

# ---- UI ----
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Air Quality Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      HTML(paste0(
        "<br>",
        "<a href='https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/nkunorse.com/images/responsive/logo_main.svg' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/nkunorse.com/images/responsive/logo_main.svg' width = '186'></a>",
        "<br>"
      )),
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Air Quality Map", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Analyze Location", tabName = "analyze", icon = icon("table")),
      menuItem("Compare Locations", tabName = "compare", icon = icon("random", lib = "glyphicon")),
      menuItem("Help", tabName = "help", icon = icon("thumbtack"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/draggabilly/2.3.0/draggabilly.pkgd.min.js"),
      tags$script(HTML('
        $(document).ready(function() {
          var elem = document.getElementById("draggableContainer");
          var draggie = new Draggabilly(elem);
        });
      ')),
      tags$style(HTML("
        .leaflet-container {
          background-color: #000;
          position: fixed;
          top: 0;
          bottom: 0;
          left: 0;
          right: 0;
          height: 100vh !important;
          width: 100% !important;
        }
        .selectize-dropdown {
          bottom: 100% !important;
          top: auto !important;
        }
        
        #draggableContainer {
          width: 100px;
          height: 100px;
          position: relative;
          background-color: rgba(255, 255, 255, 0.8);
          padding: 0px;
          border-radius: 0px;
          
        #go_to_analyze {
          background-color: #1F1F1F;
          color: #F89C14;
          border: 1px solid #F89C14;
          padding: 5px 10px;
          border-radius: 4px;
          font-size: 14px;
          font-weight: bold;
        }
        #go_to_analyze:hover {
          background-color: #6f6f6f;
        }
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                column(
                  width = 6,
                  position = "relative",
                  leafletOutput("AQMap"),
                  tags$div(id = "draggableContainer",
                           plotlyOutput("plotInfo", height = "250px", width = "250px"),
                           actionButton("go_to_analyze", "See Details", style = "margin-top: 0px;")
                  )
                ),
                tags$div(
                  selectInput("variable_select_input_map", "",
                              choices = c("AQI_2.5_10_Minute_Average", "pm2.5_10minute","temperature"),
                              selected = "AQI_2.5_10_Minute_Average"),
                  style = "width: 200px; position: fixed; bottom: 10px; right: 10px;"
                )
              )
      ),
      tabItem(tabName = "analyze",
              fluidRow(
                column(width = 6,
                       box(
                         title = "Line Graph", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("Line_Graph_Analyze")
                       ),
                       box(
                         title = "Line Graph Settings", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(width = 6,
                                  checkboxGroupInput("variable_select_input_line_graph_analyze", "Select Graph Variables",
                                          choices = c("AQI_2.5_10_Minute_Average", "pm2.5_10minute","temperature"),
                                          selected = "AQI_2.5_10_Minute_Average")
                           ),
                           column(width = 6,
                                  selectInput("time_select_input_line_graph_analyze", "Select Time Input",
                                                     choices = c("1 day", "7 days","30 days","90 days","365 days","custom range"),
                                                     selected = "7 days"),
                                  conditionalPanel(
                                    condition = "input.time_select_input_line_graph_analyze == 'custom range'",
                                    dateRangeInput("custom_date_range", "Select Date Range")
                                  )
              
                                  
                                  
                           )
                         )
                       )
                       
                       

                ),
                  column(width = 6,
                         box(
                           title = "Calendar Plot", width = NULL, solidHeader = TRUE, status = "warning",
                           plotOutput("Calendar_Plot_Analyze")
                         ),
                         box(
                           title = "Calendar Plot Settings", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                           fluidRow(
                             column(width = 6,
                                    selectInput("variable_select_input_line_calendar_analyze", "Select Calendar Variable",
                                                       choices = c("AQI_2.5_10_Minute_Average", "pm2.5_10minute","temperature"),
                                                       selected = "AQI_2.5_10_Minute_Average")
                             ),
                             column(width = 6,
                                    selectInput("time_select_input_line_calendar_analyze", "Select Calendar Type",
                                                choices = c("7 days","30 days","90 days","365 days"),
                                                selected = "30 days")
                                    
                                    
                                    
                             )
                           )
                         )
                         
                         
                         
                  )
              )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # ---- Maps ----
  
  TargetDataMap <- reactive({
    SensorDataDB[[input$variable_select_input_map]]
  })
  
  CurrentMapSensorId <- reactiveVal(0)
  
  
  
  
  twentyFourHourGraph <- function(dataset){
    CurrentMapSensorId(first(dataset$sensor_index))
    
    start_date <- as.POSIXct(Sys.Date() - 1, tz = "UTC")
    end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)

    
    #Create a ggplot based on marker clicked
    dataset <- dataset[dataset$Date >= start_date &
                         dataset$Date <= end_date, ]
    
    
    p <- ggplot(data = dataset, aes(x = as.POSIXct(last_seen, origin = "1970-01-01", tz = "UTC"), y = .data[[input$variable_select_input_map]])) +
      geom_line(color = grafana_colors[1]) +
      labs(x = "Time (Hours)", y = gsub("_", " ", input$variable_select_input_map)) +
      scale_x_datetime(breaks = date_breaks("4 hours"), labels = date_format("%I %p")) +
      ggtitle("24 Hour Plot") +
      dark_theme()
    
    output$plotInfo <- renderPlotly({
      ggplotly(p, config = list(scrollZoom = FALSE, displayModeBar = FALSE))
    })
    
  }
  
  addMarkerAnimation <- function(dataset){
    leafletProxy("AQMap") %>%
      clearGroup("markers2") %>%
      addCircleMarkers(
        data = dataset,
        lng = ~longitude,
        lat = ~latitude,
        stroke = TRUE, 
        fillOpacity = 0.85,
        group = "markers2",
        radius = 17, # Increase radius on hover
        color = getColor(dataset[[input$variable_select_input_map]], input$variable_select_input_map)
      )
  }
  
  getColor <- function(var, inputVar) {
    
    if (input$variable_select_input_map == "AQI_2.5_10_Minute_Average") {
      ifelse(var <= 50, "Green",
             ifelse(var <= 100, "Yellow",
                    ifelse(var <= 150, "Orange",
                           ifelse(var <= 200, "Red",
                                  ifelse(var <= 300, "Purple", "#7E0023")))))
    } else if (input$variable_select_input_map == "pm2.5_10minute") {
      ifelse(var <= 12.0, "Green",
             ifelse(var <= 35.4, "Yellow",
                    ifelse(var <= 55.4, "Orange",
                           ifelse(var <= 150.4, "Red",
                                  ifelse(var <= 250.4, "Purple", "#7E0023")))))
    } else if (input$variable_select_input_map == "temperature"){
      ifelse(var < 0, "Blue",
             ifelse(var <= 32, "LightBlue",
                    ifelse(var <= 50, "LightGreen",
                           ifelse(var <= 70, "Green",
                                  ifelse(var <= 90, "Yellow",
                                         ifelse(var <= 100, "Orange", "Red"))))))
    }
  }
  
  # Create leaflet map object
  output$AQMap <- renderLeaflet({
    leaflet(SensorDataDB) %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = getColor(TargetDataMap(), input$variable_select_input_map),
        radius = 15,
        stroke = FALSE, 
        fillOpacity = 0.85,
        
        
      ) %>%
      addLabelOnlyMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = TargetDataMap(),
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, style = list(
          "color" = "black",
          "font-size" = "10px"
          #"border-color" = "white"
        )),
        group = "labelMarkers"
      )
  })
  
  #startup graph (set to first var but can be changed to an average etc.)
  observe({
    hovered_lat <- first(SensorDataDB$latitude)
    hovered_lng <- first(SensorDataDB$longitude)
    
    lat_lng_filter <- SensorDataDB$latitude == hovered_lat & SensorDataDB$longitude == hovered_lng
    marker_data <- SensorDataDB[lat_lng_filter, ]
    
    lat_lng_filter_historical <- SensorDataDBHistorical$latitude == hovered_lat & SensorDataDBHistorical$longitude == hovered_lng
    historical_data <- SensorDataDBHistorical[lat_lng_filter_historical, ]
    
    #Add Plot and Marker Animation
    twentyFourHourGraph(historical_data)
    addMarkerAnimation(marker_data)
  })
  
  # Update map when zoom level changes
  observe({
    zoom <- input$AQMap_zoom
    if (!is.null(zoom)) {
      if (zoom < 13) {
        leafletProxy("AQMap", session) %>%
          hideGroup("labelMarkers")
      } else {
        leafletProxy("AQMap", session) %>%
          showGroup("labelMarkers")
      }
    }
  })
  
  #Load Graph and Animation When Markers are clicked
  observeEvent(input$AQMap_marker_click, {
    hovered_lat <- input$AQMap_marker_click$lat
    hovered_lng <- input$AQMap_marker_click$lng
    
    lat_lng_filter <- SensorDataDB$latitude == hovered_lat & SensorDataDB$longitude == hovered_lng
    marker_data <- SensorDataDB[lat_lng_filter, ]
    
    lat_lng_filter_historical <- SensorDataDBHistorical$latitude == hovered_lat & SensorDataDBHistorical$longitude == hovered_lng
    historical_data <- SensorDataDBHistorical[lat_lng_filter_historical, ]
    
    #Add Plot and Marker Animation
    twentyFourHourGraph(historical_data)
    addMarkerAnimation(marker_data)
    
    
  })
  
  #Handle When the Graph Button is Clicked
  observeEvent(input$go_to_analyze, {
    updateTabItems(session, "tabs", selected = "analyze")
    print(CurrentMapSensorId)
  })
  
  # ---- Analyze Graph ----
  
  output$Line_Graph_Analyze <- renderPlot({

    if (input$time_select_input_line_graph_analyze == "custom range" && !is.null(input$custom_date_range)) {
      start_date <- input$custom_date_range[1]
      end_date <- input$custom_date_range[2]
    } else {
      time_input <- switch(input$time_select_input_line_graph_analyze,
                           "1 day" = 1,
                           "7 days" = 7,
                           "30 days" = 30,
                           "90 days" = 90,
                           "365 days" = 365)
      start_date <- as.POSIXct(Sys.Date() - days(time_input), tz = "UTC")
      end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    }
    
    
    #start_date <- as.POSIXct(start_date, tz = "UTC")
    #end_date <- as.POSIXct(end_date, tz = "UTC")
    # Now filter the data
    filtered_data <- SensorDataDBHistorical[SensorDataDBHistorical$sensor_index == CurrentMapSensorId() &
                                              SensorDataDBHistorical$Date >= start_date &
                                              SensorDataDBHistorical$Date <= end_date, ]
    
    # Reshape data for ggplot2
    melted_data <- melt(filtered_data, id = c("Date", "sensor_index", "name", "latitude", "longitude"))
    melted_data <- melted_data[melted_data$variable %in% input$variable_select_input_line_graph_analyze, ]
    
    custom_date_format <- function(start_date, end_date) {
      # Calculate the difference in days between start_date and end_date
      date_range <- end_date - start_date
      num_days <- as.numeric(date_range, units = "days")
      
      if (num_days < 7) {
        # If the number of days is less than 4, format with hours and minutes
        return(date_format("%I %p\n%b %d"))
      } else {
        # If the number of days is 4 or more, format with month and day
        return(date_format("%b %d"))
      }
    }
    
    
    
    # Plot the filtered data using ggplot2
    p <- ggplot(melted_data, aes(x = Date, y = value, color = variable)) +
      geom_line() +
      scale_color_manual(values = grafana_colors) +
      labs(x = element_blank(), y = element_blank()) +
      scale_x_datetime(labels = custom_date_format(start_date, end_date))
    
    p + theme(legend.position = "bottom",  # Position legend below the plot
              legend.direction = "horizontal") 
  })
  
  output$Calendar_Plot_Analyze <- renderPlot({
    
    # Now filter the data
    filtered_data <- SensorDataDBHistorical[SensorDataDBHistorical$sensor_index == CurrentMapSensorId(), ]
    
    # Determine the calendar type based on selected time period
    calendar_type <- switch(input$time_select_input_line_calendar_analyze,
                            "7 days" = "7D", "30 days" = "30D", "90 days" = "90D", "365 days" = "365D")
    
    # Filter data based on selected time period
    filtered_data <- filtered_data %>%
      filter(Date >= Sys.Date() - lubridate::duration(calendar_type))
    
    # Create the calendar plot
    p <- ggplot(filtered_data, aes(x = factor(wday(Date, label = TRUE)), y = week(Date), fill = !!sym(input$variable_select_input_line_calendar_analyze))) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = getColor(filtered_data[[input$variable_select_input_line_calendar_analyze]], input$variable_select_input_line_calendar_analyze), na.value = "grey50") +
      labs(x = NULL, y = NULL) +
      facet_wrap(~ month(Date, label = TRUE), ncol = 1) +
      theme_minimal() +
      theme(panel.spacing = unit(0, "lines"))  # Remove spacing between facets
    
    print(p)
  })
  
  
}

shinyApp(ui, server)