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
library(ggrepel)
library(calendR)


modify_calendR_source <- function() {
  # Path to the calendR source file
  source_file <- system.file("R", "calendR.R", package = "calendR")
  
  # Read the contents of the source file
  source_code <- readLines(source_file)
  
  # Replace colors with dark theme colors
  source_code <- gsub("background-color: #fff;", "background-color: #2c2c2c;", source_code)
  source_code <- gsub("color: #000;", "color: #ffffff;", source_code)
  
  # Write the modified source code back to the file
  writeLines(source_code, source_file)
}

# Apply the modifications
modify_calendR_source()


# ---- Setup stuff ----


dark_theme_general <- function() {
  tags$style(HTML("

    

    .content-wrapper, .right-side {
      background-color: #1F1F1F !important;
    }

    .sidebar-menu li a {
      color: white !important;
    }
    
  "))
}


#body {
#  background-color: #1F1F1F !important;
#    color: white !important;
#}

#.main-sidebar {
#  background-color: #333333 !important;
#}
#.sidebar-menu {
#  background-color: #333333 !important;
#}
#.sidebar-menu li.header {
#  color: #F49D37 !important;
#}

#.navbar-custom-menu .navbar-nav > li > a {
#  color: white !important;
#}
#.navbar-custom-menu .navbar-nav > li > a:hover {
#  background-color: #F89C14 !important;
#}
#.navbar {
#  background-color: #333333 !important;
#}



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

SensorDataDB$Air_Quality_Index <- con2aqi("pm25", SensorDataDB$pm2.5_10minute)
SensorDataDBHistorical$Air_Quality_Index <- con2aqi("pm25", SensorDataDBHistorical$pm2.5_10minute)
SensorDataDB$Date <- as.POSIXct(SensorDataDB$last_seen, origin = "1970-01-01", tz = "UTC")
SensorDataDBHistorical$Date <- as.POSIXct(SensorDataDBHistorical$last_seen, origin = "1970-01-01", tz = "UTC")

SensorDataDBHistorical$Date_Only <- as.Date(SensorDataDBHistorical$Date)

daily_avg_aqi <- SensorDataDBHistorical %>%
  group_by(sensor_index, Date_Only) %>%
  summarise(daily_average_aqi = mean(Air_Quality_Index, na.rm = TRUE))

#print(daily_avg_aqi)

# Notifications
notifs <- dropdownMenu(type = "notifications",
                       messageItem(from = "Sales Dept", message = "Sales are steady this month."),
                       messageItem(from = "New User", message = "How do I register?", icon = icon("question"), time = "13:45"),
                       messageItem(from = "Support", message = "The new server is ready.", icon = icon("life-ring"), time = "2014-12-01"))


# ---- UI ----
ui <- dashboardPage(
  skin = "purple",
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
    dark_theme_general(),
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
                           plotOutput("plotInfo", height = "300px", width = "325px"),
                           actionButton("go_to_analyze", "See Details", style = "margin-top: 0px;")
                  )
                ),
                tags$div(
                  selectInput("variable_select_input_map", "",
                              choices = c("Air_Quality_Index","temperature"),
                              selected = "Air_Quality_Index"),
                  style = "width: 240px; position: fixed; bottom: 160px; right: 10px;"
                )
              )
      ),
      tabItem(tabName = "analyze",
              tags$style(HTML("
              .tab-content .tab-pane {
                background-color: #1F1F1F !important;
                color: white !important;
              }
              .box {
                background-color: #333333 !important;
                border-color: #444444 !important;
              }
              .box-title {
                font-size: 24px !important; /* Larger font size */
                font-weight: bold !important; /* Bold text */
                color: white !important; /* White text color */
              }
              .form-control {
                background-color: #333333 !important;
                color: white !important;
              }
              .selectize-dropdown {
                background-color: #333333 !important;
                color: white !important;
              }
              .selectize-input {
                background-color: #333333 !important;
                color: white !important;
              }
              .btn-primary {
                background-color: #F49D37 !important;
                border-color: #F49D37 !important;
              }
              .btn-primary:hover {
                background-color: #F89C14 !important;
                border-color: #F89C14 !important;
              }
              .box-header {
                background-color: #2E056B !important; /* Dark purple background */
                color: white !important; /* White text color */
              } 
              .box-body {
                font-size: 18px !important; /* Larger font size */
              }
              
              
              
        ")),
              
              fluidRow(
                column(width = 12,
                       box(
                         title = "Introduction",
                         width = NULL,
                         solidHeader = TRUE,
                         #status = "primary",
                         "The two following graphs are here to visualize the Air Quality Index, which comprises of the density of pollutants of a certain size currently in the air. Anything above moderate is cause for concern. The graph on the left shows the current air quality as time ranges, and the graph on the right gives the daily average and shows it on a calendar."
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "What is the current Air Quality Index?", width = NULL, solidHeader = TRUE, status = "primary",
                           plotOutput("Line_Graph_Analyze", 
                           
                           
                         )
                       ),
                       box(
                         title = "Line Graph Settings", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(width = 6,
                                  selectInput("variable_select_input_line_graph_analyze", "Select Graph Variables",
                                                     choices = c("Air_Quality_Index", "pm2.5_10minute","temperature"),
                                                     selected = "Air_Quality_Index")
                           ),
                           column(width = 6,
                                  selectInput("time_select_input_line_graph_analyze", "Select Time Input",
                                              choices = c("1 day", "7 days","30 days","90 days","365 days","custom range"),
                                              selected = "30 days"),
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
                         
                         title = "Air Quality Calendar Plot", 
                         width = NULL, 
                         solidHeader = TRUE, 
                         status = "warning",
                         style = "overflow: hidden;",  # Hide overflow to prevent unnecessary scrolling
                         fluidRow(
                           column(
                             width = 12,
                             position = "relative",
                             plotOutput("Calendar_Plot_Analyze")
                           ),
                           column(
                             width = 12,
                             div(
                               style = "display: flex; justify-content: center; align-items: flex-start; flex-wrap: wrap; padding: 10px;",
                               div(
                                 style = "margin-bottom: 10px; margin-right: 10px;",
                                 div(style = "background-color: #00FF00; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"),
                                 span("Good")
                               ),
                               div(
                                 style = "margin-bottom: 10px; margin-right: 10px;",
                                 div(style = "background-color: #FFFF00; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"),
                                 span("Moderate")
                               ),
                               div(
                                 style = "margin-bottom: 10px; margin-right: 10px;",
                                 div(style = "background-color: #FFA500; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"),
                                 span("Unhealthy for Vulnerable People")
                               )
                             ),
                             div(
                               style = "display: flex; justify-content: center; align-items: flex-start; flex-wrap: wrap; padding: 10px;",
                               div(
                                 style = "margin-bottom: 10px; margin-right: 10px;",
                                 div(style = "background-color: #FF0000; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"),
                                 span("Unhealthy")
                               ),
                               div(
                                 style = "margin-bottom: 10px; margin-right: 10px;",
                                 div(style = "background-color: #800080; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"),
                                 span("Very Unhealthy")
                               ),
                               div(
                                 style = "margin-bottom: 10px; margin-right: 10px;",
                                 div(style = "background-color: #7E0023; width: 15px; height: 15px; display: inline-block; margin-right: 5px;"),
                                 span("Hazardous")
                               )
                             )
                           )
                         )
                       ),
                       box(
                         title = "Calendar Plot Settings", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(width = 6,
                                  selectInput("variable_select_input_line_calendar_analyze", "Select Calendar Variable",
                                              choices = c("Air_Quality_Index", "temperature"),
                                              selected = "Air_Quality_Index")
                           ),
                           column(width = 6,
                                  selectInput("time_select_input_line_calendar_analyze", "Select Calendar Type",
                                              choices = c("month", "year"),
                                              selected = "month")
                                  
                                  
                                  
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
    
    start_date <- as.POSIXct(Sys.Date() - 3, tz = "UTC")
    end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    
    
    #Create a ggplot based on marker clicked
    dataset <- dataset[dataset$Date >= start_date &
                         dataset$Date <= end_date, ]
    
    
    linecolor <- getColor(dataset[which.max(dataset$last_seen), input$variable_select_input_map], input$variable_select_input_map)
    
    p <- ggplot(data = dataset, aes(x = as.POSIXct(last_seen, origin = "1970-01-01", tz = "UTC"), y = .data[[input$variable_select_input_map]])) +
      geom_line(color = linecolor, size = 1.5) +
      labs(x = "Day", y = "") +
      scale_x_datetime(labels = scales::time_format("%I %p\n%b %d"), breaks = pretty_breaks(n = 5)) +
      ggtitle(paste("The Current", gsub("_", " ", input$variable_select_input_map))) +
      dark_theme() +
      theme(
        plot.title = element_text(face = "bold", size = 18),  # Make title bold
        axis.title.x = element_text(face = "bold", size = 14),  # Make x-axis title bold and adjust size
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 14),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    
    max_y_value <- max(dataset[[input$variable_select_input_map]])
    
    # Set the y-axis limits dynamically
    if (max_y_value > 100) {
      p <- p + scale_y_continuous(limits = c(0, max_y_value))
    } else {
      p <- p + scale_y_continuous(limits = c(0, 100))
    }
    
  
    
    # Define the max last_seen date for annotation positioning
    max_last_seen <- max(dataset$last_seen)
    
    # Create a data frame for the labels
    labels_df <- data.frame(
      label = c("Good", "Moderate", "Unhealthy for Vulnerable People", "Unhealthy", "Very Unhealthy", "Hazardous"),
      y = c(5, 56, 106, 156, 206, 306),
      x = as.POSIXct(max_last_seen, origin = "1970-01-01", tz = "UTC")
    )
    
    p <- p +
      geom_hline(yintercept = c(0, 51, 101, 151, 201, 301), color = c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"), size = 0.5, na.rm = TRUE) +  # Make border lines thinner
      geom_label_repel(data = labels_df, aes(x = x, y = y, label = label),
                       fill = "#17171799", color = c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"), 
                       size = 5.5, fontface = "bold", vjust = 0.45, hjust = 0, show.legend = FALSE, segment.color = NA,
                       box.padding = unit(0.2, "lines"), label.padding = unit(0.2, "lines"),
                       label.r = 0.3, na.rm = TRUE, label.size=0)
    
    
    
    output$plotInfo <- renderPlot({
      p
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
    
    if (input$variable_select_input_map == "Air_Quality_Index") {
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
      ) %>%
      
      addLegend(
        position = "bottomright",
        colors = c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"),
        labels = c("Good", "Moderate", "Unhealthy for Vulnerable People", "Unhealthy", "Very Unhealthy", "Hazardous"),
        title = "Air Quality Index"
      )
    
    
  })
  
  #startup graph (set to first var but can be changed to an average etc.)
  observe({
    hovered_lat <- SensorDataDB$latitude[2]
    hovered_lng <- SensorDataDB$longitude[2]
    
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
    
    
    dataset <- SensorDataDBHistorical[SensorDataDBHistorical$sensor_index == CurrentMapSensorId() &
                                              SensorDataDBHistorical$Date >= start_date &
                                              SensorDataDBHistorical$Date <= end_date, ]
    
    linecolor <- getColor(dataset[which.max(dataset$last_seen), input$variable_select_input_line_graph_analyze], input$variable_select_input_line_graph_analyze)
    
    p <- ggplot(data = dataset, aes(x = as.POSIXct(last_seen, origin = "1970-01-01", tz = "UTC"), y = .data[[input$variable_select_input_line_graph_analyze]])) +
      geom_line(color = linecolor, size = 1.5) +
      labs(x = "Day", y = "") +
      scale_x_datetime(labels = scales::time_format("%I %p\n%b %d"), breaks = pretty_breaks(n = 5)) +
      #ggtitle(paste("The Current", gsub("_", " ", input$variable_select_input_line_graph_analyze))) +
      ggtitle("")+
      dark_theme() +
      theme(
        plot.title = element_text(face = "bold", size = 18),  # Make title bold
        axis.title.x = element_text(face = "bold", size = 14),  # Make x-axis title bold and adjust size
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 14),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    
    max_y_value <- max(dataset[[input$variable_select_input_line_graph_analyze]])
    
    # Set the y-axis limits dynamically
    if (max_y_value > 100) {
      p <- p + scale_y_continuous(limits = c(0, max_y_value))
    } else {
      p <- p + scale_y_continuous(limits = c(0, 100))
    }
    
    
    
    # Define the max last_seen date for annotation positioning
    max_last_seen <- max(dataset$last_seen)
    
    # Create a data frame for the labels
    labels_df <- data.frame(
      label = c("Good", "Moderate", "Unhealthy for Vulnerable People", "Unhealthy", "Very Unhealthy", "Hazardous"),
      y = c(5, 56, 106, 156, 206, 306),
      x = as.POSIXct(max_last_seen, origin = "1970-01-01", tz = "UTC")
    )
    
    p <- p +
      geom_hline(yintercept = c(0, 51, 101, 151, 201, 301), color = c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"), size = 0.5, na.rm = TRUE) +  # Make border lines thinner
      geom_label_repel(data = labels_df, aes(x = x, y = y, label = label),
                       fill = "#17171799", color = c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"), 
                       size = 5.5, fontface = "bold", vjust = 0.45, hjust = 0, show.legend = FALSE, segment.color = NA,
                       box.padding = unit(0.2, "lines"), label.padding = unit(0.2, "lines"),
                       label.r = 0.3, na.rm = TRUE, label.size=0)
    
    print(p)
  })
  
  output$Calendar_Plot_Analyze <- renderPlot({
    
    # Filter the data for the current sensor
    filtered_data <- daily_avg_aqi %>%
      filter(sensor_index == CurrentMapSensorId())

    
    filtered_data$colors <- getColor(filtered_data$daily_average_aqi, "Air_Quality_Index")

    filtered_data$Day_Number <- as.numeric(format(filtered_data$Date_Only, "%d"))
    filtered_data$Year_Number <- as.numeric(format(filtered_data$Date_Only, "%j"))
    
    color_text_mapping <- c(
      "Green" = "Good",
      "Yellow" = "Moderate",
      "Orange" = "Unhealthy for\nVulnerable\nPeople",
      "Red" = "Unhealthy",
      "Purple" = "Very Unhealthy",
      "#7E0023" = "Hazardous"
    )
    
    
    
    #print(input$variable_select_input_line_calendar_analyze)
    #print(input$time_select_input_line_calendar_analyze)
    if (input$time_select_input_line_calendar_analyze == "month")
    {
      events <- rep(NA, lubridate::days_in_month(as.Date(Sys.Date())))
      
      events[filtered_data$Day_Number] <- as.character(filtered_data$Day_Number)
      
      desired_order <- as.character(filtered_data$Day_Number)
      
      ordered_colors <- filtered_data$colors[order(desired_order)]
      
      #print(events[!sapply(events, is.na)])
      p <- calendR(month = lubridate::month(Sys.Date()),  # May
                   #year = 2025,
                   special.days = events,
                   special.col = ordered_colors,
                   text.pos = as.numeric(events[!sapply(events, is.na)]),
                   text = color_text_mapping[ordered_colors],
                   text.col = "black",
                   text.size = 3.7,
                   
                   bg.img = "https://www.icolorpalette.com/download/solidcolorimage/1f1f1f_solid_color_background_icolorpalette.png",
                   
                   
                   title = "Daily rainfall in #my city in 2021",  # Change the title
                   title.size = 0,                  # Font size of the title
                   title.col = 1,                    # Color of the title
                   weeknames = c("Sun", "Mon", "Teu", "Wen", "Thu", "Fri", "Sat"), # Change week day names
                   start = "M",                     # Start the week on Monday
                   col = "#f2f2f2",                  # Color of the lines of the calendar
                   lwd = 1,                          # Width of the lines of the calendar
                   lty = 1,                          # Line type of the lines of the calendar
                   mbg.col = 4,                      # Background color of the month names
                   months.col = "white",             # Color of the text of the month names
                   font.family = "mono",             # Font family of all the texts                 
                   font.style = "bold",              # Font style of the texts except the subtitle
                   #weeknames.col = "black",          # Color of the names of the days of the week
                   #days.col = 1,                     # Color of the number of the days
                   day.size = 3.5,
                   
                   weeknames.col = "white",
                   days.col = "white",
                   
                   
                   
                   
                   
                   )                   # Size of the number of days
                   
    }
    else
    {
      events <- rep(NA, ifelse(leap_year(year(Sys.Date())), 366, 365))
      #print(filtered_data$Year_Number)
      
      events[filtered_data$Year_Number] <- as.character(filtered_data$Year_Number)
      
      desired_order <- as.character(filtered_data$Year_Number)
      
      ordered_colors <- filtered_data$colors[order(desired_order)]
      
      
      p <- calendR(
                   special.days = events,
                   special.col = ordered_colors,
                   
                   
                   
                   title = "Daily rainfall in #my city in 2021",  # Change the title
                   title.size = 0,                  # Font size of the title
                   title.col = 1,                    # Color of the title
                   weeknames = c("S", "M", "T", "W", "T", "F", "S"), # Change week day names
                   start = "M",                     # Start the week on Monday
                   col = "#f2f2f2",                  # Color of the lines of the calendar
                   lwd = 0.5,                          # Width of the lines of the calendar
                   lty = 1,                          # Line type of the lines of the calendar
                   mbg.col = "#2E056B",                      # Background color of the month names
                   
                   
                   bg.img = "https://www.icolorpalette.com/download/solidcolorimage/1f1f1f_solid_color_background_icolorpalette.png",
                   
                   months.col = "white",             # Color of the text of the month names
                   font.family = "mono",             # Font family of all the texts                 
                   font.style = "bold",              # Font style of the texts except the subtitle
                   #weeknames.col = "black",          # Color of the names of the days of the week
                   #days.col = 1,                     # Color of the number of the days
                   day.size = 3,       # Size of the number of days
                   weeknames.col = "white",
                   days.col = "white")
                      
                   
    }
    
    
    
    print(p)
    
  })
  

  

  
  
  
}

shinyApp(ui, server)