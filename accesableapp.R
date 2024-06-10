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
library(ggiraph)


# Load data
SensorDataDB <- read.csv("sensor_data_current.csv")
SensorDataDBHistorical <- read.csv("sensor_data_historical.csv")

SensorDataDB$Air_Quality_Index <- con2aqi("pm25", SensorDataDB$pm2.5_10minute)
SensorDataDBHistorical$Air_Quality_Index <- con2aqi("pm25", SensorDataDBHistorical$pm2.5_10minute)
SensorDataDB$Date <- as.POSIXct(SensorDataDB$last_seen, origin = "1970-01-01", tz = "UTC")
SensorDataDBHistorical$Date <- as.POSIXct(SensorDataDBHistorical$last_seen, origin = "1970-01-01", tz = "UTC")

SensorDataDBHistorical$Date_Only <- as.Date(SensorDataDBHistorical$Date)



SensorDataDBHistorical <- SensorDataDBHistorical %>%
  rename("Temperature_(f)" = temperature)

SensorDataDB <- SensorDataDB %>%
  rename("Temperature_(f)" = temperature)

cutoff_date <- Sys.time() - days(3)
SensorDataDB <- SensorDataDB %>%
  filter(Date >= cutoff_date)


#select the inputs

getVarList <- function() {
  return(c("Air_Quality_Index", "Temperature_(f)"))
}

getColor <- function(var, inputVar) {
  
  if (inputVar == "Air_Quality_Index") {
    ifelse(var <= 50, "Green",
           ifelse(var <= 100, "Yellow",
                  ifelse(var <= 150, "Orange",
                         ifelse(var <= 200, "Red",
                                ifelse(var <= 300, "Purple", "#7E0023")))))
  } else if (inputVar == "Temperature_(f)"){
    ifelse(var < 0, "Blue",
           ifelse(var <= 32, "LightBlue",
                  ifelse(var <= 50, "LightGreen",
                         ifelse(var <= 70, "Green",
                                ifelse(var <= 90, "Yellow",
                                       ifelse(var <= 100, "Orange", "Red"))))))
  }
}

getBackgroundBarsTemplate <- function(input) {
  
  if (input == "Air_Quality_Index") {
    background_bars_template <- data.frame(
      var = "Air_Quality_Index",
      label = c("Good", "Moderate", "Unhealthy for Vulnerable People", "Unhealthy", "Very Unhealthy", "Hazardous"),
      ymin = rep(c(0, 50, 100, 150, 200, 300)),
      ymax = rep(c(50, 100, 150, 200, 300, 1000)),
      fill = rep(c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"))
    )
  } else if (input == "Temperature_(f)"){
    background_bars_template <- data.frame(
      var = "Temperature_(f)",
      label = c("Freezing", "Cold", "Cool", "Warm", "Hot", "Extremly Hot"),
      ymin = rep(c(0, 32, 50, 70, 90, 100)),
      ymax = rep(c(32, 50, 70, 90, 100, 1000)),
      fill = rep(c("LightBlue", "LightGreen", "Green", "Yellow", "Orange", "red"))
    )
  }
  
  return(background_bars_template)
}














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
        #AQMapContainer .leaflet-container {
          background-color: #000;
          position: fixed;
          top: 0;
          bottom: 0;
          left: 0;
          right: 0;
          height: 100vh !important;
          width: 100% !important;
        }
        
        #AQMapCompareContainer .leaflet-container {
          background-color: #000;
          
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
                  div(id = "AQMapContainer", leafletOutput("AQMap")),
                  tags$div(id = "draggableContainer",
                           plotOutput("plotInfo", height = "300px", width = "325px"),
                           actionButton("go_to_analyze", "See Details", style = "margin-top: 0px;")
                  )
                ),
                tags$div(
                  selectInput("variable_select_input_map", "",
                              choices = getVarList(),
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
                         "The two following graphs are here to visualize the Air Quality Index, which comprises of the density of pollutants of a certain size currently in the air. Anything above moderate is cause for concern. The first graph shows the current air quality as time ranges, and the second graph gives the daily average and shows it on a calendar."
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = uiOutput("LineGraphTitle"), width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("Line_Graph_Analyze", 
                                    
                                    
                         )
                       ),
                       box(
                         
                         title = "Line Graph Settings", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(width = 6,
                                  selectInput("variable_select_input_line_graph_analyze", "Select Graph Variables",
                                              choices = getVarList(),
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
                         
                         title = uiOutput("CalendarPlotTitle"), 
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
                             uiOutput("legend_output")
                           )
                         )
                       ),
                       box(
                         title = "Calendar Plot Settings", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(width = 6,
                                  selectInput("variable_select_input_line_calendar_analyze", "Select Calendar Variable",
                                              choices = getVarList(),
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
      ),
      tabItem(tabName = "compare",
              fluidRow(
                column(
                  width = 8,
                  box(
                    width = NULL, 
                    div(id = "AQMapCompareContainer", leafletOutput("AQMapCompare"))
                  )
                ),
                column(
                  width = 4,
                  h2("Sensor Lists"),
                  div(
                    box(
                      width = NULL, 
                      id = "listBoxes",
                      uiOutput("sensorList1UI"),
                      uiOutput("sensorList2UI")
                    ),
                    box(
                      width = NULL, 
                      checkboxInput("averageCheckboxCompare", "Average Values in Graph", value = FALSE)
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Sensor List One Line Graph", 
                    width = NULL, 
                    solidHeader = TRUE, 
                    status = "warning",
                    style = "overflow: hidden;",  # Hide overflow to prevent unnecessary scrolling
                    fluidRow(
                      column(
                        width = 12,
                        position = "relative",
                        girafeOutput("SensorListOneLinePlot")
                      )
                    )
                  )
                  
                ),
                column(
                  width = 6,
                  box(
                    title = "Sensor List Two Line Graph", 
                    width = NULL, 
                    solidHeader = TRUE, 
                    status = "warning",
                    style = "overflow: hidden;",  # Hide overflow to prevent unnecessary scrolling
                    height = "100%",  # Set the height of the box to 100% of its container
                    fluidRow(
                      column(
                        width = 12,
                        position = "relative",
                        girafeOutput("SensorListTwoLinePlot")  # Set the height of the plotly output to 100%
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
    FilteredSensorDataDB <- SensorDataDB %>%
      filter(!is.na(.[[input$variable_select_input_map]]))
    
    FilteredSensorDataDB[[input$variable_select_input_map]]
  })
  
  CurrentMapSensorId <- reactiveVal(0)
  
  
  
  
  twentyFourHourGraph <- function(dataset){
    CurrentMapSensorId(first(dataset$sensor_index))
    #print(dataset)
    start_date <- as.POSIXct(Sys.Date() - 3, tz = "UTC")
    end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    
    tryCatch({
      p <- GraphPlot(dataset, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_input_map))
    }, error = function(e) {
      print(paste("An error occurred:", e$message))
      return(NA)
    })
    
    
    
    
    output$plotInfo <- renderGirafe({
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
  
  
  
  # Create leaflet map object
  output$AQMap <- renderLeaflet({
    
    
    FilteredSensorDataDB <- SensorDataDB %>%
      filter(!is.na(.[[input$variable_select_input_map]]))
    
    
    
    leaflet(FilteredSensorDataDB) %>%
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
        
        
        colors = getBackgroundBarsTemplate(input$variable_select_input_map)$fill,
        labels = getBackgroundBarsTemplate(input$variable_select_input_map)$label,
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
  output$Line_Graph_Analyze <- renderGirafe({
    
    
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
    
    dataset <- SensorDataDBHistorical[SensorDataDBHistorical$sensor_index == CurrentMapSensorId(), ]
    
    
    print(GraphPlot(dataset, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_input_line_graph_analyze)))
    
    
  })
  
  output$LineGraphTitle <- renderUI({
    paste("What is the current", gsub("_", " ", input$variable_select_input_line_graph_analyze), "?")
  })
  
  output$CalendarPlotTitle <- renderUI({
    paste(gsub("_", " ", input$variable_select_input_line_calendar_analyze), "Calendar Plot")
    
  })
  
  GraphPlot <- function(dataset, start_date, end_date, background_bars)
  {
    
    dataset <- dataset[dataset$Date >= start_date &
                         dataset$Date <= end_date, ]
    
    start_date <- min(dataset$Date)
    end_date <- max(dataset$Date)
    
    
    #print(dataset[[input$variable_select_input_line_graph_analyze]]))
    
    #print(first(background_bars$var))
    linecolor <- getColor(dataset[which.max(dataset$Date), first(background_bars$var)], first(background_bars$var))
    
    max_y_value <- max(dataset[[input$variable_select_input_line_graph_analyze]])
    
    # Set the y-axis limits dynamically
    if (max_y_value > 101) {
      p <- p + scale_y_continuous(limits = c(0, max_y_value))
      scaletop <- max_y_value
    } else {
      p <- p + scale_y_continuous(limits = c(0, 101))
      scaletop <- 101
    }
    
    background_bars <- background_bars[background_bars$ymin <= scaletop - 6, ]
    
    
    
    p <- ggplot() +
      
      
      
      geom_rect(data = background_bars, 
                aes(xmin = start_date, xmax = end_date, ymin = ymin, ymax = ymax, fill = fill), 
                color = NA, alpha = 0.08) +
      scale_fill_identity() +
      scale_x_datetime(labels = scales::time_format("%I %p\n%b %d"), breaks = pretty_breaks(n = 5)) +
      
      geom_line(data = dataset, aes(x = Date, y = .data[[first(background_bars$var)]]), color = linecolor, size = 1.5) + 
      
      
      geom_hline(yintercept = background_bars$ymin, color = background_bars$fill, size = 0.5, na.rm = TRUE) +  # Make border lines thinner
      geom_label_repel(aes(x = max(dataset$Date), y = background_bars$ymin + 4, label = background_bars$label),
                       fill = "#17171799", color = background_bars$fill, 
                       size = 5.5, fontface = "bold", vjust = 0.45, hjust = 0, show.legend = FALSE, segment.color = NA,
                       box.padding = unit(0.2, "lines"), label.padding = unit(0.2, "lines"),
                       label.r = 0.3, na.rm = TRUE, label.size=0) +
      
      
      
      labs(x = "Day", y = "") +
      
      ggtitle(paste("Current", gsub("_", " ", first(background_bars$var))))+
      dark_theme() +
      theme(
        plot.title = element_text(face = "bold", size = 18),  # Make title bold
        axis.title.x = element_text(face = "bold", size = 14),  # Make x-axis title bold and adjust size
        #axis.title.y = element_text(face = "bold", size = 12),  # Make y-axis title bold and adjust size
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 14),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    return(p)
  }
  
  # ---- Calendar Plot ----
  
  output$Calendar_Plot_Analyze <- renderPlot({
    
    
    CurrentMonth = lubridate::month(Sys.Date())
    CurrentYear = lubridate::year(Sys.Date())
    
    if (input$time_select_input_line_calendar_analyze == "month")
    {
      filtered_SensorDataDBHistorical <- SensorDataDBHistorical %>%
        filter(year(Date_Only) == CurrentYear & month(Date_Only) == CurrentMonth)
    }
    else
    {
      filtered_SensorDataDBHistorical <- SensorDataDBHistorical %>%
        filter(year(Date_Only) == CurrentYear)
    }
    
    
    VariableFilter <- filtered_SensorDataDBHistorical %>%
      group_by(sensor_index, Date_Only) %>%
      summarise(VariableFilter = mean(!!sym(input$variable_select_input_line_calendar_analyze), na.rm = TRUE))
    
    # Filter the data for the current sensor
    filtered_data <- VariableFilter %>%
      filter(sensor_index == CurrentMapSensorId())
    
    
    
    
    filtered_data$colors <- getColor(filtered_data$VariableFilter, input$variable_select_input_line_calendar_analyze)
    
    #print(filtered_data$colors)
    
    filtered_data$Day_Number <- as.numeric(format(filtered_data$Date_Only, "%d"))
    filtered_data$Year_Number <- as.numeric(format(filtered_data$Date_Only, "%j"))
    
    legendData <- getBackgroundBarsTemplate(input$variable_select_input_line_calendar_analyze)
    
    
    color_names <- legendData$fill
    text_values <- legendData$label
    
    # Truncate text values longer than 8 characters
    text_values <- substr(text_values, 1, 6)
    
    # Combine color and text into a named vector
    color_text_mapping <- setNames(text_values, color_names)
    
    
    
    if (input$time_select_input_line_calendar_analyze == "month")
    {
      
      
      
      events <- rep(NA, lubridate::days_in_month(as.Date(Sys.Date())))
      
      events[filtered_data$Day_Number] <- as.character(sprintf("%04d", filtered_data$Day_Number))
      
      desired_order <- as.character(filtered_data$Day_Number)
      
      ordered_colors <- filtered_data$colors[order(desired_order)]
      
      
      #print(events[!sapply(events, is.na)])
      p <- calendR(month = CurrentMonth,  # May
                   #year = 2025,
                   special.days = events,
                   special.col = adjustcolor(ordered_colors, alpha.f = 0.3),
                   text.pos = as.numeric(events[!sapply(events, is.na)]),
                   text = color_text_mapping[ordered_colors],
                   text.col = "white",
                   text.size = 5,  # Increase text size
                   
                   
                   bg.img = "custom_background.png",
                   
                   
                   title = "Daily rainfall in #my city in 2021",  # Change the title
                   title.size = 0,                  # Font size of the title
                   title.col = 1,                    # Color of the title
                   weeknames = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), # Change week day names
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
        special.col = adjustcolor(ordered_colors, alpha.f = 0.6),
        
        
        
        title = "Daily rainfall in #my city in 2021",  # Change the title
        title.size = 0,                  # Font size of the title
        title.col = 1,                    # Color of the title
        weeknames = c("S", "M", "T", "W", "T", "F", "S"), # Change week day names
        start = "M",                     # Start the week on Monday
        col = "#f2f2f2",                  # Color of the lines of the calendar
        lwd = 0.5,                          # Width of the lines of the calendar
        lty = 1,                          # Line type of the lines of the calendar
        mbg.col = "#2E056B",                      # Background color of the month names
        
        bg.img = "custom_background.png",
        
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
  
  generate_legend <- function(text_list, color_list) {
    # Initialize the legend HTML with top padding
    legend_html <- "<div style='padding-top: 10px; display: flex; flex-wrap: wrap;'>"
    
    # Iterate over each text and color pair
    for (i in seq_along(text_list)) {
      # Generate HTML for each legend item
      legend_item_html <- paste0(
        "<div style='margin-bottom: 10px; margin-right: 10px;'>",
        "<div style='background-color: ", color_list[i], "; width: 15px; height: 15px; display: inline-block; margin-right: 5px;'></div>",
        "<span>", text_list[i], "</span>",
        "</div>"
      )
      
      # Append the legend item HTML to the legend HTML
      legend_html <- paste0(legend_html, legend_item_html)
    }
    
    # Close the legend HTML
    legend_html <- paste0(legend_html, "</div>")
    
    # Return the legend HTML
    return(legend_html)
  }
  
  
  
  
  # Render the legend HTML in the UI
  output$legend_output <- renderUI({
    
    legendData <- getBackgroundBarsTemplate(input$variable_select_input_line_calendar_analyze)
    
    legend_html <- generate_legend(legendData$label, legendData$fill)
    HTML(legend_html)
  })
  
  
  
  # ---- CompareMap ----
  
  
  
  output$AQMapCompare <- renderLeaflet({
    FilteredSensorDataDB <- SensorDataDB %>%
      filter(!is.na(.[["Air_Quality_Index"]]))
    
    
    leaflet(FilteredSensorDataDB) %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = getColor(TargetDataMap(), "Air_Quality_Index"),
        radius = 15,
        stroke = FALSE, 
        fillOpacity = 0.85,
        layerId = ~sensor_index
      ) %>%
      addLabelOnlyMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = TargetDataMap(),
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, style = list(
          "color" = "black",
          "font-size" = "10px"
        )),
        group = "labelMarkers"
      ) %>%
      addLabelOnlyMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = ~name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          offset = c(0, -10),
          textOnly = FALSE,
          style = list(
            "color" = "white",
            "font-size" = "8px",
            "background-color" = "rgba(0, 0, 0, 0.5)", # Semi-transparent black background
            "border" = "1px solid white",
            "padding" = "5px"
          )
        ),
        group = "labelMarkers2"
      )
  })
  
  observe({
    
    zoom <- input$AQMapCompare_zoom
    if (!is.null(zoom)) {
      if (zoom < 12) {
        leafletProxy("AQMapCompare", session) %>%
          hideGroup("labelMarkers")
      } else {
        leafletProxy("AQMapCompare", session) %>%
          showGroup("labelMarkers")
      }
    }
    
    
    
    zoom2 <- input$AQMapCompare_zoom
    if (!is.null(zoom2)) {
      if (zoom2 < 14) {
        leafletProxy("AQMapCompare", session) %>%
          hideGroup("labelMarkers2")
      } else {
        leafletProxy("AQMapCompare", session) %>%
          showGroup("labelMarkers2")
      }
    }
    
    
  })
  
  
  
  
  selectedSensor <- reactiveVal(NULL)
  
  sensorLists <- reactiveValues(
    list1 = character(),
    list2 = character()
  )
  
  # Observe map clicks
  observeEvent(input$AQMapCompare_marker_click, {
    clickedSensorId <- input$AQMapCompare_marker_click$id
    clickedSensor <- SensorDataDB %>% filter(sensor_index == clickedSensorId) %>% pull(name)
    selectedSensor(clickedSensor)
    showModal(modalDialog(
      title = "Select List",
      "Add sensor to which list?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("addList1", "List 1"),
        actionButton("addList2", "List 2")
      )
    ))
  })
  
  observeEvent(input$addList1, {
    isolate({
      sensor <- selectedSensor()
      if (!is.null(sensor) && !sensor %in% sensorLists$list1) {
        sensorLists$list1 <- unique(c(sensorLists$list1, sensor))
      }
      removeModal()
    })
  })
  
  observeEvent(input$addList2, {
    isolate({
      sensor <- selectedSensor()
      if (!is.null(sensor) && !sensor %in% sensorLists$list2) {
        sensorLists$list2 <- unique(c(sensorLists$list2, sensor))
      }
      removeModal()
    })
  })
  
  # Render the sensor lists
  output$sensorList1UI <- renderUI({
    tagList(
      h4("List 1"),
      tags$ul(
        lapply(sensorLists$list1, function(sensor) {
          tags$li(
            sensor,
            actionButton(paste0("removeList1_", sensor), "x", class = "removeBtn")
          )
        })
      )
    )
  })
  
  output$sensorList2UI <- renderUI({
    tagList(
      h4("List 2"),
      tags$ul(
        lapply(sensorLists$list2, function(sensor) {
          tags$li(
            sensor,
            actionButton(paste0("removeList2_", sensor), "x", class = "removeBtn")
          )
        })
      )
    )
  })
  
  # Handle removal of sensors from the lists
  observe({
    lapply(sensorLists$list1, function(sensor) {
      observeEvent(input[[paste0("removeList1_", sensor)]], {
        sensorLists$list1 <- setdiff(sensorLists$list1, sensor)
      })
    })
    lapply(sensorLists$list2, function(sensor) {
      observeEvent(input[[paste0("removeList2_", sensor)]], {
        sensorLists$list2 <- setdiff(sensorLists$list2, sensor)
      })
    })
  })
  
  
  output$SensorListOneLinePlot <- renderGirafe({
    
    start_date <- as.POSIXct(Sys.Date() - 7, tz = "UTC")
    end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    
    all_sensors <- sensorLists$list1
    
    filtered_data <- SensorDataDBHistorical %>%
      filter(name %in% all_sensors)
    
    avg_data <- filtered_data %>%
      group_by(Date_Only) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))#, .names = "avg_{.col}"))
    
    
    tryCatch({
      if(input$averageCheckboxCompare==TRUE)
      {
        p <- GraphPlot(filtered_data, start_date, end_date, getBackgroundBarsTemplate("Air_Quality_Index"))
      }
      else
      {
        p <- GraphPlotMulti(filtered_data, start_date, end_date, getBackgroundBarsTemplate("Air_Quality_Index"))
      }
      
      print(p)
    }, error = function(e) {
      print(paste("An error occurred:", e$message))
      return(NA)
    })
    
    
  })
  
  output$SensorListTwoLinePlot <- renderGirafe({
    
    start_date <- as.POSIXct(Sys.Date() - 7, tz = "UTC")
    end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    
    all_sensors <- sensorLists$list2
    
    filtered_data <- SensorDataDBHistorical %>%
      filter(name %in% all_sensors)
    
    
    
    avg_data <- filtered_data %>%
      group_by(Date_Only) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))#, .names = "avg_{.col}"))
    
    
    tryCatch({
      if(input$averageCheckboxCompare==TRUE)
      {
        p <- GraphPlot(filtered_data, start_date, end_date, getBackgroundBarsTemplate("Air_Quality_Index"))
      }
      else
      {
        p <- GraphPlotMulti(filtered_data, start_date, end_date, getBackgroundBarsTemplate("Air_Quality_Index"))
      }
      
      print(p)
    }, error = function(e) {
      print(paste("An error occurred:", e$message))
      return(NA)
    })
    
  })
  
  
  
  
  
  
  
  
  
  output$LineGraphTitle <- renderUI({
    paste("What is the current", gsub("_", " ", input$variable_select_input_line_graph_analyze), "?")
  })
  
  output$CalendarPlotTitle <- renderUI({
    paste(gsub("_", " ", input$variable_select_input_line_calendar_analyze), "Calendar Plot")
    
  })
  
  
  
  
  
  GraphPlotMulti <- function(dataset, start_date, end_date, background_bars)
  {
    
    dataset <- dataset[dataset$Date >= start_date &
                         dataset$Date <= end_date, ]
    
    start_date <- min(dataset$Date)
    end_date <- max(dataset$Date)
    
    
    #print(dataset[[input$variable_select_input_line_graph_analyze]]))
    
    #print(first(background_bars$var))
    linecolor <- getColor(dataset[which.max(dataset$Date), first(background_bars$var)], first(background_bars$var))
    
    max_y_value <- max(dataset[[input$variable_select_input_line_graph_analyze]])
    
    # Set the y-axis limits dynamically
    if (max_y_value > 101) {
      p <- p + scale_y_continuous(limits = c(0, max_y_value))
      scaletop <- max_y_value
    } else {
      p <- p + scale_y_continuous(limits = c(0, 101))
      scaletop <- 101
    }
    
    background_bars <- background_bars[background_bars$ymin <= scaletop - 6, ]
    
    end_points <- dataset %>%
      group_by(name) %>%
      filter(Date == max(Date)) %>%
      ungroup()
    
    dataset <- dataset %>%
      mutate(tooltip_label = as.character(glue::glue("{name}<br>{first(background_bars$var)}: {dataset[[first(background_bars$var)]]}<br>{Date_Only}")))
    
    
    
    
    
    p <- ggplot() +
      
      
      
      geom_rect(data = background_bars, 
                aes(xmin = start_date, xmax = end_date, ymin = ymin, ymax = ymax, fill = fill, text = label), 
                color = NA, alpha = 0.08) +
      scale_fill_identity() +
      scale_x_datetime(labels = scales::time_format("%I %p\n%b %d"), breaks = pretty_breaks(n = 5)) +
      
      
      geom_line_interactive(data = dataset, aes(x = Date, y = .data[[first(background_bars$var)]], color = name, data_id = sensor_index), 
                            size = 1.4, alpha = 1) +
      
      geom_point_interactive(data = dataset, aes(x = Date, y = .data[[first(background_bars$var)]], color = name, 
                                                 tooltip = tooltip_label, data_id = sensor_index), 
                             size = 1.7, alpha = 0.4) +
      
      geom_hline(yintercept = background_bars$ymin, color = background_bars$fill, size = 0.5, na.rm = TRUE) +  # Make border lines thinner
      
      geom_label_repel(aes(x = max(dataset$Date), y = background_bars$ymin, label = background_bars$label),
                       fill = "#171717", color = background_bars$fill, 
                       size = 3.5, fontface = "bold", vjust = 0.5, hjust = 0, show.legend = FALSE, segment.color = NA,
                       box.padding = unit(0.2, "lines"), label.padding = unit(0.2, "lines"),
                       label.r = 0.3, na.rm = TRUE, label.size=0) +
      
      
      
      labs(x = "Day", y = "") +
      
      ggtitle(paste("Current", gsub("_", " ", first(background_bars$var))))+
      dark_theme() +
      theme(
        plot.title = element_text(face = "bold", size = 18),  # Make title bold
        axis.title.x = element_text(face = "bold", size = 14),  # Make x-axis title bold and adjust size
        #axis.title.y = element_text(face = "bold", size = 12),  # Make y-axis title bold and adjust size
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 14),
        #legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        
        #legend.position = "bottom",  # Position legend at the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 7),  # Adjust legend text size
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(3, 3, 3, 3),
        legend.background = element_rect(fill = alpha("#000000", 0.4))
      )
    
    t <- girafe(ggobj = p, options = list(
      opts_hover(css = "stroke-width:3; opacity: 1;"),
      opts_hover_inv(css = "opacity: 0.1;"),
      opts_tooltip(css = "background-color: rgba(0, 0, 0, 0.8); color: white; border-radius: 5px; padding: 5px;"),
      opts_selection(type = "none")
    ))
    
    return(t)
  }
  
  
  
  
}

shinyApp(ui, server)
