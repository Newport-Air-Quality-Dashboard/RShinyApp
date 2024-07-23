# List of required packages, ignore if you dont know what this does
required_packages <- c(
  "shiny", "shinydashboard", "tidyverse", "leaflet", "shinycssloaders",
  "leaflet.extras", "htmltools", "plotly", "scales", "reshape2",
  "ggrepel", "calendR", "ggiraph", "base64enc", "DBI", "RSQLite",
  "shinyWidgets", "XML", "xml2", "RColorBrewer", "gstat", "sp",
  "colorRamps", "raster", "sf", "httr", "later"
)

# Check if each package is installed and install if necessary
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

install_if_missing(required_packages)

# Load all required packages
lapply(required_packages, library, character.only = TRUE)











#Welcome to using the R Shiny Map Dashboard App, below is the configuration and documentation 
#on how to make this app customizable to your use case!

#If you are not experienced with code, that is alright! Just follow the steps below carefully
#If you know what you are doing and what docs for the source itself, visit:
#https://docs.google.com/document/d/1kRoA1Xz_unkQCap3sQ6FgI87Z4Z0aHm1t8X5GNqOU-o/edit?usp=sharing

#You are expected to already have a sql style database, with a few paramaters, put the path to said DB below:
PathToDB <- "db.sqlite"

# The database is expected to have a "name" field, with the names of all the sensors, a "time_stamp" field with the time in unix, a 
# "source" that outlines the name of the source it comes from (will go over later), a unique id for the sensor
# named "sensor_index", a "latitude" and "longitude" variable, and a variety of "value" variables, like "humidity" etc.

#Note, unless you are using the database that is provided in this project, the field name "Air_Quality_Index" is not recommended

#Below change the list of parameters you plan to use from your sql database
Sql_Params <- c("name", "time_stamp", "source", "sensor_index", "latitude", "longitude", "humidity_dashboard", "temperature_dashboard", "pressure_dashboard", "\"pm2.5_aqi_dashboard\"", "\"pm2.5_dashboard\"", "\"pm2.5_a_dashboard\"", "\"pm2.5_b_dashboard\"")

#Now here, outline the names of parameters you want to be displayed on the map in a dropdown
getVarList <- function() {
  return(c("Air Quality Index", "Temperature Fahrenheit", "Pressure", "Humidity"))
}

#Also define which field is the primary one, this will filter all NA's in this field for all the data (Note to future contributors: may be worth making a system that does not universally filter this)
PrimaryField <- "Air Quality Index"

# For each of the parameters you have chosen, replace the numbers and colors below with whatever you want to make a
# custom range for what the colors are defined as

PastDaysLimit <- 30
#Amount of days from now that the map graph goes back to
#Also defines how long ago a value needs to appear for a sensor to be considered "active" and show up on the map

getColor <- function(var, inputVar) {
  if (inputVar == "Air Quality Index") {
    # Color ranges for Air Quality Index
    ifelse(var <= 50, "Green",
           ifelse(var <= 100, "Yellow",
                  ifelse(var <= 150, "Orange",
                         ifelse(var <= 200, "Red",
                                ifelse(var <= 300, "Purple", "#7E0023")))))
  } else if (inputVar == "Temperature Fahrenheit") {
    # Color ranges for Temperature
    ifelse(var < 0, "Blue",
           ifelse(var <= 20, "LightBlue",
                  ifelse(var <= 40, "LightGreen",
                         ifelse(var <= 65, "Green",
                                ifelse(var <= 80, "Yellow",
                                       ifelse(var <= 100, "Orange", "Red"))))))
  } else if (inputVar == "Pressure") {
    # Color ranges for Pressure
    ifelse(var < 980, "Red",
           ifelse(var <= 1013, "Yellow",
                  ifelse(var <= 1040, "Green", "Blue")))
  } else if (inputVar == "Humidity") {
    # Color ranges for Humidity
    ifelse(var < 30, "Yellow",
           ifelse(var <= 50, "Green", "Blue"))
  }
}

#Next, do a similar process below for each of those parameters, the data should be set up in color "buckets" where values
#beetween certain numbers fall into a bucket and get a certain color
#var should be just the name of the parameters, the labels are the names of the buckets you are making,
#the ymin and ymax represent the values that range each of the buckets, and the fill is the color for each of the buckets
#notably, ymin, ymax, and fill should match what you put in for getColor

#If you want the heatmap colors to function properly, it is very important that each bucket is the same size,
#you can still have different ranges though, by just having several buckets that have the same color and name

getBackgroundBarsTemplate <- function(input) {
  if (input == "Air Quality Index") {
    background_bars_template <- data.frame(
      var = "Air Quality Index",
      label = c("Good", "Moderate", "Unhealthy for Vulnerable People", "Unhealthy", "Very Unhealthy", "Hazardous"),
      ymin = rep(c(0, 50, 100, 150, 200, 300)),
      ymax = rep(c(50, 100, 150, 200, 300, 500)),
      fill = rep(c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023"))
    )
  } else if (input == "Temperature Fahrenheit") {
    background_bars_template <- data.frame(
      var = "Temperature Fahrenheit",
      label = c("Freezing", "Cold", "Cool", "Warm", "Hot", "Extremely Hot"),
      ymin = rep(c(0, 20, 40, 60, 80, 100)),
      ymax = rep(c(20, 40, 60, 80, 100, 120)),
      fill = rep(c("LightBlue", "LightGreen", "Green", "Yellow", "Orange", "Red"))
    )
  } else if (input == "Pressure") {
    # Bar ranges and colors for Pressure based on EPA guidelines
    background_bars_template <- data.frame(
      var = "Pressure",
      label = c("Low", "Normal", "High", "Extreme"),
      ymin = c(950, 980, 1010, 1040),
      ymax = c(980, 1010, 1040, 1070),
      fill = c("Red", "Yellow", "Green", "Blue")
    )
  } else if (input == "Humidity") {
    # Bar ranges and colors for Humidity based on EPA guidelines
    background_bars_template <- data.frame(
      var = "Humidity",
      label = c("Low", "Normal", "High"),
      ymin = c(0, 33, 66),
      ymax = c(33, 66, 100),
      fill = c("Yellow", "Green", "Blue")
    )
  }
  
  return(background_bars_template)
}

#Here you re defining the initial zoom location for the maps, change the first 2 coordinates to move
#it where you see fit

InitialMapView <- c(39.0890, -84.5008, 12) #lat, lng, zoom


#The compare map view has a list of presets that allow you to easily set up comparisons, 
#put the "name" parameter value for the sensors/points that you want to be in specific presets.
presets <- list(
  "Newport East" = c("Monitor 1 East PA","Monitor 4 East PA","338 E 9th St Newport KY 41071 PA","home PA"),
  "Newport West" = c("Monitor 9 West PA","Monitor 7 West PA","Monitor 10 West PA","Monitor 8 West PA","Monitor 6 West PA"),
  "Newport All" = c("Monitor 1 East PA","Monitor 12 East PA","Monitor 4 East PA","338 E 9th St Newport KY 41071 PA","home PA","Monitor 9 West PA","Monitor 7 West PA","Monitor 10 West PA","Monitor 8 West PA","Monitor 6 West PA","Monitor 11 Island PA"),
  "Cincinnati Area" = c("McFarland PA", "Longworth Square PA", "Lower Price Hill EPA", "MSD 6 PA", "Meals on Wheels PA", "Taft NCore PAMS EPA", "CFD Station 12 PA", "City-CHD PA")
)

# Now, you have to define the shapes and sources, you have a "primary" source, that will show up as a circle
# and secondary sources, which will have a shape with x sides, make a list with sourcename (no quotes) = x, so
# so that you get the amount of desired sides for your shape, the first source should just have its own name as a string as shown below
SourceToShapeMapper <- list(PurpleAir = "PurpleAir", EPA = 5, AQMesh = 3)
#In this example EPA is a pentagon, and AQMesh is a triangle

#Below change the link for the logo on the sidebar: First one is the logo iself, second one is where clicking it sends you
logo_link <- "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/nkunorse.com/images/responsive/logo_main.svg"
target_link <- "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/nkunorse.com/images/responsive/logo_main.svg"


#Below change the color you would like from the options of: "blue", "black", "green", "purple", "red", "yellow"

ThemeColor <- "purple"

#Very advanced, this app includes a heat map, 
#and the variogram inverse distance weighting interpolation method for the heat map is listed below:
#Do not touch unless you know what you are doing, if you want to use the custom heat map set the UseCustomIDW to TRUE

UseCustomIDW <- TRUE

# Function to calculate variogram value at a given distance
variogram_model <- function(d, n, s, r) {
  n + s * (1 - exp(-d / r))
}

# Function to calculate weights
calculate_weights <- function(d, grid_point) {
  n <- 0.5 #nugget
  s <- 1000.5 #sill
  r <- 0.06 #range
  dists <- spDistsN1(as.matrix(d[, c("x", "y")]), as.numeric(grid_point), longlat = FALSE)
  var_values <- variogram_model(dists, n, s, r)
  weights <- (2 * n + s) - var_values
  weights / sum(weights)
}

# Perform interpolation
interpolate <- function(d, grid_points) {
  interpolated_values <- sapply(1:nrow(grid_points), function(i) {
    weights <- calculate_weights(d, grid_points[i,])
    sum(weights * d$z)
  })
  interpolated_values
}

#There is an option to have people sign up for phone notifications, but it rquires the paid service "nexmo", here you can set that up

SMSTimer <- 300 #define time beetween notifications in seconds

# https://www.vonage.com/communications-apis/sms/ Here you can get an account and get the 3 following values to run notifs
nexmo_api_key <- ""
nexmo_api_secret <- ""
nexmo_from <- "" # Should be in E.164 format

#Finally, define the message you want to send
notification_message <- "This is a test message from the air quality dashboard."

#If you know CSS, you can use the code below to customize the entire look of the page

# ---- Customize Look ----
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

# Custom CSS to format boxes for home page
css_box_home <- "
  .home-page .tab-content .tab-pane {
    background-color: #1F1F1F;
    color: white;
  }
  .home-page .box{
    background-color: #000000D9;
    border-color: #000000D9;
  }
  .home-page .tab-content .tab-pane,
  .home-page .nav-tabs {
    background-color: #2E056B;
    color: white;
  }
  .home-page .box-title {
    font-size: 24px;
    font-weight: bold;
    color: white;
  }
  .home-page .form-control {
    background-color: #333333;
    color: white;
  }
  .home-page .selectize-dropdown {
    background-color: #333333;
    color: white;
  }
  .home-page .selectize-input {
    background-color: #333333;
    color: white;
  }
  .home-page .btn-primary {
    background-color: #F49D37;
    border-color: #F49D37;
  }
  .home-page .btn-primary:hover {
    background-color: #F89C14;
    border-color: #F89C14;
  }
  .home-page .box-header {
    background-color: #2E056B;
    font-size: 20px;
    color: white;
  }
  .home-page .box-body {
    font-size: 18px;
    color: white;
  }
  
  
  "

# Custom CSS to format boxes for analyze page
css_analyze <- "
  	.analyze-page .tab-content .tab-pane {
    	background-color: #1F1F1F !important;
    	color: white !important;
  	}
  	.analyze-page .box {
    	background-color: #333333 !important;
    	border-color: #444444 !important;
  	}
  	.analyze-page .box-title {
    	font-size: 24px !important; /* Larger font size */
    	font-weight: bold !important; /* Bold text */
    	color: white !important; /* White text color */
  	}
  	.analyze-page .form-control {
    	background-color: #333333 !important;
    	color: white !important;
  	}
  	.analyze-page .selectize-dropdown {
    	background-color: #333333 !important;
    	color: white !important;
  	}
  	.analyze-page .selectize-input {
    	background-color: #333333 !important;
    	color: white !important;
  	}
  	.analyze-page .btn-primary {
    	background-color: #F49D37 !important;
    	border-color: #F49D37 !important;
  	}
  	.analyze-page .btn-primary:hover {
    	background-color: #F89C14 !important;
    	border-color: #F89C14 !important;
  	}
  	.analyze-page .box-header {
    	background-color: #2E056B !important; /* Dark purple background */
    	color: white !important; /* White text color */
  	}
  	.analyze-page .box-body {
    	font-size: 18px !important; /* Larger font size */
     	color: white !important;
  	}
  
    "


# Graph theme for dark background with custom colors
dark_theme <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#1F1F1F"), # Dark background
      panel.grid.major = element_line(color = "#505050"), # Dark grid lines
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "white"), # White axes
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.background = element_rect(fill = "#1F1F1F"), # White plot background
      panel.border = element_rect(color = "white", fill = NA), # White plot border
      legend.text = element_text(color = "white"), # White legend text
      legend.background = element_rect(fill = "#1F1F1F", color = NA), # White legend background
      strip.text = element_text(color = "white"), # White strip text
      plot.title = element_text(color = "white") # White title text
    )
}






# ---- Change Home/Help ----

#Finally, below are the "box" systems for the home and help page of your site, customize them, changing the text and images
# to suit your sites needs
home_tab_boxes <- fluidPage(
  setBackgroundImage(
    src = "https://images.unsplash.com/photo-1600187230702-5f07aea6d7f2?q=80&w=2612&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D",
    shinydashboard = TRUE
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "Our Purpose",
             width = NULL,
             HTML("Air quality can impact human health and the health of the environment.
                     	The quality of our air, or how clean it is, can change from day to day, season to season, and sometimes even by the hour.
                     	This dashboard provides information to residents in the Cincinnati region on their air quality.
                     	You can use the map and other tools to see how air quality is changing.<br><br>
                     	Dashboard created in partnership by ReNewport, Databloom, Groundwork Ohio River Valley, and Northern Kentucky University")
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "A Guide to AQI and Sensors on the Map",
             width = NULL,
             HTML("The Air Quality Index (AQI) was created by the U.S. Environmental Protection Agency to tell people how clean or dirty the air is.
                     	It shows if five major pollutants (ground-level ozone, fine particles, carbon monoxide, sulfur dioxide, and nitrogen dioxide) are at levels that could harm human health.<br><br>",
                  HTML('<img src="https://www.csueastbay.edu/airquality/files/images/epa_aqi_2.JPG" style="width:75%;height:auto;">'),
                  HTML('<br><a href="https://www.airnow.gov/aqi/aqi-basics/" target="_blank">More Information About AQI</a><br>'),
                  HTML("<br>Sensors on the map are represented by three different shapes: a circle, a pentagon, and a triangle. 
                                 These symbols represent PurpleAir sensors, EPA sensors, and AQ Mesh sensors respectively.<br>"),
                  HTML('<img src="https://www.pngmart.com/files/23/Green-Circle-PNG-Image.png" style="width:10%;height:auto;">PurpleAir'),
                  HTML('<img src="https://static.vecteezy.com/system/resources/thumbnails/020/906/687/small_2x/geometric-pentagon-shape-on-a-transparent-background-free-png.png" style="width:10%;height:auto;">EPA'),
                  HTML('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/7/7d/Green_triangle.svg/1152px-Green_triangle.svg.png" style="width:10%;height:auto; transform: rotate(180deg);">AQ Mesh')
             )
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "Causes for Air Pollution to Vary",
             width = NULL,
             HTML("The amount of pollution in cities can be very different depending on the area.
                          	The devices that measure pollution are put in different places, so they show different results depending on their surroundings.<br><br>
                          	<strong>Industry</strong><br>
                          	In industrial areas, things like diesel trucks stopping or driving through, machines burning fuel can make the pollution much worse.<br><br>
                          	<strong>Traffic</strong><br>
                          	Cars and trucks cause pollution from their exhaust, tires wearing out, and brakes being used.
                          	This makes pollution higher near roads and areas with high traffic.
                          	There are also daily spikes in pollution readings in the mornings and evenings because of people traveling to and from work and school.<br><br>
                          	<strong>Weather</strong><br>
                          	Weather conditions, temperature and precipitation, can influence air quality.
                          	Warm temperatures can cause poor air quality and different weather patterns can trap or move pollution to other locations, even very far away.
                          	For example, wildfire smoke can move very long distances.<br><br>
                          	<strong>Other Causes for High Readings</strong><br>
                          	Sometimes, pollution monitors show very high readings for no clear reason.
                          	This can happen because of things like spiderwebs, bad sensor placement, or sensor breakdowns."
             )
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "Local Reporting",
             width = NULL,
             HTML("The following websites are local research and news sources reporting on the state of air quality and the environment in the Greater Cincinnati Area.<br><br>"),
             HTML('<a href="https://www.cincinnati.com/search/?q=Air+Quality" target="_blank">Cincinnati Enquirer - Air Quality</a><br>'),
             HTML('<a href="https://greenumbrella.org/" target="_blank">Green Umbrella</a><br>'),
             HTML('<a href="https://helptheair.org/" target="_blank">Kentuckiana Air Education</a>')
           )
    )
  ),
  fluidRow(
    box(
      title = "Current Articles",
      width = 12,
      solidHeader = TRUE,
      tags$head(
        tags$style(HTML("
        	.article-box {
          	border: 1px solid #ddd;
          	padding: 10px;
          	margin-bottom: 10px;
          	display: flex;
          	align-items: flex-start;
          	flex-direction: column;
        	}
      	"))
      ),
      uiOutput("feed_output")
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "Newsletter and Sensor Notifications",
             width = NULL,
             HTML("The following boxes will allow you to sign up for a text newsletter 
                       and notification system by entering your phone number below.<br><br>
                            Please enter phone numbers in +1XXXXXXX format.<br><br>"),
             textInput("phone", "Enter your phone number to subscribe:", ""),
             actionButton("subscribe", "Submit"),
             HTML("<br><br>"),
             textInput("unsubscribe_phone", "Enter your phone number to unsubscribe:", ""),
             actionButton("unsubscribe", "Submit"),
           )
    )
  )
  
)  


help_tab_boxes <- fluidPage(
  fluidRow(
    column(width = 12,
           box(
             title = "How do I use the map?",
             width = NULL,
             collapsible = TRUE,
             collapsed = TRUE,
             HTML("The map tab shows different dots, each one is a sensor.
                 Circles are PurpleAir sensors, and stars are sensors from the Environmental Protection Agency.
                 If you tap on a dot, you can see a graph of the data from that sensor.
                 There is a legend in the bottom right corner.
                 You can use the drop-down menu next to it to switch between different types of data collected."
             )
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "How do I use the analyze feature?",
             width = NULL,
             collapsible = TRUE,
             collapsed = TRUE,
             "The analyze tab has a graph and a calendar. Both show data from the last sensor the user clicked on the map.
                 Users can choose what kind of data to see and the time range for the graph and calendar."
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "How do I use the compare feature?",
             width = NULL,
             collapsible = TRUE,
             collapsed = TRUE,
             "The compare tab has a map where users can pick sensors to compare.
                 They can put the sensors into one of two lists. On the right, users can see these lists and change the type of data shown.
                 Below, the graphs that the user wants to compare are displayed."
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "What are the see-through lines on the graph?",
             width = NULL,
             collapsible = TRUE,
             collapsed = TRUE,
             "Each sensor has two parts, called a and b, that measure data together.
                 Their data is averaged to give the final result.
                 The see-through lines on the graph show the data from the a and b parts separately.
                 This helps users see what is being averaged or find problems with the sensors."
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "How should I place my PurpleAir sensor?",
             width = NULL,
             collapsible = TRUE,
             collapsed = TRUE,
             HTML("<ul>
                   <li>Sunlight: Direct sunlight can change a sensor's temperature readings and fade the color of the sensor's exterior.
                   This is normal, but it's best to keep sensors out of direct sunlight when you place them.</li>
                   <li>Polluters: Sensors should be placed away from vents, furnaces, and BBQs.
                   These things can make the sensor's pollution readings incorrect.</li>
                   <li>Foliage: Spiders and insects like the warmth of sensors and can get into the fans, causing data spikes.
                   It's best to place sensors away from trees and bushes when possible.</li>
                   <li>Elevation: The sensor should be placed at a convenient height.
                   Make sure it is high enough for good airflow and to stay dry from splashing water.</li>
                   <li>The following picture is an example of a well placed sensor.</li>"),
             HTML('<img src="https://www.minneapolismn.gov/media/-www-content-assets/images/PurpleAir-Sensor.jpg" style="width:50%;height:auto;">')
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(
             title = "Who can I contact for help?",
             width = NULL,
             collapsible = TRUE,
             collapsed = TRUE,
             HTML("Re:Newport --> info@renewportky.com<br><br>
                Data Bloom --> grow@thedatabloom.com<br><br>
                Andrew Long: Dashboard Supervisor --> LONGA@nku.edu<br><br>
                Kristy Hopfensperger: Team Organizer --> hopfenspek1@nku.edu
                ")
           )
    )
  )
)



# ---- Configuration Ends here ----

# ----Other Setup----




# Function to query data from a database within a specified date range and optional sensor index
query_data <- function(start_date, end_date, sensor_index = NULL, params = Sql_Params) {
  
  # Convert dates to numeric format (POSIXct to numeric)
  start_date <- as.numeric(start_date)
  end_date <- as.numeric(end_date)
  
  # Connect to the SQLite database (modify the connection string for your database)
  con <- dbConnect(RSQLite::SQLite(), PathToDB)
  
  # Construct the base SQL query
  base_query <- paste("SELECT ", paste(params, collapse = ", "), " FROM df_all WHERE time_stamp BETWEEN '", start_date, "' AND '", end_date, "'", sep="")
  
  # Add sensor_index condition to the query if provided
  if (!is.null(sensor_index)) {
    base_query <- paste(base_query, "AND sensor_index =", sensor_index)
  }
  
  # Execute the SQL query
  result <- dbGetQuery(con, base_query)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  # Convert columns to appropriate formats
  result$time_stamp <- as.POSIXct(result$time_stamp, origin = "1970-01-01", tz = "UTC")
  
  
  tryCatch(
    {
      result$temperature_dashboard <- ceiling(as.numeric(result$temperature_dashboard))
      result$humidity_dashboard <- ceiling(as.numeric(result$humidity_dashboard))
      result$pressure_dashboard <- ceiling(as.numeric(result$pressure_dashboard))
      
      
      # Rename columns for better readability
      result <- result %>%
        rename("Temperature Fahrenheit" = temperature_dashboard) %>%
        rename("Humidity" = humidity_dashboard) %>%
        rename("Pressure" = pressure_dashboard) %>%
        rename("Date" = time_stamp) %>%
        rename("Air Quality Index" = pm2.5_aqi_dashboard)
    }
  )
  
  
  # Add a suffix to the name column based on the source
  result$name <- paste(result$name, if_else(result$source == "PurpleAir", "PA", "EPA"))
  
  # Add a new column for date only (without time)
  result$Date_Only <- as.Date(result$Date)
  view(result)
  return(result)
}

# Function to calculate Air Quality Index (AQI) for PM2.5
calculate_aqi_pm25 <- function(pm25) {
  # Define breakpoints for AQI calculation
  breakpoints <- data.frame(
    C_low = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5),
    C_high = c(12, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4),
    AQI_low = c(0, 51, 101, 151, 201, 301, 401),
    AQI_high = c(50, 100, 150, 200, 300, 400, 500)
  )
  
  # Find the appropriate AQI rank for the given PM2.5 value
  rank <- which(pm25 <= breakpoints$C_high)[1]
  
  # Calculate the AQI based on the breakpoints
  aqi <- ceiling((breakpoints$AQI_high[rank] - breakpoints$AQI_low[rank]) /
                   (breakpoints$C_high[rank] - breakpoints$C_low[rank]) *
                   (pm25 - breakpoints$C_low[rank]) + breakpoints$AQI_low[rank])
  
  return(aqi)
}



getSensorDataDB <- reactive({
    # Query data from the past 40 days and process it
    SensorDataDB <- query_data(Sys.time() - days(PastDaysLimit), Sys.time(), NULL) %>%
      arrange(sensor_index, desc(Date)) %>%
      filter(!is.na(!!sym(PrimaryField))) %>%
      group_by(sensor_index) %>%
      slice(1) %>%
      ungroup()
  #return(SensorDataDB)
})




# Query historical data from the past 40 days
getSensorDataDBHistorical <- reactive({
  query_data(Sys.time() - days(PastDaysLimit), Sys.time(), NULL)
  #return(SensorDataDBHistorical)
  })







# ---- UI ----
ui <- dashboardPage(
  skin = ThemeColor,
  dashboardHeader(title = "Air Quality Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      HTML(paste0(
        "<br>",
        "<a href='", target_link, "' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='", logo_link, "' width='186'></a>",
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
    dark_theme_general(), # Applying the general dark theme
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/draggabilly/2.3.0/draggabilly.pkgd.min.js"),
      tags$script(HTML('
    	$(document).ready(function() {
      	var elem = document.getElementById("draggableContainer");
      	var draggie = new Draggabilly(elem);
    	});
  	')),
      tags$style(HTML(css_box_home)), # Applying custom CSS for home page boxes
      tags$style(HTML(css_analyze)), # Applying custom CSS for analyze page boxes
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
    	
    	#AQMapContainer .leaflet-control-zoom {
    	    position: fixed; top: 130px; right: 10px;
    	  }
   	 
   	 #AQMapContainer .leaflet-control-layers {
        position: fixed; top: 50px; right: 0px;
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
   	 
    	#close_draggable {
      	background-color: #1F1F1F;  /* Red background */
      	color: #FF0000;  /* White text */
      	border: 1px solid #FF0000;
      	padding: 5px 10px;
      	border-radius: 4px;
      	font-size: 14px;
      	font-weight: bold;
      	margin-top: 0px;  /* Ensure no extra margin */
    	}
    	#close_draggable:hover {
      	background-color: #6f6f6f;
    	}
   	 
   	 
     	 
    	}
  	"))
    ),
    tabItems(
      
      
      tabItem(tabName = "help", class = "home-page",
              help_tab_boxes
      ),
      
      
      
      
      
      tabItem(tabName = "home", class = "home-page",
              condition = "input.tab === 'home'",
              home_tab_boxes
      ),
      
      tabItem(tabName = "map",
              fluidRow(
                column(
                  width = 6,
                  position = "relative",
                  div(id = "AQMapContainer", leafletOutput("AQMap")),
                  conditionalPanel(
                    condition = "output.draggableVisible == true",
                    tags$div(id = "draggableContainer",
                             style = "margin-top: 0px;",
                             girafeOutput("plotInfo", height = "300px", width = "360px"),
                             div(
                               style = "display: flex !important;",
                               actionButton("go_to_analyze", "See Details", style = "margin-top: 0px;margin-top: 0px; margin-right: 5px;"),
                               actionButton("close_draggable", "Close", style = "margin-top: 0px;")
                             )
                             
                    )
                  )
                ),
                tags$div(
                  selectInput("variable_select_input_map", "",
                              choices = getVarList(),
                              selected = getVarList()[1]),
                  style = "width: 240px; position: fixed; bottom: 160px; right: 10px;"
                )
              )
      ),
      tabItem(tabName = "analyze", class = "analyze-page",
              tags$style(HTML(css_analyze)),
              
              fluidRow(
                column(width = 12,
                       box(
                         title = "Introduction",
                         width = NULL,
                         solidHeader = TRUE,
                         #status = "primary",
                         div(
                           "The two following graphs are here to visualize the Air Quality Index, which comprises of the density of pollutants of a certain size currently in the air. Anything above moderate is cause for concern. The first graph shows the current air quality as time ranges, and the second graph gives the daily average and shows it on a calendar.",
                           br(),
                           br(),
                           selectInput("sensor_dropdown", "Select a Sensor",
                                       choices = NULL,  # Initially, no choices are set
                                       selected = NULL,
                                       selectize = TRUE,  # Makes it scrollable and searchable
                                       multiple = FALSE)
                         )
                       )
                       
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = uiOutput("LineGraphTitle"), width = NULL, solidHeader = TRUE, status = "primary",
                         girafeOutput("Line_Graph_Analyze",
                                      
                                      
                         )
                       ),
                       box(
                         
                         title = "Line Graph Settings", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = TRUE,
                         fluidRow(
                           column(width = 6,
                                  selectInput("variable_select_input_line_graph_analyze", "Select Graph Variables",
                                              choices = getVarList(),
                                              selected = getVarList()[1])
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
                                              selected = getVarList()[1])
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
      tabItem(tabName = "compare", class = "analyze-page",
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
                  box(width = "260%",
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
                          selectInput("variable_select_compare", "Variable",
                                      choices = getVarList(),
                                      selected = getVarList()[1]),
                          selectInput("time_select_input_line_graph_compare", "Select Time Input",
                                      choices = c("1 day", "7 days","30 days","90 days","365 days","custom range"),
                                      selected = "30 days"),
                          conditionalPanel(
                            condition = "input.time_select_input_line_graph_compare == 'custom range'",
                            dateRangeInput("custom_date_range_compare", "Select Date Range")
                          ),
                          checkboxInput("averageCheckboxCompare", "Average Values in Graph", value = FALSE),
                          actionButton("addPresetButton", "Add Preset")
                          
                        )
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
  
  #function to create a custom star image of the chosen color
  generate_star_svg <- function(hex_color = "Green", size = 20, move_up = 0, num_sides = 5) {
    # Function to calculate the coordinates of the points
    calculate_points <- function(cx, cy, r, num_sides) {
      points <- ""
      angle_step <- 2 * pi / num_sides
      for (i in 0:(num_sides-1)) {
        angle <- i * angle_step - pi / 2
        x <- cx + r * cos(angle)
        y <- cy + r * sin(angle) - move_up
        points <- paste(points, sprintf("%.2f,%.2f", x, y), sep = " ")
      }
      return(trimws(points))
    }
    
    # Calculate points for the star
    points <- calculate_points(12, 12, 10, num_sides)
    # Generate the SVG string
    svg_string <- sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24" fill="%s">
      <polygon points="%s"/>
    </svg>',
      size, size, hex_color, points
    )
    
    
    svg_data_uri <- paste0("data:image/svg+xml;base64,", base64encode(charToRaw(svg_string)))
    
    return(svg_data_uri)
    
  }
  
  
  # ---- Maps ----
  
  # Reactive value to filter SensorDataDB based on the selected input variable for the map
  #TargetDataMap <- reactive({
  #  FilteredSensorDataDB <- getSensorDataDB() %>%
  #    filter(!is.na(.[[input$variable_select_input_map]]))
  #  FilteredSensorDataDB[[input$variable_select_input_map]]
  #})
  
  
  # Reactive value to filter SensorDataDB based on the selected input variable for comparison
  #TargetDataMapCompare <- reactive({
  #  FilteredSensorDataDB <- getSensorDataDB() %>%
  #    filter(!is.na(.[[input$variable_select_compare]]))
  #  FilteredSensorDataDB[[input$variable_select_compare]]
  #})
  
  # Reactive value to keep track of the current map sensor ID
  CurrentMapSensorId <- reactiveVal(0)
  
  # Reactive value to manage the visibility of draggable elements
  draggableVisible <- reactiveVal(TRUE)
  
  # Observe the close button event to hide the draggable elements
  observeEvent(input$close_draggable, {
    draggableVisible(FALSE)
  })
  
  # Observe the map marker click event to show the draggable elements
  observeEvent(input$AQMap_marker_click, {
    draggableVisible(TRUE)
  })
  
  # Reactive output to control the visibility of draggable elements
  output$draggableVisible <- reactive({
    draggableVisible()
  })
  outputOptions(output, "draggableVisible", suspendWhenHidden = FALSE)
  
  # Filter SensorDataDB for non-NA values in the "Air_Quality_Index" column
  #FilteredSensorDataDB <- getSensorDataDB() %>%
  #  filter(!is.na(.[[PrimaryField]]))
  
  
  # Function to generate a heatmap based on the selected variable
  getHeatmap <- function(var) {
    FilteredSensorDataDB <- getSensorDataDB() %>%
      filter(!is.na(.[[var]]))
    d <- data.frame(
      x = FilteredSensorDataDB$longitude,
      y = FilteredSensorDataDB$latitude,
      z = FilteredSensorDataDB[[var]]
    )
    
    # Define a grid of points for interpolation
    x_range <- range(d$x) + c(-0.1, 0.1)
    y_range <- range(d$y) + c(-0.1, 0.1)
    n_points <- 100
    grid_x <- seq(min(x_range), max(x_range), length.out = n_points)
    grid_y <- seq(min(y_range), max(y_range), length.out = n_points)
    grid_points <- expand.grid(x = grid_x, y = grid_y)
    
    # Create and perform IDW interpolation on the grid
    
    if(UseCustomIDW == FALSE)
    {
      #This is a version done with a built in library, got replaced by a custom solution found at the top of the code
      gs <- gstat(formula = z ~ 1, locations = ~x + y, data = d, nmax = Inf, set = list(idp = 5))
      idw_raster <- predict(gs, newdata = grid_points)
      idw_grid <- as.data.frame(idw_raster)
      colnames(idw_grid) <- c("x", "y", "z")
    }
    else
    {
      interpolated_values <- interpolate(d, grid_points)
      idw_grid <- cbind(grid_points, z = interpolated_values)
      colnames(idw_grid) <- c("x", "y", "z")
      coordinates(idw_grid) <- ~x + y
      gridded(idw_grid) <- TRUE
      
      idw_raster <- raster(idw_grid)
      
      # Plot the raster
      plot(idw_raster, main = "Variogram-based IDW Interpolation")
      
    }
    
    
    
    # Cap the maximum value of z
    background_bars <- getBackgroundBarsTemplate(var)
    maxval <- background_bars$ymax[length(background_bars$ymax)]
    idw_grid$z <- pmin(idw_grid$z, maxval)
    
    
    
    # Create a raster plot
    d <- data.frame(
      x = idw_grid$x,
      y = idw_grid$y,
      z = idw_grid$z
    )
    RasterPlot <- rasterFromXYZ(d[, c("x", "y", "z")])
    projection(RasterPlot) <- "+proj=longlat +datum=WGS84"
    
    return(RasterPlot)
  }
  
  # Function to generate a color palette for the heatmap
  getHeatmapColor <- function(var){
    if(var == "Air Quality Index")
    {
      custom_colors <- c("green", "yellow", "orange", "red", "purple", "purple", "#7E0023", "#7E0023", "#7E0023", "#7E0023")
      pal <- colorNumeric(palette = custom_colors, domain = c(0, 500))
      #notably ths makes it scale with how close it is to the next range, ie. 99 is bright yellow 51 is dull yellow, below its just universaaly yellow
    }
    else{
      background_bars_template <- getBackgroundBarsTemplate(var)
      print(background_bars_template)
      pal <- colorRampPalette(background_bars_template$fill)(length(background_bars_template$fill) + 1)
      pal <- colorNumeric(palette = pal, domain = c(min(background_bars_template$ymin), max(background_bars_template$ymax)))
    }
    
    return(pal)
  }
  
  
  
  
  
  # Function to generate a 24-hour graph for the specified dataset
  twentyFourHourGraph <- function(dataset) {
    
    CurrentMapSensorId(first(dataset$sensor_index))
    
    #update the choices for when you go to analyze
    updateSelectInput(session, "sensor_dropdown",
                      choices = getSensorDataDB()$name,
                      selected = first(dataset$name))
    
    
    start_date <- as.POSIXct(Sys.Date() - 40, tz = "UTC")
    end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    
    tryCatch({
      p <- GraphPlot(dataset, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_input_map))
    }, error = function(e) {
      p <- ggplot() + 
        labs(title = "Unfortunately a Plot could not be displayed. \n Try changing the date range or switching to a different sensor") +
        theme_void() +  # Remove all elements
        theme(
          plot.title = element_text(size = 25, color = "white", family = "mono", face = "bold"),
          plot.background = element_rect(fill = "transparent", color = NA)  # Make background transparent
        )
      
      # Print the plot
      print(p)
    })
    
    output$plotInfo <- renderGirafe({
      p
    })
  }
  
  # Function to add marker animation to the map
  addMarkerAnimation <- function(dataset) {
    leafletProxy("AQMap") %>%
      clearGroup("markers2") %>%
      addCircleMarkers(
        data = dataset,
        lng = ~longitude,
        lat = ~latitude,
        stroke = TRUE,
        fillOpacity = 0.85,
        group = "markers2",
        radius = if(dataset$source=="PurpleAir"){17}else{24},
        color = getColor(dataset[[input$variable_select_input_map]], input$variable_select_input_map)
      )
  }
  
  
  
  # Create the Leaflet map object
  output$AQMap <- renderLeaflet({
    FilteredSensorDataDB <- getSensorDataDB() %>%
      filter(!is.na(.[[input$variable_select_input_map]]))
    EPA_data <- FilteredSensorDataDB %>% filter(source != SourceToShapeMapper[[1]])
    PurpleAir_data <- FilteredSensorDataDB %>% filter(source == SourceToShapeMapper[[1]])
    
    
    leaflet(FilteredSensorDataDB) %>%
      addRasterImage(
        getHeatmap(input$variable_select_input_map),
        colors = getHeatmapColor(input$variable_select_input_map),
        opacity = 0.2,
        options = tileOptions(zIndex = 999) # Ensuring the heatmap is on top
      ) %>%
      
      addProviderTiles(provider = providers$CartoDB.VoyagerLabelsUnder, group = "Light Theme") %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter, group = "Dark Theme") %>%
      addLayersControl(
        baseGroups = c("Light Theme", "Dark Theme"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = InitialMapView[2], lat = InitialMapView[1], zoom = InitialMapView[3]) %>%
      addCircleMarkers(
        data = PurpleAir_data,
        lng = ~longitude,
        lat = ~latitude,
        color = getColor(PurpleAir_data[[input$variable_select_input_map]], input$variable_select_input_map),
        radius = 15,
        stroke = FALSE,
        fillOpacity = 0.85
      ) %>%
      addMarkers(
        data = EPA_data,
        lng = ~longitude,
        lat = ~latitude,
        icon = tryCatch({
          icons(
            iconUrl = lapply(1:nrow(EPA_data), function(i) {
              source <- EPA_data$source[i]
              #print(EPA_data[[input$variable_select_input_map]])
              #print(getColor(EPA_data[[input$variable_select_input_map]], input$variable_select_input_map))
              hex_color <- getColor(EPA_data[[input$variable_select_input_map]], input$variable_select_input_map)
              num_sides <- SourceToShapeMapper[[source]]
              generate_star_svg(hex_color[i], size = 50, num_sides = num_sides)
            }),
            iconWidth = 50, iconHeight = 50
          )
        }, error = function(e) {
          return(NULL)
        })
        
      ) %>%
      addLabelOnlyMarkers(
        data = FilteredSensorDataDB,
        lng = ~longitude,
        lat = ~latitude,
        label = FilteredSensorDataDB[[input$variable_select_input_map]],
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "10px"
          )
        ),
        group = "labelMarkers"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = rev(getBackgroundBarsTemplate(input$variable_select_input_map)$fill),
        labels = rev(getBackgroundBarsTemplate(input$variable_select_input_map)$label),
        title = getBackgroundBarsTemplate(input$variable_select_input_map)$var[1]
      )
  })
  
  # Observe map hover event and update plot and marker animation
  observe({
    SensorDataDB <- getSensorDataDB()
    SensorDataDBHistorical <- getSensorDataDBHistorical()
    hovered_lat <- SensorDataDB$latitude[2]
    hovered_lng <- SensorDataDB$longitude[2]
    lat_lng_filter <- SensorDataDB$latitude == hovered_lat & SensorDataDB$longitude == hovered_lng
    marker_data <- SensorDataDB[lat_lng_filter, ]
    lat_lng_filter_historical <- SensorDataDBHistorical$latitude == hovered_lat & SensorDataDBHistorical$longitude == hovered_lng
    historical_data <- SensorDataDBHistorical[lat_lng_filter_historical, ]
    
    addMarkerAnimation(marker_data)
    twentyFourHourGraph(historical_data)
    
  })
  
  # Observe map zoom level changes and update marker visibility
  observe({
    zoom <- input$AQMap_zoom
    if (!is.null(zoom)) {
      if (zoom < 11) {
        leafletProxy("AQMap", session) %>%
          hideGroup("labelMarkers")
      } else {
        leafletProxy("AQMap", session) %>%
          showGroup("labelMarkers")
      }
    }
  })
  
  # Observe map marker click event and update plot and marker animation
  observeEvent(input$AQMap_marker_click, {
    SensorDataDB <- getSensorDataDB()
    SensorDataDBHistorical <- getSensorDataDBHistorical()
    hovered_lat <- input$AQMap_marker_click$lat
    hovered_lng <- input$AQMap_marker_click$lng
    lat_lng_filter <- SensorDataDB$latitude == hovered_lat & SensorDataDB$longitude == hovered_lng
    marker_data <- SensorDataDB[lat_lng_filter, ]
    lat_lng_filter_historical <- SensorDataDBHistorical$latitude == hovered_lat & SensorDataDBHistorical$longitude == hovered_lng
    historical_data <- SensorDataDBHistorical[lat_lng_filter_historical, ]
    
    addMarkerAnimation(marker_data)
    twentyFourHourGraph(historical_data)
  })
  
  # Handle the event when the graph button is clicked
  observeEvent(input$go_to_analyze, {
    updateTabItems(session, "tabs", selected = "analyze")
    
    
    
    print(CurrentMapSensorId)
  })
  

  
  
  #Adding select sensor dropdown event that changes the analyze tab
  
  observeEvent(input$sensor_dropdown, {
    CurrentMapSensorId(getSensorDataDB()$sensor_index[getSensorDataDB()$name == input$sensor_dropdown])
    print(CurrentMapSensorId())
  })
  
  
  
  # ---- Analyze Graph ----
  output$Line_Graph_Analyze <- renderGirafe({
    
    # Check if the custom date range is selected and not null
    if (input$time_select_input_line_graph_analyze == "custom range" && !is.null(input$custom_date_range)) {
      start_date <- input$custom_date_range[1]
      end_date <- input$custom_date_range[2]
    } else {
      # Otherwise, use the predefined time ranges
      time_input <- switch(input$time_select_input_line_graph_analyze,
                           "1 day" = 1,
                           "7 days" = 7,
                           "30 days" = 30,
                           "90 days" = 90,
                           "365 days" = 365)
      start_date <- as.POSIXct(Sys.Date() - days(time_input), tz = "UTC")
      end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    }
    
    # Query data based on the selected date range and current sensor ID
    dataset <- query_data(start_date, end_date, CurrentMapSensorId())
    
    # Generate and print the graph plot using the dataset and date range
    tryCatch({
      print(GraphPlot(dataset, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_input_line_graph_analyze)))
    }, error = function(e) {
      p <- ggplot() +
        labs(title = "Unfortunately a Plot could not be displayed.\n\nTry changing the date range or switching to a different sensor") +
        theme_void() +  # Remove all elements
        theme(
          plot.title = element_text(size = 12, color = "white", family = "mono", face = "bold"),
          plot.background = element_rect(fill = "#1F1F1F", color = NA)  # Make background transparent
        )
      
      # Convert ggplot to a girafe plot
      girafe_plot <- girafe(ggobj = p)
      
      # Print the girafe plot
      print(girafe_plot)
    })
  })
  
  
  
  # Render the title for the line graph
  output$LineGraphTitle <- renderUI({
    paste("What is the current", gsub("_", " ", input$variable_select_input_line_graph_analyze), "?")
  })
  
  # Render the title for the calendar plot
  output$CalendarPlotTitle <- renderUI({
    paste(gsub("_", " ", input$variable_select_input_line_calendar_analyze), "Calendar Plot")
  })
  
  
  
  # Function to generate the graph plot
  GraphPlot <- function(dataset, start_date, end_date, background_bars) {
    
  
    
    
    print("test1")
    # Filter the dataset based on the date range
    dataset <- dataset[dataset$Date >= start_date & dataset$Date <= end_date, ]
    
    dataset <- dataset %>%
      filter(!is.na(.data[[first(background_bars$var)]]))
    
    
    # Determine the start and end dates of the filtered dataset
    start_date <- min(dataset$Date)
    end_date <- max(dataset$Date)
    
    # Find the maximum y-value in the dataset for the first variable in background bars
    max_y_value <- max(na.omit(dataset[[first(background_bars$var)]]))
    
    min_y_value <- min(min(na.omit(c(dataset[[first(background_bars$var)]]))), first(background_bars$ymin))
    
    # Initialize the ggplot object
    p <- ggplot()
    
    # Special handling for PurpleAir source with Air Quality Index variable
    if(first(background_bars$var) == "Air Quality Index" & first(dataset$source) == "PurpleAir") {
      
      # Subset the dataset to remove NA values for pm2.5_60minute_a
      dataset <- subset(dataset, !is.na(pm2.5_a_dashboard))
      dataset <- subset(dataset, !is.na(pm2.5_b_dashboard))
      dataset$Air_Quality_Index_A <- dataset$pm2.5_a_dashboard #calculate_aqi_pm25(dataset$pm2.5_aqi_a_dashboard)
      dataset$Air_Quality_Index_B <- dataset$pm2.5_b_dashboard #calculate_aqi_pm25(dataset$pm2.5_aqi_b_dashboard)
      
      # Determine the maximum values for both Air Quality Index A and B
      max_a <- max(dataset$Air_Quality_Index_A, na.rm = TRUE)
      max_b <- max(dataset$Air_Quality_Index_B, na.rm = TRUE)
      max_value <- max(max_a, max_b, na.rm = TRUE)
      
      # Update the maximum y-value if necessary
      if (!is.na(max_value) && max_value > max_y_value) {
        max_y_value <- max_value
      }
      
      # Add lines and points for Air Quality Index A and B to the plot
      p <- p +
        geom_line_interactive(data = dataset, aes(x = Date, y = .data[["Air_Quality_Index_A"]], color = name),
                              size = 1, alpha = 0.2) +
        geom_point_interactive(data = dataset, aes(x = Date, y = .data[["Air_Quality_Index_A"]], color = name),
                               size = 7, alpha = 0) +
        geom_line_interactive(data = dataset, aes(x = Date, y = .data[["Air_Quality_Index_B"]], color = name),
                              size = 1, alpha = 0.2) +
        geom_point_interactive(data = dataset, aes(x = Date, y = .data[["Air_Quality_Index_B"]], color = name),
                               size = 7, alpha = 0)
    }
    
    # Adjust y-axis scale based on the maximum y-value
    if (max_y_value > 101) {
      p <- p + scale_y_continuous(limits = c(0, max_y_value))
      scaletop <- max_y_value
    } else {
      p <- p + scale_y_continuous(limits = c(0, 101))
      scaletop <- 101
    }
    
    # Adjust the background bars to fit within the y-axis scale
    maxval <- background_bars$ymax[length(background_bars$ymax)]
    if(!is.null(dataset[[first(background_bars[background_bars$ymin <= scaletop - 6, ]$var)]]))
    { 
      background_bars <- background_bars[background_bars$ymin <= scaletop - 6, ]
    }
    background_bars$ymax[length(background_bars$ymax)] <- scaletop
    
    
    
    # Add a tooltip label to the dataset
    dataset <- dataset %>%
      mutate(tooltip_label = as.character(glue::glue("{name}<br>{first(background_bars$var)}: {ceiling(dataset[[first(background_bars$var)]])}<br>{Date_Only}")))
    
    print("test2")
    
    # Add background bars, lines, points, and labels to the plot
    p <- p +
      scale_fill_identity() +
      geom_rect(data = background_bars,
                aes(xmin = start_date, xmax = end_date, ymin = ymin, ymax = ymax, fill = fill),
                color = NA, alpha = 0.08) +
      scale_x_datetime(labels = scales::time_format("%I %p\n%b %d"), breaks = pretty_breaks(n = 5)) +
      geom_line_interactive(data = dataset, aes(x = Date, y = .data[[first(background_bars$var)]], color = name, data_id = sensor_index),
                            size = 1, alpha = 1) +
      geom_point_interactive(data = dataset, aes(x = Date, y = .data[[first(background_bars$var)]], color = name,
                                                 tooltip = tooltip_label, data_id = sensor_index),
                             size = 7, alpha = 0) +
      geom_hline(yintercept = background_bars$ymin, color = background_bars$fill, size = 0.5, na.rm = TRUE) +  # Make border lines thinner
      geom_label_repel(aes(x = max(dataset$Date), y = background_bars$ymin, label = background_bars$label),
                       fill = "#171717", color = background_bars$fill,
                       size = 5.5, fontface = "bold", vjust = 0.5, hjust = 0, show.legend = FALSE, segment.color = NA,
                       box.padding = unit(0.2, "lines"), label.padding = unit(0.2, "lines"),
                       label.r = 0.3, na.rm = TRUE, label.size = 0) +
      labs(x = "Day", y = "") +
      coord_cartesian(ylim = c(min_y_value, min(scaletop, maxval))) +
      ggtitle(paste("Current", gsub("_", " ", first(background_bars$var)))) +
      dark_theme() +
      theme(
        plot.title = element_text(face = "bold", size = 18),  # Make title bold
        axis.title.x = element_text(face = "bold", size = 14),  # Make x-axis title bold and adjust size
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 7),  # Adjust legend text size
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(3, 3, 3, 3),
        legend.background = element_rect(fill = alpha("#000000", 0.4))
      )
    
    # Create a girafe object with the ggplot and interactive options
    t <- girafe(ggobj = p, options = list(
      opts_hover(css = "stroke-width:3.5; opacity: 1;"),
      opts_hover_inv(css = "opacity: 0.1;"),
      opts_tooltip(css = "background-color: rgba(0, 0, 0, 0.8); color: white; border-radius: 5px; padding: 5px;"),
      opts_selection(type = "none")
    ))
    
    return(t)  # Return the girafe object
  }
  
  
  # ---- Calendar Plot ----
  
  # Render the calendar plot
  output$Calendar_Plot_Analyze <- renderPlot({
    tryCatch({
      # Print the current month
      #print(lubridate::month(Sys.Date()))
      
      # Get the current month and year
      CurrentMonth = lubridate::month(Sys.Date())
      CurrentYear = lubridate::year(Sys.Date())
      
      # Query data for the past 500 days
      dataset <- query_data(Sys.time()-days(500), Sys.time(), CurrentMapSensorId())
      
      # Filter data based on the selected time range (month or year)
      if (input$time_select_input_line_calendar_analyze == "month") {
        filtered_SensorDataDBHistorical <- dataset %>%
          filter(year(Date_Only) == CurrentYear & month(Date_Only) == CurrentMonth)
      } else {
        filtered_SensorDataDBHistorical <- dataset %>%
          filter(year(Date_Only) == CurrentYear)
      }
      
      # Group and summarize data by sensor index and date
      VariableFilter <- filtered_SensorDataDBHistorical %>%
        group_by(sensor_index, Date_Only) %>%
        summarise(VariableFilter = mean(!!sym(input$variable_select_input_line_calendar_analyze), na.rm = TRUE))
      
      # Filter data for the current sensor
      filtered_data <- VariableFilter %>%
        filter(sensor_index == CurrentMapSensorId())
      
      # Get colors for the data
      filtered_data$colors <- getColor(filtered_data$VariableFilter, input$variable_select_input_line_calendar_analyze)
      
      # Extract day and year numbers
      filtered_data$Day_Number <- as.numeric(format(filtered_data$Date_Only, "%d"))
      filtered_data$Year_Number <- as.numeric(format(filtered_data$Date_Only, "%j"))
      
      # Get legend data
      legendData <- getBackgroundBarsTemplate(input$variable_select_input_line_calendar_analyze)
      color_names <- legendData$fill
      text_values <- legendData$label
      
      # Truncate text values longer than 8 characters
      text_values <- substr(text_values, 1, 6)
      
      # Combine color and text into a named vector
      color_text_mapping <- setNames(text_values, color_names)
      
      # Plot the calendar for the selected time range (month or year)
      if (input$time_select_input_line_calendar_analyze == "month") {
        events <- rep(NA, lubridate::days_in_month(CurrentMonth))
        events[filtered_data$Day_Number] <- as.character(sprintf("%04d", filtered_data$Day_Number))
        desired_order <- as.character(filtered_data$Day_Number)
        ordered_colors <- filtered_data$colors[order(desired_order)]
        p <- calendR(
          month = CurrentMonth,
          special.days = events,
          special.col = adjustcolor(ordered_colors, alpha.f = 0.3),
          text.pos = as.numeric(events[!sapply(events, is.na)]),
          text = color_text_mapping[ordered_colors],
          text.col = "white",
          text.size = 5,  
          bg.img = "custom_background.png",
          title = "",
          title.size = 0,
          title.col = 1,
          weeknames = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
          start = "M",
          col = "#f2f2f2",
          lwd = 1,
          lty = 1,
          mbg.col = 4,
          months.col = "white",
          font.family = "mono",
          font.style = "bold",
          weeknames.col = "white",
          days.col = "white",
          day.size = 3.5
        )
      } else {
        events <- rep(NA, ifelse(leap_year(year(Sys.Date())), 366, 365))
        events[filtered_data$Year_Number] <- as.character(filtered_data$Year_Number)
        desired_order <- as.character(filtered_data$Year_Number)
        ordered_colors <- filtered_data$colors[order(desired_order)]
        p <- calendR(
          special.days = events,
          special.col = adjustcolor(ordered_colors, alpha.f = 0.6),
          title = "",
          title.size = 0,
          title.col = 1,
          weeknames = c("M", "T", "W", "T", "F", "S", "S"),
          start = "M",
          col = "#f2f2f2",
          lwd = 0.5,
          lty = 1,
          mbg.col = "#2E056B",
          bg.img = "custom_background.png",
          months.col = "white",
          font.family = "mono",
          font.style = "bold",
          weeknames.col = "white",
          days.col = "white",
          day.size = 3
        )
      }
      
      # Print the plot
      
      
      
      print(p)
    }, error = function(e) {
      p <- calendR(
        month = CurrentMonth,
        text.col = "transparent",  # Make day numbers transparent
        text.size = 5,
        bg.img = "custom_background.png",
        title = "Unfortunately a Calendar could not be displayed. \n\n Try changing the date range or sensor",
        title.size = 15,
        title.col = "white",  # Title color remains visible
        weeknames = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        start = "M",
        col = "transparent",  # Make calendar background transparent
        lwd = 0,  # Remove lines
        lty = 0,  # Remove line types
        mbg.col = "transparent",  # Make month background transparent
        months.col = "transparent",  # Make month labels transparent
        font.family = "mono",
        font.style = "bold",
        weeknames.col = "transparent",  # Make week names transparent
        days.col = "transparent",  # Make day labels transparent
        day.size = 0
      )
      
      
      
      # Print the plot
      
      
      
      print(p)
    })
  })
  
  # Function to generate the legend HTML
  generate_legend <- function(text_list, color_list) {
    legend_html <- "<div style='padding-top: 10px; display: flex; flex-wrap: wrap;'>"
    for (i in seq_along(text_list)) {
      legend_item_html <- paste0(
        "<div style='margin-bottom: 10px; margin-right: 10px;'>",
        "<div style='background-color: ", color_list[i], "; width: 15px; height: 15px; display: inline-block; margin-right: 5px;'></div>",
        "<span>", text_list[i], "</span>",
        "</div>"
      )
      legend_html <- paste0(legend_html, legend_item_html)
    }
    legend_html <- paste0(legend_html, "</div>")
    return(legend_html)
  }
  
  # Render the legend HTML in the UI
  output$legend_output <- renderUI({
    legendData <- getBackgroundBarsTemplate(input$variable_select_input_line_calendar_analyze)
    legend_html <- generate_legend(legendData$label, legendData$fill)
    HTML(legend_html)
  })
  
  
  
  
  
  # Render the Leaflet map for AQMapCompare
  output$AQMapCompare <- renderLeaflet({
    # Filter SensorDataDB to exclude rows with NA values in the selected variable
    FilteredSensorDataDB <- getSensorDataDB() %>%
      filter(!is.na(.[[input$variable_select_compare]]))
    
    EPA_data <- FilteredSensorDataDB %>% filter(source != SourceToShapeMapper[[1]])
    PurpleAir_data <- FilteredSensorDataDB %>% filter(source == SourceToShapeMapper[[1]])
    
    # Create the Leaflet map
    leaflet(FilteredSensorDataDB) %>%
      addProviderTiles(provider = providers$CartoDB.VoyagerLabelsUnder, group = "Light Theme") %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter, group = "Dark Theme") %>%
      addLayersControl(
        baseGroups = c("Light Theme", "Dark Theme"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = InitialMapView[2], lat = InitialMapView[1], zoom = InitialMapView[3]) %>%
      addCircleMarkers(
        data = PurpleAir_data,
        lng = ~longitude,
        lat = ~latitude,
        color = getColor(PurpleAir_data[[input$variable_select_compare]], input$variable_select_compare),
        radius = 15,
        stroke = FALSE,
        fillOpacity = 0.85,
        layerId = ~sensor_index
      ) %>%
      addMarkers(
        data = EPA_data,
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~sensor_index,
        icon = tryCatch({
          icons(
            iconUrl = lapply(1:nrow(EPA_data), function(i) {
              source <- EPA_data$source[i]
              hex_color <- getColor(EPA_data[[input$variable_select_input_map]], input$variable_select_input_map)
              num_sides <- SourceToShapeMapper[[source]]
              generate_star_svg(hex_color[i], size = 50, num_sides = num_sides)
            }),
            iconWidth = 50, iconHeight = 50
          )
        }, error = function(e) {
          return(NULL)
        })
        
      ) %>%
      addLabelOnlyMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = FilteredSensorDataDB[[input$variable_select_compare]],
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, style = list(
          "color" = "black !important",
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
  
  # Observe zoom level changes to hide/show label groups
  observe({
    # Get the current zoom level
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
    
    # Check the zoom level again for a different group of labels
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
  
  # Reactive value to store the selected sensor
  selectedSensor <- reactiveVal(NULL)
  
  # Reactive values to store sensor lists
  sensorLists <- reactiveValues(
    list1 = character(),
    list2 = character()
  )
  
  # Observe map clicks and show modal to select sensor list
  observeEvent(input$AQMapCompare_marker_click, {
    clickedSensorId <- input$AQMapCompare_marker_click$id
    clickedSensor <- getSensorDataDB() %>% filter(sensor_index == clickedSensorId) %>% pull(name)
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
  
  # Show modal for selecting a preset
  observeEvent(input$addPresetButton, {
    showModal(modalDialog(
      title = "Add Preset",
      selectInput("presetSelect", "Choose a Preset", choices = names(presets)),
      radioButtons("listSelect", "Add to List", choices = c("List 1", "List 2")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmAddPreset", "Add Preset")
      )
    ))
  })
  
  # Add selected preset to the chosen list
  observeEvent(input$confirmAddPreset, {
    preset <- presets[[input$presetSelect]]
    if (input$listSelect == "List 1") {
      sensorLists$list1 <- unique(c(sensorLists$list1, preset))
    } else {
      sensorLists$list2 <- unique(c(sensorLists$list2, preset))
    }
    removeModal()
  })
  
  
  
  
  
  
  # Add sensor to list 1
  observeEvent(input$addList1, {
    isolate({
      sensor <- selectedSensor()
      #if (!is.null(sensor) && !sensor %in% sensorLists$list1) {
      #  sensorLists$list1 <- unique(c(sensorLists$list1, sensor))
      #}
      sensorLists$list1 <- unique(c(sensorLists$list1, sensor))
      removeModal()
    })
  })
  
  # Add sensor to list 2
  observeEvent(input$addList2, {
    isolate({
      sensor <- selectedSensor()
      #if (!is.null(sensor) && !sensor %in% sensorLists$list2) {
      #  sensorLists$list2 <- unique(c(sensorLists$list2, sensor))
      #}
      sensorLists$list2 <- unique(c(sensorLists$list2, sensor))
      removeModal()
    })
  })
  
  # Render UI for sensor list 1
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
  
  # Render UI for sensor list 2
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
  
  # Render the line plot for sensor list 1 using the previusly made graphplot function
  output$SensorListOneLinePlot <- renderGirafe({
    if (input$time_select_input_line_graph_compare == "custom range" && !is.null(input$custom_date_range_compare)) {
      start_date <- as.POSIXct(input$custom_date_range_compare[1], tz = "UTC")
      end_date <- as.POSIXct(input$custom_date_range_compare[2], tz = "UTC")
      
    } else {
      # Otherwise, use the predefined time ranges
      time_input <- switch(input$time_select_input_line_graph_compare,
                           "1 day" = 1,
                           "7 days" = 7,
                           "30 days" = 30,
                           "90 days" = 90,
                           "365 days" = 365)
      start_date <- as.POSIXct(Sys.Date() - days(time_input), tz = "UTC")
      end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    }
    
    dataset <- query_data(start_date, end_date, NULL)
    
    all_sensors <- sensorLists$list1
    
    filtered_data <- dataset %>%
      filter(name %in% all_sensors)
    
    
    avg_data <- filtered_data %>%
      group_by(Date_Only) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))#, .names = "avg_{.col}"))
    avg_data$name <- "List One Average"
    avg_data$source <- "List One Average"
    
    
    tryCatch({
      if(input$averageCheckboxCompare==TRUE)
      {
        p <- GraphPlot(avg_data, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_compare))
      }
      else
      {
        #removes ghost lines from this plot
        filtered_data$source <- "EPA"
        p <- GraphPlot(filtered_data, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_compare))
      }
      
      print(p)
    }, error = function(e) {
      p <- ggplot() +
        theme_void() +  # Remove all elements
        theme(
          plot.background = element_rect(fill = "#1F1F1F", color = NA)  # Make background transparent
        )
      
      # Convert ggplot to a girafe plot
      girafe_plot <- girafe(ggobj = p)
      
      # Print the girafe plot
      print(girafe_plot)
    })
    
    
  })
  
  # Render the line plot for sensor list 2 using the previusly made graphplot function
  output$SensorListTwoLinePlot <- renderGirafe({
    if (input$time_select_input_line_graph_compare == "custom range" && !is.null(input$custom_date_range_compare)) {
      start_date <- as.POSIXct(input$custom_date_range_compare[1], tz = "UTC")
      end_date <- as.POSIXct(input$custom_date_range_compare[2], tz = "UTC")
      
    } else {
      # Otherwise, use the predefined time ranges
      time_input <- switch(input$time_select_input_line_graph_compare,
                           "1 day" = 1,
                           "7 days" = 7,
                           "30 days" = 30,
                           "90 days" = 90,
                           "365 days" = 365)
      start_date <- as.POSIXct(Sys.Date() - days(time_input), tz = "UTC")
      end_date <- as.POSIXct(Sys.Date() + 1, tz = "UTC") - seconds(1)
    }
    
    
    dataset <- query_data(start_date, end_date, NULL)
    
    all_sensors <- sensorLists$list2
    
    filtered_data <- dataset %>%
      filter(name %in% all_sensors)
    
    
    
    
    
    avg_data <- filtered_data %>%
      group_by(Date_Only) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))#, .names = "avg_{.col}"))
    avg_data$name <- "List Two Average"
    avg_data$source <- "List One Average"
    
    tryCatch({
      if(input$averageCheckboxCompare==TRUE)
      {
        
        p <- GraphPlot(avg_data, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_compare))
      }
      else
      {
        #removes ghost lines from this plot
        filtered_data$source <- "EPA"
        p <- GraphPlot(filtered_data, start_date, end_date, getBackgroundBarsTemplate(input$variable_select_compare))
      }
      
      print(p)
    }, error = function(e) {
      p <- ggplot() +
        theme_void() +  # Remove all elements
        theme(
          plot.background = element_rect(fill = "#1F1F1F", color = NA)  # Make background transparent
        )
      
      # Convert ggplot to a girafe plot
      girafe_plot <- girafe(ggobj = p)
      
      # Print the girafe plot
      print(girafe_plot)
    })
    
  })
  
  
  
  
  
  
  
  
  
  # Render the title for the line graph
  output$LineGraphTitle <- renderUI({
    paste("What is the current", gsub("_", " ", input$variable_select_input_line_graph_analyze), "?")
  })
  
  # Render the title for the calendar plot
  output$CalendarPlotTitle <- renderUI({
    paste(gsub("_", " ", input$variable_select_input_line_calendar_analyze), "Calendar Plot")
  })
  
  
  
  
  
  
  
  
  
  
  # Define the RSS feed URLs
  rss_urls <- c(
    "https://siftrss.com/f/e9z1dQgXqp"
  )
  
  # Function to parse RSS feed using xml2 package
  parse_rss_xml2 <- function(url) {
    rss <- read_xml(url)
    data <- data.frame(
      title = xml_text(xml_find_all(rss, "//item/title")),
      description = xml_text(xml_find_all(rss, "//item/description")),
      link = xml_text(xml_find_all(rss, "//item/link")),
      pub_date = ymd_hms(xml_text(xml_find_all(rss, "//item/pubDate")))
    )
    return(data)
  }
  
  # Function to get most recent articles from multiple feeds
  get_recent_articles <- function() {
    all_articles <- lapply(rss_urls, parse_rss_xml2)
    combined_articles <- do.call(rbind, all_articles)
    sorted_articles <- combined_articles[order(combined_articles$pub_date, decreasing = TRUE), ]
    return(sorted_articles[1:3, ])
  }
  
  # Function to render feed output
  output$feed_output <- renderUI({
    recent_articles <- get_recent_articles()
    
    if (nrow(recent_articles) > 0) {
      article_boxes <- lapply(1:nrow(recent_articles), function(i) {
        div(
          class = "article-box",
          tags$h4(HTML(paste("<b>", recent_articles$title[i], "</b>"))), # Make the title bold
          p(recent_articles$description[i]),
          p(paste("Published:", format(recent_articles$pub_date[i], "%B %d, %Y"))),
          tags$a("Read more", href = recent_articles$link[i], target = "_blank")
        )
      })
      
      tagList(article_boxes)
    } else {
      p("No articles found.")
    }
  })
  
  
  
  send_sms <- function(to, body) {
    url <- "https://rest.nexmo.com/sms/json"
    response <- tryCatch({
      POST(
        url,
        add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
        body = list(
          api_key = nexmo_api_key,
          api_secret = nexmo_api_secret,
          to = to,
          from = nexmo_from,
          text = body
        ),
        encode = "form"
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(response)) {
      return("Failed to send SMS.")
    }
    
    response_content <- content(response, "text")
    print(response_content)  # Print API response for debugging
    response_content
  }
  
  # Create a global vector to store unique phone numbers
  phone_numbers <- c()
  
  
  
  # Function to send SMS to all collected phone numbers
  send_notifications <- function() {
    for (phone in phone_numbers) {
      res <- send_sms(phone, notification_message)
    }
    # Schedule the next notification
    later::later(send_notifications, SMSTimer) # 600 seconds = 10 minutes
  }
  
  # Start the notification timer and store the task handle
  task_handle <- NULL
  
  start_notifications <- function() {
    if (is.null(task_handle)) {
      task_handle <<- later::later(send_notifications, 0)
    }
  }
  
  #--SMS Notification--
  observeEvent(input$subscribe, { 
    phone <- input$phone
    # Validate the phone number format
    if (grepl("^\\+?[1-9]\\d{1,14}$", phone)) {
      # Save the phone number to the global vector if not already present
      if (!phone %in% phone_numbers) {
        phone_numbers <<- c(phone_numbers, phone)
        showNotification("Phone number submitted!", type = "message")
      } else {
        showNotification("Phone number already submitted!", type = "warning")
      }
    } else {
      showNotification("Invalid phone number format. Please use +1XXXXXXX format.", type = "error")
    }
    # Clear the input field
    #updateTextInput(session, "phone", value = "") - causes error
  })
  
  observeEvent(input$unsubscribe, {
    phone <- input$unsubscribe_phone
    # Validate the phone number format
    if (grepl("^\\+?[1-9]\\d{1,14}$", phone)) {
      # Remove the phone number from the global vector if present
      if (phone %in% phone_numbers) {
        phone_numbers <<- phone_numbers[phone_numbers != phone]
        showNotification("Phone number unsubscribed!", type = "message")
      } else {
        showNotification("Phone number not found!", type = "warning")
      }
    } else {
      showNotification("Invalid phone number format. Please use +1XXXXXXX format.", type = "error")
    }
    # Clear the input field
    #updateTextInput(session, "unsubscribe_phone", value = "") - causes error
  })
  
  # Start the notifications when the app starts
  start_notifications()

}


shinyApp(ui, server)

        
