# R Shiny Map Dashboard App

# Features!

This app written in R shiny has 5 Different Tabs:

## Map Tab 

The map tab creates a world map view of your data, including selectable points with a graph, the ability to have several variables displayed, an interpolation heatmap, and support for several data sources each with their own icon on the map.

![image](https://github.com/user-attachments/assets/bc394d83-d162-4a5b-8de2-206d50ea366d)

## Analyze Tab

This tab allows you to take a deep dive into a specific point of data, seeing a calendar map and a line graph of the current point for any date range or variable.

![image](https://github.com/user-attachments/assets/71579b23-b0cd-44b8-bbf3-769dde3ee4e7)

## Compare Locations Tab

This tab allows you to click on points on the map in order to add them to list, then analyze comparison graphs for the 2 lists created. Presets can be created in the server to allow easy comparisons in the app.

![image](https://github.com/user-attachments/assets/4591e91c-8794-4d01-a438-dd3d89388fd6)
![image](https://github.com/user-attachments/assets/d95428b2-d1ca-4f27-8535-14126502a7fe)



## Home and Help 
These 2 tabs give direction to your project, and redirect users who are confused by the site:
![image](https://github.com/user-attachments/assets/b0632a39-41c5-4073-afff-c6f585ff513a)
![image](https://github.com/user-attachments/assets/b44e19c3-a9aa-4e47-ad13-42c6818cba42)




Welcome to using the R Shiny Map Dashboard App! Below is the configuration and documentation on how to make this app customizable to your use case.

If you know what you are doing and what docs for the source itself, visit:
https://docs.google.com/document/d/1kRoA1Xz_unkQCap3sQ6FgI87Z4Z0aHm1t8X5GNqOU-o/edit?usp=sharing

## Getting Started

### Prerequisites

- Ensure you have an SQL-style database with the necessary parameters, this code does not come with one built in.
- There is a permanent to the air quality sql dataset here: (placeholder), put the db.sql file in the same directory as the app.r file to use it as the db.
- Install R studio and open the project file to run the app.r.

### Database Requirements

The database is expected to have the following fields:
- `name`: Names of all the sensors
- `time_stamp`: Time in Unix format
- `source`: Source name of the data
- `sensor_index`: Unique ID for the sensor
- `latitude`: Latitude of the sensor
- `longitude`: Longitude of the sensor
- Various `value` fields like `humidity`, `temperature`, etc.

Note: It is recommended not to use the field name `Air_Quality_Index` unless you are using the provided database in the repo.

### Configuration
This is a replication of what you will see at the beginning of the app.r file, follow the instructions carefully and edit the config file to set up the app to your liking!

 1. **Database Path**
    
 Put the path to your DB below
   
  ```r
   PathToDB <- "db.sqlite"
  ```

2. **SQL Parameters**

   
   Change the list of parameters you plan to use from your sql database.
    ```r
    Sql_Params <- c("name", "time_stamp", "source", "sensor_index", "latitude", "longitude", "humidity", "temperature", "pressure", "\"pm2.5_aqi_dashboard\"", "\"pm2.5_dashboard\"", "\"pm2.5_aqi_a_dashboard\"", "\"pm2.5_aqi_b_dashboard\"")
    ```

3. **Display Parameters on Map**

   
   Outline the names of parameters you want to be displayed on the map in a dropdown.
    ```r
    getVarList <- function() {
      return(c("Air Quality Index", "Temperature Fahrenheit", "Pressure", "Humidity"))
    }
    ```

7. **Primary Field**

   
   Define which field is the primary one, this will filter all NA's in this field for all the data.
    ```r
    PrimaryField <- "Air Quality Index"
    ```

9. **Color Ranges for Parameters**

    
    For each of the parameters you have chosen, replace the numbers and colors below with whatever you want to make a custom range for what the colors are defined as.
    ```r
    getColor <- function(var, inputVar) {
      if (inputVar == "Air Quality Index") {
        ifelse(var <= 50, "Green", ifelse(var <= 100, "Yellow", ifelse(var <= 150, "Orange", ifelse(var <= 200, "Red", ifelse(var <= 300, "Purple", "#7E0023")))))
      } else if (inputVar == "Temperature Fahrenheit") {
        ifelse(var < 0, "Blue", ifelse(var <= 20, "LightBlue", ifelse(var <= 40, "LightGreen", ifelse(var <= 65, "Green", ifelse(var <= 80, "Yellow", ifelse(var <= 100, "Orange", "Red"))))))
      } else if (inputVar == "Pressure") {
        ifelse(var < 980, "Red", ifelse(var <= 1013, "Yellow", ifelse(var <= 1040, "Green", "Blue")))
      } else if (inputVar == "Humidity") {
        ifelse(var < 30, "Yellow", ifelse(var <= 50, "Green", "Blue"))
      }
    }
    ```

11. **Background Bars Template**

    
    Next, do a similar process below for each of those parameters, the data should be set up in color "buckets" where values beetween certain numbers fall into a bucket and get a certain color. Var should be just the name of the parameters, the labels are the names of the buckets you are making, the ymin and ymax represent the values that range each of the buckets, and the fill is the color for each of the buckets.

    Notably, ymin, ymax, and fill should match what you put in for getColor.
    
    If you want the heatmap colors to function properly, it is very important that each bucket is the same size. You can still have different ranges though, by just having several buckets that have the same color and name.

    ```r
    getBackgroundBarsTemplate <- function(input) {
      if (input == "Air Quality Index") {
        data.frame(var = "Air Quality Index", label = c("Good", "Moderate", "Unhealthy for Vulnerable People", "Unhealthy", "Very Unhealthy", "Hazardous"), ymin = rep(c(0, 50, 100, 150, 200, 300)), ymax = rep(c(50, 100, 150, 200, 300, 500)), fill = rep(c("Green", "Yellow", "Orange", "Red", "Purple", "#7E0023")))
      } else if (input == "Temperature Fahrenheit") {
        data.frame(var = "Temperature Fahrenheit", label = c("Freezing", "Cold", "Cool", "Warm", "Hot", "Extremely Hot"), ymin = rep(c(0, 20, 40, 60, 80, 100)), ymax = rep(c(20, 40, 60, 80, 100, 120)), fill = rep(c("LightBlue", "LightGreen", "Green", "Yellow", "Orange", "Red")))
      } else if (input == "Pressure") {
        data.frame(var = "Pressure", label = c("Low", "Normal", "High", "Extreme"), ymin = c(950, 980, 1010, 1040), ymax = c(980, 1010, 1040, 1070), fill = c("Red", "Yellow", "Green", "Blue"))
      } else if (input == "Humidity") {
        data.frame(var = "Humidity", label = c("Low", "Normal", "High"), ymin = c(0, 33, 66), ymax = c(33, 66, 100), fill = c("Yellow", "Green", "Blue"))
      }
    }
    ```

13. **Initial Map View**

    
    Choose the initial zoom location for the maps by setting the first 2 coordinates.
    ```r
    InitialMapView <- c(39.0890, -84.5008, 12) # lat, lng, zoom
    ```

15. **Presets for Compare Map View**

    
    The compare map view has a list of presets that allow you to easily set up comparisons, put the "name" parameter value for the sensors/points that you want to be in specific presets.
    ```r
    presets <- list(
      "Newport East" = c("Monitor 1 East PA","Monitor 12 East PA","Monitor 4 East PA","338 E 9th St Newport KY 41071 PA","home PA"),
      "Newport West" = c("Monitor 9 West PA","Monitor 7 West PA","Monitor 10 West PA","Monitor 8 West PA","Monitor 6 West PA"),
      "Newport All" = c("Monitor 1 East PA","Monitor 12 East PA","Monitor 4 East PA","338 E 9th St Newport KY 41071 PA","home PA","Monitor 9 West PA","Monitor 7 West PA","Monitor 10 West PA","Monitor 8 West PA","Monitor 6 West PA","Monitor 11 Island PA"),
      "Cincinnati Area" = c("McFarland PA", "Longworth Square PA", "Lower Price Hill EPA", "MSD 6 PA", "Meals on Wheels PA", "Taft NCore PAMS EPA", "CFD Station 12 PA", "City-CHD PA")
    )
    ```

17. **Shape and Source Definitions**

    
    Define the shapes and sources, you have a "primary" source, that will show up as a circle and secondary sources, which will have a shape with x sides, make a list with sourcename (no quotes) = x, so that you get the amount of desired sides for your shape, the first source should just have its own name as a string as shown below.
    ```r
    SourceToShapeMapper <- list(PurpleAir = "PurpleAir", EPA = 5, AQMesh = 3) # Circle, Pentagon, Triangle
    ```

19. **Logo and Target Links**

    
    Change the link for the logo on the sidebar. First one is the logo iself, second one is where clicking it sends you
    ```r
    logo_link <- "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/nkunorse.com/images/responsive/logo_main.svg"
    target_link <- "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/nkunorse.com/images/responsive/logo_main.svg"
    ```

21. **Theme Color**

    
    Choose the general color for the theme of the app.
    ```r
    ThemeColor <- "purple" # Options: "blue", "black", "green", "purple", "red", "yellow"
    ```

23. **Custom Heat Map Settings**

    
    Only mess with this if you know what you are doing. This app includes a heat map, and the variogram inverse distance weighting interpolation method for the heat map is listed below.

    You can swap beetween the custom heatmap below and a default IDW heatmap by changing UseCustomIDW to True/False.
    ```r
    UseCustomIDW <- TRUE

    variogram_model <- function(d, n, s, r) {
      n + s * (1 - exp(-d / r))
    }

    calculate_weights <- function(d, grid_point) {
      n <- 0.5 # nugget
      s <- 1000.5 # sill
      r <- 0.06 # range
      dists <- spDistsN1(as.matrix(d[, c("x", "y")]), as.numeric(grid_point), longlat = FALSE)
      var_values <- variogram_model(dists, n, s, r)
      weights <- (2 * n + s) - var_values
      weights / sum(weights)
    }

    interpolate <- function(d, grid_points) {
      sapply(1:nrow(grid_points), function(i) {
        weights <- calculate_weights(d, grid_points[i,])
        sum(weights * d$z)
      })
    }
    ```

25. **Custom CSS for Look and Feel**

    
    If you know CSS, you can use the code below to customize the entire look of the page in the actual code below this block.
    Additionally, you will have access to changing the entire home and help page text and images to suit what you wish your page to look like.
    

## Running the App

1. Place the script and the database in the same directory.
2. Run the app using the following command in R:
    ```r
    shiny::runApp("app_directory")
    ```

Replace `"app_directory"` with the path to your app directory.

## Usage

Upon running the app, you'll be able to visualize the sensor data on a Leaflet map, customize the displayed variables, and interact with various UI components to manage and visualize the data effectively.

Feel free to adjust the provided code snippets to better suit your specific use case. Enjoy exploring and visualizing your data with the R Shiny Map Dashboard App!
