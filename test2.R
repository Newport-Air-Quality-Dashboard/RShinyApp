library(tidyverse)
library(lubridate)
library(stevemisc) # You may need to install this package if not already installed

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

cutoff_date <- Sys.time() - days(3)
SensorDataDB <- SensorDataDB %>%
  filter(Date >= cutoff_date)


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








# Define important dates
not_here_dates <- c(ymd(20201103),
                    ymd(20201123), ymd(20201127))
exam_dates <- c(ymd(20201001), ymd(20201105), ymd(20201211))
semester_dates <- c(ymd(20200819), ymd(20201204))

# Create a data frame of dates
Cal <- tibble(date = seq(ymd(20200101), ymd(20201231), by = 1))

# Add information about each date
Cal <- Cal %>%
  mutate(
    mon = month(date, label = TRUE, abbr = FALSE), 
    wkdy = weekdays(date, abbreviate = TRUE), 
    wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
    semester = as.integer(date %in% semester_dates),
    exams = as.integer(date %in% exam_dates),
    not_here = as.integer(date %in% not_here_dates),
    day = day(date),
    week = wom(date)
  )

# Create categories for each date
Cal <- Cal %>%
  mutate(
    category = NA,
    category = if_else(semester == 1, "Semester", category),
    category = if_else(semester == 1 & wkdy %in% c("Tue", "Thu"), "Class Day", category),
    category = if_else(exams == 1, "Exams", category),
    category = if_else(is.na(category) | (semester == 1 & not_here == 1), "NA", category)
  )

# Create the calendar plot
calendar_plot <- Cal %>%
  ggplot(aes(wkdy, week)) +
  theme_steve_web() +
  geom_tile(aes(fill = category), color = "black", alpha = 0.8) +
  facet_wrap(~mon, scales = "free_x", ncol = 3) +
  geom_text(aes(label = day), family = "Open Sans") +
  scale_y_reverse(breaks = NULL) +
  scale_fill_manual(
    values = c("Class Day" = "steelblue", 
               "Semester" = "lightsteelblue",
               "NA" = "white",
               "Exams" = "indianred4"),
    breaks = c("Class Day", "Exams")
  ) +
  labs(
    fill = "", 
    x = "", 
    y = "",
    title = "A Hypothetical Semester Calendar for Your Particular Class",
    subtitle = "Customize this to make it yours.",
    caption = "Notable dates: Fall Break (Nov. 3... please vote), Thanksgiving Break (Nov. 23-27)"
  )

# Print the calendar plot
print(calendar_plot)

















#plotly code

#p <- ggplotly(p, tooltip = "text")
p <- ggplotly(p, tooltip = "text")

# Add hover styling to the lines
for (i in 1:length(p$x$data)) {
  if (p$x$data[[i]]$name == "lines") {
    p$x$data[[i]]$hoverinfo <- "text"
    p$x$data[[i]]$hoveron <- "plot"
    p$x$data[[i]]$line$width <- 10
  }
}

# Add annotations
annotations <- list()
for (i in 1:nrow(background_bars)) {
  annotations <- c(annotations, list(
    list(
      x = as.numeric(max(dataset$Date))- 60000,
      y = background_bars$ymin[i] + 4,
      text = background_bars$label[i],
      xref = 'x',
      yref = 'y',
      showarrow = FALSE,
      font = list(
        size = 19,
        color = background_bars$fill[i],
        family = 'Arial'
      ),
      align = 'right',
      valign = 'middle',
      bgcolor = "#17171799",
      borderpad = 4,
      bordercolor = NA,
      borderwidth = 0
    )
  ))
}



# Set legend to only include sensor names
p <- p %>% layout(
  annotations = annotations,
  legend = list(
    x = 0.5,
    y = -0.3,  # Adjust the y value to move the legend outside the plot area
    xanchor = 'center',
    yanchor = 'top',
    orientation = 'h',
    traceorder = 'normal',
    bgcolor = 'rgba(255, 255, 255, 0)',  # Set background color to transparent
    bordercolor = 'rgba(255, 255, 255, 0)',  # Set border color to transparent
    title = list(text = "")
  )
)



# Ensure only sensor names are shown in the legend
#sensor_names <- unique(dataset$name)
#print(sensor_names)
#print(p$layout$legend$traceorder)



#rows = ceiling(length(sensor_names)/3)

#print(rows)
#p <- p %>% layout(
#  height = 400 + rows * 100
#)
#for (trace in seq_along(p$data)) {
#  if (p$data[[trace]]$name == "Background") {
##    p$data[[trace]]$hoverinfo <- "none"
#  }
#}
htmlwidgets::saveWidget(config(p, displayModeBar = FALSE), "graph.html")

p <- p %>% layout(showlegend = FALSE)#, hovermode = "x unified", font = list(color = "white"))
