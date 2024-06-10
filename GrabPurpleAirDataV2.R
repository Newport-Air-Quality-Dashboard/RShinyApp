library(httr)
library(tidyverse)
library(jsonlite)

# Define the API key
api_key <- "C844D736-0E03-11EF-B9F7-42010A80000D"


location <- "both" # or 'indoor' or 'both'




get_sensorsdata <- function(nwlng, nwlat, selng, selat, location, key_read) {
  # PurpleAir API URL
  root_url <- "https://api.purpleair.com/v1/sensors/"
  
  # Box domain: lat_lon = [nwlng,, nwlat, selng, selat]
  lat_lon <- c(nwlng, nwlat, selng, selat)
  ll_api_url <- paste0("&nwlng=", lat_lon[1], "&nwlat=", lat_lon[2], "&selng=", lat_lon[3], "&selat=", lat_lon[4])
  
  # Fields to get
  fields_list <- c("sensor_index", "last_seen", "name", "latitude", "longitude", "humidity", "temperature", "pressure", "pm1.0", "pm2.5", "pm2.5_10minute")
  fields_api_url <- paste0("&fields=", paste(fields_list, collapse = "%2C"))
  
  # Final API URL
  api_url <- paste0(root_url, "?api_key=", key_read, fields_api_url, ll_api_url)
  
  # Getting data
  response <- GET(api_url)
  
  if (status_code(response) == 200) {
    json_data <- content(response, as = "text") %>%
      fromJSON()
    df <- bind_cols(json_data$data)
    names(df) <- fields_list
  } else {
    stop("API request failed")
  }
  
  print("Sensors List Obtained Successfully")
  
  return(df)
}

append_to_historical <- function(df, historical_file) {
  if (!file.exists(historical_file)) {
    write.csv(df, historical_file, row.names = FALSE)
  } else {
    write.table(df, historical_file, sep = ",", append = TRUE, col.names = !file.exists(historical_file), row.names = FALSE)
  }
}







while(TRUE) {
  
sensordata <- get_sensorsdata(-84.700, 39.233, -84.300, 38.996, location, api_key)
write.csv(sensordata, "sensor_data_current.csv", row.names = FALSE)

#quirk to be fixed, if you add a new field to be grabbed by the api sensordatahistorical will not add it unless it is reset
#append_to_historical(sensordata, "sensor_data_historical.csv")
Sys.sleep(600)

}








