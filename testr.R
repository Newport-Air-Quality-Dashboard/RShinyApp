# Load libraries
library(leaflet)
library(raster)
library(KernSmooth)


library(gstat)
library(sp)
library(leaflet)
library(shiny)
library(raster)
library(sf)
library(RColorBrewer)

set.seed(123)  # for reproducibility
d <- data.frame(
  x = runif(100, -124, -118),  # example longitude (replace with your data)
  y = runif(100, 34, 40),       # example latitude (replace with your data)
  PRECIPITATION = runif(100, 1, 200)  # example variable to interpolate (replace with your data)
)

# Define a grid of points with 4x resolution
x_range <- range(d$x)
y_range <- range(d$y)
n_points <- 1000  # number of points in each direction (total grid points will be n_points * n_points)
grid_x <- seq(min(x_range), max(x_range), length.out = n_points)
grid_y <- seq(min(y_range), max(y_range), length.out = n_points)
grid_points <- expand.grid(x = grid_x, y = grid_y)

# Create gstat model for IDW interpolation
gs <- gstat(formula = PRECIPITATION ~ 1, locations = ~x + y, data = d, nmax = Inf, set = list(idp = 2))

# Perform IDW interpolation on the grid
idw_raster <- predict(gs, newdata = grid_points)
idw_grid <- as.data.frame(idw_raster)
colnames(idw_grid) <- c("x", "y", "z")













# Generate sample data (replace with your actual dataset)
set.seed(123)


n1 <- 2

test1 <- data.frame(
 x = rep(1:n1, each = n1),
 y = rep(1:n1, n1),
 z = runif(n1^2)  
)
#print(test1)
testraster <- rasterFromXYZ(test1)
#plot(testraster)
#view(idw_grid$y)
#view()
idw_grid$x <- idw_grid$x
idw_grid$y <- idw_grid$y
#print(idw_grid$x)#runif(nrow(idw_grid), min = -1.100005, max = 1.100005)
d <- data.frame(
  #x = runif(100, 119, 120),  # example longitude (replace with your data)
  x = idw_grid$x,
  #y = idw_grid$y,       # example latitude (replace with your data)
  #z = runif(100, 1, 500)        # example intensity or density (replace with your data)
  #x = runif(100, -120, -118),  # example longitude (replace with your data)
  y = idw_grid$y,       # example latitude (replace with your data)
  z= idw_grid$z
)


# Check which columns are all NA
#na_columns <- colSums(is.na(idw_grid)) == nrow(idw_grid)

# Remove columns that are all NA
#idw_grid <- idw_grid[, !na_columns, drop = FALSE]
#idw_grid$z <- trunc(idw_grid$z)
#view(d)
#view(idw_grid)



#d <- idw_grid
#view(d)

# Perform 2D kernel density estimation
#kde <- bkde2D(d[, c("x", "y")], bandwidth = c(bw.ucv(d$x), bw.ucv(d$y)), gridsize = c(200, 200))

# Convert KDE output to raster
#kde_raster <- raster(list(x = kde$x1, y = kde$x2, z = kde$fhat))

#print(kde_raster@data@values)

# Define color palette based on z values
#pal <- colorNumeric("Spectral", values(kde_raster), na.color = "transparent")

# Define custom color ranges for intensity
RasterPlot <- rasterFromXYZ(d[, c("x", "y", "z")])
projection(RasterPlot) <- "+proj=longlat +datum=WGS84"
plot(RasterPlot)


# Define custom colors for each range
custom_colors <- c("green", "yellow", "orange", "red", "purple", "purple", "#7E0023", "#7E0023", "#7E0023", "#7E0023")
#custom_colors <- c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027")
# Create color palette based on custom ranges and colors

pal <- colorNumeric(palette = custom_colors, domain = c(0, 500))#kde_raster@data@values)


#plot(r)

# Create Leaflet map
m <- leaflet() %>%
  setView(lng = mean(d$x), lat = mean(d$y), zoom = 10) %>%
  addTiles() %>%
  addRasterImage(RasterPlot, colors = pal, opacity = 0.5) %>%
  addLegend(pal = pal, values = values(RasterPlot), title = "Intensity")

# Display the map
print(m)
