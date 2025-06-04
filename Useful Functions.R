# Useful Functions in R
# Tony Hung
# 22 May 2025

# Before running this script, make sure that you have the relevant packages installed and the data downloaded.
# Also, make sure that you change the directory to load your data successfully...

# A lot of these operations take a long time to do and requires lots of memory

# Links for data: 
# 2022 Maximum flood depth: https://arcg.is/0firDD
# 2022 PC4 data from CBS: https://download.cbs.nl/postcode/2025-cbs_pc4_2022_vol.zip 

# Install required packages
library(sf) # Useful package to work with vector data (points, lines, polygons)
library(tidyverse) # General package. Could be used together with the sf package.
library(terra) # Package to work with raster data (cells, images)

# Load and Inspect the Data ####
## 2022 Maximum flood depth (raster data) ####

flood <- rast("~/Downloads/2022_water_depth.tif")
plot(flood)

### Checking the properties of the raster data
flood
ncell(flood) # check for number of cells
ext(flood)       # check for spatial extent (xmin, xmax, ymin, ymax)

global(is.na(flood), fun = "sum") # check for NAs. Many, but not unusual, since those are not the areas we are interested in anyway...

global(flood, fun = "min", na.rm = TRUE) # checking for maximum 
global(flood, fun = "max", na.rm = TRUE) # checking for minimum

# We need to change -9999 to 0 and have all of the non-flood areas 0
flood_corrected <- ifel(flood == -9999, NA, flood)
plot(flood_corrected, col = rev(viridis::viridis(100)))


extracted_values <- extract(flood_corrected, gem, fun = mean, na.rm = TRUE,bind=T)

## 2022 PC4 Data ####

pc4 <- read_sf("~/Downloads/cbs_pc4_2022_vol.gpkg")
pc4 = pc4 %>% mutate(aantal_inwoners = na_if(aantal_inwoners,-99997)) %>% drop_na() # Dropping the -99997, which is a space holder for NA.

st_crs(pc4) # check the coordinate reference system
st_bbox(pc4) # look at the bounding box of the spatial object
glimpse(pc4)

# Basic plot 
ggplot(data = pc4) +
  geom_sf(aes(fill = aantal_inwoners)) +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Total Inhabitants per PC4 ",
       fill = "Inhabitants")

# extract geometries
pc4geo = st_geometry(pc4)

# Perhaps you need to transform the coordinate reference system...
# You can consult this list of CRS codes
st_transform(pc4, crs = 32119)

# get centroids of each PC4
st_centroid(pc4geo)

# Fancy Operations ####

# Convert raster into vector
# NOTE: convertiong from raster to vector often results in data loss!
floodpolygons = as.polygons(flood_corrected)
flood_sf = st_as_sf(floodpolygons)
# plot(flood_sf)

# Spatial join to find PC4 intersecting the flood areas
pc4inter = pc4 %>% st_filter(flood_sf, .predicate = st_intersects)

# Plotting where PC4 intersects with the flood areas
ggplot() + 
  geom_sf(data = pc4, fill = "grey",color = "white") + 
  geom_sf(data = flood_sf, fill = "lightblue") + 
  geom_sf(data = pc4inter, aes(fill = aantal_inwoners), color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "PC4 areas within the Flood Areas", fill = "Population")

# Export
st_write(pc4inter, "PC4_flood_intersection.geojson")


# Using OSM to geo-locate ####

# I've made this function some time ago and hopefully it still works. It gives you the xy coordinates of the places you are searching for. 
# If you are batch processing, you might want to consider using this function with furrr and purrr which uses parallel processing

library(httr)
library(jsonlite)

geocode_location <- function(location) {
  base_url <- "https://nominatim.openstreetmap.org/search"
  response <- GET(base_url, query = list(q = location, format = "json", limit = 1))
  data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON()
  if (length(data) == 0) {
    return(tibble(latitude = NA, longitude = NA))
  }
  tibble(
    location = location,
    latitude = as.numeric(data$lat[1]),
    longitude = as.numeric(data$lon[1])
  )
}

# For instance
geocode_location("Paris, France")
geocode_location("Amsterdam Central Station")
geocode_location("Amsterdam")
