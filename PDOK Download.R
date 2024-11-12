# Accessing PDOK data
pacman::p_load(tidyverse,httr,jsonlite) 

# Accessing Liander Electricity ####

electricnet = GET("https://service.pdok.nl/liander/elektriciteitsnetten/wfs/v1_0?request=getCapabilities&service=WFS")

library(sf)
library(httr)

# Base URL for the PDOK WFS service
wfs_base_url <- "https://service.pdok.nl/liander/elektriciteitsnetten/wfs/v1_0"

# Parameters for the specific WFS request
wfs_params <- list(
  service = "WFS",
  version = "1.1.0",
  request = "GetFeature",
  typeName = "hoogspanningskabels",  # Replace with the specific layer you need
  outputFormat = "application/json"  # GeoJSON format
)

# Construct the WFS request URL
wfs_request_url <- modify_url(wfs_base_url, query = wfs_params)
print(wfs_request_url)

# Download the WFS data
wfs_data <- st_read(wfs_request_url)

# Display the first few rows of the data
print(head(wfs_data))

## Only download Amsterdam Data ####
# This doesn't work :(

# Parameters for the specific WFS request
wfs_params <- list(
  service = "WFS",
  version = "1.1.0",
  request = "GetFeature",
  typeName = "hoogspanningskabels",  # Replace with the specific layer you need
  outputFormat = "application/json",  # GeoJSON format
  bbox = "4.7289,52.2782,5.0792,52.4312"  # Bounding box for Amsterdam
)

# Construct the WFS request URL
wfs_request_url <- modify_url(wfs_base_url, query = wfs_params)
print(wfs_request_url)

# Download the WFS data
wfs_data <- st_read(wfs_request_url)
