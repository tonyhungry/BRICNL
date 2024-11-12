# OSM ACCESS
# I give up on this because it simply just doesn't work... :(

library(osmdata)
library(sf)
library(dplyr)

bb = getbb("Netherlands")
detach("package:osmdata", unload = TRUE)

# Because the query is very large, we would have to use a different source of data...
# Download the OSM data from: https://download.geofabrik.de/europe/netherlands.html
# We would then use another package to extract the features we want :) 
# Data downloaded on the 17 Sep

library(osmextract)

# Specify the path to the downloaded .pbf file
pbf_file <- "/Volumes/SD Drive/Geo Data/netherlands-latest.osm.pbf"

keys = oe_get_keys(pbf_file, layer="points")

# Extract banks data from the file
netherlands_banks <- oe_read(
  file = pbf_file, 
  layer = "points",   # Specify that we are interested in points (nodes)
  extra_tags = "amenity"  # Ensure we include the amenity tag
)

bank_locations <- netherlands_banks[netherlands_banks$amenity == "bank", ]

# Inspect the data
print(netherlands_banks)



# I hope this would work one day... DO NOT RUN. Could possibly be my internet is too slow :(
# cycleways_england = oe_get(
#   "England",
#   quiet = FALSE,
#   query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'"
# )
# par(mar = rep(0.1, 4))
# plot(sf::st_geometry(cycleways_england))
