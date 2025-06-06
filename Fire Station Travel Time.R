# Fire Station Travel Time
# Using PC6 

library(sf)
library(tidyverse)
library(osmdata)
library(osrm)
library(osmextract)

# 
gpkg_path <- oe_get(
  place = "Netherlands",
  layer = "points",
  extra_tags = "amenity",
  download_only = TRUE  # just get the file path, don't load yet
) # ignore the warning message
sql_query <- "SELECT * FROM points WHERE amenity = 'fire_station'"

firestations <- oe_read(
  file_path = gpkg_path,
  query = sql_query
)

# st_write(firestations, "firestations_points_nl.gpkg")

pc6 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2023 2024-cbs_pc6_2023_v1/cbs_pc6_2023_v1.gpkg")
pc6_areas = pc6 %>% select(postcode6,aantal_inwoners) %>% mutate(aantal_inwoners = na_if(aantal_inwoners,-99997)) %>% drop_na()
# plot(pc6_areas)

pc6_centroids <- pc6_areas %>%
  st_centroid() %>%
  st_transform(4326)

firestations <- st_transform(firestations, 4326)

options(osrm.server = "http://localhost:5001/")
options(osrm.profile = "car")

library(purrr)
library(furrr)

plan(multisession, workers = parallel::detectCores() - 1)
batch_size <- 10
n <- nrow(pc6_centroids)
batches <- split(1:n, ceiling(seq_along(1:n) / batch_size))

route_batch <- function(index_range) {
  options(osrm.server = "http://localhost:5001/")
  options(osrm.profile = "car")
  
  message("Routing ", length(index_range), " points...")
  
  origins <- pc6_centroids[index_range, ]
  result <- osrmTable(src = origins, dst = firestations)
  
  apply(result$durations, 1, min, na.rm = TRUE)
}

min_times_list <- future_map(batches, route_batch, .progress = TRUE, .options = furrr_options(seed = TRUE))

plan(sequential)

pc6_centroids$min_drive_time_min <- unlist(min_times_list)

gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp") 

# Reproject centroids and fire stations to RD New
pc6_centroids_rd <- st_transform(pc6_centroids, 28992)
firestations_rd <- st_transform(firestations, 28992)

# Reproject PC6 population polygons (if needed)
pc6_areas_rd <- st_transform(pc6_areas, 28992)

pc6_with_muni <- st_join(pc6_centroids_rd, gb2023 %>% select(GM_CODE, GM_NAAM))

pc6_with_muni <- pc6_with_muni %>%
  left_join(pc6_areas_rd %>% st_drop_geometry() %>% select(postcode6), by = "postcode6") %>%
  mutate(
    weighted_time = min_drive_time_min * aantal_inwoners
  )

muni_summary <- pc6_with_muni %>%
  st_drop_geometry() %>%
  group_by(GM_CODE, GM_NAAM) %>%
  summarise(
    total_weighted_time = sum(weighted_time, na.rm = TRUE),
    total_population = sum(aantal_inwoners, na.rm = TRUE),
    weighted_avg_drive_time = total_weighted_time / total_population,
    .groups = "drop"
  )

muni_map <- gb2023 %>%
  left_join(muni_summary, by = c("GM_CODE", "GM_NAAM")) %>% 
  select(GM_CODE,GM_NAAM,weighted_avg_drive_time)

ggplot(muni_map) +
  geom_sf(aes(fill = weighted_avg_drive_time), color = NA) +
  scale_fill_viridis_c(name = "Avg. Drive Time (min)", na.value = "grey80") +
  theme_minimal() +
  labs(title = "Population-Weighted Avg. Drive Time to Nearest Fire Station\nby Municipality")

st_write(muni_map, "firestation_municipality_weighted.gpkg")
