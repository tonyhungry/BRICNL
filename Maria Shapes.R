# Getting the Shape Files for Maria

# Setting Up ####
library(tidyverse)
library(sf)

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")

shape = gb2023 %>% select(GM_CODE,GM_NAAM)
st_crs(shape)
summary(shape)

st_write(shape, "GEM2023_Geo.geojson", driver = "GeoJSON")


# Wind and Hail Data ####
# Questions for Maria
# How did you make these variables? Under what conditions?
# How would you describe these variables in 1 sentence?
# How would you interpret these variables?
# References?

windhail = read_sf("/Volumes/SD Drive/Geo Data/BRIC NL/WindHaildata.geojson")
glimpse(windhail)

st_crs(windhail)

ggplot(data = windhail) +
  geom_sf(aes(fill = Hail_Pa)) +  
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Hail in NL",
       fill = "Hail??")

ggplot(data = windhail) +
  geom_sf(aes(fill = Wind_Pa)) +  
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Wind in NL",
       fill = "Wind??")
