# Getting the Shape Files for Maria

# Setting Up ####
library(tidyverse)
library(sf)

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")

shape = gb2023 %>% select(GM_CODE,GM_NAAM)
st_crs(shape)
summary(shape)

