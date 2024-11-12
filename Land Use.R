# Land Use 

library(tidyverse)
library(sf)

gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gem = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_NAAM,GM_CODE,OPP_TOT)
rm(gb2023)
st_crs(gem)

st_layers("/Volumes/SD Drive/Geo Data/BRIC Data/land use.gpkg")
# layer1 = st_read("/Volumes/SD Drive/Geo Data/BRIC Data/land use.gpkg", layer = "bestand_bodemgebruik_2017")
layer2 = st_read("/Volumes/SD Drive/Geo Data/BRIC Data/land use.gpkg", layer = "bestand_bodemgebruik_2017_bodemgebruik") # Use this one

st_crs(layer2) == st_crs(gem)
unique(layer2$bodemgebruik)

# Forest and Open Natural Terrein = natural flood buffer
filtered_layer2 = layer2 %>%
  filter(bodemgebruik %in% c("Bos", "Droog natuurlijk terrein", "Nat natuurlijk terrein"))
filtered_layer2 <- st_transform(filtered_layer2, st_crs(gem))
intersected_data <- st_intersection(filtered_layer2, gem)
intersected_data$area_sqm <- st_area(intersected_data)
intersected_data$area_hectares <- as.numeric(intersected_data$area_sqm) / 10000
summed_areas <- intersected_data %>%
  group_by(GM_CODE) %>% 
  summarise(total_area = sum(area_hectares)) %>% 
  st_drop_geometry()
df = gem %>% left_join(summed_areas,by=join_by(GM_CODE)) %>% rename(BUFFER = total_area)

# Recreation area = developed open space
filtered_layer2 = layer2 %>%
  filter(bodemgebruik %in% c("Recreatie"))
filtered_layer2 <- st_transform(filtered_layer2, st_crs(gem))
intersected_data <- st_intersection(filtered_layer2, gem)
intersected_data$area_sqm <- st_area(intersected_data)
intersected_data$area_hectares <- as.numeric(intersected_data$area_sqm) / 10000
summed_areas <- intersected_data %>%
  group_by(GM_CODE) %>% 
  summarise(total_area = sum(area_hectares)) %>% 
  st_drop_geometry()

df = df %>% left_join(summed_areas,by=join_by(GM_CODE)) %>% rename(OPENSPACE = total_area)

# Agricultural land = cultivated land
filtered_layer2 = layer2 %>%
  filter(bodemgebruik %in% c("Landbouw en overig agrarisch"))
filtered_layer2 <- st_transform(filtered_layer2, st_crs(gem))
intersected_data <- st_intersection(filtered_layer2, gem)
intersected_data$area_sqm <- st_area(intersected_data)
intersected_data$area_hectares <- as.numeric(intersected_data$area_sqm) / 10000
summed_areas <- intersected_data %>%
  group_by(GM_CODE) %>% 
  summarise(total_area = sum(area_hectares)) %>% 
  st_drop_geometry()

df = df %>% left_join(summed_areas,by=join_by(GM_CODE)) %>% rename(CULTIVATE = total_area)

df = df %>% 
  mutate(PBUFFER = BUFFER / OPP_TOT * 100) %>% 
  mutate(POPENSPACE = OPENSPACE / OPP_TOT * 100) %>% 
  mutate(PCULTIVATE = CULTIVATE / OPP_TOT * 100) %>% 
  select(GM_CODE,GM_NAAM,PBUFFER,POPENSPACE,PCULTIVATE) %>% 
  st_drop_geometry()

writexl::write_xlsx(df,"land use per gem.xlsx")

