# Requested Data from Audrey

# Setting Up ####
library(tidyverse)
library(sf)

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
buurt2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/buurten_2023_V1.shp")

gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 
buurt2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/buurten_2022_V2.shp") 

gb2023 = gb2023 %>% filter(GM_CODE == "GM0599") %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na()
buurt2023 = buurt2023 %>% filter(GM_CODE == "GM0599") %>% filter(BU_CODE == "BU05991081")
gb2022 = gb2022 %>% filter(GM_CODE == "GM0599") %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na()
buurt2022 = buurt2022 %>% filter(GM_CODE == "GM0599") %>% filter(BU_CODE == "BU05991081")


# There aren't much data here... Have to get it elsewhere. 