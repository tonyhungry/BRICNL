# CBS Migration Data per Gemeente

# STILL NOT FINISHED!


library(cbsodataR)
library(tidyverse)
library(sf)

# cbs_get_meta("84547NED")
# df = cbs_get_data("84547NED",periods="2022JJ00",regios=has_substring("GM"),Geslacht ="T001038")

gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 wijkbuurtkaart_2022_v2/gemeenten_2022_V2.shp")

gb2022 = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na()

gemeentegeo = gb2022 %>% select(GM_CODE,GM_NAAM) 

gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023wijkbuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gb2023 = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na()

# read in data downloaded from CBS and
library(readr)
migration2022 <- read_delim("CBS Downloads/Gemeente Migration 2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% drop_na()
# inhabitants2022 <- read_delim("CBS Downloads/Aantal Inwoners Gemeente 2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% drop_na()

# Migration 2022 does not have Weesp.
# Gemeente geographic data has Weesp. 
# Migration is for the entire year of 2022, whereas the geographic data is for the beginning of 2022

setdiff(migration2022$`Regio's`,gb2022$GM_NAAM)
setdiff(migration2022$`Regio's`,gb2023$GM_NAAM)

colnames(migration2022) = c("Geslacht","Leeftijd","Perioden","Regios","Gevestigd","Vertrokken")
migration2022 = migration2022 %>% mutate(totalmig = Gevestigd + Vertrokken)

mig2022 = migration2022 %>% select(Regios,totalmig) %>% 
  mutate(Regios = case_when(
  Regios == "Beek (L.)" ~ "Beek",
  Regios == "'s-Gravenhage (gemeente)" ~ "s-Gravenhage",
  Regios == "Groningen (gemeente)" ~ "Groningen",
  Regios == "Hengelo (O.)" ~ "Hengelo",
  Regios == "Laren (NH.)" ~ "Laren",
  Regios == "Middelburg (Z.)" ~ "Middelburg",
  Regios == "Rijswijk (ZH.)" ~ "Rijswijk",
  Regios == "Utrecht (gemeente)" ~ "Utrecht",
  Regios == "Stein (L.)" ~ "Stein",
  TRUE ~ Regios
))


