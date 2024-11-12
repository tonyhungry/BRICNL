# BRIC INFRA

# Setting Up ####
library(tidyverse)
library(sf)

# Collecting Data ####

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 

infra23 = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_CODE,GM_NAAM,AANT_INW)

# Calculating the weighted mean between Amsterdam and Weesp
weesp = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>%
  select(GM_CODE,GM_NAAM,AANT_INW,AV5_ARTSPR,AV20ZIEK_I,AV20_HOTEL,AF_TREINST,AF_APOTH,AF_BRANDW,AF_OPRITH,P_LEEGSW) %>% st_drop_geometry() %>% 
  filter(GM_NAAM == "Amsterdam" | GM_NAAM == "Weesp") %>% 
  summarise(across(AV5_ARTSPR:AF_OPRITH, ~ weighted.mean(., AANT_INW)))

infra22 = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>%
  select(GM_CODE,GM_NAAM,AV5_ARTSPR,AV20ZIEK_I,AV20_HOTEL,AF_TREINST,AF_APOTH,AF_BRANDW,AF_OPRITH,P_LEEGSW,AV5_ONDBAS,AV5_ONDVRT) %>% st_drop_geometry() %>% 
  mutate(AV5_ARTSPR = ifelse(GM_NAAM == "Amsterdam", 68.3, AV5_ARTSPR)) %>% 
  mutate(AV20ZIEK_I = ifelse(GM_NAAM == "Amsterdam", 13, AV20ZIEK_I)) %>% 
  mutate(AV20_HOTEL = ifelse(GM_NAAM == "Amsterdam", 503, AV20_HOTEL)) %>% 
  mutate(AF_TREINST = ifelse(GM_NAAM == "Amsterdam", 2.87, AF_TREINST)) %>% 
  mutate(AF_APOTH = ifelse(GM_NAAM == "Amsterdam", 0.707, AF_APOTH)) %>% 
  mutate(AF_BRANDW = ifelse(GM_NAAM == "Amsterdam", 2.09, AF_BRANDW)) %>% 
  mutate(AF_OPRITH = ifelse(GM_NAAM == "Amsterdam", 2.18, AF_OPRITH)) %>% 
  mutate(P_LEEGSW = ifelse(GM_NAAM == "Amsterdam", 7, P_LEEGSW)) %>% 
  mutate(AV5_ONDBAS = ifelse(GM_NAAM == "Amsterdam", 55.8, AV5_ONDBAS)) %>% 
  mutate(AV5_ONDVRT = ifelse(GM_NAAM == "Amsterdam", 23.2, AV5_ONDVRT)) %>% 
  mutate(schools = (AV5_ONDBAS + AV5_ONDVRT) / 2) %>% 
  select(GM_CODE,GM_NAAM,AV5_ARTSPR,AV20ZIEK_I,AV20_HOTEL,AF_TREINST,AF_APOTH,AF_BRANDW,AF_OPRITH,P_LEEGSW,schools)

rm(weesp)

## Police and Mental Healthcare Data ####

mental <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Mental Health Care Locations.csv")
mental = mental %>% select(statcode,Count.of.Points)
colnames(mental) = c("GM_CODE","mentalloc")

police <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2024 Police Stations.csv")
police = police %>% select(statcode,Count.of.Points)
colnames(police) = c("GM_CODE","policeloc")

infra23 = infra23 %>% left_join(mental,by=join_by(GM_CODE))
infra23 = infra23 %>% left_join(police,by=join_by(GM_CODE))

infra23 = infra23 %>% 
  mutate(polgem = policeloc / AANT_INW * 1000) %>% 
  mutate(mentalgem = mentalloc / AANT_INW * 1000) %>% 
  select(-mentalloc,-policeloc,-AANT_INW)

## Airports ####

airport <- read_csv("airport.csv")
airport = airport %>% select(`FindCentroidsOutput: statcode`,`Straight Line Distance (Kilometers)`)
colnames(airport) = c("GM_CODE","airportdist")

infra23 = infra23 %>% left_join(airport,by=join_by(GM_CODE))

# Merging the datasets ####
df = infra23 %>% left_join(infra22,by=join_by(GM_CODE,GM_NAAM))

df = df %>% st_drop_geometry()

colnames(df) = c("GM_CODE","GM_NAAM","POLICE","PSYCH","AIRPORT","DOCTOR","HOSPITAL","HOTELS","TRAIN","PHARMACY","FIRE","ROAD","VACHOME","SCHOOLS")

write.csv(df,"BRIC INFRA DATA.csv")


# ####

# df = df %>% 
#   drop_na() %>% 
#   mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
#   select(geometry, everything())
# 
# corm = df %>% st_drop_geometry()
# # infradim = corm
# # rm(list = setdiff(ls(), c("socialdim", "econdim","instdim","infradim")))
# 
# M = cor(corm[,3:13])
# corrplot::corrplot(M,method="number")
# 
# corm = corm %>% 
#   mutate(average = rowMeans(select(.,polgem,mentalgem,AV5_ARTSPR,AV20ZIEK_I,AV20_HOTEL,invtrain,invpharm,invfire,invroad))) %>% 
#   select(GM_CODE,average)
# 
# df = df %>% left_join(corm,by=join_by(GM_CODE))
# df = df %>% mutate(quantiles = ntile(average,6))
# 
# ggplot(df) + 
#   geom_sf(aes(fill = average)) + 
#   ggtitle("Infrastructure Domain") + 
#   theme_minimal()
# 
# ggplot(df) +
#   geom_sf(aes(fill = factor(quantiles)), color = "black") +
#   scale_fill_viridis_d() +  # Use a built-in color scale
#   ggtitle("Infrastructure Domain") +
#   theme_minimal()
