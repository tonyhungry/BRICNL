# BRIC ENVI

# Setting Up ####
library(tidyverse)
library(sf)

# Collecting Data ####

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 

df = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_NAAM,GM_CODE,OPP_TOT,OPP_LAND) %>% 
  mutate(land = OPP_LAND / OPP_TOT * 100) %>% 
  select(-OPP_TOT)

## Findo data ####
Municipal_Spending <- readxl::read_excel("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Municipal Spending euros per inhabitant.xlsx")
colnames(Municipal_Spending)[1] = "GM_NAAM"
env = Municipal_Spending %>% select(GM_NAAM,`Openbaar groen en (openlucht) recr..`,Riolering,Afval,Milieubeheer) %>% 
  drop_na() %>% 
  mutate(GM_NAAM = ifelse(GM_NAAM == "Den Haag", "'s-Gravenhage", GM_NAAM)) %>% 
  mutate(wastespend = Riolering + Afval + Milieubeheer) %>% 
  select(GM_NAAM,`Openbaar groen en (openlucht) recr..`,wastespend)

colnames(env)[2] = "greenspend"

df = df %>% left_join(env,by=join_by(GM_NAAM))
rm(Municipal_Spending,env)
## energy use data ####
energy <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 energy use.csv", sep=";")
colnames(energy) = c("regios","gas","electricity")
energy = energy %>% drop_na() %>% 
  mutate(regios = case_when(
    regios == "Beek (L.)" ~ "Beek",
    regios == "'s-Gravenhage (gemeente)" ~ "'s-Gravenhage",
    regios == "Groningen (gemeente)" ~ "Groningen",
    regios == "Hengelo (O.)" ~ "Hengelo",
    regios == "Laren (NH.)" ~ "Laren",
    regios == "Middelburg (Z.)" ~ "Middelburg",
    regios == "Rijswijk (ZH.)" ~ "Rijswijk",
    regios == "Utrecht (gemeente)" ~ "Utrecht",
    regios == "Stein (L.)" ~ "Stein",
    TRUE ~ regios
  )) 

colnames(energy)[1] = "GM_NAAM"

df = df %>% left_join(energy,by=join_by(GM_NAAM))
rm(energy)
## Land Use data ####
land <- readxl::read_excel("land use per gem.xlsx")

df = df %>% left_join(land,by=join_by(GM_NAAM,GM_CODE))

rm(land)

## Hazard Data ####
# urban_heat_effect_per_gem <- readxl::read_excel("urban heat effect per gem.xlsx")
# colnames(urban_heat_effect_per_gem)[3] = "heat"
# 
# df = df %>% left_join(urban_heat_effect_per_gem,by=join_by(GM_NAAM,GM_CODE))
# rm(urban_heat_effect_per_gem)

maximum_flood_depth_per_gem <- readxl::read_excel("maximum flood depth per gem.xlsx")
colnames(maximum_flood_depth_per_gem)[3] = "flood"

df = df %>% left_join(maximum_flood_depth_per_gem,by=join_by(GM_NAAM,GM_CODE)) %>% 
  st_drop_geometry() %>% select(-OPP_LAND)
rm(maximum_flood_depth_per_gem)


colnames(df) = c("GM_NAAM","GM_CODE","LAND","GREENSPEND","WASTESPEND","GAS","ELEC","BUFFER","OPENSPACE","CULTIVATE","FLOOD")
write.csv(df,"BRIC ENVI DATA.csv")

# Calculation and Graphs ####
# df = df %>% 
#   drop_na() %>% 
#   mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
#   select(geometry, everything())
# 
# corm = df %>% st_drop_geometry()
# # envdim = corm
# # rm(list = setdiff(ls(), c("socialdim", "econdim","instdim","infradim","commdim","envdim")))
# 
# M = cor(corm[,3:8])
# corrplot::corrplot(M,method="number")
# 
# corm = corm %>% 
#   mutate(average = rowMeans(select(.,3:8))) %>% 
#   select(GM_CODE,average)
# 
# df = df %>% left_join(corm,by=join_by(GM_CODE))
# df = df %>% mutate(quantiles = ntile(average,6))
# 
# ggplot(df) + 
#   geom_sf(aes(fill = average)) + 
#   ggtitle("Environment Domain") + 
#   theme_minimal()
# 
# ggplot(df) +
#   geom_sf(aes(fill = factor(quantiles)), color = "black") +
#   scale_fill_viridis_d() +  # Use a built-in color scale
#   ggtitle("Environment Domain") +
#   theme_minimal()
