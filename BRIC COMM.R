# BRIC COMM

# Setting Up ####
library(tidyverse)
library(sf)

# Collecting Data ####

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 

weesp = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>%
  select(GM_CODE,GM_NAAM,AANT_INW,AV5_SUPERM,AV5_DAGLMD,AV20WARENH,AV5_RESTAU,AV5_CAFE,AV5_CAFTAR,AV5_ONDBAS,AV5_ONDVRT,AV20_BIOS,AF_BIBLIO,AV5_KDV) %>% st_drop_geometry() %>% 
  filter(GM_NAAM == "Amsterdam" | GM_NAAM == "Weesp") %>% 
  summarise(across(AV5_SUPERM:AV5_KDV, ~ weighted.mean(., AANT_INW)))

comm22 = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>%
  select(GM_CODE,GM_NAAM,AANT_INW,AV5_SUPERM,AV5_DAGLMD,AV5_RESTAU,AV5_CAFE,AV5_CAFTAR,AV20_BIOS,AF_BIBLIO,AV5_KDV) %>% st_drop_geometry() %>% 
  mutate(AV5_SUPERM = ifelse(GM_NAAM == "Amsterdam", 87.6, AV5_SUPERM)) %>% 
  mutate(AV5_DAGLMD = ifelse(GM_NAAM == "Amsterdam", 491, AV5_DAGLMD)) %>% 
  # mutate(AV20WARENH = ifelse(GM_NAAM == "Amsterdam", 38.4, AV20WARENH)) %>% 
  mutate(AV5_RESTAU = ifelse(GM_NAAM == "Amsterdam", 993, AV5_RESTAU)) %>% 
  mutate(AV5_CAFE = ifelse(GM_NAAM == "Amsterdam", 318, AV5_CAFE)) %>% 
  mutate(AV5_CAFTAR = ifelse(GM_NAAM == "Amsterdam", 421, AV5_CAFTAR)) %>% 
  # mutate(AV5_ONDBAS = ifelse(GM_NAAM == "Amsterdam", 55.8, AV5_ONDBAS)) %>% 
  # mutate(AV5_ONDVRT = ifelse(GM_NAAM == "Amsterdam", 23.2, AV5_ONDVRT)) %>% 
  mutate(AV20_BIOS = ifelse(GM_NAAM == "Amsterdam", 18.2, AV20_BIOS)) %>% 
  mutate(AV5_KDV = ifelse(GM_NAAM == "Amsterdam", 166, AV5_KDV)) %>% 
  mutate(food = (AV5_SUPERM + AV5_DAGLMD) / 2) %>% 
  mutate(horeca = (AV5_RESTAU + AV5_CAFE + AV5_CAFTAR) / 3) %>% 
  # mutate(schools = (AV5_ONDBAS + AV5_ONDVRT) / 2) %>% 
  select(GM_CODE,GM_NAAM,food,AF_BIBLIO,AV5_KDV)

rm(weesp)

## Findo data ####
Municipal_Spending <- readxl::read_excel("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Municipal Spending euros per inhabitant.xlsx")
colnames(Municipal_Spending)[1] = "GM_NAAM"
comm = Municipal_Spending %>% select(GM_NAAM,`Samenkracht en burgerparticipatie`,Wijkteams,`Begeleide participatie`,`Sportbeleid en activering`,Sportaccommodaties,`Cultuurpresentatie, cultuurproduct..`,Musea,`Cultureel erfgoed`,Media) %>% 
  drop_na() %>% 
  mutate(GM_NAAM = ifelse(GM_NAAM == "Den Haag", "'s-Gravenhage", GM_NAAM)) %>% 
  mutate(commspend = `Samenkracht en burgerparticipatie` + Wijkteams + `Begeleide participatie`) %>% 
  mutate(recrspend = `Sportbeleid en activering` + Sportaccommodaties + `Cultuurpresentatie, cultuurproduct..` + Musea + `Cultureel erfgoed` + Media) %>% 
  select(GM_NAAM,commspend,recrspend)

rm(Municipal_Spending)

## Voter data ####
turnout <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Election Turnout.csv")
turnout = turnout %>% select(RegioCode,turnout) %>% 
  mutate(voter = turnout * 100) %>% 
  select(-turnout) 
colnames(turnout) = c("GM_CODE","voter") # by GM_CODE

## Migration data ####
migration2023 <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Migration.csv", sep=";")
migration2023 = migration2023 %>% mutate(regios = case_when(
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
colnames(migration2023) = c("GM_NAAM","inmig","outmig")

df = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_NAAM,GM_CODE,AANT_INW)
df = df %>% left_join(migration2023,by=join_by(GM_NAAM))
df = df %>% 
  mutate(migration = ((((inmig + outmig)/AANT_INW)))) %>% 
  select(GM_NAAM,GM_CODE,AANT_INW,migration)

rm(migration2023)

## Crime data ####
crime23 <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Registered Crimes.csv", sep=";")
crime23 = crime23 %>%  mutate(regios = case_when(
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
colnames(crime23) = c("GM_NAAM","totalcrime")

df = df %>% left_join(crime23,by=join_by(GM_NAAM)) %>% 
  mutate(crime = (totalcrime / AANT_INW * 1000)) %>% 
  select(-totalcrime)
rm(crime23)

## Volunteer Work ####
volunteer <- read_csv("volunteer.csv")
colnames(volunteer) = c("GM_NAAM","volunteering","working","old")
volunteer = volunteer %>% select(GM_NAAM,volunteering)

## Sports Facilities #### 
sports_facilities <- read_csv("sports facilities.csv")
colnames(sports_facilities) = c("GM_NAAM","sports","working","old")
sports_facilities = sports_facilities %>% select(GM_NAAM,sports)

# Merging the datasets ####
df = df %>% left_join(turnout,by=join_by(GM_CODE)) %>% 
  left_join(comm,by=join_by(GM_NAAM))
rm(comm,turnout)

df = df %>% left_join(comm22,by=join_by(GM_CODE,GM_NAAM)) %>% select(-AANT_INW)

df = df %>% left_join(volunteer,by=join_by(GM_NAAM))
df = df %>% left_join(sports_facilities,by=join_by(GM_NAAM))
df = df %>% st_drop_geometry()

colnames(df) = c("GM_NAAM","GM_CODE","MIGRATION","CRIMES","VOTER","COMMSPEND","RECRSPEND","FOOD","LIBRARY","DAYCARE","VOLUNTEER","SPORTS")

write.csv(df,"BRIC COMM DATA.csv")  


# ####
# df = df %>% 
#   drop_na() %>% 
#   mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
#   select(geometry, everything())
# 
# corm = df %>% st_drop_geometry()
# # commdim = corm
# # rm(list = setdiff(ls(), c("socialdim", "econdim","instdim","infradim","commdim")))
# 
# 
# M = cor(corm[,3:13])
# corrplot::corrplot(M,method="number")
# 
# corm = corm %>% 
#   mutate(average = rowMeans(select(.,migration:invlib))) %>% 
#   select(GM_CODE,average)
# 
# df = df %>% left_join(corm,by=join_by(GM_CODE))
# df = df %>% mutate(quantiles = ntile(average,6))
# 
# ggplot(df) + 
#   geom_sf(aes(fill = average)) + 
#   ggtitle("Community Capital Domain") + 
#   theme_minimal()
# 
# ggplot(df) +
#   geom_sf(aes(fill = factor(quantiles)), color = "black") +
#   scale_fill_viridis_d() +  # Use a built-in color scale
#   ggtitle("Community Capital Domain") +
#   theme_minimal()
