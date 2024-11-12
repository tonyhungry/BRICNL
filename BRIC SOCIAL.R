# SOCIAL DIMENSION

# Setting Up ####
library(tidyverse)
library(sf)

# Collecting Data ####

gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")

# Most of the variables should come from 2023 data (if possible)
social23 = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
   select(GM_CODE,GM_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_EENP_HH,P_GEBNL_NL,P_GEBNL_EU,P_GEBNL_NE) %>% 
  mutate(bornd = P_GEBNL_NL + P_GEBNL_EU + P_GEBNL_NE,.keep="unused") %>% 
  mutate(working = P_15_24_JR+P_25_44_JR+P_45_64_JR,.keep="unused") %>% 
  mutate(notsingle = 100 - P_EENP_HH,.keep="unused") %>% 
  filter(!(GM_NAAM == "Buitenland"))

almeenhouten = gb2022 %>% select(GM_CODE,GM_NAAM,AANTAL_HH,AUTO_TOT,AUTO_HH) %>% 
  filter(GM_CODE == "GM0034"| GM_CODE == "GM0321"| GM_CODE == "GM0363" | GM_CODE == "GM1640")

regdif  = gb2022 %>% select(GM_CODE,GM_NAAM,AANTAL_HH,AUTO_TOT,AUTO_HH) %>% 
  filter(GM_CODE == "GM0363" | GM_CODE == "GM1640" | GM_NAAM == "Hellevoetsluis" | GM_NAAM == "Westvoorne" | GM_NAAM == "Weesp" | GM_NAAM == "Brielle")

# (8705+246065)/(9509+484574) = 0.51 # Amsterdam + Weesp
# (21165+9090+9990)/(18256+6768+7972) = 1.2 # Voorne aan Zee

social22 = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% 
  select(GM_CODE,GM_NAAM,AUTO_HH) %>% 
  mutate(AUTO_HH = ifelse(GM_CODE == "GM0321", 3.5, AUTO_HH)) %>% # calculated for Houten
  mutate(AUTO_HH = ifelse(GM_CODE == "GM0034", 2.8, AUTO_HH)) %>% # calculated for Almere
  mutate(AUTO_HH = ifelse(GM_NAAM == "Amsterdam", 0.5, AUTO_HH)) # add Weesp to Amsterdam and recalculate

newrow = tibble(GM_CODE = "GM1992",GM_NAAM = "Voorne aan Zee", AUTO_HH = 1.2)
social22 = social22 %>% bind_rows(newrow)  %>% st_drop_geometry()



## Municipal Spending ####
Municipal_Spending <- readxl::read_excel("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Municipal Spending euros per inhabitant.xlsx")
colnames(Municipal_Spending)[1] = "GM_NAAM"
health = Municipal_Spending %>% select(GM_NAAM,Volksgezondheid,`Openbaar basisonderwijs`,Onderwijshuisvesting,`Onderwijsbeleid en leerlingzaken`) %>% 
  drop_na() %>% 
  mutate(GM_NAAM = ifelse(GM_NAAM == "Den Haag", "'s-Gravenhage", GM_NAAM)) %>% 
  mutate(educspend = `Openbaar basisonderwijs` + Onderwijshuisvesting + `Onderwijsbeleid en leerlingzaken`) %>% 
  select(GM_NAAM,Volksgezondheid,educspend)

educ <- read.csv2("/Volumes/SD Drive/Geo Data/BRIC Data/2022 Education in 3 Levels.csv", header=T)
educ = educ[,4:7]
colnames(educ) = c("regions","lowerlevel","middlelevel","higherlevel")
educ = educ %>% mutate(midhigh = middlelevel + higherlevel) %>% 
  select(regions, midhigh) %>% 
  mutate(midhigh = ifelse(regions == "Schiermonnikoog", 75.4, midhigh)) %>% # Use the same value as Ameland to fill in the gap
  drop_na() %>% 
  mutate(regions = case_when(
    regions == "Beek (L.)" ~ "Beek",
    regions == "'s-Gravenhage (gemeente)" ~ "'s-Gravenhage",
    regions == "Groningen (gemeente)" ~ "Groningen",
    regions == "Hengelo (O.)" ~ "Hengelo",
    regions == "Laren (NH.)" ~ "Laren",
    regions == "Middelburg (Z.)" ~ "Middelburg",
    regions == "Rijswijk (ZH.)" ~ "Rijswijk",
    regions == "Utrecht (gemeente)" ~ "Utrecht",
    regions == "Stein (L.)" ~ "Stein",
    TRUE ~ regions
  ))


## Not on Benefits ####

# We need to first calculate the numbers Amsterdam, which is Amsterdam + Weesp
gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% 
  select(GM_CODE,GM_NAAM,AANT_INW,AO_UIT_TOT) %>%
  filter(GM_NAAM == "Amsterdam" | GM_NAAM == "Weesp") %>% st_drop_geometry()

# AANT_INW = 882633+20766 = 903399
# AO_UIT_TOT = 700+33350 = 34050
# (903399-34050)/903399 * 100 = 0.962309

AOUIT = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% 
  select(GM_CODE,GM_NAAM,AANT_INW,AO_UIT_TOT) %>% 
  mutate(benefits = (AANT_INW - AO_UIT_TOT)/AANT_INW * 100) %>% 
  mutate(benefits = ifelse(GM_NAAM == "Amsterdam", 96.2309, benefits)) %>% 
  select(GM_CODE,GM_NAAM,benefits) %>% 
  st_drop_geometry()

## psychological resilience ####

# Need to recalculate for Amsterdam using weighted average to include Weesp.

# Pop in Weesp: 20766
# Pop in Amsterdam: 882633
# %highpsyres in Weesp: 50.4
# %highpsyres in Amsterdam: 43.6
# weighted average: (50.4*20766 + 882633*43.6) / (20766+882633) = 43.75631

high_psych_resilience <- read_csv("high psych resilience.csv")
colnames(high_psych_resilience) = c("GM_NAAM","psychres","working","old")
pres = high_psych_resilience %>% select(GM_NAAM,psychres) %>% 
  mutate(psychres = ifelse(GM_NAAM == "Amsterdam", 43.75631, psychres))

# Notes on Data Imputations ####
# Voorne Aan Zee (Previously "Hellevoetsluis" "Westvoorne" "Brielle") data is not available, thus it is dropped
# Education data for Schiermonnikoog was missing, thus it is filled in using Ameland

# Merging Datasets ####

# First check the differences

# setdiff(social22$GM_NAAM,educ$regions)
# setdiff(educ$regions,social22$GM_NAAM)
# 
# setdiff(social23$GM_NAAM,health$GM_NAAM)
# setdiff(health$GM_NAAM,social23$GM_NAAM)

social = social23 %>% left_join(social22,by = join_by(GM_CODE,GM_NAAM))
social = social %>% left_join(health,by = join_by(GM_NAAM))
social = social %>% left_join(educ,by=join_by(GM_NAAM == regions))
social = social %>% left_join(AOUIT,by = join_by(GM_CODE,GM_NAAM))
social = social %>% left_join(pres,by = join_by(GM_NAAM))

df = social %>% st_drop_geometry()
colnames(df) = c("GM_CODE","GM_NAAM","BORND","WORKING","NSINGLE","CARS","HEALTHSPEND","EDUCSPEND","EDUC","NBENEFITS","PSYCHRES")

write.csv(df,"BRIC SOCIAL DATA.csv")



# df = social %>% 
#   # st_drop_geometry() %>% 
#   drop_na() %>% 
#   mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
#   select(geometry, everything())
# 
# colnames(df) = c("geometry","GM_CODE","GM_NAAM","PopDensity", "AveHHSize","BornDutch","WorkingPop","NotSingleHH","CarsPerHH","HealthSpend","EducSpend","EducationLvl")
# 
# corm = df %>% st_drop_geometry()
# # socialdim = corm
# # rm(list = setdiff(ls(), "socialdim"))
# 
# 
# M = cor(corm[,3:10])
# corrplot::corrplot(M,method="number")
# # Average HH size and Not Single HH are causes for concern
# 
# corm = corm %>% 
#   mutate(average = rowMeans(select(.,PopDensity,AveHHSize,BornDutch,WorkingPop,NotSingleHH,CarsPerHH,HealthSpend,EducSpend,EducationLvl))) %>% 
#   select(GM_CODE,average)
# 
# df = df %>% left_join(corm,by=join_by(GM_CODE))
# df = df %>% mutate(quantiles = ntile(average,6))
# 
# ggplot(df) + 
#   geom_sf(aes(fill = average)) + 
#   ggtitle("Social Domain") + 
#   theme_minimal()
# 
# ggplot(df) +
#   geom_sf(aes(fill = factor(quantiles)), color = "black") +
#   scale_fill_viridis_d() +  # Use a built-in color scale
#   ggtitle("Social Domain") +
#   theme_minimal()
