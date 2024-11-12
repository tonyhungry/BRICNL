# BRIC ECON

# Setting Up ####
library(tidyverse)
library(sf)

# Collecting Data ####

## CBS Geografische Data ####
gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 

# Need to recalculate Amsterdam to include Weesp

weesp = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% select(GM_CODE,GM_NAAM,AANT_INW,A_BEDV) %>% 
  filter(GM_NAAM == "Amsterdam" | GM_NAAM == "Weesp")

# AANT_INW = 20766 + 882633 = 903399
# A_BEDV = 3240 + 181675 = 184915
# P_LEEGSW = (5+7)/2 = 7
rm(weesp)

econ22 = gb2022 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% 
  select(GM_CODE,GM_NAAM,AANT_INW,A_BEDV) %>% 
  mutate(AANT_INW = ifelse(GM_NAAM == "Amsterdam", 903399, AANT_INW)) %>% 
  mutate(A_BEDV = ifelse(GM_NAAM == "Amsterdam", 184915, A_BEDV)) %>% 
#  mutate(P_LEEGSW = ifelse(GM_NAAM == "Amsterdam", 7, P_LEEGSW)) %>% 
  mutate(BusLoc = A_BEDV / AANT_INW * 1000) %>% 
  select(GM_CODE,GM_NAAM,BusLoc) %>% 
  st_drop_geometry()

## CBS Statline/Findo/OSM ####
owner <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Owner occupied homes.csv", sep=";")
ownhomes = owner %>% mutate(ownerhomes = Koopwoningen / Totale.woningvoorraad * 100) %>% select(Regio.s,ownerhomes)
rm(owner)
colnames(ownhomes) = c("regios","ownerhomes")

woz <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2023 WOZ.csv", sep=";")
homeval = woz %>% select(Regio.s,Gemiddelde.WOZ.waarde.van.woningen..1.000.euro.)
colnames(homeval) = c("regios","woz")
statline = ownhomes %>% left_join(homeval,by = join_by(regios))
rm(woz,homeval,ownhomes)

LabPart <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2022 Net Labour Participation.csv", sep=";")
netlabour = LabPart %>% select(Regio.s,Arbeidsdeelname.Netto.arbeidsparticipatie....)
colnames(netlabour) = c("regios","netlabour")
statline = statline %>% left_join(netlabour,by = join_by(regios))
rm(LabPart,netlabour)

income <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2022 Median Disposable income.csv", sep=";",dec = ",")
income = income %>% select(Regio.s,Inkomen.Mediaan.besteedbaar.inkomen..1.000.euro.)
colnames(income) = c("regios","medinc")
income = income %>% 
  mutate(medinc = ifelse(regios == "Rozendaal", 67.7, medinc)) %>% # 2021 Figures
  mutate(medinc = ifelse(regios == "Schiermonnikoog", 34.1, medinc)) %>% # 2021 Figures
  mutate(medinc = ifelse(regios == "Vlieland", 30.3, medinc)) # 2021 Figures
statline = statline %>% left_join(income,by = join_by(regios))
rm(income)

womenwork <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/2022 Percent Women in Workforce.csv", sep=";")
womenwork = womenwork %>% select(Regio.s,Arbeidsdeelname.Netto.arbeidsparticipatie....)
colnames(womenwork) = c("regios","womenwork")
statline = statline %>% left_join(womenwork,by = join_by(regios))
rm(womenwork)

feminc <- read.csv2("/Volumes/SD Drive/Geo Data/BRIC Data/2022 Gender median personal income.csv", na.strings=".")

# Rozendaal: 30/58.3 = 0.5145798
# Schiermonnikoog: 20.4/31.7 = 0.6435331
# Vlieland: 22.9/33.5 = 0.6835821
feminc = feminc %>% 
  mutate(femincratio = Vrouwen / Mannen) %>% 
  select(regios, femincratio) %>% 
  mutate(femincratio = ifelse(regios == "Rozendaal", 0.5145798, femincratio)) %>% # 2021 Figures
  mutate(femincratio = ifelse(regios == "Schiermonnikoog", 0.6435331, femincratio)) %>% # 2021 Figures
  mutate(femincratio = ifelse(regios == "Vlieland", 0.6835821, femincratio)) # 2021 Figures
statline = statline %>% left_join(feminc,by = join_by(regios))
rm(feminc) 

wealth <- read.csv2("/Volumes/SD Drive/Geo Data/BRIC Data/2022 Median wealth of HH.csv")
wealth = wealth %>% select(Regio.s,Mediaan.vermogen..1.000.euro.)
colnames(wealth) = c("regios","medwealth")
statline = statline %>% left_join(wealth,by = join_by(regios))
rm(wealth)

statline = statline %>% mutate(regios = case_when(
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

gini <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/Gini 2022.csv", dec=",")
colnames(gini) = c("regios","ginico")
gini = gini %>% 
  mutate(ginico = ifelse(regios == "Rozendaal", 0.29, ginico)) %>% # Replaced with an average of Rheden and Arnhem
  mutate(ginico = ifelse(regios == "Schiermonnikoog", 0.29, ginico)) %>% # 2021 Figures
  mutate(ginico = ifelse(regios == "Vlieland", 0.32, ginico)) %>% # 2021 Figures
  mutate(ginico = ifelse(regios == "Súdwest Fryslân", 0.25, ginico)) %>% # 2021 Figures
  mutate(ginico = ifelse(regios == "Ameland", 0.27, ginico)) %>% # 2021 Figures
  mutate(ginico = ifelse(regios == "Renswoude", 0.27, ginico)) %>% # 2021 Figures
  mutate(regios = ifelse(regios == "Súdwest Fryslân", "Súdwest-Fryslân", regios)) %>% 
  select(regios,ginico)
  
statline = statline %>% left_join(gini,by = join_by(regios))
rm(gini)

# Findo
Municipal_Spending <- readxl::read_excel("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Municipal Spending euros per inhabitant.xlsx")
colnames(Municipal_Spending)[1] = "GM_NAAM"
econ = Municipal_Spending %>% select(GM_NAAM,`Economische ontwikkeling`,`Fysieke bedrijfsinfrastructuur`,`Bedrijvenloket en bedrijfsregelingen`,`Economische promotie`,Arbeidsparticipatie) %>% 
  drop_na() %>% 
  mutate(GM_NAAM = ifelse(GM_NAAM == "Den Haag", "'s-Gravenhage", GM_NAAM)) %>% 
  mutate(econspend = `Economische ontwikkeling` + `Fysieke bedrijfsinfrastructuur`+ `Bedrijvenloket en bedrijfsregelingen`+`Economische promotie`) %>% 
  select(GM_NAAM,econspend,Arbeidsparticipatie)
colnames(econ) = c("regios","econspend","labourspend")
statline = statline %>% left_join(econ,by = join_by(regios))
rm(Municipal_Spending,econ)

# OSM
banks <- read.csv("/Volumes/SD Drive/Geo Data/BRIC Data/Banks per Gemeente.csv")
banks = banks %>% select(statnaam,Count.of.Points)
colnames(banks) = c("regios","banks")
banks = banks %>% mutate(regios = case_when(
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
colnames(banks)[1] = c("GM_NAAM")
# setdiff(banks$regios,econ22$GM_NAAM) # check for differences

gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")

knab = gb2023 %>% select(GM_CODE,GM_NAAM,AANT_INW) %>% 
  mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% left_join(banks,by = join_by(GM_NAAM)) %>% 
  mutate(bankpth = banks/AANT_INW * 1000) %>% 
  select(GM_CODE,GM_NAAM,bankpth)
rm(banks)

# Merging the datasets ####
# Final clean up of statline
colnames(statline)[1] = "GM_NAAM"

# setdiff(econ22$GM_NAAM,statline$regios)

df = knab %>% left_join(econ22,by=join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(statline,by=join_by(GM_NAAM)) %>% st_drop_geometry() %>% select(-medwealth)

colnames(df) = c("GM_CODE","GM_NAAM","BANKS","BUSLOC","OWNHOME","WOZ","LABPART","INCOME","WOMWORK","FEMINC","GINI","ECONSPEND","LABSPEND")

write.csv(df,"BRIC ECON DATA.csv")




# ####
# df = knab %>% left_join(econ22,by=join_by(GM_NAAM,GM_CODE)) %>% 
#   left_join(statline,by=join_by(GM_NAAM)) %>% drop_na() %>% 
#   mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
#   select(geometry, everything())
# 
# corm = df %>% st_drop_geometry()
# # econdim = corm
# # rm(list = setdiff(ls(), c("socialdim", "econdim")))
# 
# M = cor(corm[,3:15])
# corrplot::corrplot(M,method="number")
# 
# corm = corm %>% 
#   mutate(average = rowMeans(select(.,bankpth,BusLoc,P_LEEGSW,ownerhomes,woz,netlabour,medinc,womenwork,femincratio,medwealth,revgini,econspend,labourspend))) %>% 
#   select(GM_CODE,average)
# 
# df = df %>% left_join(corm,by=join_by(GM_CODE))
# df = df %>% mutate(quantiles = ntile(average,6))
# 
# ggplot(df) + 
#   geom_sf(aes(fill = average)) + 
#   ggtitle("Economic Domain") + 
#   theme_minimal()
# 
# ggplot(df) +
#   geom_sf(aes(fill = factor(quantiles)), color = "black") +
#   scale_fill_viridis_d() +  # Use a built-in color scale
#   ggtitle("Economic Domain") +
#   theme_minimal()
