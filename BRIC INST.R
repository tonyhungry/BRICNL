# BRIC INST

# Setting Up ####
library(tidyverse)
library(sf)

# Collecting Data ####

## CBS Geografische Data ####
gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")

## Findo Data ####
Municipal_Spending <- readxl::read_excel("/Volumes/SD Drive/Geo Data/BRIC Data/2023 Municipal Spending euros per inhabitant.xlsx")
colnames(Municipal_Spending)[1] = "GM_NAAM"
inst = Municipal_Spending %>% select(GM_NAAM,`Crisisbeheersing en brandweer`,`Openbare orde en veiligheid`) %>% 
  drop_na() %>% 
  mutate(GM_NAAM = ifelse(GM_NAAM == "Den Haag", "'s-Gravenhage", GM_NAAM)) 
colnames(inst) = c("GM_NAAM","crisisspend","safetyspend")

df = gb2023 %>% select(GM_CODE,GM_NAAM,AANT_INW) %>% 
  mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% 
  drop_na() %>% select(-AANT_INW) %>% 
  left_join(inst,by=join_by(GM_NAAM))

rm(Municipal_Spending,inst)

debt <- readxl::read_excel("BRIC Data/2023 Municipal Solvency and Debt Ratios.xlsx")
colnames(debt) = c("GM_NAAM","solvability","debtratio")
debt = debt %>% drop_na() %>% 
  mutate(GM_NAAM = ifelse(GM_NAAM == "Den Haag", "'s-Gravenhage", GM_NAAM)) %>% 
  select(GM_NAAM,solvability)

df = df %>% left_join(debt,by=join_by(GM_NAAM))
rm(debt)

df = df %>% st_drop_geometry()
write.csv(df, "BRIC INST DATA.csv")

df = df %>% 
  drop_na() %>% 
  mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
  select(geometry, everything())

corm = df %>% st_drop_geometry()
# instdim = corm
# rm(list = setdiff(ls(), c("socialdim", "econdim","instdim")))

M = cor(corm[,3:5])
corrplot::corrplot(M,method="number")

corm = corm %>% 
  mutate(average = rowMeans(select(.,crisisspend,safetyspend,solvability))) %>% 
  select(GM_CODE,average)

df = df %>% left_join(corm,by=join_by(GM_CODE))
df = df %>% mutate(quantiles = ntile(average,6))

ggplot(df) + 
  geom_sf(aes(fill = average)) + 
  ggtitle("Institutional Domain") + 
  theme_minimal()

ggplot(df) +
  geom_sf(aes(fill = factor(quantiles)), color = "black") +
  scale_fill_viridis_d() +  # Use a built-in color scale
  ggtitle("Institutional Domain") +
  theme_minimal()
