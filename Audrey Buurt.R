# CBS Buurt Data for Audrey
# Audrey wanted specific variables of certain neighborhoods in Rotterdam

library(sf)
library(tidyverse)

bu_code = c("BU05991572","BU05991574","BU05991571","BU05991081","BU05991082")

b2004 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2004-buurtkaart-data-2/buurt_2004_gen.shp") %>% filter(BU_2004 %in% bu_code) %>% select(BU_2004,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ) %>% rename(BU_CODE = BU_2004) %>% st_drop_geometry() %>% mutate(year = 2004)

b2005 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2005buurtkaartdata2v2/buurt_2005_gen2.shp") %>% filter(BU_2005 %in% bu_code) %>% select(BU_2005,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,INK_INW,P_LAAG_INK,P_HOOG_INK) %>% rename(BU_CODE = BU_2005) %>% st_drop_geometry() %>% mutate(year = 2005) %>% rename(P_LAAGINKP = P_LAAG_INK) %>% rename(P_HOOGINKP = P_HOOG_INK)

b2006 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2006 buurt_2006_gn2/buurt_2006_gn2.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ) %>% st_drop_geometry() %>% mutate(year = 2006)

b2007 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2007 buurt_2007_gen(2)/buurt_2007_gn2.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ) %>% st_drop_geometry() %>% mutate(year = 2007)

b2008 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2008-buurtkaart-gn-3/brt_2008_gn3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ) %>% st_drop_geometry() %>% mutate(year = 2008)

b2009 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2009-buurtkaart-gn-3/brt_2009_gn3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2009)

b2010 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2010 shape 2010 versie3.0/buurt_2010_v3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2010)

b2011 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2011 shape 2011 versie 3.0/buurt_2011_v3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW,P_LAAGINKP,P_HOOGINKP) %>% st_drop_geometry() %>% mutate(year = 2011)

b2012 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2012 WijkBuurtkaart_2012_v3/buurt_2012.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW,P_LAAGINKP,P_HOOGINKP) %>% st_drop_geometry() %>% mutate(year = 2012)

b2013 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2013 uitvoer_shape/buurt_2013.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW,P_LAAGINKP,P_HOOGINKP) %>% st_drop_geometry() %>% mutate(year = 2013)

b2014 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2014 uitvoer_shape-2/buurt_2014.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_WONV2000,P_WONT2000,INK_INW,P_LAAGINKP,P_HOOGINKP) %>% st_drop_geometry()  %>% mutate(year = 2014)

b2015 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2015 buurt_2015/buurt_2015.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW,P_LAAGINKP,P_HOOGINKP) %>% st_drop_geometry() %>% mutate(year = 2015)

b2016 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2016 Uitvoer_shape-3/buurt_2016.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000) %>% st_drop_geometry() %>% mutate(year = 2016)

b2017 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2017 WijkBuurtkaart_2017_v3/buurt_2017_V3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2017)

b2018 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2018 WijkBuurtkaart_2018_v3/buurt_2018_V3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2018)

b2019 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2019 WijkBuurtkaart_2019_v3/buurt_2019_V3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2019)

b2020 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2020 WijkBuurtkaart_2020_v3/buurt_2020_V3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2020)

b2021 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2021 WijkBuurtkaart_2021_v3/buurten_2021_V3.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000,INK_INW2,P_LAAGINKP,P_HOOGINKP) %>% rename(INK_INW = INK_INW2) %>% st_drop_geometry() %>% mutate(year = 2021)

b2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/buurten_2022_V2.shp") %>% filter(BU_CODE %in% bu_code) %>% select(BU_CODE,BU_NAAM,P_15_24_JR,P_25_44_JR,P_45_64_JR,P_WEST_AL,P_N_W_AL,WOZ,P_KOOPWON,P_HUURWON,P_HUURCORP,P_HUUROVVH,P_WONV2000,P_WONT2000) %>% st_drop_geometry() %>% mutate(year = 2022)


b2004.2022 <- bind_rows(b2004,b2005,b2006,b2007,b2008,b2009,b2010,b2011,b2012,b2013,b2014,b2015,b2016,b2017,b2018,b2019,b2020,b2021,b2022)

all_objects <- ls()
objects_to_keep <- c("b2004.2022")
rm(list = setdiff(all_objects, objects_to_keep))

df_long <- b2004.2022 %>%
  select(-BU_CODE) %>% 
  pivot_longer(cols = -c(BU_NAAM, year), names_to = "variable", values_to = "value") %>% 
  mutate(BU_NAAM = if_else(BU_NAAM == "Oud-Charlois", "Oud Charlois", BU_NAAM))



write.csv(df_long, "filtered_data_long.csv", row.names = FALSE)
write.csv(b2004.2022,"Buurten20042022.csv",row.names=F)

P_15_24_JR = df_long %>% filter(variable == "P_15_24_JR")
ggplot(P_15_24_JR, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_15_24_JR", x ="Year", y = "Percentages")

woz = df_long %>% filter(variable == "WOZ")
ggplot(woz, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="WOZ", x ="Year", y = "Gemiddelde woningwaarde [x 1 000 euro]")

P_KOOPWON = df_long %>% filter(variable == "P_KOOPWON")
ggplot(P_KOOPWON, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_KOOPWON", x ="Year", y = "Percentages")

P_HUURWON = df_long %>% filter(variable == "P_HUURWON")
ggplot(P_HUURWON, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_HUURWON", x ="Year", y = "Percentages")

P_HUURCORP = df_long %>% filter(variable == "P_HUURCORP")
ggplot(P_HUURCORP, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_HUURCORP", x ="Year", y = "Percentages")

P_HUUROVVH = df_long %>% filter(variable == "P_HUUROVVH")
ggplot(P_HUUROVVH, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_HUUROVVH", x ="Year", y = "Percentages")

INK_INW = df_long %>% filter(variable == "INK_INW")
ggplot(INK_INW, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="INK_INW", x ="Year", y = "Gemiddeld inkomen per inwoner [x 1 000 euro]")

P_LAAGINKP = df_long %>% filter(variable == "P_LAAGINKP")
ggplot(P_LAAGINKP, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_LAAGINKP", x ="Year", y = "Percentages")

P_HOOGINKP = df_long %>% filter(variable == "P_HOOGINKP")
ggplot(P_HOOGINKP, aes(fill=BU_NAAM, y=value, x=year)) + 
  geom_bar(position="dodge",stat="identity") +
  labs(title="P_HOOGINKP", x ="Year", y = "Percentages")