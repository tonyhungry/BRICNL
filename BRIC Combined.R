# BRIC ALL - Merging All data sets together

# Min-Max Transformation
# Inverse Orientation
# Correlation
# Spatial Autocorrelation

library(tidyverse)
library(corrplot)
library(Hmisc)
library(sf)

# Load in data ####

social <- read.csv("/Volumes/SD Drive/Geo Data/BRIC SOCIAL DATA.csv")
social = social %>% select(-X)
econ <- read.csv("/Volumes/SD Drive/Geo Data/BRIC ECON DATA.csv")
econ = econ %>% select(-X)
inst <- read.csv("/Volumes/SD Drive/Geo Data/BRIC INST DATA.csv")
inst = inst %>% select(-X)
infra <- read.csv("/Volumes/SD Drive/Geo Data/BRIC INFRA DATA.csv")
infra = infra %>% select(-X)
comm <- read.csv("/Volumes/SD Drive/Geo Data/BRIC COMM DATA.csv")
comm = comm %>% select(-X)
envi <- read.csv("/Volumes/SD Drive/Geo Data/BRIC NL/BRIC ENVI DATA.csv")
envi = envi %>% select(-X)

gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gem = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_NAAM,GM_CODE)
rm(gb2023)

df = gem %>% left_join(social,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(econ,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(inst,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(infra,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(comm,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(envi,join_by(GM_NAAM,GM_CODE))

# df = df %>% st_drop_geometry()
# write.csv(df,"BRIC RAW DATA.csv",row.names = F)

# Summary statistics ####

selectednames = c(Hmisc::Cs(WORKING,BORND,CARS,NBENEFITS,EDUC,EDUCSPEND,HEALTHSPEND,PSYCHRES,PSYCH,DOCTOR,WOZ,FEMINC,GINI,ECONSPEND,BANKS,LABSPEND,WOMWORK,OWNHOME,CRISISSPEND,SOLVENCY,SAFETYSPEND,HOSPITAL,VACHOME,HOTELS,TRAIN,PHARMACY,FIRE,ROAD,AIRPORT,SCHOOLS,COMMSPEND,RECRSPEND,VOLUNTEER,SPORTS,MIGRATION,CRIMES,VOTER,DAYCARE,LAND,GREENSPEND,WASTESPEND,GAS,ELEC,BUFFER,OPENSPACE,CULTIVATE,FLOOD))

sumdf = df %>% rename_with(toupper) %>% 
  na.omit() %>% 
  rename(SOLVENCY = SOLVABILITY) %>% 
  select(all_of(selectednames)) %>% 
  mutate(ECONSPEND1 = ECONSPEND + LABSPEND) %>% 
  select(-ECONSPEND, -LABSPEND) %>% 
  mutate(ENVISPEND = GREENSPEND + WASTESPEND) %>% 
  select(-GREENSPEND,-WASTESPEND) %>% 
  rename(ECONSPEND = ECONSPEND1) %>% 
  st_drop_geometry()

get_specific_stats <- function(x) {
  c(Min = min(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
    )
}

specific_stats <- sapply(sumdf, get_specific_stats)
specific = t(specific_stats)
write.csv(specific,"summarystats.csv",col.names = F)

# Min-Max Transformation #### 

dftransformed = df %>% 
  na.omit() %>% 
  mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>% 
  select(geometry, everything())

# Inverse Orientation ####

finaldf = dftransformed %>% 
  mutate(gini = 1 - GINI) %>% select(-GINI) %>% 
  mutate(train = 1 - TRAIN) %>% select(-TRAIN) %>% 
  mutate(pharmacy = 1 - PHARMACY) %>% select(-PHARMACY) %>% 
  mutate(fire = 1 - FIRE) %>% select(-FIRE) %>% 
  mutate(road = 1 - ROAD) %>% select(-ROAD) %>% 
  mutate(library = 1 - LIBRARY) %>% select(-LIBRARY) %>% 
  mutate(invmig = 1 - MIGRATION) %>% select(-MIGRATION) %>% 
  mutate(crimes = 1 - CRIMES) %>% select(-CRIMES) %>% 
  mutate(invgas = 1 - GAS) %>% select(-GAS) %>% 
  mutate(invelec = 1 - ELEC) %>% select(-ELEC) %>% 
#  mutate(invheat = 1 - heat) %>% select(-heat) %>% 
  mutate(invflood = 1 - FLOOD) %>% select(-FLOOD) %>% 
  st_drop_geometry()

finaldf = finaldf %>% rename_with(toupper) %>% 
  rename(SOLVENCY = SOLVABILITY) %>% 
  rename(MIGRATION = INVMIG) %>% 
  rename(GAS = INVGAS) %>% 
  rename(ELEC = INVELEC) %>% 
  rename(FLOOD = INVFLOOD)

# colnames(finaldf)[3:60] = c("POPDENSITY","HHSIZE","BORND","WORKING","NSINGLE","CARS","HEALTHSPEND","EDUCSPEND","EDUC","BANKS","BUSLOC","VACHOME","OWNHOME","WOZ","LABPART","INCOME","WOMWORK","FEMINC","WEALTH","ECONSPEND","LABSPEND","CRISISSPEND","SAFETYSPEND","SOLVENCY","OAD","STED","POLICE","PSYCH","DOCTOR","HOSPITAL","HOTELS","VOTER","COMMSPEND","RECRSPEND","FOOD","DEPARTMENT","HORECA","SCHOOLS","CINEMA","DAYCARE","LAND","GREENSPEND","WASTESPEND","BUFFER","OPENSPACE","CULTIVATE","GINI","TRAIN","PHARMACY","FIRE","ROAD","LIBRARY","MIGRATION","CRIMES","GAS","ELEC","HEAT","FLOOD")

write.csv(finaldf,"BRIC ALL DATA.csv",row.names = F)

# Correlation for all variables ####

M = cor(finaldf[,3:56],method="spearman")
corrplot(M, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
M[abs(M) < 0.7] = NA

M = as.data.frame(M)
not_all_na <- function(x) any(!is.na(x))
M = M %>% select(where(not_all_na))
M = as.matrix(M)
corrplot(M, method = "color", na.label = " ", tl.col = "black", tl.srt = 45, is.corr = FALSE)

allmat = as.matrix(finaldf[,3:55])
sig = rcorr(allmat)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
sigtable = flattenCorrMatrix(sig$r, sig$P)
# str(sigtable)
sigtable = sigtable %>% filter(cor > 0.8 | cor < -0.8)
writexl::write_xlsx(sigtable,"sigtable.xlsx")

# Took out OAD,STED,DEPARTMENT,HORECA,CINEMA,WEALTH,HHSIZE





# Spatial autocorrelation ####
# Assuming most of it has spatial autocorrelation...
geodf = gem %>% left_join(finaldf,by=join_by(GM_NAAM,GM_CODE)) %>% 
  select(-OAD,-STED,-DEPARTMENT,-HORECA,-CINEMA,-WEALTH,-HHSIZE,-GM_NAAM,-GM_CODE) %>% 
  relocate(WORKING,POPDENSITY,NSINGLE,BORND,CARS,EDUC,EDUCSPEND,HEALTHSPEND,BUSLOC,VACHOME,OWNHOME,WOZ,LABPART,INCOME,WOMWORK,FEMINC,GINI,ECONSPEND,LABSPEND,BANKS,CRISISSPEND,SOLVENCY,SAFETYSPEND,DOCTOR,HOSPITAL,HOTELS,TRAIN,PHARMACY,FIRE,ROAD,PSYCH,POLICE,FOOD,SCHOOLS,DAYCARE,LIBRARY,COMMSPEND,RECRSPEND,VOTER,MIGRATION,CRIMES,LAND,GREENSPEND,WASTESPEND,GAS,ELEC,BUFFER,OPENSPACE,CULTIVATE,HEAT,FLOOD)

st_crs(geodf)
write.csv(geodf,"GEO BRIC MOST VARS.csv")

# spatial_df <- st_as_sf(geodf, coords = c("lon", "lat"), crs = 28992)
# st_write(spatial_df, "GEO BRIC MOST VARS.geojson")
# st_write(spatial_df, dsn = "GEO BRIC MOST VARS.gpkg", driver = "GPKG")

nb <- poly2nb(geodf)
no_neighbors <- which(card(nb) == 0)
geodf_no_neighbors_removed <- geodf[-no_neighbors, ]
nb_filtered <- poly2nb(geodf_no_neighbors_removed)
listw <- nb2listw(nb_filtered, style = "W")

variable <- geodf_no_neighbors_removed$WORKING
variable <- na.omit(variable)
moran_result <- moran.test(variable, listw)
print(moran_result)

variable <- geodf_no_neighbors_removed$POPDENSITY
variable <- na.omit(variable)
moran_result <- moran.test(variable, listw)
print(moran_result)

variable <- geodf_no_neighbors_removed$FLOOD
variable <- na.omit(variable)
moran_result <- moran.test(variable, listw)
print(moran_result)









