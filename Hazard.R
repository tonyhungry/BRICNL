# Hazard Maps

# Loading necessary packages
library(sf)
library(terra)

gb2023 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gem = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_NAAM,GM_CODE)
rm(gb2023)

# Urban Heat ####
heat <- rast("/Volumes/SD Drive/Geo Data/BRIC Data/Urban Heat Effect/Stedelijk_hitte_eiland_effect_01062022_v2.tif")

global(heat, fun = "min", na.rm = TRUE) # Looking for the lowest value
has_na <- global(is.na(heat), fun = "sum")
print(has_na)
plot(is.na(heat), main = "Missing Values in Raster") # negligible
st_crs(gem)
crs(heat)

extracted_values <- extract(heat, gem, fun = mean, na.rm = TRUE,bind=T)
df = as.data.frame(extracted_values)
writexl::write_xlsx(df,"urban heat effect per gem.xlsx")
rm(df,heat,extracted_values)

# Water Depth ####
flood <- rast("/Volumes/SD Drive/Geo Data/BRIC Data/2022 water depth.tif")
plot(flood)

global(flood, fun = "min", na.rm = TRUE)
global(flood, fun = "max", na.rm = TRUE)

has_na <- global(is.na(flood), fun = "sum")
print(has_na)
plot(is.na(flood), main = "Missing Values in Raster")

flood_corrected <- ifel(flood == -9999, 0, flood) # have all of the non-flood areas as 0
plot(flood_corrected)

extracted_values <- extract(flood_corrected, gem, fun = mean, na.rm = TRUE,bind=T)
df = as.data.frame(extracted_values)
writexl::write_xlsx(df,"maximum flood depth per gem.xlsx")
rm(df,heat,extracted_values)

# Corine 