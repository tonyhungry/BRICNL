# CBS PC4 Explore

pacman::p_load(tidyverse,sf) 

b2015 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2015 CBS-PC4-2015-v2/CBS_PC4_2015_v2.shp") 
variables = list(colnames(b2015))
rm(b2015)

b2016 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2016 CBS-PC4-2016-v2/CBS_PC4_2016_v2.shp") 
variables = append(variables,list(colnames(b2016)))
rm(b2016)

b2017 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2017 CBS-PC4-2017-v3/CBS_PC4_2017_v3.shp") 
variables = append(variables,list(colnames(b2017)))
rm(b2017)

b2018 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2018 CBS-PC4-2018-v3/CBS_PC4_2018_v3.shp") 
variables = append(variables,list(colnames(b2018)))
rm(b2018)

b2019 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2019 2023-cbs_pc4_2019_vol/cbs_pc4_2019_vol.gpkg") 
variables = append(variables,list(colnames(b2019)))
rm(b2019)

b2020 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2020 2023-cbs_pc4_2020_vol/cbs_pc4_2020_vol.gpkg") 
variables = append(variables,list(colnames(b2020)))
rm(b2020)

b2021 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2021 2024-cbs_pc4_2021_vol/cbs_pc4_2021_vol.gpkg") 
variables = append(variables,list(colnames(b2021)))
rm(b2021)

b2022 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2022 2024-cbs_pc4_2022_v2/cbs_pc4_2022_v2.gpkg") 
variables = append(variables,list(colnames(b2022)))
rm(b2022)

b2023 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC4 Data/2023 2024-cbs_pc4_2023_v1/cbs_pc4_2023_v1.gpkg") 
variables = append(variables,list(colnames(b2023)))
rm(b2023)

n.obs <- sapply(variables, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(variables, "[", i = seq.max)
mat = as.data.frame(mat)

write.csv(mat, "PC4 variables.csv", row.names = FALSE)
