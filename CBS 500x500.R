# CBS 500*500 Explore

pacman::p_load(tidyverse,sf) 

b1971 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/1971 2014 2017-CBSvierkant500m-2/CBSvierkant500m_19712014_201711.shp") 
variables = list(colnames(b1971))
rm(b1971)

b2015 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2015 2019-cbs_vk500_2015_v2/CBS_VK500_2015_v2.shp") 
variables = append(variables,list(colnames(b2015)))
rm(b2015)

b2016 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2016 2019-cbs_vk500_2016_v2/CBS_VK500_2016_v2.shp") 
variables = append(variables,list(colnames(b2016)))
rm(b2016)

b2017 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2017 2021-cbs_vk500_2017_v3/cbs_vk500_2017_v3.shp") 
variables = append(variables,list(colnames(b2017)))
rm(b2017)

b2018 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2018 2021-cbs_vk500_2018_v3/CBS_VK500_2018_v3.shp") 
variables = append(variables,list(colnames(b2018)))
rm(b2018)

b2019 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2019 cbs_vk500_2019_vol2.gpkg") 
variables = append(variables,list(colnames(b2019)))
rm(b2019)

b2020 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2020 cbs_vk500_2020_vol.gpkg") 
variables = append(variables,list(colnames(b2020)))
rm(b2020)

b2021 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2021 cbs_vk500_2021_vol.gpkg") 
variables = append(variables,list(colnames(b2021)))
rm(b2021)

b2022 = read_sf("/Volumes/SD Drive/Geo Data/CBS 500*500 Data/2022 cbs_vk500_2022_v2.gpkg") 
variables = append(variables,list(colnames(b2022)))
rm(b2022)

n.obs <- sapply(variables, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(variables, "[", i = seq.max)
mat = as.data.frame(mat)

write.csv(mat, "500*500 variables.csv", row.names = FALSE)
