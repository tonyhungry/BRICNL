# CBS 100*100 Squares Explore

pacman::p_load(tidyverse,sf) 

Old = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2000 - 2014 CBSvierkant100m/CBSvierkant100m_20002014_201410.shp") %>% 
  mutate(across(where(is.integer), ~na_if(., -99998)))

Old = Old %>% mutate(across(where(is.integer), ~na_if(., -99997))) %>% 
  mutate(across(where(is.integer), ~na_if(., -99999))) %>% 
  mutate(across(where(is.character), ~na_if(., "nihil"))) %>% 
  mutate(across(where(is.character), ~na_if(., "geheim")))

one20002014 = colnames(Old)
sum(is.na(Old))

one2015 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2015 2019-cbs-vk100-2015_v2/CBS_VK100_2015_v2.shp")
variables = list(one20002014,colnames(one2015))
rm(one2015)

one2016 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2016 2019-cbs-vk100-2016_v2/CBS_VK100_2016_v2.shp")
variables = append(variables,list(colnames(one2016)))
rm(one2016)

one2017 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2017 2021-cbs_vk100_2017_v3/cbs_vk100_2017_v3.shp")
variables = append(variables,list(colnames(one2017)))
rm(one2017)

one2018 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2018 2021-cbs_vk100_2018_v3/cbs_vk100_2018_v3.shp")
variables = append(variables,list(colnames(one2018)))
rm(one2018)

one2019 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2019 cbs_vk100_2019_vol.gpkg")
variables = append(variables,list(colnames(one2019)))
rm(one2019)

one2020 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2020 cbs_vk100_2020_vol.gpkg")
variables = append(variables,list(colnames(one2020)))
rm(one2020)

one2021 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2021 cbs_vk100_2021_vol.gpkg")
variables = append(variables,list(colnames(one2021)))
rm(one2021)

one2022 = read_sf("/Volumes/SD Drive/Geo Data/CBS 100*100 Data/2022 cbs_vk100_2022_v2.gpkg")
variables = append(variables,list(colnames(one2022)))
rm(one2022)


n.obs <- sapply(variables, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(variables, "[", i = seq.max)
mat = as.data.frame(mat)

write.csv(mat, "100*100 variables.csv", row.names = FALSE)
