# CBS PC6 Explore

pacman::p_load(tidyverse,sf) 

b20082010a <- read.csv2("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2008 2010 data-en-toelichting-postcodegebieden-2008-versie-2012-02-21/Eindbestand DemRenV.dat", sep="") # This is actually 2010 data
variables = list(colnames(b20082010a))
rm(b20082010a)

b20082010b = read.csv("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2008 2010 data-en-toelichting-postcodegebieden-2008-versie-2012-02-21/Eindbestand SEC.dat", sep="") # This is actually 2008 data
variables = append(variables,list(colnames(b20082010b)))
rm(b20082010b)

b2012 <- read.delim("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2012_2014_PC6_demografie_en_wonen/2012_PC6_demografie_en_wonen.DAT")
variables = append(variables,list(colnames(b2012)))
rm(b2012)

b2014 <- read.csv("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2012_2014_PC6_demografie_en_wonen/2014_PC6_demografie_en_wonen.DAT", sep="")
variables = append(variables,list(colnames(b2014)))
rm(b2014)

b2015 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2015 CBS-PC6-2015-v2/CBS_PC6_2015_v2.shp") 
variables = append(variables,list(colnames(b2015)))
rm(b2015)

b2016 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2016 CBS-PC6-2016-v2/CBS_PC6_2016_v2.shp") 
variables = append(variables,list(colnames(b2016)))
rm(b2016)

b2017 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2017 CBS-PC6-2017-v3/CBS_PC6_2017_v3.shp") 
variables = append(variables,list(colnames(b2017)))
rm(b2017)

b2018 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2018 CBS-PC6-2018-v3/CBS_PC6_2018_v3.shp") 
variables = append(variables,list(colnames(b2018)))
rm(b2018)

b2019 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2019 2023-cbs_pc6_2019_vol/cbs_pc6_2019_vol.gpkg") 
variables = append(variables,list(colnames(b2019)))
rm(b2019)

b2020 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2020 2023-cbs_pc6_2020_vol/cbs_pc6_2020_vol.gpkg") 
variables = append(variables,list(colnames(b2020)))
rm(b2020)

b2021 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2021 2024-cbs_pc6_2021_vol/cbs_pc6_2021_vol.gpkg") 
variables = append(variables,list(colnames(b2021)))
rm(b2021)

b2022 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2022 2024-cbs_pc6_2022_v2/cbs_pc6_2022_v2.gpkg") 
variables = append(variables,list(colnames(b2022)))
rm(b2022)

b2023 = read_sf("/Volumes/SD Drive/Geo Data/CBS PC6 Data/2023 2024-cbs_pc6_2023_v1/cbs_pc6_2023_v1.gpkg") 
variables = append(variables,list(colnames(b2023)))
rm(b2023)

n.obs <- sapply(variables, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(variables, "[", i = seq.max)
mat = as.data.frame(mat)

write.csv(mat, "PC6 variables.csv", row.names = FALSE)
