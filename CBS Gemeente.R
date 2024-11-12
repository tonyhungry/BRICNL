# CBS Gemeente Explore

pacman::p_load(tidyverse,sf) 

gb2004 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2004-buurtkaart-data-2/gem_2004_gen.shp") 
variables = list(colnames(gb2004))
rm(gb2004)

gb2005 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2005buurtkaartdata2v2/gem_2005_gen2.shp") 
variables = append(variables,list(colnames(gb2005)))
rm(gb2005)

gb2006 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2006 buurt_2006_gn2/gem_2006_gn2.shp") 
variables = append(variables,list(colnames(gb2006)))
rm(gb2006)

gb2007 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2007 buurt_2007_gen(2)/gem_2007_gn2.shp") 
variables = append(variables,list(colnames(gb2007)))
rm(gb2007)

gb2008 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2008-buurtkaart-gn-3/gem_2008_gn3.shp") 
variables = append(variables,list(colnames(gb2008)))
rm(gb2008)

gb2009 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2009-buurtkaart-gn-3/gem_2009_gn3.shp") 
variables = append(variables,list(colnames(gb2009)))
rm(gb2009)

gb2010 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2010 shape 2010 versie3.0/gem_2010_v3.shp") 
variables = append(variables,list(colnames(gb2010)))
rm(gb2010)

gb2011 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2011 shape 2011 versie 3.0/gem_2011_v3.shp")
variables = append(variables,list(colnames(gb2011)))
rm(gb2011)

gb2012 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2012 WijkBuurtkaart_2012_v3/gem_2012.shp")
variables = append(variables,list(colnames(gb2012)))
rm(gb2012)

gb2013 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2013 uitvoer_shape/gem_2013.shp") 
variables = append(variables,list(colnames(gb2013)))
rm(gb2013)

gb2014 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2014 uitvoer_shape-2/gem_2014.shp") 
variables = append(variables,list(colnames(gb2014)))
rm(gb2014)

gb2015 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2015 buurt_2015/gem_2015.shp") 
variables = append(variables,list(colnames(gb2015)))
rm(gb2015)

gb2016 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2016 Uitvoer_shape-3/gem_2016.shp") 
variables = append(variables,list(colnames(gb2016)))
rm(gb2016)

gb2017 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2017 WijkBuurtkaart_2017_v3/gemeente_2017_V3.shp") 
variables = append(variables,list(colnames(gb2017)))
rm(gb2017)

gb2018 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2018 WijkBuurtkaart_2018_v3/gemeente_2018_V3.shp") 
variables = append(variables,list(colnames(gb2018)))
rm(gb2018)

gb2019 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2019 WijkBuurtkaart_2019_v3/gemeente_2019_V3.shp") 
variables = append(variables,list(colnames(gb2019)))
rm(gb2019)

gb2020 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2020 WijkBuurtkaart_2020_v3/gemeente_2020_V3.shp") 
variables = append(variables,list(colnames(gb2020)))
rm(gb2020)

gb2021 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2021 WijkBuurtkaart_2021_v3/gemeenten_2021_V3.shp") 
variables = append(variables,list(colnames(gb2021)))
rm(gb2021)

gb2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/gemeenten_2022_V2.shp") 
variables = append(variables,list(colnames(gb2022)))
rm(gb2022)

n.obs <- sapply(variables, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(variables, "[", i = seq.max)
mat = as.data.frame(mat)

write.csv(mat, "Gemeente variables.csv", row.names = FALSE)

# Comparing variable names between the Buurt and Wijk

x = 2004:2022
y = rep("b",19)
names(variables) = paste(y,x,sep="")
names(variables1) = paste(y,x,sep="")

setequal(variables$b2004,variables1$b2004)
setdiff(variables$b2004,variables1$b2004) # Elements in Wijk but not in Buurt

check_differences <- function(dataset1, dataset2) {
  differences <- list()
  
  # Check if both datasets have the same number of columns
  if (length(dataset1) != length(dataset2)) {
    stop("Both datasets must have the same number of columns")
  }
  
  # Iterate over each column
  for (i in 1:length(dataset1)) {
    column_name <- names(dataset1)[i]
    diff <- setdiff(dataset2[[i]], dataset1[[i]])
    
    if (length(diff) > 0) {
      differences[[column_name]] <- diff
    } else {
      differences[[column_name]] <- "No differences"
    }
  }
  
  return(differences)
}

check_differences(variables1,variables)

setdiff(variables1$b2004,variables$b2004) # Elements in Buurt but not in Wijk
