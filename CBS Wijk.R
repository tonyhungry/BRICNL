# CBS Wijk Explore

pacman::p_load(tidyverse,sf) 

b2004 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2004-buurtkaart-data-2/wijk_2004_gen.shp") 
variables = list(colnames(b2004))
rm(b2004)

b2005 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2005buurtkaartdata2v2/wijk_2005_gen2.shp") 
variables = append(variables,list(colnames(b2005)))
rm(b2005)

b2006 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2006 buurt_2006_gn2/wijk_2006_gn2.shp") 
variables = append(variables,list(colnames(b2006)))
rm(b2006)

b2007 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2007 buurt_2007_gen(2)/wijk_2007_gn2.shp") 
variables = append(variables,list(colnames(b2007)))
rm(b2007)

b2008 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2008-buurtkaart-gn-3/wijk_2008_gn3.shp") 
variables = append(variables,list(colnames(b2008)))
rm(b2008)

b2009 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2009-buurtkaart-gn-3/wijk_2009_gn3.shp") 
variables = append(variables,list(colnames(b2009)))
rm(b2009)

b2010 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2010 shape 2010 versie3.0/wijk_2010_v3.shp") 
variables = append(variables,list(colnames(b2010)))
rm(b2010)

b2011 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2011 shape 2011 versie 3.0/wijk_2011_v3.shp")
variables = append(variables,list(colnames(b2011)))
rm(b2011)

b2012 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2012 WijkBuurtkaart_2012_v3/wijk_2012.shp")
variables = append(variables,list(colnames(b2012)))
rm(b2012)

b2013 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2013 uitvoer_shape/wijk_2013.shp") 
variables = append(variables,list(colnames(b2013)))
rm(b2013)

b2014 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2014 uitvoer_shape-2/wijk_2014.shp") 
variables = append(variables,list(colnames(b2014)))
rm(b2014)

b2015 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2015 buurt_2015/wijk_2015.shp") 
variables = append(variables,list(colnames(b2015)))
rm(b2015)

b2016 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2016 Uitvoer_shape-3/wijk_2016.shp") 
variables = append(variables,list(colnames(b2016)))
rm(b2016)

b2017 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2017 WijkBuurtkaart_2017_v3/wijk_2017_V3.shp") 
variables = append(variables,list(colnames(b2017)))
rm(b2017)

b2018 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2018 WijkBuurtkaart_2018_v3/wijk_2018_V3.shp") 
variables = append(variables,list(colnames(b2018)))
rm(b2018)

b2019 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2019 WijkBuurtkaart_2019_v3/wijk_2019_V3.shp") 
variables = append(variables,list(colnames(b2019)))
rm(b2019)

b2020 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2020 WijkBuurtkaart_2020_v3/wijk_2020_V3.shp") 
variables = append(variables,list(colnames(b2020)))
rm(b2020)

b2021 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2021 WijkBuurtkaart_2021_v3/wijken_2021_V3.shp") 
variables = append(variables,list(colnames(b2021)))
rm(b2021)

b2022 <- read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2022 WijkBuurtkaart_2022_v2/wijken_2022_V2.shp") 
variables = append(variables,list(colnames(b2022)))
rm(b2022)

n.obs <- sapply(variables, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(variables, "[", i = seq.max)
mat = as.data.frame(mat)

write.csv(mat, "Wijk variables.csv", row.names = FALSE)

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
