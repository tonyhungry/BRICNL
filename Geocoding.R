pacman::p_load(tidyverse,tidygeocoder,sf,mapview) 

library(readxl)
original <- read_excel("~/Downloads/openbaar-databestand-ggz-en-vz-verslavingszorg-clientervaringen-cqi-verslagjaar-2023-januari-tm-december.xlsx")

df = original %>% distinct(OrganisatieNaam,LocatieNaam,.keep_all = T) %>% 
  select(OrganisatieNaam,LocatieNaam,LocatiePlaats,LocatiePostcode,LocatieHuisnummer)


# Using PDOK API to geocode ####
library(httr)
library(jsonlite)

## Made a function to use the PDOK API ####
geocode_pdok <- function(postcode, house_number) {
  base_url <- "https://api.pdok.nl/bzk/locatieserver/search/v3_1/free"
  query <- paste0(postcode, " ", house_number)
  response <- GET(base_url, query = list(q = query, fl = "id,centroide_ll", start = 0, rows = 1, wt = "json"))
  
  if (response$status_code == 200) {
    content <- content(response, "text")
    json_data <- fromJSON(content, flatten = TRUE)
    
    if ("response" %in% names(json_data) && length(json_data$response$docs) > 0) {
      coords <- json_data$response$docs$centroide_ll
      coords = str_extract(coords, "\\(([^)]+)\\)")
      coords = str_replace_all(coords, "[()]", "")
      lon = as.numeric(word(coords, 1))
      lat = as.numeric(word(coords, 2))
      return(list(lat = as.numeric(lat), lon = as.numeric(lon)))
    } else {
      return(list(lat = NA, lon = NA))
    }
  } else {
    stop("Failed to fetch data from PDOK API")
  }
}

## Use the function to parse the dataset through the API and then append it to the dataset ####
geodedf = df %>%
  rowwise() %>%
  mutate(geocode = list(geocode_pdok(LocatiePostcode, LocatieHuisnummer))) %>%
  mutate(lat = geocode$lat, lon = geocode$lon) %>%
  select(-geocode)

write.csv(geodedf, "Mental Health Care Locations 2023.csv", row.names = FALSE)


# THIS WORKS!
something = GET(url = "https://api.pdok.nl/bzk/locatieserver/search/v3_1/free", query = list(q = "1031JG 5",fl = "id,centroide_ll", start = 0, rows = 1, wt = "json"))

# content = content(something,"text")
# jason = fromJSON(content,flatten=T)
# coord = jason$response$docs$centroide_ll
# coord = str_extract(coord, "\\(([^)]+)\\)")
# coord = str_replace_all(coord, "[()]", "")
# lon = as.numeric(word(coord, 1))
# lat = as.numeric(str_extract(coord, "[^ ]+$"))



# This is the easiest one, but costs money :(
# library(ggmap)
# register_google(key = "AIzaSyAFNig9u8hyYI7vLCsyUnnURj_53MZ6GAQ")
# 
# googlegeodf = df %>% mutate_geocode(LocatiePostcode)
