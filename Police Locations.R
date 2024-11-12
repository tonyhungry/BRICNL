# Police Locations

library(httr)
library(jsonlite)

# Data queried on 18 September 2024

# Define API endpoint
url <- "https://api.politie.nl/v4/politiebureaus/all" 
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(content)
str(data)
df1 <- data[["politiebureaus"]]


url <- "https://api.politie.nl/v4/politiebureaus/all?offset=100"
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(content)
str(data)
df2 <- data[["politiebureaus"]]

url <- "https://api.politie.nl/v4/politiebureaus/all?offset=200"
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(content)
str(data)
df3 <- data[["politiebureaus"]]

url <- "https://api.politie.nl/v4/politiebureaus/all?offset=300"
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(content)
str(data)
df4 <- data[["politiebureaus"]]

rownames(df1) = NULL
rownames(df2) = NULL
rownames(df3) = NULL
rownames(df4) = NULL

df = bind_rows(df1,df2,df3,df4)
df = df %>% select(naam,bezoekadres,locaties) %>% drop_na() %>% 
  unnest_wider(bezoekadres) %>% 
  unnest_wider(locaties)

df = df %>% 
  mutate(postcode = ifelse(grepl("^[0-9]{4}[A-Z]{2}", postcode),           
                                      gsub("^([0-9]{4})([A-Z]{2}).*", "\\1 \\2", postcode),  
                                      ifelse(grepl("^[0-9]{4} [A-Z]{2}", postcode), 
                                             sub("^([0-9]{4} [A-Z]{2}).*", "\\1", postcode), 
                                             NA)))
df = df %>% 
  mutate(postcode = ifelse(naam == "Politieservicepunt Reuver", "5953 AL", postcode)) %>% 
  drop_na()

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

geodedf = df %>%
  rowwise() %>%
  mutate(geocode = list(geocode_pdok(postadres,postcode))) %>%
  mutate(lat = geocode$lat, lon = geocode$lon) %>%
  select(-geocode)

geodedf = geodedf %>% select(naam,postadres,postcode,lat,lon)
write.csv(geodedf, "Police Stations 2024.csv", row.names = FALSE)
