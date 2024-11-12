# CBS open data (Statline) direct download and use

library(cbsodataR)

# Downloading table list
toc <- cbs_get_toc()
head(toc)

# Downloading entire dataset (can take up to 30s)
data <- cbs_get_data("83765NED")
head(data)

df = cbs_get_data("85967NED",periods="2022")

# Downloading metadata
metadata <- cbs_get_meta("83765NED")
head(metadata)

# Downloaded the labour participation 
metadata1 <- cbs_get_meta("85796NED")
df = cbs_get_data("85796NED",Geslacht="T001038",Persoonskenmerken="T009002",GeboortelandOuders="T001638",Herkomstland="T001040",RegioS=has_substring("GM"),Perioden="2022JJ00")


metadata <- cbs_get_meta("83765NED")
data <- cbs_get_data("83765NED", WijkenEnBuurten = "GM0363",select = c("WijkenEnBuurten", "AantalInwoners_5"))

toc = toc %>% mutate(english = str_detect(Identifier, "(eng|ENG)$"))

# Identify which descriptions has wijk or buurt in it
toc = toc %>% rowwise() %>%
  mutate(wb = any(str_detect(c_across(c(Title, ShortTitle, ShortDescription, Summary)), "(wijk|buurt|wijken|buurten)$"))) %>%
  ungroup() 

wb = toc %>% filter(wb == TRUE) %>% select(Identifier,Title,ShortTitle,ShortDescription,Summary) %>% arrange(Title)
