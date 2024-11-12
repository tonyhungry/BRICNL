# Try COINr Package

library(COINr)

df <- read.csv("/Volumes/SD Drive/Geo Data/BRIC RAW DATA.csv")

colnames(df)[1:2] = c("uName","uCode")
df = df %>% rename(CRISISSPEND = crisisspend) %>% 
  rename(SAFETYSPEND = safetyspend) %>% 
  rename(SOLVENCY = solvability) %>% 
  rename(AIRPORTS = AIRPORT)

bricmeta <- readxl::read_excel("bricmeta.xlsx")

coin = new_coin(iData = df, iMeta=bricmeta)

# setdiff(bricmeta$iCode,colnames(df)[3:56])
# setdiff(colnames(df)[3:56],bricmeta$iCode)

coin = Normalise(coin,dset="Raw")

coin <- Aggregate(coin, dset = "Normalised")
dset_aggregated <- get_dset(coin, dset = "Aggregated")
nc <- ncol(dset_aggregated)

dset_aggregated[(nc - 10) : nc] |>
  head(5) |>
  signif(3)
