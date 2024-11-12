# Election data 2023 Tweede Kamer Election

library(tidyverse)

df <- read.csv("~/Downloads/TK2023_uitslag.csv", sep=";")

summary(df$VeldType)
unique(df$VeldType)

df = df %>% filter(VeldType == "Kiesgerechtigden" | VeldType == "Opkomst") %>% 
  filter(str_starts(RegioCode,"G")) %>% 
  mutate(RegioCode = str_replace(RegioCode, "^G", "GM")) %>% # corrects the code for CBS harmonization
  select(Regio,RegioCode,VeldType,Waarde) %>% 
  pivot_wider(names_from =VeldType,values_from = Waarde) # lengthens the dataset for easier calculation

df = df %>% mutate(turnout = Opkomst/Kiesgerechtigden)

getwd()
write.csv(df,"/Volumes/SD Drive/Geo Data/BRIC Data/2023 Election Turnout.csv")
