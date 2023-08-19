# organize catch data

library(tidyverse)

# load catch data for individual ADF&G management areas

# southeast Alaska
se <- read.csv("./data/southeast.catch.csv")

head(se)

# removing chinook since this is not included in all areas; also adding area

se <- se %>% 
  select(-Chinook) %>%
  mutate(Area = "southeast")

# Cook Inlet
ci <- read.csv("./data/cook.inlet.catch.csv")

head(ci)

# add area
ci <- ci %>%
  mutate(Area = "cook.inlet")

# Kodiak
kod <- read.csv("./data/kodiak.catch.csv")

head(kod)

# fix missing area in 2022 (set all to "Kodiak")
kod$Area <- "kodiak"


# rearrange and drop Chinook
kod <- kod %>%
  select(Year, Sockeye, Coho, Pink, Chum, Area)

# Prince William Sound
pws <- read.csv("./data/pws.catch.csv")

head(pws)

# add area
pws$Area = "prince.william.sound"

# remove Chinook
pws <- pws %>%
  select(-Chinook)

# Chignik
chig <- read.csv("./data/chignik.catch.csv")

head(chig)

update <- read.csv("./data/chignik.2020.2021.csv")

names(chig)[1] <- "Year"

chig <- rbind(chig, update)


# remove Chinook and add area
chig <- chig %>%
  select(-Chinook) %>%
  mutate(Area = "chignik")

# S. Peninsula
pen <- read.csv("./data/south.peninsula.catch.csv")

head(pen)

# remove Chinook and add area
pen <- pen %>%
  select(-chinook) %>%
  mutate(area = "south.peninsula")

# double-check names
names(se); names(pws); names(ci); names(kod); names(chig); names(pen) # looks good

# make all the names identical and combine
names(se) <- names(pws) <- names(ci) <- names(kod) <- names(chig) <-
  c("year", "sockeye", "coho", "pink", "chum", "area")

goa.catch <- rbind(se, pws, ci, kod, chig, pen)

# and save
write.csv(goa.catch, "./data/goa.catch.csv", row.names = F)
