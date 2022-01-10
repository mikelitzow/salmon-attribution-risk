# analyze effects of sst variability on salmon catch
# within the historical record (1965-2021)

library(tidyverse)
library(gtools)
theme_set(theme_bw())

# load goa catch
goa.catch <- read.csv("./data/goa.catch.csv")

# get goa totals (all areas combined)
goa.catch$year <- as.factor(goa.catch$year)
goa.catch <- goa.catch %>%
  pivot_longer(cols = c(-year, -area), names_to = "species", values_to = "catch")

goa.catch <- goa.catch %>%
  group_by(year, species) %>%
    summarise(catch = sum(catch)) %>%
  mutate(log.catch = log(catch, 10))

goa.catch$year <- as.numeric(as.character(goa.catch$year))

# now plot to check
ggplot(goa.catch, aes(year, log.catch, color = species)) +
  geom_line() 

# limit to 1965:2021 and scale
goa.catch <- goa.catch %>%
  filter(year >= 1965) %>%
  select(-catch)

# separate even and odd pink
pink <- goa.catch$species == "pink"
even <- even(goa.catch$year)
odd <- odd(goa.catch$year)

goa.catch$species[pink & even] <- "pink.even"
goa.catch$species[pink & odd] <- "pink.odd"


# now scale
goa.catch <- goa.catch %>%
  pivot_wider(names_from = species, values_from = log.catch)

goa.catch[,2:6] <- apply(goa.catch[,2:6], 2, scale)

# and plot to check
goa.catch <- goa.catch %>%
  pivot_longer(cols = -year)

plot <- na.omit(goa.catch)

ggplot(plot, aes(year, value)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, col = "grey") +
  facet_wrap(~name)

# quick thought - the signal of increasing chum hatchery production
# appears to be clear in the late 1980s and 1990s
# perhaps we should control for GOA hatchery inputs of chum and pink

# finally, lag to reflect average time between ocean entry and catch:
# one year for pink and coho
# two years for sockeye
# three years for chum

goa.catch$ocean.entry <- case_when(
  goa.catch$name %in% c("pink.odd", "pink.even", "coho") ~ goa.catch$year - 1,
  goa.catch$name == "sockeye" ~ goa.catch$year - 2,
  goa.catch$name == "chum" ~ goa.catch$year - 3
  )

# remove NAs
goa.catch <- na.omit(goa.catch)

ggplot(goa.catch, aes(ocean.entry, value, color = name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, col = "grey") 


# and bring in sst time series
sst <- read.csv("./data/goa.sst.csv")

# "winter" = November-March (year corresponding to January); annual = January - December
# both winter and annual data scaled wrt 20th century (1901-1999 for winter; 1900-1999 for annual)
# .2 denotes two-yr running mean (year of and year following year of interest)
# .3 denotes three-yr running mean (year before, year of, and year following year of interest)