# Analyze effects of sst variability on salmon catch
# within the historical record (1965-2021)


source("./scripts/load.R")
dir.create("./figures/sst_catch", showWarnings = FALSE)


## Clean catch data ----------------------------------------

## Read in salmon data
goa_catch <- read.csv("./data/goa.catch.csv")
nbc_catch <- read.csv("./data/nbc.catch.csv")
goa_age   <- read.csv("./data/goa_age.csv")
nbc_age   <- read.csv("./data/bc_age.csv")

## Add regions
goa_catch$region <- "GOA"
nbc_catch$region <- "NBC"

## Combine
catch_wide <- plyr::rbind.fill(goa_catch, nbc_catch)

## Read in SST data
sst <- read.csv("./data/regional_north_pacific_ersst_anomaly_time_series.csv")
goa_sst <- sst[sst$region == "Gulf_of_Alaska", ]
bc_sst <- sst[sst$region == "British_Columbia_Coast", ]

## Subset years
catch_wide <- catch_wide[catch_wide$year >= 1965, ]

## Sum across catch regions
sock <- plyr::ddply(catch_wide, .(region, year), summarize, catch = sum(sockeye))
coho <- plyr::ddply(catch_wide, .(region, year), summarize, catch = sum(coho))
pink <- plyr::ddply(catch_wide, .(region, year), summarize, catch = sum(pink))
chum <- plyr::ddply(catch_wide, .(region, year), summarize, catch = sum(chum))
pink_even <- pink[pink$year %% 2 == 0, ]
pink_odd  <- pink[pink$year %% 2 != 0, ]

## Add species
sock$species <- "Sockeye"
coho$species <- "Coho"
chum$species <- "Chum"
pink$species <- "Pink"
pink_even$species <- "Pink even"
pink_odd$species  <- "Pink odd"

## Combine in long format
catch <- rbind(sock, coho, chum, pink_even, pink_odd)
catch$species_fac <- factor(catch$species, levels = unique(catch$species))
catch$region_fac  <- factor(catch$region, levels = unique(catch$region))

## Add era variable
catch$era <- "1977-1988"
catch$era <- ifelse(catch$year >= 1989, "1989-2021", catch$era)
catch$era <- ifelse(catch$year <= 1976, "1965-1976", catch$era)

# ML: quick thought - the signal of increasing chum hatchery production appears
# to be clear in the late 1980s and 1990s perhaps we should control for GOA
# hatchery inputs of chum and pink


## Combine catch + sst -------------------------------------
## "winter" = November-March (year corresponding to January);
##            annual = January - December both winter and annual data scaled wrt
##            20th century (1901-1999 for winter; 1900-1999 for annual)
## .2 denotes two-yr running mean (year of and year following year of interest)
## .3 denotes three-yr running mean (year before, year of, and year following year of interest)

## Get mean age props for sockeye
age_wgt_goa <- c(mean(goa_age$R_ocean_1),
                 mean(goa_age$R_ocean_2),
                 mean(goa_age$R_ocean_3),
                 mean(goa_age$R_ocean_4),
                 mean(goa_age$R_ocean_5))

age_wgt_nbc <- c(mean(nbc_age$R_ocean_1),
                 mean(nbc_age$R_ocean_2),
                 mean(nbc_age$R_ocean_3),
                 mean(nbc_age$R_ocean_4),
                 mean(nbc_age$R_ocean_5))



## Add SST
catch$annual_sst   <- NA
catch$annual_sst_2 <- NA
catch$annual_sst_3 <- NA
catch$winter_sst   <- NA
catch$winter_sst_2 <- NA
catch$winter_sst_3 <- NA
for(i in 1:nrow(catch)) {
    rg <- catch$region[i]
    sp <- catch$species[i]
    yr <- catch$year[i]
    if(rg == "GOA") {
        sst_dat <- goa_sst
        age_wgt_i <- age_wgt_goa
    }
    if(rg == "NBC") {
        sst_dat <- bc_sst
        age_wgt_i <- age_wgt_nbc
    }
    if(sp == "Chum") {
        sst_i <- sst_dat[sst_dat$year == yr - 3, ]
        annual_sst   <- sst_i$annual.anomaly.unsmoothed
        annual_sst_2 <- sst_i$annual.anomaly.two.yr.running.mean
        annual_sst_3 <- sst_i$annual.anomaly.three.yr.running.mean
        winter_sst   <- sst_i$winter.anomaly.unsmoothed
        winter_sst_2 <- sst_i$winter.anomaly.two.yr.running.mean
        winter_sst_3 <- sst_i$winter.anomaly.three.yr.running.mean
    }
    if(sp %in% c("Coho", "Pink", "Pink even", "Pink odd")) {
        sst_i <- sst_dat[sst_dat$year == yr - 1, ]
        annual_sst   <- sst_i$annual.anomaly.unsmoothed
        annual_sst_2 <- sst_i$annual.anomaly.two.yr.running.mean
        annual_sst_3 <- sst_i$annual.anomaly.three.yr.running.mean
        winter_sst   <- sst_i$winter.anomaly.unsmoothed
        winter_sst_2 <- sst_i$winter.anomaly.two.yr.running.mean
        winter_sst_3 <- sst_i$winter.anomaly.three.yr.running.mean
    }
    if(sp == "Sockeye") {
        sst_i <- sst_dat[sst_dat$year %in% (yr - 1):(yr - 5), ]
        annual_sst   <- weighted.mean(sst_i$annual.anomaly.unsmoothed, w = rev(age_wgt_i))
        annual_sst_2 <- weighted.mean(sst_i$annual.anomaly.two.yr.running.mean, w = rev(age_wgt_i))
        annual_sst_3 <- weighted.mean(sst_i$annual.anomaly.three.yr.running.mean, w = rev(age_wgt_i))
        winter_sst   <- weighted.mean(sst_i$winter.anomaly.unsmoothed, w = rev(age_wgt_i))
        winter_sst_2 <- weighted.mean(sst_i$winter.anomaly.two.yr.running.mean, w = rev(age_wgt_i))
        winter_sst_3 <- weighted.mean(sst_i$winter.anomaly.three.yr.running.mean, w = rev(age_wgt_i))
    }
    catch$annual_sst[i]   <- annual_sst
    catch$annual_sst_2[i] <- annual_sst_2
    catch$annual_sst_3[i] <- annual_sst_3
    catch$winter_sst[i]   <- winter_sst
    catch$winter_sst_2[i] <- winter_sst_2
    catch$winter_sst_3[i] <- winter_sst_3
}


## Create different time periods ---------------------------
catch_1965 <- catch
catch_1965 <- plyr::ddply(catch_1965, .(region, species), transform,
                          log_catch = log(catch),
                          log_catch_stnd = scale(log(catch)),
                          catch_stnd = scale(catch))


catch <- catch[catch$year >= 1989, ]
catch <- plyr::ddply(catch, .(region, species), transform,
                     log_catch = log(catch),
                     log_catch_stnd = scale(log(catch)),
                     catch_stnd = scale(catch))


write.csv(catch, file = "./data/catch.csv", row.names = FALSE)
write.csv(catch, file = "./data/catch_1965.csv", row.names = FALSE)


## Autocorrelation
acf(catch$log_catch_stnd[catch$region == "GOA" & catch$species == "Chum"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "GOA" & catch$species == "Sockeye"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "GOA" & catch$species == "Coho"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "GOA" & catch$species == "Pink even"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "GOA" & catch$species == "Pink odd"], plot = FALSE)[1]

acf(catch$log_catch_stnd[catch$region == "NBC" & catch$species == "Chum"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "NBC" & catch$species == "Sockeye"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "NBC" & catch$species == "Coho"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "NBC" & catch$species == "Pink even"], plot = FALSE)[1]
acf(catch$log_catch_stnd[catch$region == "NBC" & catch$species == "Pink odd"], plot = FALSE)[1]





## Plot data -----------------------------------------------
g <- ggplot(catch_1965) +
    aes(year, log_catch, color = species) +
    geom_line() +
    facet_wrap( ~ region, ncol = 1, scale = "free_y") +
    theme_bw()
print(g)
ggsave("./figures/sst_catch/catch.png", width = 8, height = 6)


g <- ggplot(catch_1965[catch_1965$region == "NBC", ]) +
    aes(x = winter_sst, y = log_catch_stnd, color = era) +
    geom_point() +
    facet_wrap( ~ species) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("Northern BC") +
    theme_bw()
print(g)
ggsave("./figures/sst_catch/nbc_catch_sst_era.png", width = 8, height = 6)


g <- ggplot(catch_1965[catch_1965$region == "GOA", ]) +
    aes(x = winter_sst, y = log_catch_stnd, color = era) +
    geom_point() +
    facet_wrap( ~ species) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggtitle("GOA") +
    theme_bw()
print(g)
ggsave("./figures/sst_catch/goa_catch_sst_era.png", width = 8, height = 6)
