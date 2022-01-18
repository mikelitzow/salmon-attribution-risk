## Historical catch-sst analysis

library(plyr)
library(reshape2)
library(ggplot2)
library(brms)
dir.create("./outputs", showWarnings = FALSE)
dir.create("./figures", showWarnings = FALSE)
cols <- as.vector(palette.colors(palette = "Okabe-Ito"))


## Combine catch and sst -----------------------------------

## Read in data
goa_catch <- read.csv("./data/goa.catch.csv")
goa_sst   <- read.csv("./data/goa.sst.csv")
goa_age   <- read.csv("./data/goa_age.csv")
bc_age    <- read.csv("./data/bc_age.csv")

## Subset years
goa_catch <- goa_catch[goa_catch$year >= 1950, ]

## Get mean age props for sockeye
o1 <- mean(goa_age$R_ocean_1)
o2 <- mean(goa_age$R_ocean_2)
o3 <- mean(goa_age$R_ocean_3)
o4 <- mean(goa_age$R_ocean_4)
o5 <- mean(goa_age$R_ocean_5)
age_wgt <- c(o1, o2, o3, o4, o5)

## Sum across catch regions
sock <- plyr::ddply(goa_catch, .(year), summarize, catch = sum(sockeye))
coho <- plyr::ddply(goa_catch, .(year), summarize, catch = sum(coho))
pink <- plyr::ddply(goa_catch, .(year), summarize, catch = sum(pink))
chum <- plyr::ddply(goa_catch, .(year), summarize, catch = sum(chum))
pink_even <- pink[pink$year %% 2 == 0, ]
pink_odd  <- pink[pink$year %% 2 != 0, ]

## Add species
sock$species <- "Sockeye"
coho$species <- "Coho"
chum$species <- "Chum"
pink$species <- "Pink"
pink_even$species <- "Pink even"
pink_odd$species <- "Pink odd"

## Combine in long format
catch <- rbind(sock, coho, chum, pink_even, pink_odd)

## Add era variable
catch$era <- ifelse(catch$year <= 1988, "1977-1988", "1989-2013")
catch$era <- ifelse(catch$year >= 2014, "2014-2021", catch$era)
catch$era <- ifelse(catch$year <= 1976, "1950-1976", catch$era)

## Scale catch
catch <- plyr::ddply(catch, .(species), transform,
                     log_catch = log(catch),
                     log_catch_stnd = scale(log(catch)),
                     catch_stnd = scale(catch))

## Add SST
catch$annual_sst   <- NA
catch$annual_sst_2 <- NA
catch$annual_sst_3 <- NA
catch$winter_sst   <- NA
catch$winter_sst_2 <- NA
catch$winter_sst_3 <- NA
for(i in 1:nrow(catch)) {
    yr <- catch$year[i]
    sp <- catch$species[i]
    if(sp == "Chum") {
        sst_i <- goa_sst[goa_sst$year == yr - 3, ]
        annual_sst   <- sst_i$annual.sst
        annual_sst_2 <- sst_i$annual.sst.2
        annual_sst_3 <- sst_i$annual.sst.2
        winter_sst   <- sst_i$winter.sst
        winter_sst_2 <- sst_i$winter.sst.2
        winter_sst_3 <- sst_i$winter.sst.2
    }
    if(sp %in% c("Coho", "Pink", "Pink even", "Pink odd")) {
        sst_i <- goa_sst[goa_sst$year == yr - 1, ]
        annual_sst   <- sst_i$annual.sst
        annual_sst_2 <- sst_i$annual.sst.2
        annual_sst_3 <- sst_i$annual.sst.2
        winter_sst   <- sst_i$winter.sst
        winter_sst_2 <- sst_i$winter.sst.2
        winter_sst_3 <- sst_i$winter.sst.2
    }
    if(sp == "Sockeye") {
        sst_i <- goa_sst[goa_sst$year %in% (yr - 1):(yr - 5), ]
        annual_sst   <- weighted.mean(sst_i$annual.sst, w = rev(age_wgt))
        annual_sst_2 <- weighted.mean(sst_i$annual.sst.2, w = rev(age_wgt))
        annual_sst_3 <- weighted.mean(sst_i$annual.sst.3, w = rev(age_wgt))
        winter_sst   <- weighted.mean(sst_i$winter.sst, w = rev(age_wgt))
        winter_sst_2 <- weighted.mean(sst_i$winter.sst.2, w = rev(age_wgt))
        winter_sst_3 <- weighted.mean(sst_i$winter.sst.3, w = rev(age_wgt))
    }
    catch$annual_sst[i]   <- annual_sst
    catch$annual_sst_2[i] <- annual_sst_2
    catch$annual_sst_3[i] <- annual_sst_3
    catch$winter_sst[i]   <- winter_sst
    catch$winter_sst_2[i] <- winter_sst_2
    catch$winter_sst_3[i] <- winter_sst_3
}

## Save catch data
write.csv(catch, file = "./data/catch.csv", row.names = FALSE)



## Plot data -----------------------------------------------
g <- ggplot(catch) +
    aes(x = year, y = log_catch_stnd, color = era) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    facet_wrap( ~ species, scale = "free_y") +
    theme_bw()
print(g)

g <- ggplot(catch) +
    aes(x = winter_sst, y = log_catch_stnd, color = era) +
    geom_point() +
    facet_wrap( ~ species) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    theme_bw()
print(g)



## Fit models ----------------------------------------------
## Independent intercepts + slopes + variances for each species, but shared autocorrelation

priors <- c(set_prior("student_t(3, 0, 5)", class = "b"),
            set_prior("student_t(3, 0, 5)", class = "b", dp = "sigma"),
            set_prior("normal(0, 2)", class = "ar"))


## GOA Winter SST
form_winter <- bf(log_catch_stnd ~ 0 + species + species:winter_sst +
                  ar(time = year, gr = species, p = 1), sigma ~ 0 + species)
fit_goa_winter <- brm(form_winter,
                      data = catch[catch$year > 1988, ],
                      cores = 4, chains = 4,
                      prior = priors,
                      save_pars = save_pars(all = TRUE),
                      seed = 123, iter = 4000)
fit_goa_winter <- add_criterion(fit_goa_winter, c("loo", "bayes_R2"))
save(fit_goa_winter, file = "./outputs/fit_goa_winter.RData")

load("./outputs/fit_goa_winter.RData")
summary(fit_goa_winter)
ce <- conditional_effects(fit_goa_winter)
plot(ce, ask = FALSE)
mean(fit_goa_winter$criteria$bayes_R2[ , 1])

## Posterior predictive checks
pp_check(fit_goa_winter, type = "dens_overlay", nsamples = 50)
pp_check(fit_goa_winter, type = "scatter_avg", nsamples = 50)

## Density plot of slopes
samp <- posterior_samples(fit_goa_winter, pars = "winter_sst")
df <- data.frame(Coho = samp[["b_speciesCoho:winter_sst"]],
                 Chum = samp[["b_speciesChum:winter_sst"]],
                 Pink_even = samp[["b_speciesPinkeven:winter_sst"]],
                 Pink_odd = samp[["b_speciesPinkodd:winter_sst"]],
                 Sockeye = samp[["b_speciesSockeye:winter_sst"]])
dfm <- melt(df, id.var = NULL, variable.name = "species", value.name = "slope")

g <- ggplot(dfm) +
    geom_density(aes(x = slope, color = species)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    scale_color_manual(values = cols) +
    labs(x = "SST slope", y = "Density", color = "Species", title = "GOA Winter SST") +
    theme_bw()
print(g)
ggsave("./figures/slopes_goa_winter_sst.jpg")
