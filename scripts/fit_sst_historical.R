## Fit historical sst models
## Fit separate models for region,species,covar combinations

source("./scripts/load.R")

dir.create("./figures/fit_sst_historical", showWarnings = FALSE)

catch <- read.csv("./data/catch.csv")

covars <- c("winter_sst", "winter_sst_2", "winter_sst_3",
            "annual_sst", "annual_sst_2", "annual_sst_3")


## Prep data -----------------------------------------------
dat_goa_chum      <- catch[catch$region == "GOA" & catch$species == "Chum" & catch$year > 1988, ]
dat_goa_sockeye   <- catch[catch$region == "GOA" & catch$species == "Sockeye" & catch$year > 1988, ]
dat_goa_coho      <- catch[catch$region == "GOA" & catch$species == "Coho" & catch$year > 1988, ]
dat_goa_pink_even <- catch[catch$region == "GOA" & catch$species == "Pink even" & catch$year > 1988, ]
dat_goa_pink_odd  <- catch[catch$region == "GOA" & catch$species == "Pink odd" & catch$year > 1988, ]

dat_nbc_chum      <- catch[catch$region == "NBC" & catch$species == "Chum" & catch$year > 1988, ]
dat_nbc_sockeye   <- catch[catch$region == "NBC" & catch$species == "Sockeye" & catch$year > 1988, ]
dat_nbc_coho      <- catch[catch$region == "NBC" & catch$species == "Coho" & catch$year > 1988, ]
dat_nbc_pink_even <- catch[catch$region == "NBC" & catch$species == "Pink even" & catch$year > 1988, ]
dat_nbc_pink_odd  <- catch[catch$region == "NBC" & catch$species == "Pink odd" & catch$year > 1988, ]

dat_fraser_sockeye   <- catch[catch$region == "FrBC" & catch$species == "Sockeye" & catch$year > 1988, ]


## lm models -----------------------------------------------

## GOA Chum
lm_sst_goa_chum <- fit_lm_list(dat_goa_chum, covars)
save(lm_sst_goa_chum, file = "./outputs/lm_sst_goa_chum.RData")

## GOA Sockeye
lm_sst_goa_sockeye <- fit_lm_list(dat_goa_sockeye, covars)
save(lm_sst_goa_sockeye, file = "./outputs/lm_sst_goa_sockeye.RData")

## GOA coho
lm_sst_goa_coho <- fit_lm_list(dat_goa_coho, covars)
save(lm_sst_goa_coho, file = "./outputs/lm_sst_goa_coho.RData")

## GOA Pink Even
lm_sst_goa_pink_even <- fit_lm_list(dat_goa_pink_even, covars)
save(lm_sst_goa_pink_even, file = "./outputs/lm_sst_goa_pink_even.RData")

## GOA Pink Odd
lm_sst_goa_pink_odd <- fit_lm_list(dat_goa_pink_odd, covars)
save(lm_sst_goa_pink_odd, file = "./outputs/lm_sst_goa_pink_odd.RData")


## NBC Chum
lm_sst_nbc_chum <- fit_lm_list(dat_nbc_chum, covars)
save(lm_sst_nbc_chum, file = "./outputs/lm_sst_nbc_chum.RData")

## NBC Sockeye
lm_sst_nbc_sockeye <- fit_lm_list(dat_nbc_sockeye, covars)
save(lm_sst_nbc_sockeye, file = "./outputs/lm_sst_nbc_sockeye.RData")

## NBC Coho
lm_sst_nbc_coho <- fit_lm_list(dat_nbc_coho, covars)
save(lm_sst_nbc_coho, file = "./outputs/lm_sst_nbc_coho.RData")

## NBC Pink Even
lm_sst_nbc_pink_even <- fit_lm_list(dat_nbc_pink_even, covars)
save(lm_sst_nbc_pink_even, file = "./outputs/lm_sst_nbc_pink_even.RData")

## NBC Pink Odd
lm_sst_nbc_pink_odd <- fit_lm_list(dat_nbc_pink_odd, covars)
save(lm_sst_nbc_pink_odd, file = "./outputs/lm_sst_nbc_pink_odd.RData")

## Fraser Sockeye
lm_sst_fraser_sockeye <- fit_lm_list(dat_fraser_sockeye, covars)
save(lm_sst_fraser_sockeye, file = "./outputs/lm_sst_fraser_sockeye.RData")


## Load models ##
load("./outputs/lm_sst_goa_chum.RData")
load("./outputs/lm_sst_goa_sockeye.RData")
load("./outputs/lm_sst_goa_coho.RData")
load("./outputs/lm_sst_goa_pink_even.RData")
load("./outputs/lm_sst_goa_pink_odd.RData")
load("./outputs/lm_sst_nbc_chum.RData")
load("./outputs/lm_sst_nbc_sockeye.RData")
load("./outputs/lm_sst_nbc_coho.RData")
load("./outputs/lm_sst_nbc_pink_even.RData")
load("./outputs/lm_sst_nbc_pink_odd.RData")
load("./outputs/lm_sst_fraser_sockeye.RData")



## Summarize best models ##
ms_lm <- vector("list", 11)
ms_lm[[1]]  <- brms_summarize(lm_sst_goa_chum, "GOA", "Chum")
ms_lm[[2]]  <- brms_summarize(lm_sst_goa_sockeye, "GOA", "Sockeye")
ms_lm[[3]]  <- brms_summarize(lm_sst_goa_coho, "GOA", "Coho")
ms_lm[[4]]  <- brms_summarize(lm_sst_goa_pink_even, "GOA", "Pink even")
ms_lm[[5]]  <- brms_summarize(lm_sst_goa_pink_odd, "GOA", "Pink odd")
ms_lm[[6]]  <- brms_summarize(lm_sst_nbc_chum, "NBC", "Chum")
ms_lm[[7]]  <- brms_summarize(lm_sst_nbc_sockeye, "NBC", "Sockeye")
ms_lm[[8]]  <- brms_summarize(lm_sst_nbc_coho, "NBC", "Coho")
ms_lm[[9]]  <- brms_summarize(lm_sst_nbc_pink_even, "NBC", "Pink even")
ms_lm[[10]] <- brms_summarize(lm_sst_nbc_pink_odd, "NBC", "Pink odd")
ms_lm[[11]] <- brms_summarize(lm_sst_fraser_sockeye, "FrBC", "Sockeye")
best_lm <- lapply(ms_lm, function(m) m[m$dLOOIC == 0, ])
best_lm <- plyr::rbind.fill(best_lm)



## Plot best models ##

## get conditional effects
ce_lm <- vector("list", nrow(best_lm))
ce_lm[[1]]  <- ce_best(lm_sst_goa_chum, 1, "GOA", "Chum")
ce_lm[[2]]  <- ce_best(lm_sst_goa_sockeye, 1, "GOA", "Sockeye")
ce_lm[[3]]  <- ce_best(lm_sst_goa_coho, 1, "GOA", "Coho")
ce_lm[[4]]  <- ce_best(lm_sst_goa_pink_even, 1, "GOA", "Pink even")
ce_lm[[5]]  <- ce_best(lm_sst_goa_pink_odd, 1, "GOA", "Pink odd")
ce_lm[[6]]  <- ce_best(lm_sst_nbc_chum, 1, "NBC", "Chum")
ce_lm[[7]]  <- ce_best(lm_sst_nbc_sockeye, 1, "NBC", "Sockeye")
ce_lm[[8]]  <- ce_best(lm_sst_nbc_coho, 1, "NBC", "Coho")
ce_lm[[9]]  <- ce_best(lm_sst_nbc_pink_even, 1, "NBC", "Pink even")
ce_lm[[10]] <- ce_best(lm_sst_nbc_pink_odd, 1, "NBC", "Pink odd")
ce_lm[[11]] <- ce_best(lm_sst_fraser_sockeye, 1, "FrBC", "Sockeye")
ce_lm <- plyr::rbind.fill(ce_lm)

## get data for best models
dat_best_lm <- vector("list", nrow(best_lm))
for(i in 1:nrow(best_lm)) {
    m <- best_lm[i, ]
    dat_best_lm[[i]] <- catch[catch$region == m$region & catch$species == m$species,
                           c("region", "species", "year", unlist(m$data_vars))]
    names(dat_best_lm[[i]]) <- c("region", "species", "year", "log_catch_stnd", "sst")
}
dat_best_lm <- rbind.fill(dat_best_lm)

g <- ggplot(ce_lm) +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    geom_point(data = dat_best_lm, aes(x = sst, y = log_catch_stnd), color = "grey40") +
    geom_line(aes(x = effect1__, y = estimate__), color = "red3", size = 1) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                fill = "grey70", alpha = 0.40) +
    facet_grid(species ~ region)
print(g)
ggsave("./figures/fit_sst_historical/ce_lm.png", width = 6, height = 7)



## Posterior predictive checks ##
pp_check(lm_sst_goa_chum[[best_lm$index[best_lm$region == "GOA" &
                                          best_lm$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_goa_sockeye[[best_lm$index[best_lm$region == "GOA" &
                              best_lm$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_goa_coho[[best_lm$index[best_lm$region == "GOA" &
                              best_lm$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_goa_pink_even[[best_lm$index[best_lm$region == "GOA" &
                              best_lm$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_goa_pink_odd[[best_lm$index[best_lm$region == "GOA" &
                              best_lm$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)

pp_check(lm_sst_nbc_chum[[best_lm$index[best_lm$region == "NBC" &
                                          best_lm$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_nbc_sockeye[[best_lm$index[best_lm$region == "NBC" &
                              best_lm$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_nbc_coho[[best_lm$index[best_lm$region == "NBC" &
                              best_lm$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_nbc_pink_even[[best_lm$index[best_lm$region == "NBC" &
                              best_lm$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_nbc_pink_odd[[best_lm$index[best_lm$region == "NBC" &
                              best_lm$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_sst_fraser_sockeye[[best_lm$index[best_lm$region == "FrBC" &
                                                best_lm$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)


## lm + ar1 models -----------------------------------------

## GOA Chum
lm_ar1_sst_goa_chum <- fit_lm_list(dat_goa_chum, covars, ar1 = TRUE)
save(lm_ar1_sst_goa_chum, file = "./outputs/lm_ar1_sst_goa_chum.RData")

## GOA Sockeye
lm_ar1_sst_goa_sockeye <- fit_lm_list(dat_goa_sockeye, covars, ar1 = TRUE)
save(lm_ar1_sst_goa_sockeye, file = "./outputs/lm_ar1_sst_goa_sockeye.RData")

## GOA coho
lm_ar1_sst_goa_coho <- fit_lm_list(dat_goa_coho, covars, ar1 = TRUE)
save(lm_ar1_sst_goa_coho, file = "./outputs/lm_ar1_sst_goa_coho.RData")

## GOA Pink Even
lm_ar1_sst_goa_pink_even <- fit_lm_list(dat_goa_pink_even, covars, ar1 = TRUE)
save(lm_ar1_sst_goa_pink_even, file = "./outputs/lm_ar1_sst_goa_pink_even.RData")

## GOA Pink Odd
lm_ar1_sst_goa_pink_odd <- fit_lm_list(dat_goa_pink_odd, covars, ar1 = TRUE)
save(lm_ar1_sst_goa_pink_odd, file = "./outputs/lm_ar1_sst_goa_pink_odd.RData")


## NBC Chum
lm_ar1_sst_nbc_chum <- fit_lm_list(dat_nbc_chum, covars, ar1 = TRUE)
save(lm_ar1_sst_nbc_chum, file = "./outputs/lm_ar1_sst_nbc_chum.RData")

## NBC Sockeye
lm_ar1_sst_nbc_sockeye <- fit_lm_list(dat_nbc_sockeye, covars, ar1 = TRUE)
save(lm_ar1_sst_nbc_sockeye, file = "./outputs/lm_ar1_sst_nbc_sockeye.RData")

## NBC Coho
lm_ar1_sst_nbc_coho <- fit_lm_list(dat_nbc_coho, covars, ar1 = TRUE)
save(lm_ar1_sst_nbc_coho, file = "./outputs/lm_ar1_sst_nbc_coho.RData")

## NBC Pink Even
lm_ar1_sst_nbc_pink_even <- fit_lm_list(dat_nbc_pink_even, covars, ar1 = TRUE)
save(lm_ar1_sst_nbc_pink_even, file = "./outputs/lm_ar1_sst_nbc_pink_even.RData")

## NBC Pink Odd
lm_ar1_sst_nbc_pink_odd <- fit_lm_list(dat_nbc_pink_odd, covars, ar1 = TRUE)
save(lm_ar1_sst_nbc_pink_odd, file = "./outputs/lm_ar1_sst_nbc_pink_odd.RData")

## Fraser Sockeye
lm_ar1_sst_fraser_sockeye <- fit_lm_list(dat_fraser_sockeye, covars, ar1 = TRUE)
save(lm_ar1_sst_fraser_sockeye, file = "./outputs/lm_ar1_sst_fraser_sockeye.RData")


## Load models ##
load("./outputs/lm_ar1_sst_goa_chum.RData")
load("./outputs/lm_ar1_sst_goa_sockeye.RData")
load("./outputs/lm_ar1_sst_goa_coho.RData")
load("./outputs/lm_ar1_sst_goa_pink_even.RData")
load("./outputs/lm_ar1_sst_goa_pink_odd.RData")
load("./outputs/lm_ar1_sst_nbc_chum.RData")
load("./outputs/lm_ar1_sst_nbc_sockeye.RData")
load("./outputs/lm_ar1_sst_nbc_coho.RData")
load("./outputs/lm_ar1_sst_nbc_pink_even.RData")
load("./outputs/lm_ar1_sst_nbc_pink_odd.RData")
load("./outputs/lm_ar1_sst_fraser_sockeye.RData")


## Summarize best models ##
ms_lm_ar1 <- vector("list", 10)
ms_lm_ar1[[1]]  <- brms_summarize(lm_ar1_sst_goa_chum, "GOA", "Chum")
ms_lm_ar1[[2]]  <- brms_summarize(lm_ar1_sst_goa_sockeye, "GOA", "Sockeye")
ms_lm_ar1[[3]]  <- brms_summarize(lm_ar1_sst_goa_coho, "GOA", "Coho")
ms_lm_ar1[[4]]  <- brms_summarize(lm_ar1_sst_goa_pink_even, "GOA", "Pink even")
ms_lm_ar1[[5]]  <- brms_summarize(lm_ar1_sst_goa_pink_odd, "GOA", "Pink odd")
ms_lm_ar1[[6]]  <- brms_summarize(lm_ar1_sst_nbc_chum, "NBC", "Chum")
ms_lm_ar1[[7]]  <- brms_summarize(lm_ar1_sst_nbc_sockeye, "NBC", "Sockeye")
ms_lm_ar1[[8]]  <- brms_summarize(lm_ar1_sst_nbc_coho, "NBC", "Coho")
ms_lm_ar1[[9]]  <- brms_summarize(lm_ar1_sst_nbc_pink_even, "NBC", "Pink even")
ms_lm_ar1[[10]] <- brms_summarize(lm_ar1_sst_nbc_pink_odd, "NBC", "Pink odd")
ms_lm_ar1[[11]] <- brms_summarize(lm_ar1_sst_fraser_sockeye, "FrBC", "Sockeye")
best_lm_ar1 <- lapply(ms_lm_ar1, function(m) m[m$dLOOIC == 0, ])
best_lm_ar1 <- plyr::rbind.fill(best_lm_ar1)
# pairs(lm_ar1_sst_goa_chum[[1]])


## Plot best models ##

## get conditional effects
ce_lm_ar1 <- vector("list", nrow(best_lm_ar1))
ce_lm_ar1[[1]]  <- ce_best(lm_ar1_sst_goa_chum, 1, "GOA", "Chum")
ce_lm_ar1[[2]]  <- ce_best(lm_ar1_sst_goa_sockeye, 1, "GOA", "Sockeye")
ce_lm_ar1[[3]]  <- ce_best(lm_ar1_sst_goa_coho, 1, "GOA", "Coho")
ce_lm_ar1[[4]]  <- ce_best(lm_ar1_sst_goa_pink_even, 1, "GOA", "Pink even")
ce_lm_ar1[[5]]  <- ce_best(lm_ar1_sst_goa_pink_odd, 1, "GOA", "Pink odd")
ce_lm_ar1[[6]]  <- ce_best(lm_ar1_sst_nbc_chum, 1, "NBC", "Chum")
ce_lm_ar1[[7]]  <- ce_best(lm_ar1_sst_nbc_sockeye, 1, "NBC", "Sockeye")
ce_lm_ar1[[8]]  <- ce_best(lm_ar1_sst_nbc_coho, 1, "NBC", "Coho")
ce_lm_ar1[[9]]  <- ce_best(lm_ar1_sst_nbc_pink_even, 1, "NBC", "Pink even")
ce_lm_ar1[[10]] <- ce_best(lm_ar1_sst_nbc_pink_odd, 1, "NBC", "Pink odd")
ce_lm_ar1[[11]] <- ce_best(lm_ar1_sst_fraser_sockeye, 1, "FrBC", "Sockeye")
ce_lm_ar1 <- plyr::rbind.fill(ce_lm_ar1)

## get data for best models
dat_best_lm_ar1 <- vector("list", nrow(best_lm_ar1))
for(i in 1:nrow(best_lm_ar1)) {
    m <- best_lm_ar1[i, ]
    dat_best_lm_ar1[[i]] <- catch[catch$region == m$region & catch$species == m$species,
                           c("region", "species", unlist(m$data_vars))]
    names(dat_best_lm_ar1[[i]]) <- c("region", "species", "log_catch_stnd", "sst", "year")
}
dat_best_lm_ar1 <- rbind.fill(dat_best_lm_ar1)

g <- ggplot(ce_lm_ar1) +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    geom_point(data = dat_best_lm_ar1, aes(x = sst, y = log_catch_stnd), color = "grey40") +
    geom_line(aes(x = effect1__, y = estimate__), color = "red3", size = 1) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                fill = "grey70", alpha = 0.40) +
    facet_grid(species ~ region)
print(g)
ggsave("./figures/fit_sst_historical/ce_lm_ar1.png", width = 6, height = 7)



## Posterior predictive checks ##
pp_check(lm_ar1_sst_goa_chum[[best_lm_ar1$index[best_lm_ar1$region == "GOA" &
                                          best_lm_ar1$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_goa_sockeye[[best_lm_ar1$index[best_lm_ar1$region == "GOA" &
                              best_lm_ar1$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_goa_coho[[best_lm_ar1$index[best_lm_ar1$region == "GOA" &
                              best_lm_ar1$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_goa_pink_even[[best_lm_ar1$index[best_lm_ar1$region == "GOA" &
                              best_lm_ar1$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_goa_pink_odd[[best_lm_ar1$index[best_lm_ar1$region == "GOA" &
                              best_lm_ar1$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)

pp_check(lm_ar1_sst_nbc_chum[[best_lm_ar1$index[best_lm_ar1$region == "NBC" &
                                          best_lm_ar1$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_nbc_sockeye[[best_lm_ar1$index[best_lm_ar1$region == "NBC" &
                              best_lm_ar1$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_nbc_coho[[best_lm_ar1$index[best_lm_ar1$region == "NBC" &
                              best_lm_ar1$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_nbc_pink_even[[best_lm_ar1$index[best_lm_ar1$region == "NBC" &
                              best_lm_ar1$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_nbc_pink_odd[[best_lm_ar1$index[best_lm_ar1$region == "NBC" &
                              best_lm_ar1$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(lm_ar1_sst_fraser_sockeye[[best_lm_ar1$index[best_lm_ar1$region == "FrBC" &
                              best_lm_ar1$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)



## gam models ----------------------------------------------

## GOA Chum
gam_sst_goa_chum <- fit_gam_list(dat_goa_chum, covars)
save(gam_sst_goa_chum, file = "./outputs/gam_sst_goa_chum.RData")

## GOA Sockeye
gam_sst_goa_sockeye <- fit_gam_list(dat_goa_sockeye, covars)
save(gam_sst_goa_sockeye, file = "./outputs/gam_sst_goa_sockeye.RData")

## GOA coho
gam_sst_goa_coho <- fit_gam_list(dat_goa_coho, covars)
save(gam_sst_goa_coho, file = "./outputs/gam_sst_goa_coho.RData")

## GOA Pink Even
gam_sst_goa_pink_even <- fit_gam_list(dat_goa_pink_even, covars)
save(gam_sst_goa_pink_even, file = "./outputs/gam_sst_goa_pink_even.RData")

## GOA Pink Odd
gam_sst_goa_pink_odd <- fit_gam_list(dat_goa_pink_odd, covars)
save(gam_sst_goa_pink_odd, file = "./outputs/gam_sst_goa_pink_odd.RData")


## NBC Chum
gam_sst_nbc_chum <- fit_gam_list(dat_nbc_chum, covars)
save(gam_sst_nbc_chum, file = "./outputs/gam_sst_nbc_chum.RData")

## NBC Sockeye
gam_sst_nbc_sockeye <- fit_gam_list(dat_nbc_sockeye, covars)
save(gam_sst_nbc_sockeye, file = "./outputs/gam_sst_nbc_sockeye.RData")

## NBC Coho
gam_sst_nbc_coho <- fit_gam_list(dat_nbc_coho, covars)
save(gam_sst_nbc_coho, file = "./outputs/gam_sst_nbc_coho.RData")

## NBC Pink Even
gam_sst_nbc_pink_even <- fit_gam_list(dat_nbc_pink_even, covars)
save(gam_sst_nbc_pink_even, file = "./outputs/gam_sst_nbc_pink_even.RData")

## NBC Pink Odd
gam_sst_nbc_pink_odd <- fit_gam_list(dat_nbc_pink_odd, covars)
save(gam_sst_nbc_pink_odd, file = "./outputs/gam_sst_nbc_pink_odd.RData")

## Fraser Sockeye
gam_sst_fraser_sockeye <- fit_gam_list(dat_fraser_sockeye, covars)
save(gam_sst_fraser_sockeye, file = "./outputs/gam_sst_fraser_sockeye.RData")


## Load models ##
load("./outputs/gam_sst_goa_chum.RData")
load("./outputs/gam_sst_goa_sockeye.RData")
load("./outputs/gam_sst_goa_coho.RData")
load("./outputs/gam_sst_goa_pink_even.RData")
load("./outputs/gam_sst_goa_pink_odd.RData")
load("./outputs/gam_sst_nbc_chum.RData")
load("./outputs/gam_sst_nbc_sockeye.RData")
load("./outputs/gam_sst_nbc_coho.RData")
load("./outputs/gam_sst_nbc_pink_even.RData")
load("./outputs/gam_sst_nbc_pink_odd.RData")
load("./outputs/gam_sst_fraser_sockeye.RData")



## Summarize best models ##
ms_gam <- vector("list", 10)
ms_gam[[1]]  <- brms_summarize(gam_sst_goa_chum, "GOA", "Chum")
ms_gam[[2]]  <- brms_summarize(gam_sst_goa_sockeye, "GOA", "Sockeye")
ms_gam[[3]]  <- brms_summarize(gam_sst_goa_coho, "GOA", "Coho")
ms_gam[[4]]  <- brms_summarize(gam_sst_goa_pink_even, "GOA", "Pink even")
ms_gam[[5]]  <- brms_summarize(gam_sst_goa_pink_odd, "GOA", "Pink odd")
ms_gam[[6]]  <- brms_summarize(gam_sst_nbc_chum, "NBC", "Chum")
ms_gam[[7]]  <- brms_summarize(gam_sst_nbc_sockeye, "NBC", "Sockeye")
ms_gam[[8]]  <- brms_summarize(gam_sst_nbc_coho, "NBC", "Coho")
ms_gam[[9]]  <- brms_summarize(gam_sst_nbc_pink_even, "NBC", "Pink even")
ms_gam[[10]] <- brms_summarize(gam_sst_nbc_pink_odd, "NBC", "Pink odd")
ms_gam[[11]] <- brms_summarize(gam_sst_fraser_sockeye, "FrBC", "Sockeye")
best_gam <- lapply(ms_gam, function(m) m[m$dLOOIC == 0, ])
best_gam <- plyr::rbind.fill(best_gam)



## Plot best models ##

## get conditional effects
ce_gam <- vector("list", nrow(best_gam))
ce_gam[[1]]  <- ce_best(gam_sst_goa_chum, 1, "GOA", "Chum")
ce_gam[[2]]  <- ce_best(gam_sst_goa_sockeye, 1, "GOA", "Sockeye")
ce_gam[[3]]  <- ce_best(gam_sst_goa_coho, 1, "GOA", "Coho")
ce_gam[[4]]  <- ce_best(gam_sst_goa_pink_even, 1, "GOA", "Pink even")
ce_gam[[5]]  <- ce_best(gam_sst_goa_pink_odd, 1, "GOA", "Pink odd")
ce_gam[[6]]  <- ce_best(gam_sst_nbc_chum, 1, "NBC", "Chum")
ce_gam[[7]]  <- ce_best(gam_sst_nbc_sockeye, 1, "NBC", "Sockeye")
ce_gam[[8]]  <- ce_best(gam_sst_nbc_coho, 1, "NBC", "Coho")
ce_gam[[9]]  <- ce_best(gam_sst_nbc_pink_even, 1, "NBC", "Pink even")
ce_gam[[10]] <- ce_best(gam_sst_nbc_pink_odd, 1, "NBC", "Pink odd")
ce_gam[[11]] <- ce_best(gam_sst_fraser_sockeye, "FrBC", "Sockeye")
ce_gam <- plyr::rbind.fill(ce_gam)

## get data for best models
dat_best_gam <- vector("list", nrow(best_gam))
for(i in 1:nrow(best_gam)) {
    m <- best_gam[i, ]
    dat_best_gam[[i]] <- catch[catch$region == m$region & catch$species == m$species,
                           c("region", "species", "year", unlist(m$data_vars))]
    names(dat_best_gam[[i]]) <- c("region", "species", "year", "log_catch_stnd", "sst")
}
dat_best_gam <- rbind.fill(dat_best_gam)

g <- ggplot(ce_gam) +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    geom_point(data = dat_best_gam, aes(x = sst, y = log_catch_stnd), color = "grey40") +
    geom_line(aes(x = effect1__, y = estimate__), color = "red3", size = 1) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                fill = "grey70", alpha = 0.40) +
    facet_grid(species ~ region)
print(g)
ggsave("./figures/fit_sst_historical/ce_gam.png", width = 6, height = 7)



## Posterior predictive checks ##
pp_check(gam_sst_goa_chum[[best_gam$index[best_gam$region == "GOA" &
                                          best_gam$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_goa_sockeye[[best_gam$index[best_gam$region == "GOA" &
                              best_gam$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_goa_coho[[best_gam$index[best_gam$region == "GOA" &
                              best_gam$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_goa_pink_even[[best_gam$index[best_gam$region == "GOA" &
                              best_gam$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_goa_pink_odd[[best_gam$index[best_gam$region == "GOA" &
                              best_gam$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)

pp_check(gam_sst_nbc_chum[[best_gam$index[best_gam$region == "NBC" &
                                          best_gam$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_nbc_sockeye[[best_gam$index[best_gam$region == "NBC" &
                              best_gam$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_nbc_coho[[best_gam$index[best_gam$region == "NBC" &
                              best_gam$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_nbc_pink_even[[best_gam$index[best_gam$region == "NBC" &
                              best_gam$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_nbc_pink_odd[[best_gam$index[best_gam$region == "NBC" &
                              best_gam$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_sst_fraser_sockeye[[best_gam$index[best_gam$region == "FrBC" &
                              best_gam$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)


## gam + s(time) models ------------------------------------

## GOA Chum
gam_time_sst_goa_chum <- fit_gam_list(dat_goa_chum, covars, time = "smooth")
save(gam_time_sst_goa_chum, file = "./outputs/gam_time_sst_goa_chum.RData")

## GOA Sockeye
gam_time_sst_goa_sockeye <- fit_gam_list(dat_goa_sockeye, covars, time = "smooth")
save(gam_time_sst_goa_sockeye, file = "./outputs/gam_time_sst_goa_sockeye.RData")

## GOA coho
gam_time_sst_goa_coho <- fit_gam_list(dat_goa_coho, covars, time = "smooth")
save(gam_time_sst_goa_coho, file = "./outputs/gam_time_sst_goa_coho.RData")

## GOA Pink Even
gam_time_sst_goa_pink_even <- fit_gam_list(dat_goa_pink_even, covars, time = "smooth")
save(gam_time_sst_goa_pink_even, file = "./outputs/gam_time_sst_goa_pink_even.RData")

## GOA Pink Odd
gam_time_sst_goa_pink_odd <- fit_gam_list(dat_goa_pink_odd, covars, time = "smooth")
save(gam_time_sst_goa_pink_odd, file = "./outputs/gam_time_sst_goa_pink_odd.RData")


## NBC Chum
gam_time_sst_nbc_chum <- fit_gam_list(dat_nbc_chum, covars, time = "smooth")
save(gam_time_sst_nbc_chum, file = "./outputs/gam_time_sst_nbc_chum.RData")

## NBC Sockeye
gam_time_sst_nbc_sockeye <- fit_gam_list(dat_nbc_sockeye, covars, time = "smooth")
save(gam_time_sst_nbc_sockeye, file = "./outputs/gam_time_sst_nbc_sockeye.RData")

## NBC Coho
gam_time_sst_nbc_coho <- fit_gam_list(dat_nbc_coho, covars, time = "smooth")
save(gam_time_sst_nbc_coho, file = "./outputs/gam_time_sst_nbc_coho.RData")

## NBC Pink Even
gam_time_sst_nbc_pink_even <- fit_gam_list(dat_nbc_pink_even, covars, time = "smooth")
save(gam_time_sst_nbc_pink_even, file = "./outputs/gam_time_sst_nbc_pink_even.RData")

## NBC Pink Odd
gam_time_sst_nbc_pink_odd <- fit_gam_list(dat_nbc_pink_odd, covars, time = "smooth")
save(gam_time_sst_nbc_pink_odd, file = "./outputs/gam_time_sst_nbc_pink_odd.RData")


## Fraser Sockeye
gam_time_sst_fraser_sockeye <- fit_gam_list(dat_fraser_sockeye, covars, time = "smooth")
save(gam_time_sst_fraser_sockeye, file = "./outputs/gam_time_sst_fraser_sockeye.RData")


## Load models ##
load("./outputs/gam_time_sst_goa_chum.RData")
load("./outputs/gam_time_sst_goa_sockeye.RData")
load("./outputs/gam_time_sst_goa_coho.RData")
load("./outputs/gam_time_sst_goa_pink_even.RData")
load("./outputs/gam_time_sst_goa_pink_odd.RData")
load("./outputs/gam_time_sst_nbc_chum.RData")
load("./outputs/gam_time_sst_nbc_sockeye.RData")
load("./outputs/gam_time_sst_nbc_coho.RData")
load("./outputs/gam_time_sst_nbc_pink_even.RData")
load("./outputs/gam_time_sst_nbc_pink_odd.RData")
load("./outputs/gam_time_sst_fraser_sockeye.RData")



## Summarize best models ##
ms_gam_time <- vector("list", 10)
ms_gam_time[[1]]  <- brms_summarize(gam_time_sst_goa_chum, "GOA", "Chum")
ms_gam_time[[2]]  <- brms_summarize(gam_time_sst_goa_sockeye, "GOA", "Sockeye")
ms_gam_time[[3]]  <- brms_summarize(gam_time_sst_goa_coho, "GOA", "Coho")
ms_gam_time[[4]]  <- brms_summarize(gam_time_sst_goa_pink_even, "GOA", "Pink even")
ms_gam_time[[5]]  <- brms_summarize(gam_time_sst_goa_pink_odd, "GOA", "Pink odd")
ms_gam_time[[6]]  <- brms_summarize(gam_time_sst_nbc_chum, "NBC", "Chum")
ms_gam_time[[7]]  <- brms_summarize(gam_time_sst_nbc_sockeye, "NBC", "Sockeye")
ms_gam_time[[8]]  <- brms_summarize(gam_time_sst_nbc_coho, "NBC", "Coho")
ms_gam_time[[9]]  <- brms_summarize(gam_time_sst_nbc_pink_even, "NBC", "Pink even")
ms_gam_time[[10]] <- brms_summarize(gam_time_sst_nbc_pink_odd, "NBC", "Pink odd")
ms_gam_time[[11]] <- brms_summarize(gam_time_sst_fraser_sockeye, "FrBC", "Sockeye")
best_gam_time <- lapply(ms_gam_time, function(m) m[m$dLOOIC == 0, ])
best_gam_time <- plyr::rbind.fill(best_gam_time)



## Plot best models ##

## get conditional effects
ce_gam_time <- vector("list", nrow(best_gam_time))
ce_gam_time[[1]]  <- ce_best(gam_time_sst_goa_chum, 1, "GOA", "Chum")
ce_gam_time[[2]]  <- ce_best(gam_time_sst_goa_sockeye, 1, "GOA", "Sockeye")
ce_gam_time[[3]]  <- ce_best(gam_time_sst_goa_coho, 1, "GOA", "Coho")
ce_gam_time[[4]]  <- ce_best(gam_time_sst_goa_pink_even, 1, "GOA", "Pink even")
ce_gam_time[[5]]  <- ce_best(gam_time_sst_goa_pink_odd, 1, "GOA", "Pink odd")
ce_gam_time[[6]]  <- ce_best(gam_time_sst_nbc_chum, 1, "NBC", "Chum")
ce_gam_time[[7]]  <- ce_best(gam_time_sst_nbc_sockeye, 1, "NBC", "Sockeye")
ce_gam_time[[8]]  <- ce_best(gam_time_sst_nbc_coho, 1, "NBC", "Coho")
ce_gam_time[[9]]  <- ce_best(gam_time_sst_nbc_pink_even, 1, "NBC", "Pink even")
ce_gam_time[[10]] <- ce_best(gam_time_sst_nbc_pink_odd, 1, "NBC", "Pink odd")
ce_gam_time[[11]] <- ce_best(gam_time_sst_fraser_sockeye, 1, "FrBC", "Sockeye")
ce_gam_time <- plyr::rbind.fill(ce_gam_time)

## get data for best models
dat_best_gam_time <- vector("list", nrow(best_gam_time))
for(i in 1:nrow(best_gam_time)) {
    m <- best_gam_time[i, ]
    dat_best_gam_time[[i]] <- catch[catch$region == m$region & catch$species == m$species,
                           c("region", "species", unlist(m$data_vars))]
    names(dat_best_gam_time[[i]]) <- c("region", "species", "log_catch_stnd", "sst", "year")
}
dat_best_gam_time <- rbind.fill(dat_best_gam_time)

g <- ggplot(ce_gam_time) +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    geom_point(data = dat_best_gam_time, aes(x = sst, y = log_catch_stnd), color = "grey40") +
    geom_line(aes(x = effect1__, y = estimate__), color = "red3", size = 1) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                fill = "grey70", alpha = 0.40) +
    facet_grid(species ~ region)
print(g)
ggsave("./figures/fit_sst_historical/ce_gam_time.png", width = 6, height = 7)



## Posterior predictive checks ##
pp_check(gam_time_sst_goa_chum[[best_gam_time$index[best_gam_time$region == "GOA" &
                                best_gam_time$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_goa_sockeye[[best_gam_time$index[best_gam_time$region == "GOA" &
                                   best_gam_time$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_goa_coho[[best_gam_time$index[best_gam_time$region == "GOA" &
                              best_gam_time$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_goa_pink_even[[best_gam_time$index[best_gam_time$region == "GOA" &
                                     best_gam_time$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_goa_pink_odd[[best_gam_time$index[best_gam_time$region == "GOA" &
                                    best_gam_time$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)

pp_check(gam_time_sst_nbc_chum[[best_gam_time$index[best_gam_time$region == "NBC" &
                                best_gam_time$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_nbc_sockeye[[best_gam_time$index[best_gam_time$region == "NBC" &
                                   best_gam_time$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_nbc_coho[[best_gam_time$index[best_gam_time$region == "NBC" &
                                best_gam_time$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_nbc_pink_even[[best_gam_time$index[best_gam_time$region == "NBC" &
                                     best_gam_time$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_nbc_pink_odd[[best_gam_time$index[best_gam_time$region == "NBC" &
                                    best_gam_time$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_time_sst_fraser_sockeye[[best_gam_time$index[best_gam_time$region == "FrBC" &
                                    best_gam_time$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)



## gam + ar1 models ----------------------------------------

## GOA Chum
gam_ar1_sst_goa_chum <- fit_gam_list(dat_goa_chum, covars, time = "ar1")
save(gam_ar1_sst_goa_chum, file = "./outputs/gam_ar1_sst_goa_chum.RData")

## GOA Sockeye
gam_ar1_sst_goa_sockeye <- fit_gam_list(dat_goa_sockeye, covars, time = "ar1")
save(gam_ar1_sst_goa_sockeye, file = "./outputs/gam_ar1_sst_goa_sockeye.RData")

## GOA coho
gam_ar1_sst_goa_coho <- fit_gam_list(dat_goa_coho, covars, time = "ar1")
save(gam_ar1_sst_goa_coho, file = "./outputs/gam_ar1_sst_goa_coho.RData")

## GOA Pink Even
gam_ar1_sst_goa_pink_even <- fit_gam_list(dat_goa_pink_even, covars, time = "ar1")
save(gam_ar1_sst_goa_pink_even, file = "./outputs/gam_ar1_sst_goa_pink_even.RData")

## GOA Pink Odd
gam_ar1_sst_goa_pink_odd <- fit_gam_list(dat_goa_pink_odd, covars, time = "ar1")
save(gam_ar1_sst_goa_pink_odd, file = "./outputs/gam_ar1_sst_goa_pink_odd.RData")


## NBC Chum
gam_ar1_sst_nbc_chum <- fit_gam_list(dat_nbc_chum, covars, time = "ar1")
save(gam_ar1_sst_nbc_chum, file = "./outputs/gam_ar1_sst_nbc_chum.RData")

## NBC Sockeye
gam_ar1_sst_nbc_sockeye <- fit_gam_list(dat_nbc_sockeye, covars, time = "ar1")
save(gam_ar1_sst_nbc_sockeye, file = "./outputs/gam_ar1_sst_nbc_sockeye.RData")

## NBC Coho
gam_ar1_sst_nbc_coho <- fit_gam_list(dat_nbc_coho, covars, time = "ar1")
save(gam_ar1_sst_nbc_coho, file = "./outputs/gam_ar1_sst_nbc_coho.RData")

## NBC Pink Even
gam_ar1_sst_nbc_pink_even <- fit_gam_list(dat_nbc_pink_even, covars, time = "ar1")
save(gam_ar1_sst_nbc_pink_even, file = "./outputs/gam_ar1_sst_nbc_pink_even.RData")

## NBC Pink Odd
gam_ar1_sst_nbc_pink_odd <- fit_gam_list(dat_nbc_pink_odd, covars, time = "ar1")
save(gam_ar1_sst_nbc_pink_odd, file = "./outputs/gam_ar1_sst_nbc_pink_odd.RData")

## Fraser Sockeye
gam_ar1_sst_fraser_sockeye <- fit_gam_list(dat_fraser_sockeye, covars, time = "ar1")
save(gam_ar1_sst_fraser_sockeye, file = "./outputs/gam_ar1_sst_fraser_sockeye.RData")

## Load models ##
load("./outputs/gam_ar1_sst_goa_chum.RData")
load("./outputs/gam_ar1_sst_goa_sockeye.RData")
load("./outputs/gam_ar1_sst_goa_coho.RData")
load("./outputs/gam_ar1_sst_goa_pink_even.RData")
load("./outputs/gam_ar1_sst_goa_pink_odd.RData")
load("./outputs/gam_ar1_sst_nbc_chum.RData")
load("./outputs/gam_ar1_sst_nbc_sockeye.RData")
load("./outputs/gam_ar1_sst_nbc_coho.RData")
load("./outputs/gam_ar1_sst_nbc_pink_even.RData")
load("./outputs/gam_ar1_sst_nbc_pink_odd.RData")
load("./outputs/gam_ar1_sst_fraser_sockeye.RData")



## Summarize best models ##
ms_gam_ar1 <- vector("list", 10)
ms_gam_ar1[[1]]  <- brms_summarize(gam_ar1_sst_goa_chum, "GOA", "Chum")
ms_gam_ar1[[2]]  <- brms_summarize(gam_ar1_sst_goa_sockeye, "GOA", "Sockeye")
ms_gam_ar1[[3]]  <- brms_summarize(gam_ar1_sst_goa_coho, "GOA", "Coho")
ms_gam_ar1[[4]]  <- brms_summarize(gam_ar1_sst_goa_pink_even, "GOA", "Pink even")
ms_gam_ar1[[5]]  <- brms_summarize(gam_ar1_sst_goa_pink_odd, "GOA", "Pink odd")
ms_gam_ar1[[6]]  <- brms_summarize(gam_ar1_sst_nbc_chum, "NBC", "Chum")
ms_gam_ar1[[7]]  <- brms_summarize(gam_ar1_sst_nbc_sockeye, "NBC", "Sockeye")
ms_gam_ar1[[8]]  <- brms_summarize(gam_ar1_sst_nbc_coho, "NBC", "Coho")
ms_gam_ar1[[9]]  <- brms_summarize(gam_ar1_sst_nbc_pink_even, "NBC", "Pink even")
ms_gam_ar1[[10]] <- brms_summarize(gam_ar1_sst_nbc_pink_odd, "NBC", "Pink odd")
ms_gam_ar1[[11]] <- brms_summarize(gam_ar1_sst_fraser_sockeye, "FrBC", "Sockeye")
best_gam_ar1 <- lapply(ms_gam_ar1, function(m) m[m$dLOOIC == 0, ])
best_gam_ar1 <- plyr::rbind.fill(best_gam_ar1)



## Plot best models ##

## get conditional effects
ce_gam_ar1 <- vector("list", nrow(best_gam_ar1))
ce_gam_ar1[[1]]  <- ce_best(gam_ar1_sst_goa_chum, 1, "GOA", "Chum")
ce_gam_ar1[[2]]  <- ce_best(gam_ar1_sst_goa_sockeye, 1, "GOA", "Sockeye")
ce_gam_ar1[[3]]  <- ce_best(gam_ar1_sst_goa_coho, 1, "GOA", "Coho")
ce_gam_ar1[[4]]  <- ce_best(gam_ar1_sst_goa_pink_even, 1, "GOA", "Pink even")
ce_gam_ar1[[5]]  <- ce_best(gam_ar1_sst_goa_pink_odd, 1, "GOA", "Pink odd")
ce_gam_ar1[[6]]  <- ce_best(gam_ar1_sst_nbc_chum, 1, "NBC", "Chum")
ce_gam_ar1[[7]]  <- ce_best(gam_ar1_sst_nbc_sockeye, 1, "NBC", "Sockeye")
ce_gam_ar1[[8]]  <- ce_best(gam_ar1_sst_nbc_coho, 1, "NBC", "Coho")
ce_gam_ar1[[9]]  <- ce_best(gam_ar1_sst_nbc_pink_even, 1, "NBC", "Pink even")
ce_gam_ar1[[10]] <- ce_best(gam_ar1_sst_nbc_pink_odd, 1, "NBC", "Pink odd")
ce_gam_ar1[[11]] <- ce_best(gam_ar1_sst_fraser_sockeye, 1, "FrBC", "Sockeye")
ce_gam_ar1 <- plyr::rbind.fill(ce_gam_ar1)

## get data for best models
dat_best_gam_ar1 <- vector("list", nrow(best_gam_ar1))
for(i in 1:nrow(best_gam_ar1)) {
    m <- best_gam_ar1[i, ]
    dat_best_gam_ar1[[i]] <- catch[catch$region == m$region & catch$species == m$species,
                           c("region", "species", unlist(m$data_vars))]
    names(dat_best_gam_ar1[[i]]) <- c("region", "species", "log_catch_stnd", "sst", "year")
}
dat_best_gam_ar1 <- rbind.fill(dat_best_gam_ar1)

g <- ggplot(ce_gam_ar1) +
    geom_hline(yintercept = 0, color = "grey50", linetype = 2) +
    geom_point(data = dat_best_gam_ar1, aes(x = sst, y = log_catch_stnd), color = "grey40") +
    geom_line(aes(x = effect1__, y = estimate__), color = "red3", size = 1) +
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                fill = "grey70", alpha = 0.40) +
    facet_grid(species ~ region)
print(g)
ggsave("./figures/fit_sst_historical/ce_gam_ar1.png", width = 6, height = 7)



## Posterior predictive checks ##
pp_check(gam_ar1_sst_goa_chum[[best_gam_ar1$index[best_gam_ar1$region == "GOA" &
                                          best_gam_ar1$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_goa_sockeye[[best_gam_ar1$index[best_gam_ar1$region == "GOA" &
                              best_gam_ar1$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_goa_coho[[best_gam_ar1$index[best_gam_ar1$region == "GOA" &
                              best_gam_ar1$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_goa_pink_even[[best_gam_ar1$index[best_gam_ar1$region == "GOA" &
                              best_gam_ar1$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_goa_pink_odd[[best_gam_ar1$index[best_gam_ar1$region == "GOA" &
                              best_gam_ar1$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)

pp_check(gam_ar1_sst_nbc_chum[[best_gam_ar1$index[best_gam_ar1$region == "NBC" &
                                          best_gam_ar1$species == "Chum"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_nbc_sockeye[[best_gam_ar1$index[best_gam_ar1$region == "NBC" &
                              best_gam_ar1$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_nbc_coho[[best_gam_ar1$index[best_gam_ar1$region == "NBC" &
                              best_gam_ar1$species == "Coho"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_nbc_pink_even[[best_gam_ar1$index[best_gam_ar1$region == "NBC" &
                              best_gam_ar1$species == "Pink even"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_nbc_pink_odd[[best_gam_ar1$index[best_gam_ar1$region == "NBC" &
                              best_gam_ar1$species == "Pink odd"]]],
         type = "dens_overlay", ndraws = 50)
pp_check(gam_ar1_sst_fraser_sockeye[[best_gam_ar1$index[best_gam_ar1$region == "FrBC" &
                              best_gam_ar1$species == "Sockeye"]]],
         type = "dens_overlay", ndraws = 50)
