## Fit FAR example - GOA sockeye

source("./scripts/load.R")

dir.create("./figures/fit_far_historical", showWarnings = FALSE)

catch <- read.csv("./data/GOA_sockeye_catch_far.csv")

covars <- c("annual_far_3")


## Prep data -----------------------------------------------

dat_goa_sockeye   <- catch[catch$region == "GOA" & catch$species == "Sockeye" & catch$year > 1988, ]


## gam + ar1 models ----------------------------------------


## GOA Sockeye
gam_ar1_far_goa_sockeye <- fit_gam_list(dat_goa_sockeye, covars, time = "ar1")
save(gam_ar1_sst_goa_sockeye, file = "./outputs/gam_ar1_far_goa_sockeye.RData")

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

