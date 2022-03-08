## Functions for project




## ce_best -------------------------------------------------
ce_best <- function(fits, var = 1, region = NULL, species = NULL) {

    s <- brms_summarize(fits, order_looic = FALSE)
    ind <- which.min(s$LOOIC)

    out <- conditional_effects(fits[[ind]])[[var]]
    if(!is.null(species)) out <- data.frame(species = species, out)
    if(!is.null(region)) out <- data.frame(region = region, out)
    return(out)
}



## brms_summarize ------------------------------------------
brms_summarize <- function(fits, region = NULL, species = NULL,
                           order_looic = TRUE, data_vars = TRUE) {
    ## Summarize a list of brms model fits
    ##
    ## fit = list of 'brmsfit' objects

    m_names <- names(fits)
    if(is.null(m_names)) m_names <- paste0("m", 1:length(fits))

    lst <- lapply(fits, function(m) .brms_summarize(m, data_vars = data_vars))
    s <- plyr::rbind.fill(lst)

    s <- data.frame(model = m_names, s)

    s$dLOOIC <- s$LOOIC - min(s$LOOIC)

    s$index   <- which.min(s$LOOIC)
    s$region  <- region
    s$species <- species

    if(order_looic) s <- s[order(s$dLOOIC), ]

    return(s)
}

.brms_summarize <- function(fit, data_vars = TRUE) {
    ## Summarize a brms model fit
    ##
    ## fit = a 'brmsfit' object
    f <- fit

    if(!is(f, "brmsfit"))
        stop("Input not a 'brmsfit' object")

    diver <- get_num_divergent(f$fit)
    neff  <- as.vector(neff_lowest(f$fit)[1])
    draws <- total_draws(f$fit)
    rhat  <- as.vector(rhat_highest(f$fit)[1])

    resids <- resid(f)
    acf1   <- acf(resids[ , "Estimate"], plot = FALSE)$acf[ , , 1][2]

    r2 <- NULL
    if(length(f$criteria$bayes_R2) > 0) {
        r2 <- mean(f$criteria$bayes_R2[ , 1])
    }

    looic <- f$criteria$loo$estimates["looic", "Estimate"]


    out <- data.frame(n_divergences = diver,
                      neff_min = neff,
                      draws = draws,
                      rhat_max = rhat,
                      acf1 = acf1)
    out$R2 <- r2
    out$LOOIC <- looic

    if(data_vars) {
        data_vars <- names(f$data)
        out$data_vars <- list(data_vars)
    }

    return(out)
}



## fit_gam_list --------------------------------------------
fit_gam_list <- function(data, covars, moment_match = TRUE, time = FALSE) {
    ## Fit a series of brms gam models
    ##
    ## Returns a list of brms fit objects, one model for each covariate in the
    ## 'covars' vector.
    ##
    ## data = data.frame
    ## covars = vector of column names in 'data' to include as covars
    ## moment_match = should the LOO use moment matching for high Pareto-k points
    ## time = should an s(time) component be added to the model
    fits <- vector("list", length(covars))
    names(fits) <- covars

    priors <- c(set_prior("student_t(3, 0, 3)", class = "Intercept"),
                set_prior("student_t(3, 0, 3)", class = "b"),
                set_prior("student_t(3, 0, 3)", class = "sds"),
                set_prior("student_t(3, 0, 3)", class = "sigma"))

    for(i in seq_along(covars)) {

        if(!time) {
            form_i <- bf(paste0("log_catch_stnd ~ 1 + s(", covars[i], ", k = 5)"))
        }

        if(time) {
            form_i <- bf(paste0("log_catch_stnd ~ 1 + s(", covars[i], ", k = 5) + ",
                                "s(year, k = 5)"))
        }

        fit_i <- brm(form_i,
                     data = data,
                     cores = 4, chains = 4,
                     prior = priors,
                     save_pars = save_pars(all = TRUE),
                     control = list(adapt_delta = 0.999),
                     seed = 123, iter = 4000)
        fit_i <- add_criterion(fit_i, c("loo", "bayes_R2"),
                               moment_match = moment_match)

        fits[[i]] <- fit_i
    }
    return(fits)
}



## fit_lm_list ---------------------------------------------
fit_lm_list <- function(data, covars, moment_match = TRUE, ar1 = FALSE) {
    ## Fit a series of brms simple linear regression models
    ##
    ## Returns a list of brms fit objects, one model for each covariate in the
    ## 'covars' vector.
    ##
    ## data = data.frame
    ## covars = vector of column names in 'data' to include as covars
    ## moment_match = should the LOO use moment matching for high Pareto-k points
    ## ar1 = should an AR1 component be included in each model
    fits <- vector("list", length(covars))
    names(fits) <- covars

    for(i in seq_along(covars)) {

        if(!ar1) {
            priors <- c(set_prior("student_t(3, 0, 3)", class = "Intercept"),
                        set_prior("student_t(3, 0, 3)", class = "b"),
                        set_prior("student_t(3, 0, 3)", class = "sigma"))
            form_i <- bf(paste0("log_catch_stnd ~ 1 + ",  covars[i]))
        }

        if(ar1) {
            priors <- c(set_prior("student_t(3, 0, 3)", class = "Intercept"),
                        set_prior("student_t(3, 0, 3)", class = "b"),
                        set_prior("student_t(3, 0, 3)", class = "sigma"),
                        set_prior("normal(0, 0.5)", class = "ar"))
            form_i <- bf(paste0("log_catch_stnd ~ 1 + ",  covars[i], " + ar(time = year, p = 1)"))
        }

        fit_i <- brm(form_i,
                     data = data,
                     cores = 4, chains = 4,
                     prior = priors,
                     save_pars = save_pars(all = TRUE),
                     control = list(adapt_delta = 0.99),
                     seed = 123, iter = 4000)
        fit_i <- add_criterion(fit_i, c("loo", "bayes_R2"),
                               moment_match = moment_match)

        fits[[i]] <- fit_i
    }
    return(fits)
}



## Stan diag -----------------------------------------------
rhat_highest <- function(stanfit, k = 4, pars, exclude_lp = TRUE) {
    rhat <- get_rhat(stanfit, pars = pars)
    if(exclude_lp)  {
        ind <- which(names(rhat) == "lp__")
        rhat <- rhat[-ind]
    }
    rhat.max <- rev(sort(rhat)[(length(rhat) - k):length(rhat)])
    return(rhat.max)
}

neff_lowest <- function(stanfit, k = 4, pars, exclude_lp = TRUE) {
    neff <- get_neff(stanfit, pars = pars)
    if(exclude_lp)  {
        ind <- which(names(neff) == "lp__")
        neff <- neff[-ind]
    }
    neff.min <- sort(neff)[1:k]
    return(neff.min)
}

get_rhat <- function(stanfit, pars) {
    if(!is(stanfit, "stanfit")) {
        stop("Input not of class stanfit")
    }
    summary(stanfit, pars = pars)$summary[ , "Rhat"]
}

get_neff <- function(stanfit, pars) {
    if(!is(stanfit, "stanfit")) {
        stop("Input not of class stanfit")
    }
    summary(stanfit, pars = pars)$summary[ , "n_eff"]
}

total_draws <- function(stanfit) {
    ## N chains * N draws  -- post warmup
    if(!is(stanfit, "stanfit")) {
        stop("Input not of class stanfit")
    }
    dim(stanfit)[1] * dim(stanfit)[2]
}


## fill_time_series ----------------------------------------
fill_time_series <- function(data) {
    ## Fill salmon data time series so that all brood years are consecutive,
    ## filling missing years with NA.
    ##
    ## This function takes as input, a brood table with columns `stock_id`,
    ## `stock`, and `brood_yr` and fills in any non-consecutive BY within a
    ## stocks times series with NA values for all other columns present in
    ## `data`.
    ##
    ## When filtering the data by the `use` column, data values in the middle of
    ## the time series for a particular salmon stocks also get removed if `use`
    ## is set to 0. This function adds back in those data points, setting them
    ## to NA.

    id <- unique(data$stock_id)
    lst <- vector("list", length(id))
    for(i in seq_along(id)) {
        sub <- data[data$stock_id == id[i], ]
        brood_yr <- min(sub$brood_yr):max(sub$brood_yr)
        stock_id <- unique(sub$stock_id)
        stock <- unique(sub$stock)
        df <- data.frame(stock_id = stock_id, stock = stock, brood_yr = brood_yr,
                         stringsAsFactors = FALSE)
        lst[[i]] <- merge(df, sub, by = c("stock_id", "stock", "brood_yr"),
                          all.x = TRUE)
    }
    df <- do.call("rbind", c(lst, make.row.names = FALSE))

    ## Don't want NA in these columns
    out <- plyr::ddply(df, .(stock_id), transform,
                       region = unique(na.omit(region)),
                       sub_region = unique(na.omit(sub_region)),
                       ocean_region = unique(na.omit(ocean_region)),
                       lat = unique(na.omit(lat)),
                       lon = unique(na.omit(lon)))
    return(out)
}
