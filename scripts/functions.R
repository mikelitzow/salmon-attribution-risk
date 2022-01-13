## Functions for project


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
