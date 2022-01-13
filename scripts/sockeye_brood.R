## Sockeye salmon brood tables


source("./scripts/functions.R")
library(plyr)


## Download sockeye brood tables ---------------------------
## Downloads sockeye salmon brood tables from NCEAS and writes it to CSV
## files for processing in other scripts. Downloaded data are saved in the
## "./data" directory.
##
## Dataset DOI: 10.5063/CR5RR9

## Uncomment if the data need to be re-downloaded
# bt_url <- "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A8371e85b-b693-4f22-9454-714ee17056f7"
# download.file(bt_url, "./data/raw_brood_table_2022_01_13.csv")



## Read in downloaded data ---------------------------------
s_brood_raw <- read.table("./data/raw_brood_table_2022_01_13.csv",
                          sep = ",", skip = 0, stringsAsFactors = FALSE,
                          header = TRUE)

head(s_brood_raw)
tail(s_brood_raw)
nrow(s_brood_raw)
sapply(s_brood_raw, class)
summary(s_brood_raw)



## Clean-up column names -----------------------------------
r_cols <- grep("^R[[:digit:]]\\.[[:digit:]]", names(s_brood_raw), value = TRUE)

s_brood_nor <- s_brood_raw[ , !names(s_brood_raw) %in% r_cols]
names(s_brood_nor) <- tolower(names(s_brood_nor))
names(s_brood_nor) <- gsub(".", "_", names(s_brood_nor), fixed = TRUE)
names(s_brood_nor)[names(s_brood_nor) == "broodyear"] <- "brood_yr"
names(s_brood_nor)[names(s_brood_nor) == "totalescapement"] <- "spawners"
names(s_brood_nor)[names(s_brood_nor) == "totalrecruits"] <- "recruits"

## add ordering column
s_brood_nor$read_order <- 1:nrow(s_brood_nor)

s_brood <- cbind(s_brood_nor, s_brood_raw[ , names(s_brood_raw) %in% r_cols])



## Clean-up master brood table -----------------------------

# NA values need to be replaced with 0s for years with no recruits
# This replacement is only done for the "recruit" columns
r_cols <- grep("^R[[:digit:]]\\.[[:digit:]]", names(s_brood), value = TRUE)
s_brood[ , r_cols][is.na(s_brood[ , r_cols])] <- 0


## Don't use Nushagak prior to 1985
# s_brood[s_brood$stock == "Nushagak", ]
s_brood$useflag[s_brood$stock == "Nushagak" & s_brood$brood_yr < 1985] <- 0


## 2011 Weaver total returns are suspect
# s_brood[s_brood$stock == "Weaver", ]
s_brood$useflag[s_brood$stock == "Weaver" & s_brood$brood_yr == 2011] <- 0


## Don't use L. Washington
## Questionable methods w/ brood table creation
s_brood$useflag[s_brood$stock == "Washington"] <- 0


## Subset usable data points
s_brood_use <- s_brood[s_brood$useflag == 1, ]


## Add R2.5 age-class if it doesn't exist
if(!"R2.5" %in% names(s_brood)) {
    s_brood_use$R2.5 <- 0
}

head(s_brood_use)
tail(s_brood_use)
nrow(s_brood_use)
sapply(s_brood_use, class)
summary(s_brood_use)


## Subsetting years ----------------------------------------
##
## In the data processing that Jeanette conducted, small age classes were filled
## in using the mean of the 2 previous if that age class was less than 10% of
## the total returns in the two previous years.
##
## From Jeanette on infilling small age classes:
##   "If age-specific abundance is NA and abundance is less than 10% of the
##   total brood return in the previous 2 brood years, then the abundance for
##   that age group is estiamted using the mean of the age-specific values from
##   the previous 2 brood years. If the age-specific value that is to be filled
##   in is greater than 10% of the total brood return, then that value remains
##   NA."
##
## From Jeanette on the UseFlag column:
##   "Set the UseFlag based on whether a row has a complete age class
##   estimation. A year of data is considered complete when all of the major
##   age classes have real values (not NA). A major age class is defined for
##   these purposes as any age class where the long term mean is greater than
##   1% of the total recruits for that population."
bt_out_0 <- s_brood_use[s_brood_use$brood_yr >= 1950 & s_brood_use$brood_yr <= 2020, ] ## use all data

## Drop years with missing data
bt_out_1 <- bt_out_0[complete.cases(bt_out_0), ]

## Fill in missing years that fall w/in min and max BY for each stock
bt_out_2 <- fill_time_series(bt_out_1)



## Reorder columns -----------------------------------------
r_cols <- grep("^R[[:digit:]]\\.[[:digit:]]", names(bt_out_2), value = TRUE)
m_cols <- c("stock_id",
            "stock",
            "jurisdiction",
            "region",
            "ocean_region",
            "sub_region",
            "lat",
            "lon",
            "brood_yr",
            "recruits",
            "spawners")
bt_out_3 <- cbind(bt_out_2[ , m_cols],
                  bt_out_2[ , r_cols])

head(bt_out_3)
tail(bt_out_3)
sapply(bt_out_3, class)



## Calculate ocean age -------------------------------------
bt_out_4 <- plyr::ddply(bt_out_3, c("stock_id", "brood_yr"),function(x) {
    ## Total recruits
    R <- sum(x[ , grep("^R[[:digit:]]\\.[[:digit:]]", names(x))], na.rm = TRUE)
    ## proportion of recruits that spent a certain number of years in the ocean
    R_ocean_1 <- sum(x[ , grep("^R[[:digit:]]\\.1", names(x))], na.rm = TRUE)
    R_ocean_2 <- sum(x[ , grep("^R[[:digit:]]\\.2", names(x))], na.rm = TRUE)
    R_ocean_3 <- sum(x[ , grep("^R[[:digit:]]\\.3", names(x))], na.rm = TRUE)
    R_ocean_4 <- sum(x[ , grep("^R[[:digit:]]\\.4", names(x))], na.rm = TRUE)
    R_ocean_5 <- sum(x[ , grep("^R[[:digit:]]\\.5", names(x))], na.rm = TRUE)
    d <- data.frame(stock = x$stock,
                    jurisdiction = x$jurisdiction,
                    region = x$region,
                    ocean_region = x$ocean_region,
                    sub_region = x$sub_region,
                    lat = x$lat,
                    lon = x$lon,
                    recruits = R,
                    spawners = x$spawners,
                    R_ocean_1,
                    R_ocean_2,
                    R_ocean_3,
                    R_ocean_4,
                    R_ocean_5,
                    stringsAsFactors = FALSE)
    r_cols <- grep("^R[[:digit:]]\\.[[:digit:]]", names(x), value = TRUE)
    cbind(d, x[ , r_cols])
})


head(bt_out_4)
tail(bt_out_4)



## Final brood table ---------------------------------------
brood_table <- bt_out_4
head(brood_table)
tail(brood_table)
sapply(brood_table, class)
summary(brood_table)



## Create stock info table ---------------------------------
brood_info <- plyr::ddply(brood_table, .(stock_id), summarize,
                          stock = unique(stock),
                          region = unique(region),
                          ocean_region = unique(ocean_region),
                          sub_region = unique(sub_region),
                          lat = unique(lat),
                          lon = unique(lon),
                          na_count = sum(is.na(recruits)),
                          n_years = sum(!is.na(recruits)),
                          yr_start = min(brood_yr),
                          yr_end = max(brood_yr)
)
brood_info$stock <- factor(brood_info$stock, levels = unique(brood_info$stock))



## Create return table -------------------------------------
r_cols <- sort(grep("^R[[:digit:]]\\.[[:digit:]]", names(brood_table), value = TRUE))
max_age <- 8
age_info <- rbind(data.frame(r = c("R0.1"), age = 2),
                  data.frame(r = c("R0.2", "R1.1"), age = 3),
                  data.frame(r = c("R0.3", "R1.2", "R2.1"), age = 4),
                  data.frame(r = c("R0.4", "R1.3", "R2.2", "R3.1"), age = 5),
                  data.frame(r = c("R0.5", "R1.4", "R2.3", "R3.2", "R4.1"), age = 6),
                  data.frame(r = c(        "R1.5", "R2.4", "R3.3", "R4.2"), age = 7),
                  data.frame(r = c(                "R2.5", "R3.4", "R4.3"), age = 8))

return_table_full <- plyr::ddply(brood_table, c("stock_id"), function(x) {
    ## 1. Subset age-specific recruit columns
    r <- x[ , r_cols]

    ## 2. Define brood and return years
    by <- x$brood_yr
    ry <- min(by):(max(by) + max_age)

    ## 3. Create empty matrix to store return data: [year x age]
    m <- matrix(NA, nrow = length(ry), ncol = length(r_cols),
                dimnames = list(ry, r_cols))

    ## 4. Fill return year matrix
    for(i in 1:nrow(age_info)) {
        r_i <- age_info$r[i]                      ## current age
        off <- age_info$age[i]                    ## age offset
        ind <- off:(off + length(r[ , r_i]) - 1)  ## rows in m to replace
        m[ind , r_i] <- r[ , r_i]                 ## fill return table
    }

    ## 5. Calculate returns by ocean age
    R_ocean_1 <- apply(m[ , grep("^R[[:digit:]]\\.1", age_info$r)], 1, sum, na.rm = TRUE)
    R_ocean_2 <- apply(m[ , grep("^R[[:digit:]]\\.2", age_info$r)], 1, sum, na.rm = TRUE)
    R_ocean_3 <- apply(m[ , grep("^R[[:digit:]]\\.3", age_info$r)], 1, sum, na.rm = TRUE)
    R_ocean_4 <- apply(m[ , grep("^R[[:digit:]]\\.4", age_info$r)], 1, sum, na.rm = TRUE)
    R_ocean_5 <- apply(m[ , grep("^R[[:digit:]]\\.5", age_info$r)], 1, sum, na.rm = TRUE)

    ## 6. Create output dataframe
    data.frame(stock = unique(x$stock),
               region = unique(x$region),
               ocean_region = unique(x$ocean_region),
               return_yr = ry,
               returns = apply(m, 1, sum, na.rm = TRUE),
               ## don't use if no data are available for: age-3 -- age-6
               use = c(rep(0, 5), rep(1, length(ry) - 11), rep(0, 6)),
               R_ocean_1 = R_ocean_1,
               R_ocean_2 = R_ocean_2,
               R_ocean_3 = R_ocean_3,
               R_ocean_4 = R_ocean_4,
               R_ocean_5 = R_ocean_5,
               m)
})
return_table <- return_table_full[return_table_full$use == 1, ]



## Calculate GOA age proportions ---------------------------
x1 <- return_table[return_table$ocean_region == "GOA", ]
x2 <- return_table[return_table$region == "SEAK", ]
goa_return <- rbind(x2, x1)
unique(goa_return$stock)
head(goa_return)

goa_age <- plyr::ddply(goa_return, c("return_yr"), function(x) {
    N <- nrow(x)
    R <- sum(x$returns)
    R_ocean_1 <- sum(x$R_ocean_1) / R
    R_ocean_2 <- sum(x$R_ocean_2) / R
    R_ocean_3 <- sum(x$R_ocean_3) / R
    R_ocean_4 <- sum(x$R_ocean_4) / R
    R_ocean_5 <- sum(x$R_ocean_5) / R
    data.frame(region = "GOA",
               return_yr = unique(x$return_yr),
               n_stocks = N,
               R = R,
               R_ocean_1 = R_ocean_1,
               R_ocean_2 = R_ocean_2,
               R_ocean_3 = R_ocean_3,
               R_ocean_4 = R_ocean_4,
               R_ocean_5 = R_ocean_5)
})



## Save outputs --------------------------------------------
write.csv(brood_table, file = "./data/brood_table.csv", row.names = FALSE)
write.csv(brood_info, file = "./data/brood_info.csv", row.names = FALSE)
write.csv(return_table, file = "./data/return_table.csv", row.names = FALSE)
write.csv(goa_age, file = "./data/goa_age.csv", row.names = FALSE)
