## Sockeye salmon brood tables


dir.create("./outputs/", showWarnings = FALSE)
source("./scripts/functions.R")
library(plyr)


## Download sockeye brood tables ---------------------------
## Downloads sockeye salmon brood tables from NCEAS and writes it to CSV
## files for processing in other scripts. Downloaded data are saved in the
## "./data" directory.
##
## Dataset DOI: 10.5063/CR5RR9

## Brood table
bt_url <- "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A8371e85b-b693-4f22-9454-714ee17056f7"
download.file(bt_url, "./data/raw_brood_table_2022_01_13.csv")



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



## Calculate age proportions -------------------------------
## Create data frame with estimates of recruits, spawners, proportion of
## recruits that entered ocean at age, and proportion of recruits at an ocean
## age (years in the ocean).
bt <- plyr::ddply(s_brood_use, c("stock_id", "brood_yr"),function(x) {
                      R <- sum(x[ , grep("^R[[:digit:]]\\.[[:digit:]]", names(x))],
                               na.rm = TRUE) # total recruits
                      S <- x$spawners # spawners
                      ## proportion of recruits that entered ocean at age
                      ocean_entry_0 <- sum(x[ , grep("^R0\\.", names(x))], na.rm = TRUE) / R
                      ocean_entry_1 <- sum(x[ , grep("^R1\\.", names(x))], na.rm = TRUE) / R
                      ocean_entry_2 <- sum(x[ , grep("^R2\\.", names(x))], na.rm = TRUE) / R
                      ocean_entry_3 <- sum(x[ , grep("^R3\\.", names(x))], na.rm = TRUE) / R
                      ocean_entry_4 <- sum(x[ , grep("^R4\\.", names(x))], na.rm = TRUE) / R
                      ## proportion of recruits that spent a certain number of
                      ## years in the ocean
                      ocean_yrs_1 <- sum(x[ , grep("^R[[:digit:]]\\.1", names(x))], na.rm = TRUE) / R
                      ocean_yrs_2 <- sum(x[ , grep("^R[[:digit:]]\\.2", names(x))], na.rm = TRUE) / R
                      ocean_yrs_3 <- sum(x[ , grep("^R[[:digit:]]\\.3", names(x))], na.rm = TRUE) / R
                      ocean_yrs_4 <- sum(x[ , grep("^R[[:digit:]]\\.4", names(x))], na.rm = TRUE) / R
                      ocean_yrs_5 <- sum(x[ , grep("^R[[:digit:]]\\.5", names(x))], na.rm = TRUE) / R
                      R0.1 <- x[ , "R0.1"] / R
                      R0.2 <- x[ , "R0.2"] / R
                      R0.3 <- x[ , "R0.3"] / R
                      R0.4 <- x[ , "R0.4"] / R
                      R0.5 <- x[ , "R0.5"] / R
                      R1.1 <- x[ , "R1.1"] / R
                      R1.2 <- x[ , "R1.2"] / R
                      R1.3 <- x[ , "R1.3"] / R
                      R1.4 <- x[ , "R1.4"] / R
                      R1.5 <- x[ , "R1.5"] / R
                      R2.1 <- x[ , "R2.1"] / R
                      R2.2 <- x[ , "R2.2"] / R
                      R2.3 <- x[ , "R2.3"] / R
                      R2.4 <- x[ , "R2.4"] / R
                      R2.5 <- x[ , "R2.5"] / R
                      R3.1 <- x[ , "R3.1"] / R
                      R3.2 <- x[ , "R3.2"] / R
                      R3.3 <- x[ , "R3.3"] / R
                      R3.4 <- x[ , "R3.4"] / R
                      R4.1 <- x[ , "R4.1"] / R
                      R4.2 <- x[ , "R4.2"] / R
                      R4.3 <- x[ , "R4.3"] / R
                      data.frame(stock = x$stock,
                                 jurisdiction = x$jurisdiction,
                                 region = x$region,
                                 ocean_region = x$ocean_region,
                                 sub_region = x$sub_region,
                                 lat = x$lat,
                                 lon = x$lon,
                                 recruits = R,
                                 spawners = S,
                                 ocean_entry_0,
                                 ocean_entry_1,
                                 ocean_entry_2,
                                 ocean_entry_3,
                                 ocean_entry_4,
                                 ocean_yrs_1,
                                 ocean_yrs_2,
                                 ocean_yrs_3,
                                 ocean_yrs_4,
                                 ocean_yrs_5,
                                 R0.1, R0.2, R0.3, R0.4, R0.5,
                                 R1.1, R1.2, R1.3, R1.4, R1.5,
                                 R2.1, R2.2, R2.3, R2.4, R2.5,
                                 R3.1, R3.2, R3.3, R3.4,
                                 R4.1, R4.2, R4.3,
                                 stringsAsFactors = FALSE)
                      })

## Check that all Rx.x columns were included
r1 <- grep("^R[[:digit:]]\\.[[:digit:]]", names(s_brood_use), value = TRUE)
r2 <- grep("^R[[:digit:]]\\.[[:digit:]]", names(bt), value = TRUE)
if(!all.equal(sort(r1), sort(r2)))
    stop("Missing ages in brood table cleaning")

# Drop years with missing data
bt_out_1 <- bt[complete.cases(bt), ]


## Subsetting years
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
##
# bt_out_2 <- bt_out_1[bt_out_1$BY >= 1950 & bt_out_1$BY <= 2007, ]


## Fill in missing years that fall w/in min and max BY for each stock
bt_out_3 <- fill_time_series(bt_out_1)




## Create consecutive stock number -------------------------
bt_split <- split(bt_out_3, bt_out_3$stock_id)
bt_out_4 <- lapply(1:length(bt_split), function(i) {
                       dat_i <- bt_split[[i]]
                       dat_i$stock_no <- i
                       return(dat_i)
                    })
bt_out_4 <- plyr::rbind.fill(bt_out_4)



## Scale spawners and recruits -----------------------------
bt_out_5 <- bt_out_4
names(bt_out_5)[names(bt_out_5) == "spawners"] <- "spawners_raw"
names(bt_out_5)[names(bt_out_5) == "recruits"] <- "recruits_raw"
bt_out_5$spawners <- bt_out_5$spawners_raw / 1e5
bt_out_5$recruits <- bt_out_5$recruits_raw / 1e5



## Reorder columns -----------------------------------------
r_cols <- grep("^R[[:digit:]]\\.[[:digit:]]", names(bt_out_5), value = TRUE)
o_cols <- grep("^ocean\\_[[:digit:]]", names(bt_out_5), value = TRUE)
m_cols <- c("stock_id",
            "stock_no",
            "stock",
            "jurisdiction",
            "region",
            "ocean_region",
            "sub_region",
            "lat",
            "lon",
            "brood_yr",
            "recruits",
            "spawners",
            "recruits_raw",
            "spawners_raw")
bt_out_6 <- cbind(bt_out_5[ , m_cols],
                  bt_out_5[ , o_cols],
                  bt_out_5[ , r_cols])

head(bt_out_6)
tail(bt_out_6)
sapply(bt_out_6, class)



## Final brood table ---------------------------------------
brood_table <- bt_out_6
head(brood_table)
tail(brood_table)
sapply(brood_table, class)
summary(brood_table)



## Create stock info table ---------------------------------
brood_info <- plyr::ddply(brood_table, .(stock_id), summarize,
                         stock_no = unique(stock_no),
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



## Save outputs --------------------------------------------
save(brood_table, file = "./outputs/brood_table.RData")
save(brood_info, file = "./outputs/brood_info.RData")
