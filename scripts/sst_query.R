# query ersstv5 as observational cimate data

library(tidyverse)
library(ncdf4)
library(zoo)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(oce)


# set palette
new.col <- oceColorsPalette(64)

# set theme
theme_set(theme_bw())


## load and process ERSSTv5 observations ----------------------------------

# load ERSST for comparison - 1960 through 2021

# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1900-01-01):1:(2021-12-01T00:00:00Z)][(0.0):1:(0.0)][(52):1:(62)][(198):1:(226)]", "~temp")


# load and process SST data
# nc <- nc_open("~temp")

nc <- nc_open("./data/nceiErsstv5_ff4b_09ec_9259.nc")

# process

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))
m <- months(d)
yr <- years(d)

x <- ncvar_get(nc, "longitude")
y <- ncvar_get(nc, "latitude")

SST <- ncvar_get(nc, "sst", verbose = F)

SST <- aperm(SST, 3:1)  

SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
lat <- rep(y, length(x))   
lon <- rep(x, each = length(y))   
dimnames(SST) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))

# plot to check

# need to drop Bristol Bay cells
BB <- c("N58E200", "N58E202", "N56E200")
SST[,BB] <- NA

# trim spatial area
drop <-  lat < 56 
SST[,drop] <- NA

drop <-  lat > 55 & lon == 198
SST[,drop] <- NA

# and check
temp.mean <- colMeans(SST, na.rm=T)
z <- t(matrix(temp.mean,length(y)))  
image.plot(x,y,z, col=oceColorsPalette(64), xlim=c(195,230), ylim=c(52,62))
map('world2Hires',c('Canada', 'usa'), fill=T,xlim=c(130,250), ylim=c(20,66),add=T, lwd=1, col="lightyellow3")

# calculate monthly mean
obs.sst <- rowMeans(SST, na.rm = T)

# and annual observed means
ann.sst <- tapply(obs.sst, as.numeric(as.character(yr)), mean)

plot <- data.frame(year = 1900:2021,
                   ann.sst = ann.sst)

ggplot(plot, aes(year, ann.sst)) +
  geom_line() +
  geom_point()

# scale wrt 1900-1999

sc.ann.sst <- (ann.sst - mean(ann.sst[1:100])) / sd(ann.sst[1:100])

# plot to check
plot <- data.frame(year = 1900:2021,
                   sc.ann.sst = sc.ann.sst)

ggplot(plot, aes(year, sc.ann.sst)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "grey")

# now get winter sst (Nov - March)

# assign winter year (year corresponding to January)
win.yr <- as.numeric(as.character(yr))

# increase by one if month is November or December
win.yr[m %in% c("Nov", "Dec")] <- win.yr[m %in% c("Nov", "Dec")] + 1

# now restrict to winter
win.yr <- win.yr[m %in% c("Nov", "Dec", "Jan", "Feb", "Mar")]
win.obs.sst <- obs.sst[m %in% c("Nov", "Dec", "Jan", "Feb", "Mar")]

# calculate mean for each winter
win.sst <- tapply(win.obs.sst, win.yr, mean)

# remove 1900 and 2022, which are incomplete
win.sst <- win.sst[names(win.sst) %in% 1901:2021]

# plot to check
plot <- data.frame(year = 1901:2021,
                   win.sst = win.sst)

ggplot(plot, aes(year, win.sst)) +
  geom_line() +
  geom_point()

# scale wrt 1901-1999

sc.win.sst <- (win.sst - mean(win.sst[1:99])) / sd(win.sst[1:99])

# plot to check
plot <- data.frame(year = 1901:2021,
                   sc.win.sst = sc.win.sst)

ggplot(plot, aes(year, sc.win.sst)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "grey")

# combine with rolling means

export <- data.frame(year = 1900:2021,
                     annual.sst = sc.ann.sst,
                     annual.sst.2 = rollmean(sc.ann.sst, 2, align = "right", fill=NA),
                     annual.sst.3 = rollmean(sc.ann.sst, 3, align = "center", fill=NA),
                     
                     winter.sst = c(NA, sc.win.sst),
                     winter.sst.2 = c(NA, rollmean(sc.win.sst, 2, align = "right", fill=NA)),
                     winter.sst.3 = c(NA, rollmean(sc.win.sst, 3, align = "center", fill=NA))
                     )

# and save 
write.csv(export, "./data/goa.sst.csv", row.names = F)
