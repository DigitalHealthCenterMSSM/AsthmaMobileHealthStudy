# Asthma Mobile Health Study
## Unabridged Trigger Analysis R Code
####### LOAD LIBRARIES
library(cluster)
library(colorspace)
library(reshape2)
library(gplots)
library(ggplot2)
library(maps)
library(mapproj)
library(caTools)
library(TTR)
library(fields)
library(parallel)
library(maptools)
library(RColorBrewer)
library(classInt)
library(ggmap)
library(zipcode)
data(zipcode)
########## LOAD DATA
longlat.data <- read.csv("longlat.txt", header = TRUE)  ###this file is NOT provided to protect user location information
pollen.data <- read.csv("pollen_month.csv")  #source: http://www.pollen.com
wild.data <- read.csv("wildfire_longlat.csv", header = T)  #source: http://www3.epa.gov/airdata/ad_data.html
WA <- read.csv("waairquality12_24.csv")  #source: http://www3.epa.gov/airdata/ad_data.html
gaz <- read.table("2014_Gaz_zcta_national.txt", header = TRUE, colClasses = c("character", 
    "NULL", "NULL", "NULL", "NULL", "numeric", "numeric"))  ##source:https://www.census.gov/geo/maps-data/data/gazetteer2014.html
gaz.place <- read.delim("2014_Gaz_place_national.txt", header = TRUE, sep = "\t", colClasses = c("character", 
    "character", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric", 
    "numeric"))  ##source:https://www.census.gov/geo/maps-data/data/gazetteer2014.html
temp.loc <- read.table("temp_loc.txt", header = TRUE)  ###source: https://www.ncdc.noaa.gov/data-access
zip <- readShapePoly("cb_2014_us_zcta510_500k.shp")  ####### source: https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html
noaa_tmax.data <- read.table("noaa_tmax.txt", header = TRUE, sep = "\t", check.names = FALSE)  ###source: https://www.ncdc.noaa.gov/data-access
aqi <- read.table("aqi_pm2pt5.txt", header = FALSE)  #source: http://www3.epa.gov/airdata/ad_data.html
############ DEFINE FUNCTIONS
get_temp <- function(y, user.temp) {
    n.clust <- length(levels(factor(air.loc[, y])))
    cond <- unlist(lapply(lapply(tapply(air.loc[, y], air.loc$healthCode, na.omit), unique), 
        length) == 1)
    temp <- unlist(lapply(tapply(air.loc[, y], air.loc$healthCode, na.omit), unique)[cond])
    temp <- data.frame(healthCode = names(temp), clust.ext = as.character(temp))
    temp.data <- merge(air.loc, temp, by = "healthCode", all.x = T)
    temp.data$clust.ext <- factor(temp.data$clust.ext)
    temp.data[temp.data$healthCode %in% user.temp, ]
}
get_triggers <- function(x) {
    if (length(x) > 0) {
        x <- x[!is.na(x)]
        out <- gsub("\\[", "", unlist(strsplit(gsub(",", "][", paste(x, collapse = ",")), 
            "\\]")))
        out[out == "Other"] <- "NA"
        out[out == ""] <- "NA"
        
        out <- factor(out[out != ""], levels = trig.lev0)
        out
    } else {
        out < factor(NA, levels = trig.lev1)
        out
    }
}
get_top.trig <- function(x) {
    x <- factor(x, levels = trig.lev1)
    n <- sum(table(x))
    if (n > 10) {
        top.trig <- names(table(x))[which.max(table(x))]
    } else {
        NA
    }
}
get_out2 <- function(x, n, filter) {
    if (n >= filter) {
        out2 <- table(factor(x, levels = trig.lev1))/sum(table(x))
    } else {
        out2 <- rep(NA, length(trig.lev1))
    }
    out2
}
get_dist <- function(x, y, mycut.week, filter) {
    out <- tapply(x, cut(as.Date(y), as.Date(mycut.week), include.lowest = T, right = T), 
        as.character)
    N <- lapply(out, length)
    out1 <- lapply(out, get_triggers)
    out2 <- t(mapply(get_out2, out1, N, filter))
    out3 <- out2
    row.names(out3) <- names(out)
    list(out3 = out3, N = N)
}
get_out.table <- function(x, n) {
    if (n >= 10) {
        out2 <- table(factor(x, levels = c("True", "False")))/sum(table(x))
    } else {
        out2 <- rep(NA, 2)
    }
    out2
}
get_dist2 <- function(x, y, mycut.season) {
    out <- tapply(x, cut(as.Date(y), as.Date(mycut.season), include.lowest = T, right = T), 
        as.character)
    N <- lapply(out, length)  ####number of individuals 
    out2 <- t(mapply(get_out.table, out, N))
    out3 <- out2
    row.names(out3) <- names(out)
    list(out3 = out3, N = N)
}
my.bbands = function(x, n, sd) {
    mavg = runmean(x, n)
    sdev = runsd(x, n)
    up <- mavg + sd * sdev
    dn <- mavg - sd * sdev
    res <- cbind(dn, mavg, up)
    colnames(res) <- c("dn", "mavg", "up")
    res
}
get_station <- function(x) {
    ### long then lat
    distance <- rdist.earth(station.temp[, c(3, 2)], t(matrix(c(x$longitude, x$latitude))))
    station <- as.character(station.temp[which.min(distance), ]$station)
    distance <- min(distance)
    list(station = station, distance = distance)
}
get_rad <- function(x, y) {
    as.numeric(rdist.earth(t(matrix(c(x[1], y[2]))), t(matrix(c(x[1], y[1])))))
}
clean_x <- function(x) {
    ifelse(is.null(x), NA, x)
}
get_zip.data <- function(x) {
    dist1 <- rdist.earth(gaz[, c(3, 2)], t(matrix(c(x$longitude, x$latitude))))
    dist2 <- rdist.earth(gaz.place[, c(5, 4)], t(matrix(c(x$longitude, x$latitude))))
    pick1 <- which.min(dist1)
    pick2 <- which.min(dist2)
    geoid <- as.character(gaz[pick1, ]$GEOID)
    zip.distance <- dist1[pick1]
    state <- as.character(gaz.place[pick2, ]$USPS)
    name <- as.character(gaz.place[pick2, ]$NAME)
    place.distance <- dist2[pick2]
    list(zip.dist = zip.distance, geoid = geoid, state = state, place = name, place.dist = place.distance)
}
get_weather.data <- function(x, y) {
    station.date <- tapply(x$date, x$station, as.Date, format = "%Y%m%d")
    station.temp <- tapply(x[, y], x$station, as.numeric)
    mapply(function(x, y) {
        temp = y[order(x)]
        names(temp) = x[order(x)]
        return(temp)
    }, station.date, station.temp)
}
latlong2state <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon per state (plus DC, minus HI &
    # AK)
    states <- map("state", fill = TRUE, col = "transparent", plot = FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs = IDs, proj4string = CRS("+proj=longlat +datum=wgs84"))
    # Convert pointsDF to a SpatialPoints object
    pointsSP <- SpatialPoints(pointsDF, proj4string = CRS("+proj=longlat +datum=wgs84"))
    # Use 'over' to get _indices_ of the Polygons object containing each point
    indices <- over(pointsSP, states_sp)
    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
}
get_zip.data <- function(x) {
    dist1 <- rdist.earth(gaz[, c(3, 2)], t(matrix(c(x$longitude, x$latitude))))
    dist2 <- rdist.earth(gaz.place[, c(5, 4)], t(matrix(c(x$longitude, x$latitude))))
    pick1 <- which.min(dist1)
    pick2 <- which.min(dist2)
    geoid <- as.character(gaz[pick1, ]$GEOID)
    zip.distance <- dist1[pick1]
    state <- as.character(gaz.place[pick2, ]$USPS)
    name <- as.character(gaz.place[pick2, ]$NAME)
    place.distance <- dist2[pick2]
    list(zip.dist = zip.distance, geoid = geoid, state = state, place = name, place.dist = place.distance)
}
get_station <- function(x) {
    distance <- rdist.earth(temp[, c(3, 2)], t(matrix(c(x$longitude, x$latitude))))
    station <- as.character(temp[which.min(distance), ]$station)
    distance <- min(distance)
    list(station = station, distance = distance)
}
get_troubles <- function(x) {
    if (length(x) > 0) {
        x <- x[!is.na(x)]
        out <- gsub("\\[", "", unlist(strsplit(gsub(",", "][", paste(x, collapse = ",")), 
            "\\]")))
        out[out == "Other"] <- "NA"
        out[out == ""] <- "NA"
        out <- factor(out[out != ""], levels = as.character(1:22))
        out
    } else {
        out < factor(NA, levels = as.character(1:22))
        out
    }
}
########## PRE_PROCESSING CODE
zipcode <- subset(zipcode)
names(noaa_tmax) <- c("station", "date", "TMAX")
noaa <- noaa_tmax
temp <- temp.loc[temp.loc$station %in% noaa$station, ]
temp1 <- apply(longlat.data[, c(2, 3)], 1, as.list)
out <- mclapply(temp1, get_station, mc.cores = 4)
station <- unlist(lapply(out, function(x) x[[1]]))  
station.distance <- unlist(lapply(out, function(x) x[[2]]))
station.state <- temp.loc[match(station, temp.loc$station), ]$state
json <- gsub("\\.json", "", longlat.data$filename)
out <- data.frame(station, longlat.data[, c("longitude", "latitude")], station.state, station.distance, 
    json)
### nearest station must be within 50 miles
pdf(file = "histogram_gcd.pdf", width = 10.5, height = 5.5)
par(mfrow = c(1, 2))
hist(out$station.distance, xlab = "Great-circle distance (miles)", main = "Distance to nearest station")
hist(out[out$station.distance < 50, ]$station.distance, xlab = "Great-circle distance (miles)", 
    main = "Distance to nearest station < 50 miles")
dev.off()
apploc <- out[out$station.distance < 50, ]  
################### map longitude and latitude to zip code
temp <- data.frame(longitude = apploc$longitude, latitude = apploc$latitude)
temp <- apply(temp, 1, as.list)
zip.data <- mclapply(temp, get_zip.data, mc.cores = 4)
zip.zcta <- unlist(lapply(zip.data, function(x) x$geoid))
zip.dist <- unlist(lapply(zip.data, function(x) x$zip.dist))
zip.state <- unlist(lapply(zip.data, function(x) x$state))
zip.place <- unlist(lapply(zip.data, function(x) x$place))
out <- data.frame(apploc, zip.dist, zip.state, zip.place, zip.zcta)
par(mfrow = c(1, 2))
hist(out$zip.dist, xlab = "Great-circle distance (miles)", main = "Distance to nearest zip")
hist(out[out$zip.dist < 50, ]$zip.dist, xlab = "Great-circle distance (miles)", main = "Distance to nearest zip < 50 miles")
apploc <- out[out$zip.dist < 50, ]  
cond <- noaa$date >= "20150309" & noaa$date <= "20150909"
noaa_tmax <- noaa[cond, ]
my.tmax <- get_weather.data(noaa_tmax, "TMAX")
########################## 
names(aqi) <- c("aqi_json", "aqi_value", "aqi_date")
aqi <- data.frame(aqi)
temp = table.list[[25]]@values
json.data <- temp[, c("healthCode", "latlong.json", "createdOn", "aqiResponse.json.reports")]
longlat <- paste(apploc$longitude, apploc$latitude, sep = ",")
data1 <- apploc[match(json.data$latlong.json, apploc$json), c("latitude", "longitude")]
data2 <- aqi[match(json.data$aqiResponse.json.reports, aqi$aqi_json), c("aqi_value", "aqi_date")]
air.loc <- data.frame(json.data, data1, data2)
date <- as.Date(substring(air.loc$createdOn, 1, 10))
air.loc <- air.loc[date <= "2015-09-09" & date >= "2015-03-09", ]
############## take mean lat long, require variance be smalle than threshold
date <- as.Date(substring(air.loc$createdOn, 1, 10))
merge.a <- factor(paste(air.loc$healthCode, date, sep = " "))
m1 <- data.frame(air.loc, merge.a = factor(merge.a))  
############### 
long <- tapply(m1$longitude, factor(m1$merge.a), mean, na.rm = T)
long <- lapply(long, clean_x)
range.long <- tapply(m1$longitude, factor(m1$merge.a), function(x) range(x))
lat <- tapply(m1$latitude, factor(m1$merge.a), mean, na.rm = T)
lat <- lapply(lat, clean_x)
range.lat <- tapply(m1$latitude, factor(m1$merge.a), function(x) range(x))
######### max range in lat-long over a single day may not to exceed 5 miles
rad <- mapply(get_rad, range.long, range.lat)
rad <- unlist(lapply(rad, clean_x))
hist(rad[rad < 20])
###################### 
temp <- data.frame(longitude = unlist(long), latitude = unlist(lat))
temp <- apply(temp, 1, as.list)
zip.data <- mclapply(temp, get_zip.data, mc.cores = 4)
zip.zcta <- lapply(zip.data, function(x) x$geoid)
zip.zcta <- unlist(lapply(zip.zcta, clean_x))
zip.dist <- lapply(zip.data, function(x) x$zip.dist)
zip.dist <- unlist(lapply(zip.dist, clean_x))
zip.state <- lapply(zip.data, function(x) x$state)
zip.state <- unlist(lapply(zip.state, clean_x))
zip.place <- lapply(zip.data, function(x) x$place)
zip.place <- unlist(lapply(zip.place, clean_x))
zip.place.dist <- lapply(zip.data, function(x) x$place.distance)
zip.place.dist <- unlist(lapply(zip.place.dist, clean_x))
########################### 
names(noaa_tmax) <- c("station", "date", "TMAX")
names(noaa_tmax) <- c("station", "date", "TMAX")
noaa <- noaa_tmax
station.temp <- temp.loc[temp.loc$station %in% noaa$station, ]
station.data <- mclapply(temp, get_station, mc.cores = 4)
station <- lapply(station.data, function(x) x$station)
station <- unlist(lapply(station, clean_x))
station.dist <- lapply(station.data, function(x) x$distance)
station.dist <- unlist(lapply(station.dist, clean_x))
loc_daily <- data.frame(long = unlist(long), lat = unlist(lat), zip.zcta, zip.dist, zip.state, 
    zip.place, zip.place.dist, station.dist, station)
##### 
loc_daily <- loc_daily[rad < 20, ]
################ 
merge.b <- paste(m1$healthCode, m1$aqi_date, sep = " ")
m1 <- data.frame(m1, merge.b)
b <- tapply(m1$aqi_date, factor(m1$merge.b), as.character)
z <- tapply(m1$aqi_value, factor(m1$merge.b), c)
air.date <- lapply(b, function(x) names(which.max(table(factor(x)))))
air.date[grep("NULL", air.date)] <- NA
air.date <- as.character(unlist(air.date))
#### take mean across valid dates
air.value <- mapply(function(x, y, q) {
    mean(q[x %in% y], na.rm = T)
}, b, air.date, z)
air.value <- as.numeric(unlist(air.value))
########## 
temp <- lapply(lapply(b, unique), length)
disc <- names(temp)[unlist(temp) >= 2]
#### 
out <- strsplit(names(b), " ")
healthCode <- unlist(lapply(out, function(x) x[1]))
air.date <- data.frame(healthCode = healthCode, pm2pt5.date = air.date, pm2pt5.value = air.value)
temp <- paste(air.date$healthCode, air.date$pm2pt5.date, sep = " ")
table(duplicated(temp))
air_daily <- air.date
##################### now merge all
m0 <- Dai.data[, c("healthCode", "date", "get_worse", "day_symptoms")]
merge.0 <- paste(Dai.data$healthCode, Dai.data$date, sep = " ")
m0 <- data.frame(m0, merge.0)
merge.1 <- row.names(loc_daily)
m1 <- data.frame(loc_daily, merge.1)
merge.2 <- paste(air_daily$healthCode, air_daily$pm2pt5.date, sep = " ")
m2 <- data.frame(air_daily, merge.2)
temp <- merge(m0, m1, by.x = "merge.0", by.y = "merge.1", all.x = T)
temp <- merge(temp, m2, by.x = "merge.0", by.y = "merge.2", all.x = T)
temp <- temp[, -1]
names(temp)[1] <- "healthCode"
temp <- data.frame(temp)
map.state <- c()
for (i in 1:nrow(temp)) {
    if (sum(is.na(temp[i, c(5, 6)])) == 0) {
        map.state[i] <- latlong2state(temp[i, c(5, 6)])
    } else {
        map.state[i] <- NA
    }
    print(i)
}
air.loc <- data.frame(temp, map.state)
############### 
date.levels <- levels(factor(substring(Dai.data$date, 1, 10)))
date5 <- factor(cut(as.Date(date.levels), 37, labels = FALSE))
mytemp <- my.tmax  #8,347 stations
temp <- lapply(mytemp, function(x) as.numeric(x[match(date.levels, names(x))]))
temp <- do.call(rbind, temp)
colnames(temp) <- date.levels
temp <- t(apply(temp, 1, function(x) tapply(x, date5, median, na.rm = T)))
mystations <- intersect(as.character(apploc$station), row.names(temp))  #1318
out.tmax <- apply(temp, 2, function(x) tapply(x, factor(row.names(temp), levels = mystations), 
    mean, na.rm = T))
cond <- apply(out.tmax, 1, function(x) sum(is.na(x)) == 0)
out.tmax <- out.tmax[cond, ]  ####we have 4742 if 5day used and #3039 if all day
out <- list()
inames <- row.names(out.tmax)
for (i in 1:length(inames)) {
    # same station maps to different zips
    a <- unique(apploc[grep(inames[i], apploc$station), ]$zip.zcta)
    n <- length(a)
    iout <- matrix(rep(out.tmax[i, ], n), n, 37, byrow = TRUE)
    rownames(iout) <- as.character(a)
    out[[i]] <- iout
}
out <- do.call(rbind, out)
out.tmax <- out
dmat <- daisy(out.tmax)
myclust <- hclust(dmat)
plot(myclust, cex = 0.001, axes = T, sub = "", xlab = "", ylab = "", main = "Median Maximum Temperature (5-day)", 
    las = 2)
clust.1000 <- cutree(myclust, h = 1000)
clust.1400 <- cutree(myclust, h = 1400)
clust.5 <- cutree(myclust, k = 5)
temp <- data.frame(zips = row.names(out.tmax), clust.1400, clust.1000, clust.5)
out <- apply(out.tmax, 2, function(x) tapply(x, temp$clust.5, mean, na.rm = T))
apply(out, 1, mean)
zip.clusters <- temp
############# functions
get_error <- function(n, sd) {
    qnorm(0.975) * sd/sqrt(n)
}
trig.lab <- c("A cold", "Exercise", "More activity", "Strong smells", "Exhuast fumes", "House dust", 
    "Dogs", "Cats", "Other furry animals", "Mold", "Pollen", "Extreme heat", "Extreme Cold", 
    "Change in weather", "Period", "Air quality", "Smoking", "Stress", "Feelings", "Laughter", 
    "I don't know", "None of these")
date.levels <- levels(factor(substring(Dai.data$date, 1, 10)))
mycols <- rep(NA, 17)
mycols[7] <- "factor"
mycols <- as.list(mycols)
air.loc <- data.frame(air.loc, zip.clusters[match(air.loc$zip.zcta, zip.clusters$zips), c("clust.1400", 
    "clust.1000", "clust.5")])
user.temp <- cohorts$robust
temp.zip <- get_temp("zip.zcta", cohorts$robust)
temp.state <- get_temp("zip.state", cohorts$robust)
temp.place <- get_temp("zip.place", cohorts$robust)
temp.clust2 <- get_temp("clust.1400", cohorts$robust)
########## 
map("state", interior = FALSE)
map("state", boundary = FALSE, col = "gray", add = TRUE)
# align data with map definitions by (partial) matching state,county names, which include
# multiple polygons for some counties
my.state.name = state.name
for (i in 1:length(state.name)) {
    name = state.name[i]
    my.state.name[i] = paste(tolower(substr(name, 1, nchar(name))), sep = "")
}
temp = state.abb[match(map("state", plot = FALSE)$names, my.state.name)]
temp[34:37] = "NY"
temp[38:40] = "NC"
temp[53:55] = "VA"
temp[56:60] = "WA"
temp[20:22] = "MA"
temp[23:24] = "MI"
temp[8] = "DC"
mapstate <- temp
temp <- temp.clust2
test <- tapply(temp$zip.state, temp$clust.ext, function(x) sort(table(factor(x))))
tempo <- lapply(mapstate, function(x) as.numeric(c(test[[1]][x], test[[2]][x])))
colors.match <- rep(NA, length(tempo))
mylabel <- lapply(tempo, which.max)
colors.match[mylabel %in% 1] <- "blue"
colors.match[mylabel %in% 2] <- "red"
colors.match[!mylabel %in% c(1, 2)] <- "gray"
########################################################### SUPPLEMENTARY FIGURE 1B
map("state", col = colors.match, fill = TRUE, resolution = 0, lty = 0.5, projection = "polyconic")
legend("topright", c("North", "South"), fill = c("blue", "red"))
trig.lev0 <- as.character(1:22)
# c('Pollen','Change.in.weather','Stress','Extreme heat','More.activity')
alt = c(11, 12, 16)
trig.lev1 = trig.lev0[alt]
trig.lab1 <- trig.lab[alt]
trig.colors1 <- trig.colors[alt]
summer.start <- "2015-06-21"
summer <- temp[as.Date(temp$date) >= summer.start, ]
summer.trigs <- tapply(summer$get_worse, summer$clust.ext, get_triggers)
summer.top.trig <- lapply(summer.trigs, get_top.trig)
spring <- temp[as.Date(temp$date) <= summer.start, ]
spring.trigs <- tapply(spring$get_worse, spring$clust.ext, get_triggers)
spring.top.trig <- lapply(spring.trigs, get_top.trig)
out <- cbind(spring.top.trig, summer.top.trig)
trig <- tapply(temp$get_worse, factor(temp$clust.ext), as.character)
date <- tapply(temp$date, factor(temp$clust.ext), as.character)
mycut.5day <- levels(cut(as.Date(date.levels), 37))
mycut.month <- c("2015-03-09", "2015-04-09", "2015-05-09", "2015-06-09", "2015-07-09", "2015-08-09", 
    "2015-09-09")
mycut.season <- c("2015-03-09", summer.start, "2015-09-9")
mycut.week <- levels(cut(as.Date(date.levels), "week"))
out <- list()
nout <- list()
for (i in 1:length(trig)) {
    out[[i]] <- get_dist(trig[[i]], date[[i]], mycut.season, 10)[[1]]
    nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut.season, 10)[[2]]  #####this is the number of individuals
}
spring.out <- lapply(out, function(x) x[1, ])
spring.out <- do.call(rbind, spring.out)
summer.out <- lapply(out, function(x) x[2, ])
summer.out <- do.call(rbind, summer.out)
Season <- rep(c("spring", "summer"), nrow(spring.out))
forplot <- lapply(1:nrow(spring.out), function(x) rbind(spring.out[x, ], summer.out[x, ]))
forplot <- do.call(rbind, forplot)
trig.colors = c("green", "red", "blue")
row.names(forplot) = rep(c("Spring", "Summer"), 2)
########################################################### FIGURE 4A
barplot(t(forplot), col = trig.colors)
legend("topleft", trig.lab1, fill = trig.colors)
################################################################ 
date <- tapply(temp$date, factor(temp$clust.ext), as.Date)
temp <- temp.clust2
mycut <- mycut.5day
trig <- tapply(temp$get_worse, factor(temp$clust.ext), as.character)
date <- tapply(temp$date, factor(temp$clust.ext), as.Date)
trig.out <- list()
trig.nout <- list()
for (i in 1:length(date)) {
    trig.out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[1]]
    trig.nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[2]]
}
heat.out <- t(do.call(rbind, lapply(trig.out, function(x) x[, trig.lev1 %in% "12"])))
colnames(heat.out) <- names(trig)
get_tmax.plot <- function(astation, mycol, data.levels) {
    data <- lapply(mytemp, function(x) as.numeric(x[match(date.levels, names(x))]))
    data <- do.call(rbind, data)
    data <- data[row.names(data) %in% astation, ]/10
    m <- apply(data, 2, mean, na.rm = T)
    s <- apply(data, 2, sd, na.rm = T)
    e <- get_error(apply(data, 2, sum, na.rm = T), s)
    # plot.mat=rbind(m+e,m,m-e)
    # main.lab<-as.character(apploc[match(azip,apploc$zip.zcta),]$zip.place)
    # plot(smooth.spline(m),ylim=c(0,50),main='',xlab='',ylab='mean maximum temp
    # (celsius)',col=mycol,type='l',lty=2)
    plot.mat = my.bbands(m, n = 4, sd = 1)
    matplot(plot.mat[, 1:3], ylim = c(0, 50), main = "", xlab = "", ylab = "mean maximum temp (celsius)", 
        col = mycol, type = "l", lwd = c(1, 2, 1), lty = c(1, 2, 1), xaxt = "n")
    # arrows(1:length(m),as.numeric(m-e),1:length(m),as.numeric(m+e),code=3,length=0.05,angle=90)
    out <- 1:nrow(plot.mat)
}
################################################################ FIGURE 4B
par(mar = c(6.1, 6.1, 8.1, 8.1))
temp <- temp.clust2
extrap.zcta <- temp.zip$clust.ext
region.zip <- tapply(temp.clust2$zip.zcta, temp.clust2$clust.ext, function(x) levels(factor(x)))
mymonth <- cut(as.Date(temp.clust2$date), "month", labels = c("Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep"))
mypollen <- mapply(function(x, y) pollen.data[match(x, pollen.data$ZIP), colnames(pollen.data) %in% 
    y], extrap.zcta, mymonth)
a <- mymonth[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[1]]]
b <- mypollen[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[1]]]
pollen.north <- tapply(b, a, mean, na.rm = T)
a <- mymonth[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[2]]]
b <- mypollen[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[2]]]
pollen.south <- tapply(b, a, mean, na.rm = T)
clust.col <- c("blue", "red", "orange", "green", "purple")
check <- pollen.out[, apply(pollen.out, 2, function(x) sum(!is.na(x))) >= 5][, 1:2]
n.check <- nout[apply(pollen.out, 2, function(x) sum(!is.na(x))) >= 5][1:2]
mycut = date.levels
trig.out <- list()
trig.nout <- list()
for (i in 1:length(date)) {
    trig.out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[1]]
    trig.nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[2]]
}
pollen.out <- t(do.call(rbind, lapply(trig.out, function(x) x[, trig.lev1 %in% "11"])))
colnames(pollen.out) <- names(trig)
main.lab <- levels(temp$clust.ext)
for (i in 1:ncol(check)) {
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, temp$clust.ext, unique)[colnames(check)[i]])))
    nzip <- as.numeric(unlist(n.check[[i]]))
    myplot <- get_meanplot(azip, "", clust.col[i])
    par(new = TRUE)
    out
}
for (i in 1:ncol(check)) {
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    nzip <- as.numeric(unlist(n.check[[i]]))
    par(new = T)
    y = check[, i]
    y <- y[!is.na(y)]
    forplot = BBands(y, n = 10, sd = 1)
    matplot(match(names(y), date.levels), forplot[, 1:3], type = "l", ylim = c(0, 0.4), xlim = c(1, 
        185), axes = F, ylab = "", xlab = "", col = reg.colors[i], xaxt = "n", lty = c(2, 
        1, 2), lwd = c(1, 2, 1))
}
legend("topright", col = c("red", "blue", "red", "blue"), c("Pollen count (south)", "Pollen count (north)", 
    "% Trigger Dist (south)", "% Trigger Dist (north)"), lty = c(2, 2, 1, 1), lwd = c(1, 
    1, 2, 2), cex = 0.75)
axis(4, at = 0:10/10, lab = 0:10/(2 * 5), las = 2)
axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
    date.levels)]), las = 2)
mtext("% Trigger Dist.", side = 4, line = 3)
################################################################ FIGURE 4C
par(mar = c(6.1, 6.1, 8.1, 8.1))
check <- heat.out[, apply(heat.out, 2, function(x) sum(!is.na(x))) >= 5]
n.check <- nout[apply(heat.out, 2, function(x) sum(!is.na(x))) >= 5]
myplot <- mycut.5day
mytemp <- my.tmax
for (i in 1:ncol(check)) {
    astation <- na.omit(as.character(unlist(tapply(temp$station, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    myplot <- get_tmax.plot(astation, clust.col[i], date.levels)
    nzip <- as.numeric(unlist(n.check[[i]]))
    par(new = TRUE)
}
reg.colors = c("blue", "red")
for (i in 1:ncol(check)) {
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    nzip <- as.numeric(unlist(n.check[[i]]))
    par(new = T)
    y = check[, i]
    forplot <- y[!is.na(y)]
    matplot(match(names(forplot), date.levels), my.bbands(forplot, 10, 1), type = "l", ylim = c(0, 
        0.1), xlim = c(1, 185), axes = F, ylab = "", xlab = "", col = reg.colors[i], lty = c(2, 
        1, 2), lwd = c(1, 2, 1))
}
legend("topleft", col = c("red", "blue", "red", "blue"), c("Max temp. (south)", "Max temp. (north)", 
    "% Trigger Dist (south)", "% Trigger Dist (north)"), lty = c(2, 2, 1, 1), lwd = c(1, 
    1, 2, 2), cex = 0.75)
axis(4, at = 0:10/100, lab = 0:10/(100), las = 2)
axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
    date.levels)]), las = 2)
mtext("% Trigger Dist.", side = 4, line = 3)
################################################################ 
pollen.data$ZIP <- as.character(pollen.data$ZIP)
pollen.data[1, 1] <- "07677"
get_meanplot <- function(azip, main.lab, mycol) {
    data <- as.matrix(pollen.data[pollen.data$ZIP %in% azip, 2:13])
    pollen.zip <- pollen.data[pollen.data$ZIP %in% azip, ]$ZIP
    m <- apply(data, 2, mean)
    s <- apply(data, 2, sd)
    e <- get_error(apply(data, 2, sum), s)
    # main.lab<-as.character(apploc[match(azip,apploc$zip.zcta),]$zip.place)
    # main.lab=main.lab
    out <- plot(1:12 + 0.5, m, ylim = c(0, 10), xlim = c(3, 9), main = main.lab, ylab = "mean pollen count", 
        type = "l", col = mycol, lty = 2, xaxt = "n", las = 2, xlab = "")
    arrows(1:12 + 0.5, as.numeric(m - e), 1:12 + 0.5, as.numeric(m + e), code = 3, xlim = c(3, 
        9), length = 0.05, angle = 90, col = mycol)
    pollen.zip
}
################################################################ 
temp <- temp.zip
wild <- data.frame(wild.data)
wa.fires = as.character(wild[wild$State %in% "Washington", ]$Name)
wa.data <- WA[, 6:13]
mymat <- apply(wa.data[, 2:ncol(wa.data)], 2, function(x) tapply(x, wa.data$pm2pt5.date, 
    mean, na.rm = T))
mymat <- mymat[-1, ]
dat.mat = BBands(mymat[, 6][!is.na(mymat[, 6])], n = 2, "SMA", sd = 1)[, 1:3]
matplot(match(names(mymat[, 6][!is.na(mymat[, 6])]), date.levels), dat.mat, type = "l", ylim = c(0, 
    200), xlim = c(1, 185), xaxt = "n", lty = c(1, 2, 1), xlab = "", ylab = "Max AQI", col = "black")
axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
    date.levels)]), las = 2)
get_zip.nearby <- function(x, dist) {
    cond <- rdist.earth(loc.data[, c("long", "lat")], t(matrix(c(as.numeric(x$Longitude), 
        as.numeric(x$Latitude))))) <= dist
    unique(as.character(loc.data[cond, ]$zip.zcta))
    
}
wa.fires = as.character(wild[wild$State %in% "Washington", ]$Name)
loc.data <- temp[unlist(apply(temp[, c("long", "lat")], 1, function(x) sum(!is.na(x)) == 
    2)), c("zip.zcta", "long", "lat")]
wild <- apply(wild, 1, as.list)
par(new = T)
plot(NA, ylim = c(0, 200), xlim = c(1, 185), xlab = "", ylab = "", xaxt = "n")
forplot = unlist(lapply(wild, function(x) x[x$Name %in% wa.fires]$Start.Date))
lab.forplot = unlist(lapply(wild, function(x) x[x$Name %in% wa.fires]$Name))
abline(v = match(forplot, date.levels))
text(match(forplot, date.levels), 100, lab.forplot, srt = 90, cex = 0.5)
zip.nearby <- lapply(wild, get_zip.nearby, 200)
all.wa = unique(unlist(zip.nearby[unlist(lapply(wild, function(x) x$Name %in% wa.fires))]))
wf.info = lapply(zip.nearby, function(x) length(unique(temp.zip[temp.zip$zip.zcta %in% x, 
    ]$healthCode)))
names(wf.info) = unlist(lapply(wild, function(x) x$Name))
aqi.out <- list()
zip.clust <- list()
mycut <- mycut.week
for (j in 1:length(zip.nearby)) {
    zip.clust[[j]] <- factor(temp.zip$clust.ext %in% zip.nearby[[j]])
    trig <- tapply(temp$get_worse, factor(zip.clust[[j]], levels = TRUE), as.character)
    date <- tapply(temp$date, factor(zip.clust[[j]], levels = TRUE), as.Date)
    out <- list()
    nout <- list()
    for (i in 1:length(date)) {
        out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 5)[[1]]
        nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 5)[[2]]
    }
    aqi.out[[j]] <- t(do.call(rbind, lapply(out, function(x) x[, trig.lev1 %in% "16"])))
    colnames(aqi.out[[j]]) <- names(trig)
}
################################################################ FIGURE 4D
pdf("check_aqi_wildfire.pdf")
clust.col <- rep("black", length(zip.clust))
for (j in 1:length(zip.clust)) {
    par(mar = c(6.1, 6.1, 8.1, 8.1))
    #### 
    check <- aqi.out[[j]][, apply(aqi.out[[j]], 2, function(x) sum(!is.na(x))) >= 5]
    n.check <- nout[apply(aqi.out[[j]], 2, function(x) sum(!is.na(x))) >= 5]
    if (length(check) > 0) {
        y = check
        forplot <- y[!is.na(y)]
        dat.mat = my.bbands(forplot, 3, 1)
        row.names(dat.mat) = names(forplot)
        matplot(match(row.names(dat.mat), date.levels), dat.mat, type = "l", ylim = c(0, 
            0.4), xlim = c(1, 185), axes = T, ylab = "", xlab = "", main = "")
        start = match(wild[[j]]$Start.Date, date.levels)
        end = ifelse(as.Date(wild[[j]]$End.Date) > "2015-09-09", 185, match(wild[[j]]$End.Date, 
            date.levels))
        rect(start, -1, end, 200, col = rgb(0.5, 0.5, 0.5, 1/4), border = rgb(0.5, 0.5, 0.5, 
            1/4))
        legend("topleft", col = c(clust.col[i]), c("PM2.5", "% Trigger Dist (Air Quality)"), 
            lty = c(2, 1), lwd = c(1, 3), cex = 0.75, bg = "white")
        axis(4, at = (0:10) * 20, lab = (0:10) * 5/(100), las = 2)
        mtext("% Trigger Dist.", side = 4, line = 2, cex = 1, las = 2)
    }
}
dev.off()
################################################################ 
temp <- temp.clust2
reg.users = tapply(temp$healthCode, temp$clust.ext, unique)
bl.order = c("Animals", "Pollen", "A cold", "Smoking", "Exercise", "House dust", "Air quality", 
    "Change in weather", "More activity", "Mold", "Strong smells", "Extreme Cold", "Other", 
    "Stress", "Extreme heat", "I don't know", "None of these")
get_out.table = function(user.set) {
    out <- lapply(YouA.data[YouA.data$healthCode %in% user.set, ]$asthma_gets_worse_with, 
        get_troubles)
    out.table <- table(unlist(out))/sum(table(unlist(out)))
    names(out.table) <- trig.lab
    other = sum(out.table[c(19, 20, 15)])
    animals = sum(out.table[7:9])
    mynew = out.table[c(1:5, 6, 10:14, 16:18, 21:22)]
    names(mynew) = trig.lab[c(1:5, 6, 10:14, 16:18, 21:22)]
    mynew = c(Animals = animals, Other = other, mynew)
    # mynew[match(bl.order,names(mynew))]
    sort(mynew, decreasing = T)
}
bl.north = get_out.table(reg.users[[1]])
bl.south = get_out.table(reg.users[[2]])
trig.lab <- c("A cold", "Exercise", "More activity", "Strong smells", "Exhuast fumes", "House dust", 
    "Dogs", "Cats", "Other furry animals", "Mold", "Pollen", "Extreme heat", "Extreme Cold", 
    "Change in weather", "Period", "Air quality", "Smoking", "Stress", "Feelings", "Laughter", 
    "I don't know", "None of these")
n <- length(trig.lab)
library("colorspace")
trig.colors <- sample(rainbow_hcl(n), n, replace = F)
trig.lev0 <- as.character(1:22)
mycut <- date.levels
trig.lev1 = trig.lev0
trig <- tapply(temp$get_worse, factor(temp$clust.ext), as.character)
date <- tapply(temp$date, factor(temp$clust.ext), as.Date)
out <- list()
nout <- list()
for (i in 1:length(date)) {
    out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[1]]
    nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[2]]
}
get_stacks = function(x, bl.reg, mymain) {
    colnames(x) = trig.lab
    other = apply(x[, c(19, 20, 15, 5)], 1, sum)
    animals = apply(x[, 7:9], 1, sum)
    reg.ts <- cbind(Animals = animals, Other = other, x[, c(1:5, 6, 10:14, 16:18, 21:22)])
    reg.ts <- apply(reg.ts, 2, function(x) smooth.spline(na.omit(x))$y)
    reg.ts = t(reg.ts[, match(names(bl.reg), colnames(reg.ts))])
    reg.ts = cbind(bl.reg, reg.ts)
    barplot(prop.table(reg.ts, 2)[nrow(reg.ts):1, 1:133], main = mymain, col = trig.colors[match(row.names(reg.ts), 
        names(trig.colors))], border = NA, space = c(0, 10, rep(0, 104), 0.5, rep(0, 26)), 
        xaxt = "n")
    legend("topright", names(trig.colors[match(row.names(reg.ts), names(trig.colors))]), 
        fill = trig.colors[match(row.names(reg.ts), names(trig.colors))][18:1], cex = 0.8)
    
    axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
        date.levels)]), las = 2)
}
##################################### SUPPLEMENTARY FIGURE 5
par(mfrow = c(3, 1))
trig.colors = sample(rainbow(length(bl.north)), length(bl.north))
names(trig.colors) = names(bl.north)
get_stacks(out[[1]], bl.north, "north")
get_stacks(out[[2]], bl.south, "south")
