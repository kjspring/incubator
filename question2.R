# Question 2

# Load the data
dat <- read.csv("data.csv", stringsAsFactors=F)
                           
# What is the average length of a trip in seconds?
## Convert to date time
dat$starttime <- strptime(dat$starttime, format="%Y-%m-%d %H:%M:%S")
dat$stoptime <- strptime(dat$stoptime, format="%Y-%m-%d %H:%M:%S")

# Find total time by subtracting the start from stop times
dat$totaltime <- difftime(dat$stoptime, dat$starttime, units="secs")

# Find the average lenght of a trip in seconds
mean(dat$totaltime)

# Time difference of 886.5745 secs

# Estimate the minimum fraction of the original dataset that must be missing
## First sort the data by bikeid
dat1 <- dat[with(dat, order(bikeid, starttime)),]

# Convert to Date object
dat1$starttime <- as.Date(dat1$starttime, format="%Y-%m-%d")
dat1$stoptime <- as.Date(dat1$stoptime, format="%Y-%m-%d")

## Algorithm
## 1. Subset by bikeid
## 2. Subset by date
## 3. Make network using the start.station.id and end.station.id variable
## 4. Find any missing edges in that personal bikeid/day network

## The below function takes in the entire data frame. It then subsets the data
## by bikeid and the date where all the trips taken in a single day are
## subsetted together. It then uses the graph theory utility igraph
## to create a network of the travels of that bikeid during that day
## Finally, the question is how much missing data is there, so any vertice in the
## network that does not have an even number of edges would be missing at least one
## data point. This is only estimated because it may be missing more than one.

library(igraph)

## Subset by bike id
bike_id <- unique(dat$bikeid)
count = 0
for (i in 1:length(bike_id)) {
    dat1.1 <- subset(dat1, bikeid==bike_id[i])
    dates <- unique(dat1.1$starttime)
    for (t in 1:length(dates)) {
        dat1.2 <- subset(dat1.1, starttime == dates[t])
        travels  <- as.matrix(data.frame(origin=dat1.2$start.station.id, destination=dat1.2$end.station.id)) # edgelist
        g <- graph.data.frame(travels) # create adjacency matrix
        deg <- degree(g) # calculate the vertice degree
        count_g <- (length(which(deg %% 2 == 1)) - 2 ) / 4# calculate any vertice degree that is not even
        count = count + count_g
    }
}

## 64919

# Based on the available data, what is the average amount of time a bike spends at a station in seconds?
## To find this value I need to offset the data frame by one
dat1 <- dat[with(dat, order(bikeid, starttime)),]
dat1$starttime_D <- as.Date(dat1$starttime, format="%Y-%m-%d")
daily_station_wait <- vector()
rider_station_wait <- vector()

for (i in 1:length(bike_id)) {
    dat1.1 <- subset(dat1, bikeid==bike_id[i])
    dates <- unique(dat1.1$starttime_D)
    for (t in 1:length(dates)) {
        dat1.2 <- subset(dat1.1, starttime_D == dates[t])
        offset <- dat1.2$starttime
        offset <- offset[-1] # create an offsetted vector
        dat1.2 <- dat1.2[-nrow(dat1.2), ] # drop the last row from the
        dat1.2$leavetime <- offset # put the offset values in the starttime
        dat1.2$stationtime <- difftime(dat1.2$leavetime, dat1.2$stoptime, units="secs")
        daily_station_wait[t] <- mean(dat1.2$stationtime)
    }
    rider_station_wait[i] <- mean(daily_station_wait[complete.cases(daily_station_wait)])
}

mean(rider_station_wait)
# 6572.34

# Describe and explain the major qualitative usage pattern for station fe2a5f and how it differs from station fec8ff
## create adjacency matrix for all the data
travels  <- as.matrix(data.frame(origin=dat$start.station.id, destination=dat$end.station.id)) # edgelist
g <- graph.data.frame(travels) # create adjacency matrix
g_less <- delete.edges(g, which(E(g)$weight != 6000))

## calculate the degree for each station
deg <- degree(g)

## Order the degree vector from least to most
deg <- deg[order(deg)]
deg["fe2a5f"]
deg["fec8ff"]

# Estimate the number of bikes at station 8f0f64 and 4a4b61 for each hour on the hour of 2013/10/30
stations <- c('8f0f64', '4a4b61')
date <- as.Date("2013-10-30")
dat2 <- dat[with(dat, order(starttime, bikeid)),]
dat3 <- subset(dat2, starttime_D==date)
dat_4 <- subset(dat3, start.station.id == stations[1])
dat_5 <- subset(dat3, start.station.id == stations[2])

## How many bikes are at these stations on the hour?

### Drop the seconds from the data
dat_4$hour <- format(dat_4$starttime, format="%H")
dat_4$minute <- format(dat_4$starttime, format="%M")
dat_5$hour <- format(dat_5$starttime, format="%H")
dat_5$minute <- format(dat_5$starttime, format="%M")

### Modular arithmetic to find if the bike is there on the hour
dat_4sub <- dat_4[as.numeric(dat_4$minute)%%(60) == 0, ] # subset so only riders at the station are counted
hourly_dat <- matrix(data=NA, nrow=24, ncol=2) # preallocate matrix

for (i in 0:23) {
    hourly_dat[i+1, 1] <- i # add to data frame
    hourly_dat[i+1, 2] <- nrow(dat_4sub[dat_4sub$hour == as.numeric(i), ])
}

dat_5sub <- dat_5[as.numeric(dat_5$minute)%%(60) == 0, ] # subset so only riders at the station are counted
hourly_dat5 <- matrix(data=NA, nrow=24, ncol=2) # preallocate matrix

for (i in 0:23) {
    hourly_dat5[i+1, 1] <- i # add to data frame
    hourly_dat5[i+1, 2] <- nrow(dat_5sub[dat_4sub$hour == as.numeric(i), ])
}

hourly_dat <- cbind(hourly_dat, hourly_dat5[,2])
hourly_dat_frame <- as.data.frame(hourly_dat)
colnames(hourly_dat_frame) <- c("hour", '8f0f64', '4a4b61')
write.csv(hourly_dat_frame, file = "dataOntheHour.csv", row.names=F)

# Predict the number of bicycles arriving at station 912d97, 2da8d7, 010d01, 36ba2f, fa4911 on 2013/11/26 in each hour.
stations <- c('912d97', '2da8d7', '010d01', '36ba2f', 'fa4911')
date <- as.Date('2013-11-26')

## Subset the data to get only these stations
dat2 <- dat1[dat1$start.station.id %in% stations, ]
dat2$day <- weekdays(dat2$starttime_D) # add day of the week
dat2$hour <- format(dat2$starttime, format="%H")

## Find the average bike use for each weekday for each station
past <- matrix(NA, nrow=24, ncol=length(stations) + 1) # preallocate matrix for past data
past[,1] <- 0:23 # add the hours to the matrix
for (i in 1:length(stations)){
    dat3 <- dat2[dat2$start.station.id == stations[i], ]
    dat3 <- dat3[dat3$day == weekdays(date), ]
    for (e in 0:23) {
        past[e+1, i+1] <- ceiling(nrow(dat3[as.numeric(dat3$hour) == e, ]) / length(unique(dat3$starttime_D)))
    }
}

## Save as csv
predict <- as.data.frame(past)
colnames(predict) <- c("hour", stations)
write.csv(predict, file="predict.csv", row.names=F)
