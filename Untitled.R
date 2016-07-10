library(fitbitScraper)
library(lubridate)
library(scales)
library(ggplot2)
library(gridExtra)
cookie <- login(email="", password="")
#first date 2015-01-27

#set range of dates to pull data from
dates <- seq(as.Date("2015-01-27"), as.Date("2015-09-3"), by="days")

#construct lists number of steps (etc) tacken each day
steps.ls <- lapply(dates, function(x){
  dates <- as.character(x)
  get_intraday_data(cookie, what = "steps", dates)
})
steps <- do.call(rbind, steps.ls)

distance.ls <- lapply(dates, function(x){
  dates <- as.character(x)
  get_intraday_data(cookie, what = "distance", dates)
})
distance <- do.call(rbind, distance.ls)

floors.ls <- lapply(dates, function(x){
  dates <- as.character(x)
  get_intraday_data(cookie, what = "floors", dates)
})
floors <- do.call(rbind, floors.ls)

activemin.ls <- lapply(dates, function(x){
  dates <- as.character(x)
  get_intraday_data(cookie, what = "active-minutes", dates)
})
activemin <- do.call(rbind, activemin.ls)

#merge lists for steps, distance, floors and active minutes
fitbit.ls <- list(steps, distance, floors, activemin)
fitbit <- Reduce(function(...) merge(..., by="time", all=T), fitbit.ls)

#construct time variables for tick and day
fitbit$day <- wday(fitbit$time, label=TRUE) 
fitbit$hour <- strftime(fitbit$time, format="%H:%M:%S")

write.csv(fitbit, "~/Dropbox/Home/Fitness/fitbitdata.csv")
fitbit <- read.csv("~/Dropbox/Home/Fitness/fitbitdata.csv", header = TRUE, row.names = 1)
##-----------------------------------------##
#   Graphics
##-----------------------------------------##

ggplot(fitbit, aes(x = as.POSIXct(fitbit$hour, format = "%H:%M:%S"), y = steps)) + geom_point(alpha = .5) + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw() + geom_bar(data = steeps.df, aes(x = hour, y = steps, fill = steps), stat = 'identity', fill = 'red')

#plot average number of steps each 15 minutes 
steeps.df <- aggregate(fitbit[, "steps"], list(fitbit$hour), mean, na.rm = TRUE)
colnames(steeps.df) <- c("hour", "steps")
steeps.df$hour <-  as.POSIXct(steeps.df$hour, format = "%H:%M:%S")
ggplot(steeps.df, aes(x = hour, y = steps, fill = steps)) + geom_bar(stat = 'identity') + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw() + scale_fill_gradient(high = "#2171B5", low = "#6BAED6" )

p.steps <- ggplot(steeps.df, aes(x = hour, y = steps)) + geom_bar(stat = 'identity', fill = "#0072B2") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw() + theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text=element_text(size=16)) + xlab("time")

#plot average number of steps on days each 15 minutes 
df.ls <- split(fitbit, fitbit$day)
df.day <- lapply(df.ls, function(x){
  out <- aggregate(x[, "steps"], list(x$hour), mean, na.rm = TRUE)
})
df.day[[1]]$day <- names(df.day)[1]
df.day[[2]]$day <- names(df.day)[2]
df.day[[3]]$day <- names(df.day)[3]
df.day[[4]]$day <- names(df.day)[4]
df.day[[5]]$day <- names(df.day)[5]
df.day[[6]]$day <- names(df.day)[6]
df.day[[7]]$day <- names(df.day)[7]
df.day <- as.data.frame(do.call(rbind, df.day))
df.day$day <- ordered(df.day$day, levels = names(df.ls))
colnames(df.day) <- c("hour", "steps", "day")
df.day$hour <-  as.POSIXct(df.day$hour, format = "%H:%M:%S")
ggplot(df.day, aes(x = hour, y = steps, fill = day)) + geom_bar(stat = 'identity') + facet_grid(day ~ .) + theme_bw() + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour")

##-----------------------------------------##
#   heart rate
##-----------------------------------------##
bpm.ls <- lapply(dates, function(x){
  dates <- as.character(x)
  bpm <- get_intraday_data(cookie, what = "heart-rate", dates)
  colnames(bpm) <- c("date", "hr")
  bpm$hr[bpm$hr == 0] <- NA
  bpm$day <- wday(bpm$date, label=TRUE) 
  bpm$time <- strftime(bpm$date, format="%H:%M:%S")
  bpm
})
bpm <- do.call(rbind, bpm.ls)
bpm$time <-  as.POSIXct(bpm$time, format = "%H:%M:%S")

##-----------------------------------------##
#   sleep
##-----------------------------------------##
sleep.ls <- get_sleep_data(cookie, start_date = "2015-01-27", end_date = "2015-09-03")
sleep <- sleep.ls$df

sleep$sleepDuration <- do.call(rbind, sleep$sleepDuration)
sleep$startTime <- as.POSIXct(paste(sleep$date, sleep$startTime), format = "%Y-%m-%d %H:%M")
sleep$startTime <- xts:::align.time(sleep$startTime, n = 60*5)

sleep$endTime <- sleep$startTime + sleep$sleepDuration*60
sleep$endTime<- xts:::align.time(sleep$endTime, n = 60*5)

asleep <- split(sleep, row.names(sleep))
asleep <- lapply(asleep, function(x){
  sleepy <- seq(x$startTime, x$endTime, by="5 min")
})
asleep <- unlist(asleep)
asleep <- as.POSIXct(asleep, "%Y-%m-%d %H:%M", origin = origin, tz = "Australia/Sydney")

df <- data.frame(matrix(, nrow = length(asleep), ncol = 2))
colnames(df) <- c("date", "asleep")
df[1] <- asleep
df[2] <- 1

bpm.sleep <- merge(bpm, df, by = 'date', all = TRUE)
bpm.sleep$asleep[is.na(bpm.sleep$asleep)] <- 0
bpm.sleep$time <- strftime(bpm.sleep$time, format="%H:%M:%S")
bpm.sleep$asleep[is.na(bpm.sleep$hr)] <- NA

write.csv(bpm.sleep, "~/Dropbox/Home/Fitness/bpm_sleep.csv")
fitbit <- read.csv("~/Dropbox/Home/Fitness/bpm_sleep.csv", header = TRUE, row.names = 1)
##-----------------------------------------##
#   Graphics - BPM and sleep
##-----------------------------------------##

#all heart rate
p.bpm <- ggplot(bpm, aes(x = time, y = hr, group = 1, colour = hr)) + geom_point() + geom_smooth(colour = 'black', size = 2) + theme_bw() + scale_colour_gradient(high = "red", low = "blue") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text=element_text(size=16)) + ylab("Beats per Minute") + theme(legend.position="none")

#heart rate by day
ggplot(bpm, aes(x = time, y = hr, group = 1, colour = hr)) + geom_point() + geom_smooth(colour = 'black', size = 2) + theme_bw() + scale_colour_gradient(high = "red", low = "blue") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + facet_grid(day ~ .)

#average sleep
sleep.df <- aggregate(bpm.sleep[, "asleep"], list(bpm.sleep$time), mean, na.rm = TRUE)
colnames(sleep.df) <- c("time", "asleep")
sleep.df$time <- as.POSIXct(sleep.df$time, format = "%H:%M:%S")
ggplot(sleep.df, aes(x = time, y = asleep)) + geom_bar(stat="identity", fill = "#0072B2") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw()

p.sleep <- ggplot(sleep.df, aes(x = time, y = asleep)) + geom_density(stat="identity", fill = "#0072B2", alpha = .8) + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw() + theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text=element_text(size=16))

#plot average number of steps on days each 15 minutes 
df.ls <- split(bpm.sleep, bpm.sleep$day)
df.day <- lapply(df.ls, function(x){
  out <- aggregate(x[, "asleep"], list(x$time), mean, na.rm = TRUE)
})
df.day[[1]]$day <- names(df.day)[1]
df.day[[2]]$day <- names(df.day)[2]
df.day[[3]]$day <- names(df.day)[3]
df.day[[4]]$day <- names(df.day)[4]
df.day[[5]]$day <- names(df.day)[5]
df.day[[6]]$day <- names(df.day)[6]
df.day[[7]]$day <- names(df.day)[7]
df.day <- as.data.frame(do.call(rbind, df.day))
df.day$day <- ordered(df.day$day, levels = names(df.ls))
colnames(df.day) <- c("time", "asleep", "day")
df.day$time <-  as.POSIXct(df.day$time, format = "%H:%M:%S")
ggplot(df.day, aes(x = time, y = asleep, fill = day)) + geom_density(stat = 'identity', alpha = .8) + facet_grid(day ~ .) + theme_bw() + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour")

p.bpm <- ggplot(bpm, aes(x = time, y = hr, group = 1, colour = hr)) + geom_point() + geom_smooth(colour = 'black', size = 2) + theme_bw() + scale_colour_gradient(high = "red", low = "blue") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme(axis.title.y=element_text(size=20), axis.text=element_text(size=16), axis.text.x = element_blank(), legend.position="none") + ylab("Beats per Minute") + xlab("")

p.steps <- ggplot(steeps.df, aes(x = hour, y = steps)) + geom_bar(stat = 'identity', fill = "#0072B2") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw() + theme(axis.title.y=element_text(size=20), axis.text=element_text(size=16), axis.text.x = element_blank()) + xlab("")

p.sleep <- ggplot(sleep.df, aes(x = time, y = asleep)) + geom_density(stat="identity", fill = "#0072B2", alpha = .8) + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + theme_bw() + theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text=element_text(size=16), axis.text.x = element_text(angle = -45, hjust = -.05))


grid.arrange(p.bpm, p.steps, p.sleep)






















