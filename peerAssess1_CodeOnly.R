# Read the data

d <- read.csv("activity.csv")


# Mean total number of steps taken per day

## 1. Calculate the daily total number of steps.

library(data.table)
dd <- data.table(d)
dDaily <- dd[,sum(steps, na.rm=T), by=date]
setnames(dDaily, "V1", "sumsteps")

## 2. Make a histogram

hist(dDaily$sumsteps, breaks=10,
     main = "Histogram of Daily Number of Steps",
     xlab="Daily Number of Steps")
abline(v=mean(dDaily$sumsteps, na.rm=T), col="blue")

##3. Calculate and report the mean and median

mean(dDaily$sumsteps, na.rm=T)
median(dDaily$sumsteps, na.rm=T)

# Average daily pattern

## 1   

dInterval <- dd[,mean(steps, na.rm=T), by=interval]
setnames(dInterval, "V1", "meansteps")
plot(dInterval$meansteps, 
     type="l", col="blue", 
     main="Mean Number of Steps", xlab = "# of Interval", ylab="Mean Steps", lwd=1)

## 2  

maxStepInt <- which.max(dInterval$meansteps)
maxStepInt
dInterval$interval[maxStepInt]

# Imputing missing values

## 1. Total number of missing values

sum(is.na(d$steps))

## 2. Imputing strategy
# Mean of the interval will be fine.

## 3. New dataset with missing values filled in

library(dplyr)
d2 <- left_join(d, dInterval, by="interval")
misrow <- is.na(d2$steps)
d2$steps[misrow] <- d2$meansteps[misrow]
d2$meansteps <- NULL


sum(is.na(d2$steps))

## 4. New histogram, mean and median total number.

dd2 <- data.table(d2)
dDaily2 <- dd2[,sum(steps, na.rm=T), by=date]
setnames(dDaily2, "V1", "sumsteps")

### Make a histogram

hist(dDaily2$sumsteps, breaks=10,
     main = "Histogram of Daily Number of Steps",
     xlab="Daily Number of Steps")
abline(v=mean(dDaily2$sumsteps, na.rm=T), col="blue")

### Mean and median total number of steps 

mean(dDaily2$sumsteps, na.rm=T)
median(dDaily2$sumsteps, na.rm=T)


# Differences bw weekdays and weekends

## 1. create new factor variable

library(lubridate)
dd2 <- mutate(dd2, wd=factor(wday(date) %in% 2:6, 
                             labels=c("weekday", "weekend")))

## 2. Panel plot time series

pd1 <- group_by(dd2, wd, interval, add=T)
plotData <- summarize(pd1, meansteps=mean(steps))

library(lattice)
xyplot(meansteps~interval|wd, data=plotData, 
       type = "l",
       layout=c(1,2))