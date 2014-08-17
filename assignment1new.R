weekdaysfunction <- function() {

activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date, format= "%Y-%m-%d")

stepsPerDay <- aggregate(steps ~ date, data=activity, FUN = sum)
hist(stepsPerDay$steps, col='blue',breaks=seq(0,26000,2000), 
     main = "Histogram of Total-Steps per day", xlab="Total Steps")

meanTotalSteps   <- mean(stepsPerDay$steps)
medianTotalSteps <- median(stepsPerDay$steps)

intervals <- aggregate(steps ~ interval, data=activity, FUN = mean)
plot(intervals$interval, intervals$steps, type="l", 
     ylab="Avg. Steps per day", xlab="5min Interval",
     main="Avg. Daily Activity Pattern")


maxIdx <- which.max(intervals$steps)
ivlMax <- intervals$interval[maxIdx]

naIdx  <- which(is.na(activity$steps))
numNAs <- length(naIdx)

activityImp <- activity
activityImp$steps[naIdx] <- sapply(activityImp[naIdx,"interval"], 
                                   function(a) {
                                     idx <- which(intervals[,"interval"]==a)
                                     intervals[idx,"steps"]
                                   } ) 

stepsPerDayImp <- aggregate(steps ~ date, data=activityImp, FUN = sum)
hist(stepsPerDayImp$steps, breaks=seq(0,26000,2000), 
     main = "Histogram of Total-Steps per day", xlab="Total Steps")

meanTotalStepsImp   <- mean(stepsPerDayImp$steps)
medianTotalStepsImp <- median(stepsPerDayImp$steps)

#library(timeDate)
activityWD <- activityImp
#activityWD$weekday   <- as.factor(weekdays(activityWD$date)) # not necessary, just for comparison
isw <- gsub("TRUE", "weekday", as.character(isWeekday(activityWD$date)))
activityWD$isweekday <- as.factor(gsub("FALSE", "weekend", isw))

library(lattice)
intervalsWD <- aggregate(steps ~ interval + isweekday, data=activityWD, FUN = mean)
xyplot(steps ~ interval | isweekday, data = intervalsWD, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
}