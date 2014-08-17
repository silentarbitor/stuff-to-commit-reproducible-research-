---
title: "Untitled"
output: html_document
---

open the datafile that has been downloaded into the working directory.
The date column is converted to date format


```r
activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date, format= "%Y-%m-%d")
```

Create Histogram of total number of steps per day


```r
stepsPerDay <- aggregate(steps ~ date, data=activity, FUN = sum)
hist(stepsPerDay$steps, breaks=seq(0,26000,2000), 
     main = "Total Steps Per Day", xlab="Total Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Mean and Median total number of steps a day


```r
mean(stepsPerDay$steps)
```

```
## [1] 10766
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```
mean is 10766.19
median is 10765


Time series plot for avg steps taken a day with a 5 min interval

```r
intervals <- aggregate(steps ~ interval, data=activity, FUN = mean)
plot(intervals$interval, intervals$steps, type="l", 
     ylab="Avgerage Steps per Day", xlab="5 minute Interval",
     main="Avgerage Daily Activity Pattern") 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Imputting Missing Values


```r
NAlist  <- which(is.na(activity$steps))
numNAs <- length(NAlist)
```

Each NA is replaced by the average from the respective interval

```r
activity2 <- activity
activity2$steps[NAlist] <- sapply(activity2[NAlist,"interval"], 
                            function(x) {
                                idex <- which(intervals[,"interval"]==x)
                                intervals[idex,"steps"]
                            } ) 
```

Histogram of total steps with missing values replaced

```r
stepsPerDay2 <- aggregate(steps ~ date, data=activity2, FUN = sum)
hist(stepsPerDay2$steps, breaks=seq(0,26000,2000), 
     main = "Total Steps Per Day", xlab="Total Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Weekday Feature to show difference between weekdays and weekends


```r
library(timeDate)
```

```
## Error: there is no package called 'timeDate'
```

```r
activity3 <- activity2
win <- gsub("TRUE", "weekday", as.character(isWeekday(activity3$date)))
```

```
## Error: could not find function "isWeekday"
```

```r
activity3$isweekday <- as.factor(gsub("FALSE", "weekend", win))
```

```
## Error: object 'win' not found
```

plotting time intervals using isweekday


```r
library(lattice)
intervalswkdy <- aggregate(steps ~ interval + isweekday, data=activitywkdy, FUN = mean)
```

```
## Error: object 'activitywkdy' not found
```

```r
xyplot(steps ~ interval | isweekday, data = intervalswkdy, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

```
## Error: object 'intervalswkdy' not found
```

During weekdays, the activities are more unevenly distributed, there are higher variances in the average number of steps a day.
