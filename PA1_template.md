---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

###  Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

```r
library(dplyr)
library(ggplot2)
library(zoo)
library(knitr)
data<-read.csv('activity.csv',sep = ",")
```
###  Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date<-as.Date(data$date)
nonadata<-na.omit(data)
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day


```r
totday<- nonadata %>% group_by(date) %>% summarise_each(funs = sum)
kable(totday)
```



|date       | steps| interval|
|:----------|-----:|--------:|
|2012-10-02 |   126|   339120|
|2012-10-03 | 11352|   339120|
|2012-10-04 | 12116|   339120|
|2012-10-05 | 13294|   339120|
|2012-10-06 | 15420|   339120|
|2012-10-07 | 11015|   339120|
|2012-10-09 | 12811|   339120|
|2012-10-10 |  9900|   339120|
|2012-10-11 | 10304|   339120|
|2012-10-12 | 17382|   339120|
|2012-10-13 | 12426|   339120|
|2012-10-14 | 15098|   339120|
|2012-10-15 | 10139|   339120|
|2012-10-16 | 15084|   339120|
|2012-10-17 | 13452|   339120|
|2012-10-18 | 10056|   339120|
|2012-10-19 | 11829|   339120|
|2012-10-20 | 10395|   339120|
|2012-10-21 |  8821|   339120|
|2012-10-22 | 13460|   339120|
|2012-10-23 |  8918|   339120|
|2012-10-24 |  8355|   339120|
|2012-10-25 |  2492|   339120|
|2012-10-26 |  6778|   339120|
|2012-10-27 | 10119|   339120|
|2012-10-28 | 11458|   339120|
|2012-10-29 |  5018|   339120|
|2012-10-30 |  9819|   339120|
|2012-10-31 | 15414|   339120|
|2012-11-02 | 10600|   339120|
|2012-11-03 | 10571|   339120|
|2012-11-05 | 10439|   339120|
|2012-11-06 |  8334|   339120|
|2012-11-07 | 12883|   339120|
|2012-11-08 |  3219|   339120|
|2012-11-11 | 12608|   339120|
|2012-11-12 | 10765|   339120|
|2012-11-13 |  7336|   339120|
|2012-11-15 |    41|   339120|
|2012-11-16 |  5441|   339120|
|2012-11-17 | 14339|   339120|
|2012-11-18 | 15110|   339120|
|2012-11-19 |  8841|   339120|
|2012-11-20 |  4472|   339120|
|2012-11-21 | 12787|   339120|
|2012-11-22 | 20427|   339120|
|2012-11-23 | 21194|   339120|
|2012-11-24 | 14478|   339120|
|2012-11-25 | 11834|   339120|
|2012-11-26 | 11162|   339120|
|2012-11-27 | 13646|   339120|
|2012-11-28 | 10183|   339120|
|2012-11-29 |  7047|   339120|

### If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(totday$steps,xlab = "Steps",col = "red",breaks = 20,main = "Total Number of steps per day")
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

### Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totday$steps)
```

```
## [1] 10766.19
```

```r
median(totday$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
  meanday<-subset(nonadata,select = c('steps','interval')) %>% group_by(interval) %>% summarise_each(funs = mean)
  with(meanday,plot(interval,steps,type='l',col="red",xlab="5 Minute Interval",ylab="Average Steps",main="Average Daily Activity Pattern"))
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34-1.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanday$interval[which.max(meanday$steps)]
```

```
## [1] 835
```

## Imputing missing values
###  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
count(data[is.na(data$steps),])
```

```
##      n
## 1 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Using the mean for that 5-minute interval to the NA values


```r
means<-nonadata %>% group_by(interval) %>% summarise_each(funs = mean)
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filldata<-data
filldata$steps<-na.aggregate.default(data$steps,data$interval,FUN = mean,na.rm = TRUE)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totfillday<- filldata %>% group_by(date) %>% summarise_each(funs = sum)
hist(totfillday$steps,xlab = "Steps",col = "red",breaks = 20,main = "Total Number of steps per day")
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-39-1.png)

```r
mean(totfillday$steps)
```

```
## [1] 10766.19
```

```r
median(totfillday$steps)
```

```
## [1] 10766.19
```

This shows that there is no change in the mean but minor change in the median of the data after imputing the NA values of the dataset

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
filldata$week[weekdays(filldata$date,abbreviate = TRUE) %in% c("Sat","Sun")]<- "Weekend"
filldata$week[is.na(filldata$week)]<-"Weekday"
filldata$week<-as.factor(filldata$week)
str(filldata$week)
```

```
##  Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
meanfillday<-filldata %>% group_by(interval,week) %>% summarise_each(funs = mean)
xyplot(steps ~ interval|week,data = meanfillday,type='l',layout=c(1,2))
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41-1.png)
