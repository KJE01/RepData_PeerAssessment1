---
title: "Reproducible Research: Peer Assignment 1"
output: 
  html_document:
    keep_md: true
---



The libraries 'dplyr' and 'lattice' are needed for this assignment.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

======================================================================================================================

## Introduction assignment 1 of course 'Reproducible Research'

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data (52K)](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

First the data has to be loaded. It is assumed that the data has been downloaded already (see link above) and saved in the working directory.


```r
df <- read.csv('activity.csv', header=TRUE, sep=',')
```

## What is the mean and total number of steps taken per day?

1. Calculate the total number of steps taken per day.


```r
Temp <- group_by(df, date) %>% summarise_at(vars(steps),funs(sum(as.numeric(.,na.rm=TRUE))))
```

2. A histogram of the total number of steps taken each day is shown


```r
hist(Temp$steps, main="Histogram of number of steps per day", xlab="Number of steps", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(Temp$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(Temp$steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

First calculate the average number of steps per time interval.


```r
Temp1 <- group_by(df, interval) %>% summarise_at(vars(steps),funs(mean), na.rm=TRUE)
```

Then make the plot.


```r
plot(Temp1$interval, Temp1$steps, type="l", main="Average number of steps per time interval", xlab="time interval", ylab="average number of steps", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Temp1[which.max(Temp1$steps), 1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
sum(Reduce('|', lapply(df, is.na)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy is to use the mean for that 5-minute interval.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
df2 <- df
df2$steps <- ifelse(is.na(df2$steps) == TRUE, Temp1$steps[Temp1$interval %in% df2$interval], df2$steps) 
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

First the data is grouped by the date and the total number of steps is calculated, followed by plotting the histogram. Finally the mean and median are calculated.


```r
Temp2 <- group_by(df2, date) %>% summarise_at(vars(steps),funs(sum(as.numeric(.,na.rm=TRUE))))
hist(Temp2$steps, main="Histogram of number of steps per day", xlab="Number of steps", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(Temp2$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(Temp2$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment?
The values do not differ materially from the first part of the assignment. Only the meadian is now equal to the mean.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

The sum of the number of steps for the original data is:


```r
sum(Temp$steps, na.rm=TRUE)
```

```
## [1] 570608
```

And the sum of the number of steps of the imputed data is:


```r
sum(Temp2$steps, na.rm=TRUE)
```

```
## [1] 656737.5
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
df2 <- mutate(df2, day_type=as.factor(ifelse(weekdays(as.Date(df2$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")))
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

First the data is grouped by the day_type and the time interval and the average number of steps is calculated. Then the plot is drawn.


```r
Temp3 <- group_by(df2, interval, day_type) %>% summarise_at(vars(steps),funs(mean), na.rm=TRUE)
xyplot(steps~interval | factor(day_type), data=Temp3, pch=19, main="Steps per time interval", xlab="interval",  ylab="Number of steps",layout=c(1,2),type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

