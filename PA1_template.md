---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data
1. Load data

```r
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "activity.zip")
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

2. Install and run packages

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
library(stringr)
library(sqldf)
```

```
## Loading required package: gsubfn
```

```
## Loading required package: proto
```

```
## Warning in doTryCatch(return(expr), name, parentenv, handler): unable to load shared object '/Library/Frameworks/R.framework/Resources/modules//R_X11.so':
##   dlopen(/Library/Frameworks/R.framework/Resources/modules//R_X11.so, 0x0006): Library not loaded: /opt/X11/lib/libSM.6.dylib
##   Referenced from: <B3716E5A-BF4D-3CA3-B8EB-89643DB72A04> /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/modules/R_X11.so
##   Reason: tried: '/opt/X11/lib/libSM.6.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libSM.6.dylib' (no such file), '/opt/X11/lib/libSM.6.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libSM.6.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-11.0.18+10/Contents/Home/lib/server/libSM.6.dylib' (no such file)
```

```
## tcltk DLL is linked to '/opt/X11/lib/libX11.6.dylib'
```

```
## Could not load tcltk.  Will use slower R code instead.
```

```
## Loading required package: RSQLite
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(lattice)
library(mice)
```

```
## 
## Attaching package: 'mice'
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
```

3. Transform data

```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps~date, data = activity, sum)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, xlab = "Steps per day", 
    main = "Steps per day", col="purple", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken 
per day

```r
mean_steps_per_day <- mean(steps_per_day$steps)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
median_steps_per_day <- median(steps_per_day$steps)
median_steps_per_day
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average 
number of steps taken, averaged across all days (y-axis)

```r
avg_daily_activity <- aggregate(steps ~ interval, activity, mean)
plot(avg_daily_activity$interval, avg_daily_activity$steps, type = "l", main = "Average daily activity pattern", xlab = "Interval", ylab = "Steps", col="lightblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
avg_daily_activity$interval[which.max(avg_daily_activity$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.

```r
activity_imputed <- mice(activity, m=5, method = "pmm")
```

3. Create a new dataset that is equal to the original dataset but with the 
missing data filled in.

```r
activity_no_NAs <- complete(activity_imputed, 1)
```

4. Make a histogram of the total number of steps taken each day and calculate 
and report the mean and median total number of steps taken per day. 

```r
steps_per_day_imputed <- aggregate(steps ~ date, activity_no_NAs, sum)
hist(steps_per_day_imputed$steps, main = "Steps per day", xlab = "Steps", 
     col = "darkblue", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
mean_steps_per_day_no_NAs <- mean(steps_per_day_imputed$steps)
mean_steps_per_day_no_NAs
```

```
## [1] 10595.33
```

```r
median_steps_per_day_no_NAs <- median(steps_per_day_imputed$steps)
median_steps_per_day_no_NAs
```

```
## [1] 10571
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and 
“weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_no_NAs$day <- as.POSIXlt(activity_no_NAs$date)$wday
activity_no_NAs$dayType <- as.factor(ifelse(activity_no_NAs$day == 0 | activity_no_NAs$day == 6, "weekend", "weekday"))
activity_no_NAs <- subset(activity_no_NAs, select = -c(day))
```

2. Make a panel plot containing a time series plot of the 5-minute interval 
(x-axis) and the average number of steps taken, averaged across all weekday days 
or weekend days (y-axis).

```r
weekday_activity <- activity_no_NAs[activity_no_NAs$dayType == "weekday",]
weekend_activity <- activity_no_NAs[activity_no_NAs$dayType == "weekend",]
activity_pattern_weekday <- aggregate(steps ~ interval, weekday_activity, mean)
activity_pattern_weekend <- aggregate(steps ~ interval, weekend_activity, mean)
plot(activity_pattern_weekday, type = "l", col = "lightpink", main = "Weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
plot(activity_pattern_weekend, type = "l", col = "plum1", main = "Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-2.png)<!-- -->
