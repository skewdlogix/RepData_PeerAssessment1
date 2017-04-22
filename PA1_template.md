---
title: "Reproducible Research: Peer Assessment 1"
author: "skewdlogix"
date: "April 20, 2017"
output:
    html_document:
        keep_md: true
---



### Overview

The GitHub repository was forked from https://github.com/rdpeng/RepData_PeerAssessment1 to my GitHub account and then cloned to a local repository. This template was provided in the repo as well as the dataset required for the assignment.The goal is to analyze activity data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. 


### Initial Workspace Preparation

Remove any old files and clean up workspace


```r
rm(list=ls(all=TRUE))
```

Call appropriate libraries for functions


```r
library(lubridate)
library(dplyr)
library(knitr)
library(markdown)
library(lattice)
```

Set working directory and assign it to wd


```r
setwd("C:/Documents and Settings/Stephen/Documents/GitHub/RepData_PeerAssessment1")
wd <- getwd()
```


### Unzip Files to be Analyzed

Extract list of all files in the zip file and assign to p  


```r
a <- "activity.zip"
p <- unzip(file.path(wd, a))
```


### Loading and preprocessing the data

Load the data from the extracted file ensuring that the data is in the correct format for analysis  
View files and examine structure


```r
activity <- read.csv(p, colClasses= c("integer","Date","integer"))
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset

#### Calculate the total number of steps taken per day


```r
total_steps <- activity %>% group_by(date) %>% summarize(steps = sum(steps, na.rm= TRUE))
```

```
## Error in summarize(., steps = sum(steps, na.rm = TRUE)): argument "by" is missing, with no default
```

```r
head(total_steps, n= 10)
```

```
## Error in head(total_steps, n = 10): object 'total_steps' not found
```

#### Create a histogram of the total number of steps taken each day


```r
hist(total_steps$steps, breaks=50, main= "Total Number of Steps Taken Each Day", xlab= "Total Steps per Day", ylab= "Number of Days", col= "red")
```

```
## Error in hist(total_steps$steps, breaks = 50, main = "Total Number of Steps Taken Each Day", : object 'total_steps' not found
```

#### Calculate the mean and median of the total number of steps taken each day

The mean of the total number of steps taken per day is 

```r
mean(total_steps$steps)
```

```
## Error in mean(total_steps$steps): object 'total_steps' not found
```

The median of the total number of steps taken per day is

```r
median(total_steps$steps)
```

```
## Error in median(total_steps$steps): object 'total_steps' not found
```

## What is the average daily activity pattern?

#### Time-series Plot

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_steps <- activity %>% group_by(interval) %>% summarize(average_steps = mean(steps, na.rm= TRUE))
```

```
## Error in summarize(., average_steps = mean(steps, na.rm = TRUE)): argument "by" is missing, with no default
```

```r
head(average_steps)
```

```
## Error in head(average_steps): object 'average_steps' not found
```

```r
with(average_steps, plot(average_steps ~ interval, type="l", lwd= 1, col= "red", main= "Average Number of Steps Taken By 5 Minute Interval", ylab="Average Number of Steps Taken", xlab= "5 Minute Interval"))
```

```
## Error in with(average_steps, plot(average_steps ~ interval, type = "l", : object 'average_steps' not found
```

#### 5 Minute Interval Calculations

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5 minute interval averaged across all days with the maximum number of steps is

```r
average_steps$interval[which.max(average_steps$average_steps)]
```

```
## Error in eval(expr, envir, enclos): object 'average_steps' not found
```

## Imputing missing values

#### Calculate NAs

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

The total number of NAs for each variable in the dataframe is

```r
na_count <-sapply(activity, function(y) sum(is.na(y)))
na_count
```

```
##    steps     date interval 
##     2304        0        0
```

#### Devise a strategy for filling in all of the missing values in the dataset. 

The stategy chosen is to replace each NA value with the mean of the respective interval that it belongs to. This strategy is more flexible than just using a simple mean replacement over the entire dataframe.

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_imputed <- activity
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity_imputed <- activity_imputed %>% group_by(interval) %>% mutate(steps=impute.mean(steps))
head(activity_imputed, n=10)
```

```
## Source: local data frame [10 x 3]
## Groups: interval [10]
## 
##        steps       date interval
##        <dbl>     <date>    <int>
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

#### Create a histogram of the total number of steps taken each day


```r
total_steps_adj <- activity_imputed %>% group_by(date) %>% summarize(steps = sum(steps, na.rm= TRUE))
```

```
## Error in summarize(., steps = sum(steps, na.rm = TRUE)): argument "by" is missing, with no default
```

```r
head(total_steps_adj, n= 10)
```

```
## Error in head(total_steps_adj, n = 10): object 'total_steps_adj' not found
```

```r
hist(total_steps_adj$steps, breaks=50, main= "Total Number of Steps Taken Each Day (Imputed Data)", xlab= "Total Steps per Day", ylab= "Number of Days", col= "red")
```

```
## Error in hist(total_steps_adj$steps, breaks = 50, main = "Total Number of Steps Taken Each Day (Imputed Data)", : object 'total_steps_adj' not found
```

#### Calculate the mean and median of the total number of steps taken each day

The mean of the total number of steps taken per day using imputed data is 

```r
mean(total_steps_adj$steps)
```

```
## Error in mean(total_steps_adj$steps): object 'total_steps_adj' not found
```

The median of the total number of steps taken per day using imputed data is

```r
median(total_steps_adj$steps)
```

```
## Error in median(total_steps_adj$steps): object 'total_steps_adj' not found
```

These values are slightly different than the calculated mean and median from the data with NAs.The histogram of the data becomes more symmetrical and loses its skewed shape and more closely represents a normal bell curve with both the mean and the median being the same value. The total number of daily steps increases as there are now values replacing the NAs. 

## Are there differences in activity patterns between weekdays and weekends?

#### Adding Factor Variable for Weekday and Weekend

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_imputed$DayofWeek <- ifelse(as.POSIXlt(activity_imputed$date)$wday %in% c(0,6),"weekend", "weekday")
activity_imputed$DayofWeek <- as.factor(activity_imputed$DayofWeek)
head(activity_imputed, n=10)
```

```
## Source: local data frame [10 x 4]
## Groups: interval [10]
## 
##        steps       date interval DayofWeek
##        <dbl>     <date>    <int>    <fctr>
## 1  1.7169811 2012-10-01        0   weekday
## 2  0.3396226 2012-10-01        5   weekday
## 3  0.1320755 2012-10-01       10   weekday
## 4  0.1509434 2012-10-01       15   weekday
## 5  0.0754717 2012-10-01       20   weekday
## 6  2.0943396 2012-10-01       25   weekday
## 7  0.5283019 2012-10-01       30   weekday
## 8  0.8679245 2012-10-01       35   weekday
## 9  0.0000000 2012-10-01       40   weekday
## 10 1.4716981 2012-10-01       45   weekday
```

#### Time-series Plot Using Factors to Subset Data

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
average_steps_adj <- activity_imputed %>% group_by(interval, DayofWeek) %>% summarize(average_steps = mean(steps, na.rm= TRUE))
```

```
## Error in summarize(., average_steps = mean(steps, na.rm = TRUE)): argument "by" is missing, with no default
```

```r
head(average_steps_adj)
```

```
## Error in head(average_steps_adj): object 'average_steps_adj' not found
```

```r
with(average_steps_adj, xyplot(average_steps ~ interval | DayofWeek, type="l", lwd= 1, layout= c(1, 2), col= "red", main= "Average Number of Steps Taken By 5 Minute Interval", ylab="Average Number of Steps Taken", xlab= "5 Minute Interval"))
```

```
## Error in with(average_steps_adj, xyplot(average_steps ~ interval | DayofWeek, : object 'average_steps_adj' not found
```














