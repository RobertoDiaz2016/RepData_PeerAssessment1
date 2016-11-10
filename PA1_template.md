# Reproducible Research: Peer Assessment 1


```r
library(knitr)
opts_chunk$set(echo=TRUE)
```

## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv",na.strings="NA",stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

```r
steps_per_day <- aggregate(steps~date,data=activity,sum)

require(ggplot2)
library(ggplot2)
ggplot(steps_per_day,aes(steps,fill=date)) +
geom_histogram(binwidth = 53)
```

![](PA1_template_files/figure-html/MeanTotalStepsPerDay-1.png)<!-- -->

```r
na_mean_steps_per_day <- as.integer(mean(steps_per_day$steps))
na_median_steps_per_day <- median(steps_per_day$steps)
```
Mean of the total number of steps taken per day: 10766  
Median of the total number of steps taken per day: 10765  


## What is the average daily activity pattern?

```r
avg_steps_per_day <- aggregate(steps~interval,data=activity,mean)

ggplot(avg_steps_per_day, aes(interval, steps)) + 
        geom_line()
```

![](PA1_template_files/figure-html/AverageDailyActivityPattern-1.png)<!-- -->

```r
interval_max_steps <- avg_steps_per_day[avg_steps_per_day$steps==max(avg_steps_per_day$steps),]
```

Interval: 835, on average across all the days in the dataset, contains the maximum number of steps: 206.1698113


## Imputing missing values

```r
na_rows <- nrow(activity[is.na(activity),])

# Used avg_steps_per_day for estimated values of na steps
activity_tidy <- activity
activity_tidy[is.na(activity_tidy)==TRUE] <- as.integer(avg_steps_per_day$steps)

steps_per_day <- aggregate(steps~date,data=activity_tidy,sum)

ggplot(steps_per_day,aes(steps,fill=date)) +
geom_histogram(binwidth = 53)
```

![](PA1_template_files/figure-html/ImputMissingValues-1.png)<!-- -->

```r
mean_steps_per_day <- as.integer(mean(steps_per_day$steps))
median_steps_per_day <- median(steps_per_day$steps)
```


Total number of missing values in the dataset (i.e. the total number of rows with NAs): 2304

Mean of the total number of steps taken per day: 10749  
Median of the total number of steps taken per day: 10641  

These values differ from the estimates from the first part of the assignment, an overall increase of max count with a left shift of its step indicator

Imputing missing data on the estimates of the total daily number of steps impacts:

Means went from 10766 to 10749

Medians went from 10765 to 10641

Histograms went from a max count of 3 at appoximately 15000 steps to a max count of 8 at slightly above 10000 steps


## Are there differences in activity patterns between weekdays and weekends?

```r
activity_tidy <- transform(activity_tidy,day=factor(ifelse(weekdays(as.Date(activity_tidy$date), abbreviate = FALSE) %in% c("Saturday","Sunday"),"weekend","weekday")))

avg_steps_per_day <- aggregate(interval~steps +day,data=activity_tidy,mean)

ggplot(avg_steps_per_day, aes(interval, steps)) + 
        geom_line() +
        facet_wrap(~day,ncol = 1)
```

![](PA1_template_files/figure-html/DifferenceActivityPatternsWeekdaysWeekends-1.png)<!-- -->
