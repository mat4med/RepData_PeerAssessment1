# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
load required library

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

Load data

```r
activity <- read.csv("activity.csv")
```

convert "date" from factor to date data type

```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?
group data by date

```r
activity_grp <- group_by(activity, date)
```

then, calculate total of steps for each day

```r
total_steps <- summarise(activity_grp, total_steps_per_day = sum(steps, na.rm = TRUE))
```

find the mean

```r
mean(total_steps$total_steps_per_day)
```

```
## [1] 9354.23
```
find the median

```r
median(total_steps$total_steps_per_day)
```

```
## [1] 10395
```

draw histogram and add the mean and median

```r
hist(total_steps$total_steps_per_day)
abline(v = mean(total_steps$total_steps_per_day), lwd = 2, lty = 2, col = "blue")	  
abline(v = median(total_steps$total_steps_per_day), lwd = 2, lty = 2, col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)\


## What is the average daily activity pattern?
find the average steps

```r
ave_steps <- summarise(activity_grp, ave_steps_per_day = mean(steps, na.rm = TRUE))
stepsInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```

then plot

```r
plot(steps ~ interval, data = stepsInterval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\

```r
## other thought to plot avearge ?! 
## plot(ave_steps_per_day ~ date, ave_steps, type = "l")
```

find max. ave. steps

```r
##max(ave_steps$ave_steps_per_day[complete.cases(ave_steps)])
stepsInterval[which.max(stepsInterval$steps), ]$interval
```

```
## [1] 835
```
then find the date with max. ave.

```r
na.omit(ave_steps[ave_steps$ave_steps_per_day>=73,])
```

```
## Source: local data frame [1 x 2]
## 
##         date ave_steps_per_day
##       (date)             (dbl)
## 1 2012-11-23          73.59028
```

## Imputing missing values
find the total number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

find the ave. steps and use it for missing values

```r
m <- as.integer(mean(ave_steps$ave_steps_per_day, na.rm = TRUE))
```

function to create new dataset

```r
fill_miss_values <- function(){
	## copy "activity" data frame
	updated_activity <- activity
	## get the average steps of all days, as integer
	m <- as.integer(mean(ave_steps$ave_steps_per_day, na.rm = TRUE))
	##[1] 37

	for(i in seq_len(nrow(updated_activity))) {
		## if steps are na
		if(is.na(updated_activity$steps[i])){
			## replace it with average steps
			updated_activity$steps[i] <- m
		}
	}
	## return updated activity
	updated_activity
}

updated_activity <- fill_miss_values()
```

Compare between old dataset and new dataset
group data by date

```r
updated_activity_grp <- group_by(updated_activity, date)
```

calculate total of steps for each day

```r
updated_total_steps <- summarise(updated_activity_grp, total_steps_per_day = sum(steps, na.rm = TRUE))
```

A histogram of the total number of steps taken each day

```r
updated_total_steps2 <- aggregate(steps ~ date, data = updated_activity, sum)
hist(updated_total_steps2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)\


find mean and median (notice: both are higher than the estimates in first part)

```r
mean(updated_total_steps$total_steps_per_day)
```

```
## [1] 10751.74
```

```r
median(updated_total_steps$total_steps_per_day)
```

```
## [1] 10656
```

load library

```r
library(ggplot2)
```
plot both datasets to show the impact of imputing missing data

```r
## original data:
#plot(total_steps_per_day ~ date, total_steps, type = "l", col = "blue")
## updated data:
#lines(total_steps_per_day ~ date, updated_total_steps, col = "red")
# Group by date   
updated_grp_date <- updated_activity %>% group_by(date)
# Summarize the total steps per day (Tot_steps)
total_steps_per_day <- updated_grp_date %>% summarize(total_steps = sum(steps))

ggplot(total_steps_per_day, aes(x = total_steps))+
  geom_histogram() +
  ggtitle('Total Steps with replacement of missing values') 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)\

```r
#plot(total_steps ~ date, total_steps_per_day, type = "h", col = "blue")
```

## Are there differences in activity patterns between weekdays and weekends?
load library

```r
library(chron)
```

add a new column "days" as TRUE & FALSE then convert it to "weekday" & "weekend"

```r
updated_activity <- mutate(updated_activity, days = is.weekend(date))
t <- sapply(updated_activity$days, function(x){if(x) {x <- "weekend"} else {x <- "weekday"}})
t <- as.factor(t)
updated_activity$days <- t
```
load library

```r
library(ggplot2)
```
plot weekdays and weekends

```r
# Group by week and interval   
updated_weekly_interval <- updated_activity %>% group_by(days,interval)
# Summarize the total steps per day (Ave_steps)
updated_ave_steps <- updated_weekly_interval %>% summarize(mean_steps = mean(steps))

weekdays_weekends <- qplot(interval, mean_steps, data = updated_ave_steps, 
	color = days, geom = "line", fill = days, facets = days~.)
weekdays_weekends
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png)\



