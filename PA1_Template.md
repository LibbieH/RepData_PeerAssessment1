#Reproducible Research Course Project 1
Elizabeth Harris  
June 11, 2017

##Loading and Preprocessing the Data
This section of code reads the data into R Studio then transform the data variable
from class = factor to class = Date

```r
setwd("~/R Assignments/Reproducible Research")
library(ggplot2)
activity <- read.csv("activity.csv")
sapply(activity, class)
```

```
##     steps      date  interval 
## "integer"  "factor" "integer"
```

```r
activity_date <- read.csv("activity.csv",colClasses=c("numeric","Date","numeric"))
sapply(activity_date, class)
```

```
##     steps      date  interval 
## "numeric"    "Date" "numeric"
```

##Mean Total Number of Steps per Day
This section of code calculates the total number of steps taken per day, then uses
that information to create a historgram showing the frequency with which each total number
of steps occurs in the data set.  The total number of steps per day is also used
to calculate a mean and median for the numbe rof steps take each day.

###1. Calculate Total Number of Steps per Day

```r
steps_by_date <- tapply(activity_date$steps,activity_date$date,sum,na.rm=TRUE)
```

###2. Histogram of Total Number of Steps per Day

```r
hist(steps_by_date,xlab="Total Number of Steps",
     main="Total Number of Steps by Frequency of Days Occured")
```

![](PA1_Template_files/figure-html/histogram_total_steps-1.png)<!-- -->

###3. Caluculate Mean and Median Steps Taken Each Day

```r
mean(steps_by_date)
```

```
## [1] 9354.23
```

```r
median(steps_by_date)
```

```
## [1] 10395
```

##Average Daily Activity Pattern
This chunk of code creates a time series plot that compares the average number of 
steps (y-axis) to the 5-minute time interval.  Ultimately this shows when, on average, 
during the day people in the data set were most or least activity.  The final piece
of code tells us which interval has the highest average number of steps.

###1. Time Series Plot of Average Number of Steps Taken

```r
average_daily <- aggregate(activity_date$steps, by=list(activity_date$interval),
       FUN=mean, na.rm=TRUE)
names(average_daily) <- c("interval", "mean")
plot(average_daily$interval, average_daily$mean,
     type = "l", xlab="Interval", 
     ylab="Average Number of Steps", main="Average Number of Steps per Interval")
```

![](PA1_Template_files/figure-html/time_series-1.png)<!-- -->

###2. Determine Interval with Maximum Number of Steps (On Average)

```r
average_daily[which.max(average_daily$mean), ]$interval
```

```
## [1] 835
```

##Imputing Missing Values
The presence of missing values, coded as NA, in the data set may introduce bias into some calculations or summaries in the data.  For this reason, we will need to impute, or replace, the missing data.  The following chunk of code calculates the number of missing values in the data set, devise an imputation strategy, create a new data set that is equal to the original with no missing data, and recreates the histogram, median, and mean steps taken each day.

###1. Calculate Number of Missing Values in Dataset

```r
sum(is.na(activity_date$steps))
```

```
## [1] 2304
```

###2. Strategy of Replacing Missing Values

```r
imputed_avgdaily <- average_daily$mean[match(activity_date$interval, average_daily$interval)]
```

###3. New Data Set with No Missing Values

```r
activity_date_imputed <- transform(activity_date, steps = ifelse(is.na(activity_date$steps),
       yes = imputed_avgdaily, no = activity_date$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_date_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
sum(is.na(activity_date_imputed))
```

```
## [1] 0
```

###4. Updated Histogram, Median, and Mean Number of Steps per Day

```r
impute_steps_by_date <- tapply(activity_date_imputed$steps,activity_date_imputed$date,
       sum,na.rm=TRUE)
hist(impute_steps_by_date,xlab="Total Number of Steps",
     main="Total Number of Steps by Frequency of Days Occured")
```

![](PA1_Template_files/figure-html/newcalculations-1.png)<!-- -->

```r
mean(impute_steps_by_date)
```

```
## [1] 10766.19
```

```r
median(impute_steps_by_date)
```

```
## [1] 10766.19
```

##Comparing Activity Patterns on Weekdays and Weekends

###1. Create New Factor Variable with Two Levels - "Weekend" & "Weekday"

```r
activity_weekday <- weekdays(activity_date_imputed$date, abbreviate = FALSE)
activity_datetype <- cbind(activity_date_imputed, activity_weekday)
datetype_level <- list(weekday = c("Monday", "Tuesday","Wednesday", "Thursday",
       "Friday"), weekend = c("Saturday", "Sunday"))
activity_datetype_level <- ifelse(activity_datetype$activity_weekday=="Saturday" 
       | activity_datetype$activity_weekday == "Sunday", "Weekend", "Weekday")
activity_datetype_level <- cbind(activity_datetype, activity_datetype_level)
avg_steps_datetype <- aggregate(steps~interval + activity_datetype_level, activity_datetype_level, mean, na.rm = TRUE)
```

###2. Panel Plot Comparing Activity Levels on Weekends and Weekdays

```r
plot<- ggplot(avg_steps_datetype, aes(x = interval , y = steps, color = 
       activity_datetype_level)) + geom_line() + labs(title = 
       "Average Daily Steps by Day Type", x = "Interval", y = "Average Number of Steps") +
       facet_wrap(~activity_datetype_level, ncol = 1, nrow=2)
print(plot)
```

![](PA1_Template_files/figure-html/panel plot-1.png)<!-- -->
