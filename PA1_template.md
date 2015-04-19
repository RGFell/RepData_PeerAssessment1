# Reproducible Research: Peer Assessment 1

Download data from "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

## Loading and preprocessing the data

Unzip the content of the activity.zip file into your project main dir


```r
data <- read.csv("activity.csv", sep=",",  header=TRUE)
data$date <- as.Date(data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
library(dplyr)
sumSteps <- data %>% group_by(date) %>% summarise(sum(steps))
colnames(sumSteps) <- c("Date", "Sum")
```

- The Mean for total daily steps 


```r
mean(sumSteps$Sum, na.rm = TRUE)
```

```
## [1] 10766.19
```

- The Median for total daily steps


```r
median(sumSteps$Sum, na.rm = TRUE)
```

```
## [1] 10765
```

### Histogram of total number of steps taken per day

![](PA1_template_files/figure-html/histogram-1.png) 

## What is the average daily activity pattern?


```r
avgSteps <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm=TRUE))
colnames(avgSteps) <- c("Interval", "Avg")

avgDaily <- avgSteps$Avg
stepsTS <- ts(avgDaily)

plot.ts(stepsTS)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

And we use the following code to first get the interval on which the average
max daily steps is

```r
which(avgDaily == max(avgDaily))
```

```
## [1] 104
```
and that give us 835 minutes in the day is the 5 minute interval with more
steps in average.

## Imputing missing values

Check for missing values on the data set, and if there are all concentrate on 
the steps variable.


```r
table(is.na(data))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

There are 2304 NA, but in which variable are all of them?


```r
any(is.na(data$steps))
```

```
## [1] TRUE
```

```r
any(is.na(data$date))
```

```
## [1] FALSE
```

```r
any(is.na(data$interval))
```

```
## [1] FALSE
```

According to this there are all in the steps variable

Now fill the NA with the mean of the interval for the full given period. We do
that using the package dplyr


```r
newData <- data
newData <- newData %>% 
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
```

Summarize the new data set using dplyr in order to get the total steps per day,
maek an histogram and take mean and median

```r
newSumSteps <- newData %>% group_by(date) %>%  summarise(sum = sum(steps))
```

Histogram 

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

It has the same distribution, but with more steps per day

- Mean

```r
mean(sumSteps$Sum, na.rm = TRUE)
```

```
## [1] 10766.19
```

- Median

```r
median(sumSteps$Sum, na.rm = TRUE)
```

```
## [1] 10765
```

Mean and median are exact the same as the previous data set.

## Are there differences in activity patterns between weekdays and weekends?


Change the system time on my computer so the days will apear in English,

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

Check for the day of the week usinf timeDate package

```r
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.1.3
```

```r
newData <- newData %>% group_by(date) %>% mutate(weekend= isWeekend(as.timeDate(date)))
```
   
check for weekend and weekdays

```r
newData <- mutate(newData, day = ifelse(weekend, "Weekend", "Weekday"))
```


create the dataset to plot

```r
wplot <- newData %>%
        group_by(interval, day) %>%
        summarise(avgEnd = mean(steps, na.rm=TRUE))
```


plot with lattice package

```r
library(lattice)

xyplot(avgEnd ~ interval | day, data = wplot, type = "l", layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

You can tell from the panel plot that the individual on average does more steps
on the weekends than on weekdays




