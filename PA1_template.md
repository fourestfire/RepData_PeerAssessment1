# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
str(data) ##loading data and showing structure
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?


```r
steps <- aggregate(data$steps, list(data = data$date), sum)
hist(steps$x, 
     breaks = 30,
     main = "Distribution of Total Steps per Day, 10/1/12 - 11/30/12",
     xlab = "# of Steps",
     ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean1 <- mean(steps$x, na.rm = TRUE) ## Mean total steps per day
mean1
```

```
## [1] 10766.19
```

```r
median1 <- median(steps$x, na.rm = TRUE) ## Median total steps per day
median1
```

```
## [1] 10765
```
    
## What is the average daily activity pattern?


```r
interval <- aggregate(data$steps, list(data = data$interval), mean, na.rm = TRUE)
plot(interval$data, interval$x, type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "Time of Day",
     ylab = "Average Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxinterval <- interval[which.max(interval[,2]),]
maxinterval[,1]    ## 5 minute time interval with the most average steps
```

```
## [1] 835
```


## Imputing missing values


```r
sum(is.na(data$steps)) ## Total number of rows with missing data
```

```
## [1] 2304
```

```r
## Logic for imputing missing values: if step count for a given row is NA, put in the mean step count for that 5-minute interval. To do this, iterate over each row of the dataset.

for (i in 1:length(data$steps)) {                                
    if (is.na(data[i,1])) {
       a <- data[i,3] ##find the interval value for the missing row
       data[i,1] = 10
       }
}
 
data2 <- data ## new dataset with filled in missing values
data <- read.csv("activity.csv") ## reset original dataframe

steps2 <- aggregate(data2$steps, list(data2 = data2$date), sum)
hist(steps2$x, 
     breaks = 30,
     main = "Distribution of Total Steps per Day, NAs imputed",
     xlab = "# of Steps",
     ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean2 <- mean(steps2$x, na.rm = TRUE) ## New mean total steps per day
mean2
```

```
## [1] 9731.934
```

```r
median2 <- median(steps2$x, na.rm = TRUE) ## New median total steps per day
median2
```

```
## [1] 10395
```

```r
meandifference <- mean1 - mean2
meandifference ## what is the difference between the means from the first and second data sets?
```

```
## [1] 1034.254
```

```r
mediandifference <- median1 - median2
mediandifference ## what is the difference between the medians from the first and second data sets?
```

```
## [1] 370
```

## Are there differences in activity patterns between weekdays and weekends?


```r
data2$weektype <- weekdays(as.Date(data2$date)) ## add column for weekday vs. weekend
```
