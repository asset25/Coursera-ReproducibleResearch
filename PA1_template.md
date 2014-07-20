# Reproducible Research Peer Assignment 1

This assignment is intended to load and analyze the activity monitoring data provided.

First, we load the dataset into R.


```r
activity <- read.csv("activity.csv")
```

### What is the mean total number of steps taken per day?

A histogram of the total number of steps taken per day looks like this:

```r
stepsperday <- as.numeric(by(activity$steps, activity$date, function(x) sum(x, na.rm=TRUE)))
hist(stepsperday)
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
meanstepsperday <- mean(stepsperday, na.rm=TRUE)
medianstepsperday <- median(stepsperday, na.rm=TRUE)
```

The mean number of steps taken per day is **9354**. The median number of steps taken per day is **10395**. 

### What is the average daily activity pattern?


```r
meansteps <- as.numeric(by(activity$steps, activity$interval, function(x) mean(x, na.rm=TRUE)))
intervalset <- activity$interval[which(activity$date == "2012-10-01")]
plot(intervalset, meansteps, type="l", xlab="5-Minute Intervals", ylab="Average # Steps")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
maxmeansteps <- intervalset[which(meansteps == max(meansteps))]
```

On average across all days in the dataset, the 5-minute interval that contains the most steps is the **835** interval.

### Imputing missing values


```r
rowswithNA <- as.numeric(is.na(activity$steps))
totalrowswithNA <- sum(rowswithNA)
```

The total number of rows with missing data in the dataset is **2304**.

We will impute missing data by filling in the mean number of steps for the 5-minute interval for each of the missing values. The new dataset will be called **activityfilled**.


```r
activityfilled <- activity
intervalswithNA <- activityfilled$interval[which(rowswithNA >0)] 
#meanstepsbyinterval <- data.frame(intervalset, meansteps)
fillvalues <- meansteps[match(intervalswithNA, intervalset)]
activityfilled$steps[which(rowswithNA > 0)] <- fillvalues
```

Now, we repeat the analysis of steps per day done above with the new dataset with imputed values. A histogram of the total number of steps taken per day now looks like this:

```r
filledstepsperday <- as.numeric(by(activityfilled$steps, activityfilled$date, function(x) sum(x, na.rm=TRUE)))
hist(filledstepsperday)
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

```r
filledmeanstepsperday <- mean(filledstepsperday, na.rm=TRUE)
filledmedianstepsperday <- median(filledstepsperday, na.rm=TRUE)
```

The mean number of steps taken per day is now **10766**. The median number of steps taken per day is now **10766**. Having filled in the missing values with the mean steps per interval has resulted in a higher value for the mean number of steps per day (9354 vs. 10766). Interestingly, the median for the dataset is now equal to the mean.

### Are there differences in activity patterns between weekdays and weekends?

First we create a factor variable to distinguish weekdays and weekends.


```r
fdata = factor(c(1,2), labels=c("weekday", "weekend"))
weekday_days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend_days = c("Saturday", "Sunday")
```

Now we use the weekdays() function to divide up our data into weekday and weekend steps. We add a new column, dayend, to the activityfilled data frame to achieve this.


```r
activityfilled$dayend <- ifelse(is.element(weekdays(as.Date(activityfilled$date)), weekday_days), fdata[1], fdata[2])
```

Next, we plot the average steps over all days by 5-minute intervals, divided up by weekdays and weekends.


```r
weekday_data <- which(activityfilled$dayend == 1)
weekend_data <- which(activityfilled$dayend == 2)
meanstepsweekday <- as.numeric(by(activityfilled$steps[weekday_data], activity$interval[weekday_data], function(x) mean(x, na.rm=TRUE)))
meanstepsweekend <- as.numeric(by(activityfilled$steps[weekend_data], activity$interval[weekend_data], function(x) mean(x, na.rm=TRUE)))
par(mfrow=c(2,1))
plot(intervalset, meanstepsweekday, type="l", xlab="5-Minute Intervals", ylab="Avg # Steps", main="Average # Weekday Steps")
plot(intervalset, meanstepsweekend, type="l", xlab="5-Minute Intervals", ylab="Avg # Steps", main="Average # Weekend Steps")
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 
