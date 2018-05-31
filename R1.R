# load all packages used in this exploratory analysis
library(knitr)
library(dplyr)
library(ggplot2)

# Loading and preprocessing the data
# Show any code that is needed to
# 
# 1. Load the data
# 2. Process/transform the data (if necessary) into a format suitable for your analysis

data <- read.csv("activity.csv")

act <- data[which(!(is.na(data$steps))),]

head(act,20)

# What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day

stepsbyday <- act %>% group_by(date) %>% summarise(total_step = sum(steps))
stepsbyday <- as.data.frame(stepsbyday)
head(stepsbyday,20)


sbd <- tapply(act$steps,act$date,sum,rm.na=TRUE)
head(sbd)

# 2. If you do not understand the difference between a histogram and a barplot, 
# research the difference between them. Make a histogram of the total number of steps taken each day

hist(stepsbyday[,2],main = "Histogram of the total number of steps taken each day",xlab = "Total number of steps in a day")


# 3. Calculate and report the mean and median of the total number of steps taken per day

summary(stepsbyday)

# What is the average daily activity pattern?
# 
# 1. Make a time series plot (i.e. type = "l") of the 
#    5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

stepsbyinterval <- aggregate(steps ~ interval, act, mean)
head(stepsbyinterval)

plot(stepsbyinterval$interval,stepsbyinterval$steps,type="l",
     main="average steps across all days",xlab="Interval",ylab="average steps")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxInt <- which.max(stepsbyinterval$steps)
stepsbyinterval[maxInt,]

# Imputing missing values
# 1. Note that there are a number of days/intervals where there are missing values (coded as NA). 
#    The presence of missing days may introduce bias into some calculations or summaries of the data.
#    Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

totalNA <- sum(is.na(data))
totalNA

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be 
#    sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# use the mean of the 5-minute interval to replace the missing value.


# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

data_imputed <- data
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    data_imputed$steps[i] <- stepsbyinterval[stepsbyinterval$interval == data_imputed$interval[i],]$steps
  }
}

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the
#    mean and median total number of steps taken per day. Do these values differ from the estimates 
#    from the first part of the assignment? What is the impact of imputing missing data on the estimates 
#    of the total daily number of steps?

total_step_daily <- data_imputed %>% group_by(date) %>% summarise(total_step = sum(steps))
total_step_daily <- as.data.frame(total_step_daily)

hist(total_step_daily[,2],main="total number of steps taken each day",xlab="Total number of steps in a day")

summary(total_step_daily)
summary(stepsbyday)

# the median has increased by 1 for the inputed data. The quartiles have been changed.

# Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#    indicating whether a given date is a weekday or weekend day.

data_imputed$date_type <- ifelse(as.POSIXlt(data_imputed$date)$wday %in% c(0,6),'weekend','weekday')
head(data_imputed)

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#    interval (x-axis) and the average number of steps taken, averaged across all weekday 
#    days or weekend days (y-axis). See the README file in the GitHub repository to see an 
#    example of what this plot should look like using simulated data.

library(ggplot2)
averagedActivityDataImputed <- aggregate(steps ~ interval + date_type, data=data_imputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(date_type ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")

