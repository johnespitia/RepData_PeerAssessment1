## Loading and preprocessing the data

data <- read.csv("activity.csv")

## What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day
# 2. If you do not understand the difference between a histogram and a barplot, research the   
#    difference between them. Make a histogram of the total number of steps taken each day

total_steps <- tapply(data$steps, data$date,  FUN=sum, na.rm=TRUE)
hist(total_steps, breaks = 5, main = " Total number of steps taken per day", 
      xlab = "Steps per day", ylab = "Number of days", col = "Green")

# 3. Calculate and report the mean and median of the total number of steps taken per day

mean(total_steps)
median(total_steps)

## What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average      number of steps taken, averaged across all days (y-axis)

Avg_pattern <- aggregate(steps ~ interval, data, mean,  na.rm = T)
plot(Avg_pattern , type = "l", main = ("Steps vs. Interval"), 
    xlab = "5-minute interval", ylab = "Number of steps" , col = "Blue")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum     number of steps?

Max_steps <- which.max(Avg_pattern$steps)
Max_steps

## Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number     of rows with NAs)

Total_NAs <- paste(" the total number of rows with NAs is: ", sum(as.numeric(is.na(data$steps))))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does     not need to be sophisticated. For example, you could use the mean/median for that day, or 
# the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled     in.

filled_data <- data

for (i in 1:nrow(filled_data)){
    if (is.na(filled_data$steps[i])){
        filled_data$steps[i] <- Avg_pattern$steps[which(filled_data$interval[i] == Avg_pattern$interval)]}
}

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the       mean and median total number of steps taken per day. Do these values differ from the 
#    estimates from the first part of the assignment? What is the impact of imputing missing data     on the estimates of the total daily number of steps?

# aggregate steps as per date to get total number of steps in a day
total_steps <- aggregate(steps ~ date, filled_data, sum)

# create histogram of total number of steps in a day
hist(total_steps$steps, main=" Total number of steps per day", xlab="Steps in a day", col = "Green")

mean(total_steps$steps)

median(total_steps$steps)

## Are there differences in activity patterns between weekdays and weekends? 
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend"    
# indicating whether a given date is a weekday or weekend day.


total_steps <- aggregate(steps ~ date + interval , filled_data, sum)
total_steps$date <- as.Date(total_steps$date)
total_steps$day <- weekdays(total_steps$date)

total_steps$weekday_weekend <- ifelse(total_steps$day %in% c("Saturday", "Sunday"),"Weekend", "Weekday")

 # convert weekday_weekend to factor
total_steps$weekday_weekend <- as.factor(total_steps$weekday_weekend)

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval      (x-axis) and the average number of steps taken, averaged across all weekday days or weekend     days (y-axis). See the README file in the GitHub repository to see an example of what this      plot should look like using simulated data

Avg_weekday_weekend <- aggregate(steps ~ interval + weekday_weekend, data=total_steps, mean)

p <- ggplot(Avg_weekday_weekend, aes(interval, steps)) + geom_line() + facet_grid(weekday_weekend ~ .) +
    xlab("5-minute interval") + ylab("Number of steps") 
# Change theme
p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black")) 




