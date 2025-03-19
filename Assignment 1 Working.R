###Assignment 1 Working for Report

## Loading and preprocessing the data
#1. Load the data (i.e. `read.csv()`)
#load the data 
data <- read.csv(unz("activity.zip", "activity.csv"))
#inspect data to better understand for future work. 
head(data)
str(data)
colSums(is.na(data))
#2. Process/transform the data (if necessary) into a format suitable for your analysis
#transform the data so the date column is in date format  
data$date <- as.Date(data$date, format = "%Y-%m-%d")



## What is mean total number of steps taken per day?
#1. Make a histogram of the total number of steps taken each day
#calcuclate the total number of steps per day while ignoring NAs
daily_steps <- tapply(data$steps, data$date, sum, na.rm = TRUE)
print(daily_steps)
#create a  histogram of the total number of steps taken each day
hist(daily_steps, 
     main = "Total Steps per Day",
     xlab = "Total Steps",
     col = "green",
     breaks = 10
)
#2. Calculate and report the **mean** and **median** total number of steps taken per day
#this creates a vector with the mean steps for each day not the overal mean of all the days
#not helpful! try again. 
mean_steps <- with(data, tapply(steps, date, mean, na.rm = TRUE))
head(mean_steps)
#mean and median number os steps taken per day
mean(daily_steps)
median(daily_steps)


## What is the average daily activity pattern?
#1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
#find average steps per interval
#find average steps per interval
interval_ave <- with(data, tapply(steps, interval, mean, na.rm = TRUE))
head(interval_ave)
#plot averagre steps per interval with a line plot
plot(as.numeric(names(interval_ave)), interval_ave, type = "l",
        main = "Average Daily Activity Pattern",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps")
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#find max
max(interval_ave, na.rm = TRUE)
#find index where interval occurs
which.max(interval_ave)
#print name of interval where max value occurs for report
names(interval_ave)[which.max(interval_ave)]



## Imputing missing values
#Note that there are a number of days/intervals where there are missing
#values (coded as `NA`). The presence of missing days may introduce
#bias into some calculations or summaries of the data.

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)
sum(is.na(data$steps))
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
#this creates a vector with the mean steps for each day not the overal mean of all the days
#create a copy of hte steps column
data$steps_noNAs <- data$steps
#find the indices where steps are missing
na_indices <- is.na(data$steps_noNAs)
#relace NAs with 'interval_ave' values
data$steps_noNAs[na_indices] <- interval_ave[as.character(data$interval[na_indices])]
#check steps_copy for NAs
sum(is.na(data$steps_noNAs))
summary(data$steps_noNAs)
summary(data$steps)

#4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
#find mean of new intervals with Nas replaced
internal_ave_noNAs <- with(data, tapply(steps_noNAs, interval, mean, na.rm = TRUE))
head(internal_ave_noNAs)
#plot averagre steps per interval with a line plot
plot(as.numeric(names(internal_ave_noNAs)), internal_ave_noNAs, type = "l",
     main = "Average Daily Activity Pattern with Missing Values Replaced",
     xlab = "5-Minute Interval",
     ylab = "Average Number of Steps",
     col = "blue")
#find max
max(internal_ave_noNAs, na.rm = TRUE)
#find index where interval occurs
which.max(internal_ave_noNAs)
#print name of interval where max value occurs for report
names(internal_ave_noNAs)[which.max(internal_ave_noNAs)]
    


## Are there differences in activity patterns between weekdays and weekends?
