# Course #5
# Author: Magdalena Grzmiel
 
install.packages("data.table")
library(data.table)

# set working directory
setwd("/Users/magda/Desktop/Coursera/course5 reproducable research/programmins_assignment_1")

# download data from web
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, dest="dataset.zip", mode="wb") 

# get the file name and read the data
file_name <-unzip ("dataset.zip", exdir = "./")
data<-read.csv(file_name)

#convert to data table
data.dt <- data.table(data)

# convert the date to date format
data$date <- as.Date(data$date)
data$steps <- as.double(data$steps)
data$interval <- as.numeric(data$interval)

# Part 1: What is mean total number of steps taken per day?
# calc the number of steps per day
steps_per_day<-data.dt[,list(steps_total=sum(steps, na.rm = TRUE)), by='date']

# draw the histogram
hist(steps_per_day$steps_total, xlab='Total number of steps', 
     main='Total number of steps taken each day', col="green")

# Calculate the mean and median of the total number of steps taken per day
mean_steps_per_day<-mean(steps_per_day$steps_total, na.rm = TRUE)
median_steps_per_day<-median(steps_per_day$steps_total, na.rm = TRUE)

# Part 2: What is the average daily activity pattern?
steps_per_interval<-data.dt[,list(avg_steps_per_interval=mean(steps, na.rm = TRUE)), by='interval']

# Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval
# (x-a xis) and the average number of steps taken, averaged across all days (y-axis)
plot(steps_per_interval, type="l", main = "Plot")
plot(x=steps_per_interval$interval, y=steps_per_interval$avg_steps_per_interval,
     type="l", main="Average number of steps taken per interval", 
     xlab="Interval", ylab="Number of steps")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
max_steps<-max(steps_per_interval$avg_steps_per_interval)
interval_with_max<-subset(steps_per_interval, avg_steps_per_interval == max_steps)

# get the interval
interval_with_max$interval

# Part 3:Imputing missing values
# Calculate the total number of missing values in the dataset 
nrow(na.omit(data.dt))

# Devise a strategy for filling in all of the missing values in the dataset. 
# use the  median for each 5-minute interval.
replace_na <- function(x) {
        dt <-copy(x)
        dt$date <- as.Date(data$date)
        dt$steps <- as.double(data$steps)
        dt$interval <- as.numeric(data$interval)
        
        spi<- dt[,list(avg_steps_per_interval=mean(steps, na.rm = TRUE)), by='interval']
        for (i in 1:nrow(dt)) {
                if (dt[i,is.na(steps)]){
                        dt[i, steps:=spi[interval==data.dt[i, interval], 
                                                             avg_steps_per_interval]]
               }
        }

        return (dt)
}


# Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_no_na <- replace_na(data.dt)

# Make a histogram of the total number of steps taken each day
# calc the number of steps per day
steps_per_day_no_na<-data_no_na[,list(steps_total=sum(steps)), by='date']

# draw the histogram
hist(steps_per_day_no_na$steps_total, xlab='Total number of steps', 
     main='Total number of steps taken each day', col="blue")


# Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
mean_steps_per_day_no_na<-mean(steps_per_day_no_na$steps_total, na.rm = TRUE)
median_steps_per_day_no_na<-median(steps_per_day_no_na$steps_total, na.rm = TRUE)

# Part 4
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.
data.dt$day <- weekdays(as.Date(data.dt$date))

data.dt[day != "Saturday" && day !="Sunday", day_of_week:="weekday"]
data.dt[(day == "Saturday"), day_of_week:="weekend"]
data.dt[(day == "Sunday"), day_of_week:="weekend"]


# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
steps_per_intr_weekdays<-data.dt[,list(avg_steps_per_intr=mean(steps, na.rm = TRUE)), 
                                 by=c("day_of_week", "interval")]

library ("lattice")
par(mfrow=c(2,1)) 
xyplot(avg_steps_per_intr ~ interval | factor(day_of_week), 
       data=steps_per_intr_weekdays, type="l",  layout = c(1,2), xlab="Interval",
       ylab="Number of steps")

