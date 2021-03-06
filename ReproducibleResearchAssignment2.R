##Code for reading in the dataset and/or processing the data

#Set up working directory
setwd("D:/Downloads/Coursera/Reproducible Research/Course Project 1")

#URL to download the data zip file
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

#Download the zip file into workspace
download.file(fileUrl, destfile="activity.zip")

#Unzip the file, read the file using read.csv() to load the data into R
data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE, sep = ",", na.strings = "NA")

#Look at the summary of the data
summary(data)

#Look at the struture of the data
str(data)

#Looking at the first 6 rows of the dataset:
head(data)

#CConverting the "date" variable to a Date classe and the "interval" variable to a factor
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

##What is mean total number of steps taken per day?
#Subsetting the dataset to ignore missing values
NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
head(data_no_NA)

#Aggregating the number of steps taken each day
#Creating a data frame with the steps taken for each day
steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)
#Adding column names to the created data frame
colnames(steps_each_day) <- c("date", "steps")

#Making a histogram of the total number of steps taken each day.
hist(as.numeric(steps_each_day$steps), breaks = 20, col = "Blue", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

#Calculating the mean and median total number of steps taken per day.
mean(steps_each_day$steps)
median(steps_each_day$steps)

##What is the average daily activity pattern?
#Calculating the average number of steps taken, averaged across all days.
#Calculating the average
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)

#Adding columns names
colnames(steps_per_interval) <- c("interval", "average_steps")

#ploting the average daily activity pattern 
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

#The maximum number of average steps
max_steps <- max(steps_per_interval$average_steps)
max_steps

#The 5-minute interval that contains the maximum number of steps
intervale_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
intervale_max_steps

#The total number of missing values in the dataset (for each variable):

#For the "steps" variable:
sum(is.na(as.character(data$steps)))

#For the "date" variable:
sum(is.na(as.character(data$date)))

#For the "interval" variable:
sum(is.na(as.character(data$interval)))

#The strategy for filling in all of the missing values in the dataset. Missing values are replaced by the mean of that 5-minute interval.
#finding the indices of missing values (NAs)
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
#Imputing missing values using the mean for that 5-minute interval
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
        steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
}))

#Checking the complete data with the summary and str methods
summary(complete_data)
str(complete_data)

#Making a histogram of the total number of steps taken each day for the complete dataset.
#Creating a data frame with the steps taken for each day
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
#Adding column names to the created data frame
colnames(steps_each_day_complete) <- c("date", "steps")

#Making the histogram
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

#Calculating the mean and median total number of steps taken per day for the complete dataset.
mean(steps_each_day_complete$steps)
median(steps_each_day_complete$steps)

#Are there differences in activity patterns between weekdays and weekends?
#Creating a factor variable "day "to store the day of the week:
complete_data$day <- as.factor(weekdays(complete_data$date))

#Creating a logical variable "is_weekday" (weekday=TRUE, weekend = FALE) :
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 


#Calculating the average number of steps for weekdays
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)


#Calculating the average number of steps for weekends
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

#Adding columns names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
#Adding a column to indecate the day
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

#Merging the two togather
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)
#Converting the day variabke to a factor
week_data$day <- as.factor(week_data$day)

#Making the plot
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")

#The plot shows that that activity on the weekends tends to be more spread out over the day compared to the weekdays.
