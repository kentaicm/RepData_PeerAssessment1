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
NAnumbers <- is.na(as.character(data$steps))
datanoNA <- data[!NAnumbers,]
head(datanoNA)

#Aggregating the number of steps taken each day
#Creating a data frame with the steps taken for each day
stepseachday <- aggregate(steps ~ date, data = datanoNA, sum)
#Adding column names to the created data frame
colnames(stepseachday) <- c("date", "steps")

#Making a histogram of the total number of steps taken each day.
hist(as.numeric(stepseachday$steps), breaks = 20, col = "Blue", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

#Calculating the mean and median total number of steps taken per day.
mean(stepseachday$steps)
median(stepseachday$steps)

##What is the average daily activity pattern?
#Calculating the average number of steps taken, averaged across all days.
#Calculating the average
stepsperinterval <- aggregate(datanoNA$steps, by=list(interval=datanoNA$interval), FUN=mean)

#Adding columns names
colnames(stepsperinterval) <- c("interval", "averagesteps")

#ploting the average daily activity pattern 
plot(as.integer(levels(stepsperinterval$interval)), stepsperinterval$averagesteps, type="l",xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

#The maximum number of average steps
maxsteps <- max(stepsperinterval$averagesteps)
maxsteps

#The 5-minute interval that contains the maximum number of steps
intervalemaxsteps<-stepsperinterval[which.max(stepsperinterval$averagesteps),]$interval
intervalemaxsteps

#The total number of missing values in the dataset (for each variable):

#For the "steps" variable:
sum(is.na(as.character(data$steps)))

#For the "date" variable:
sum(is.na(as.character(data$date)))

#For the "interval" variable:
sum(is.na(as.character(data$interval)))

#The strategy for filling in all of the missing values in the dataset. Missing values are replaced by the mean of that 5-minute interval.
#finding the indices of missing values (NAs)
NAnumbers <- which(is.na(as.character(data$steps)))
completedata <- data
#Imputing missing values using the mean for that 5-minute interval
completedata[NAnumbers, ]$steps<-unlist(lapply(NAnumbers, FUN=function(NAnumbers){
        stepsperinterval[data[NAnumbers,]$interval==stepsperinterval$interval,]$averagesteps
}))

#Checking the complete data with the summary and str methods
summary(completedata)
str(completedata)

#Making a histogram of the total number of steps taken each day for the complete dataset.
#Creating a data frame with the steps taken for each day
stepseachdaycomplete <- aggregate(steps ~ date, data = completedata, sum)
#Adding column names to the created data frame
colnames(stepseachdaycomplete) <- c("date", "steps")

#Making the histogram
hist(as.numeric(stepseachdaycomplete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

#Calculating the mean and median total number of steps taken per day for the complete dataset.
mean(stepseachdaycomplete$steps)
median(stepseachdaycomplete$steps)

#Are there differences in activity patterns between weekdays and weekends?
#Creating a factor variable "day "to store the day of the week:
completedata$day <- as.factor(weekdays(completedata$date))

#Creating a logical variable "isweekday" (weekday=TRUE, weekend = FALE) :
completedata$isweekday <- ifelse(!(completedata$day %in% c("Saturday","Sunday")), TRUE, FALSE) 


#Calculating the average number of steps for weekdays
weekdaysdata <- completedata[completedata$isweekday,]
stepsperintervalweekdays <- aggregate(weekdaysdata$steps, by=list(interval=weekdaysdata$interval), FUN=mean)


#Calculating the average number of steps for weekends
weekendsdata <- completedata[!completedata$isweekday,]
stepsperintervalweekends <- aggregate(weekendsdata$steps, by=list(interval=weekendsdata$interval), FUN=mean)

#Adding columns names
colnames(stepsperintervalweekdays) <- c("interval", "averagesteps")
colnames(stepsperintervalweekends) <- c("interval", "averagesteps")
#Adding a column to indecate the day
stepsperintervalweekdays$day <- "Weekday"
stepsperintervalweekends$day <- "Weekend"

#Merging the two togather
weekdata <- rbind(stepsperintervalweekends, stepsperintervalweekdays)
#Converting the day variabke to a factor
weekdata$day <- as.factor(weekdata$day)

#Making the plot
library(lattice)
xyplot(averagesteps ~  interval | day, data = weekdata, layout = c(1,2), type ="l", ylab="Number of Steps")

#The plot shows that that activity on the weekends tends to be more spread out over the day compared to the weekdays.
