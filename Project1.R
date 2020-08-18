#Loading the required packages and the dataset
library("data.table")
library(ggplot2)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

activityDT <- data.table::fread(input = "data/activity.csv")

#Calculating the total number of steps
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
head(Total_Steps, 10)

#Histogram of total number of steps
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]


#Plot of average daily activity pattern
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(IntervalDT, aes(x = interval , y = steps)) + 
  geom_line(size=1) + labs(title = "Average Daily Steps", x = "Interval", y = "Average Steps per day") + theme(plot.title = element_text(hjust = 0.5))

IntervalDT[steps == max(steps), .(max_interval = interval)]

#Finding the actual missing values
nrow(activityDT[is.na(steps),])

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]


data.table::fwrite(x = activityDT, file = "data/tidyData.csv", quote = FALSE)


#Calculating the total number of steps taken per day
Total_Steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

#Calculating the mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

ggplot(Total_Steps, aes(x = steps)) + 
  geom_histogram(binwidth = 1000) + 
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")+theme(plot.title = element_text(hjust = 0.5))

#Computing the difference in activity patters

activityDT <- data.table::fread(input = "data/activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)

#-----------------------------------------------------------
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

#Plot comparing the Average Daily steps by weektype
ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + 
  geom_line() + 
  labs(title = "Average Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + 
  facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2) + theme(plot.title = element_text(hjust = 0.5))


