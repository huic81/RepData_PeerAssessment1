############################################################################################
#The purpose of this project is to write a report that answers the questions detailed below. 
#Ultimately, you will need to complete the entire assignment in a single R markdown document 
#that can be processed by knitr and be transformed into an HTML file. 
#Final output required to submit:
#1) a PA1_template.Rmd.
#2) PA1_template.md and PA1_template.html files produced by processing your R markdown file 
#   with knit2html() function in R (from the knitr package) by running the function from the 
#   console.
#3) any figures included

#This PA1.R does the following. 
#1.Code for reading in the dataset and/or processing the data
#2.Histogram of the total number of steps taken each day
#3.Mean and median number of steps taken each day
#4.Time series plot of the average number of steps taken
#5.The 5-minute interval that, on average, contains the maximum number of steps
#6.Code to describe and show a strategy for imputing missing data
#7.Histogram of the total number of steps taken each day after missing values are imputed
#8.Panel plot comparing the average number of steps taken per 5-minute interval across 
#  weekdays and weekends
#9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
############################################################################################

# Preparation Steps
############################################################################################
## Install Pre-requisites
############################################################################################
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(Hmisc)
library(lattice)

############################################################################################
##Set working directory
############################################################################################
setwd("~/Git/RepData_PeerAssessment1")

# Asisgnment Steps
############################################################################################
## STEP 1: Loading and preprocessing the data
############################################################################################
###1.Load the data (i.e. read.csv())
#### download zip file from website if not exists
if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfilepath <- "./activity.zip"
if(!file.exists(destfilepath)) {
        download.file(fileUrl,destfile = destfilepath)
}

#### unzip data
zipfile <- unzip(destfilepath, exdir = "./data")

#### load the data
activityData <- read.csv("./data/activity.csv")

###2.Process/transform the data (if necessary) into a format suitable for your analysis
#### Transform the date attribute to date format instead of factor
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")

############################################################################################
## STEP 2: What is mean total number of steps taken per day?
############################################################################################
###For this part of the assignment, we can ignore the missing values in the dataset.
###1.Calculate the total number of steps taken per day
activityDataNoNA <- na.omit(activityData) 
activityStepsByDay <- aggregate(steps ~ date, activityDataNoNA, sum)
colnames(activityStepsByDay) <- c("date","totalSteps")
head(activityStepsByDay)

###2.If you do not understand the difference between a histogram and a barplot, 
###research the difference between them. 
###Make a histogram of the total number of steps taken each day
hist(activityStepsByDay$totalSteps, breaks = 25, xlab="Total Number of Steps", 
     main = "Histogram of Total Number of Steps Each Day", col = "blue")

###3.Calculate and report the mean and median of the total number of steps taken per day
TotalStepsByDayMean   <- mean(activityStepsByDay$totalSteps, na.rm=TRUE)
TotalStepsByDaymedian <- median(activityStepsByDay$totalSteps, na.rm=TRUE)

############################################################################################
## STEP 3: What is the average daily activity pattern?
############################################################################################
###1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
###average number of steps taken, averaged across all days (y-axis)

###Get the average of the interval across all days
activityStepsInterval <- aggregate(steps ~ interval, activityDataNoNA, mean)
colnames(activityStepsInterval) <- c("interval","meanSteps")
head(activityStepsInterval)

###make the time series plot
plot(x = activityStepsInterval$interval, y = activityStepsInterval$meanSteps,
        type = "l", col = "blue", 
        main = "Time Series Plot of Each of The 5-Minute Interval\n and the Average Number of Steps Taken Across All Days",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps Taken,\n Across All Days"
)

###2.Which 5-minute interval, on average across all the days in the dataset, contains the 
###maximum number of steps?
maxAvgStepsIndex <- which.max(activityStepsInterval$meanSteps)
activityStepsInterval[maxAvgStepsIndex,]
activityStepsInterval[maxAvgStepsIndex,]$interval

############################################################################################
## STEP 4: Imputing missing values
############################################################################################
###Note that there are a number of days/intervals where there are missing values (coded as 
###NA). The presence of missing days may introduce bias into some calculations or summaries 
###of the data.

###1.Calculate and report the total number of missing values in the dataset (i.e. the total 
###number of rows with NAs)
nrow(activityData[is.na(activityData$steps),])

###2.Devise a strategy for filling in all of the missing values in the dataset. The strategy 
###does not need to be sophisticated. For example, you could use the mean/median for that 
###day, or the mean for that 5-minute interval, etc.
###We will use the mean for the 5-minute interval to fill in all the missing values.

###3.Create a new dataset that is equal to the original dataset but with the missing data 
###filled in.
###use impute function to fill NA data with mean (average)
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
###check if all NAs rows was imputed
nrow(activityData[is.na(activityDataImputed$steps),])

###4.Make a histogram of the total number of steps taken each day and Calculate and report 
###the mean and median total number of steps taken per day. Do these values differ from the 
###estimates from the first part of the assignment? 
###What is the impact of imputing missing data on the estimates of the total daily number 
###of steps?

###Calculate the total number of steps taken per day
activityStepsByDayImputed <- aggregate(steps ~ date, activityDataImputed, sum)
colnames(activityStepsByDayImputed) <- c("date","totalSteps")
head(activityStepsByDayImputed)

###Make a histogram of the total number of steps taken each day
hist(activityStepsByDayImputed$totalSteps, breaks = 25, xlab="Total Number of Steps", 
     main = "Histogram of Total Number of Steps Each Day (Imputed)", col = "blue")

###Calculate and report the mean and median of the total number of steps taken per day
TotalImputedStepsByDayMean   <- mean(activityStepsByDayImputed$totalSteps, na.rm=TRUE)
TotalImputedStepsByDaymedian <- median(activityStepsByDayImputed$totalSteps, na.rm=TRUE)

###Difference of Mean between without missing value and imputed value
###Original Value: 'r TotalStepsByDayMean'
TotalStepsByDayMean
###Impputed Value: 'r TotalImputedStepsByDayMean'
TotalImputedStepsByDayMean

###Difference of Median between without missing value and imputed value
###Original Value: 'r TotalStepsByDaymedian'
TotalStepsByDaymedian
###Impputed Value: 'r TotalImputedStepsByDaymedian'
TotalImputedStepsByDaymedian

############################################################################################
## STEP 5: Are there differences in activity patterns between weekdays and weekends?
############################################################################################
###For this part the weekdays() function may be of some help here. Use the dataset with the 
###filled-in missing values for this part.
###1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
###indicating whether a given date is a weekday or weekend day.
activityDataWEWD <- activityDataImputed
#activityDataWEWD$weekday <- weekdays(activityDataWEWD$date)
activityDataWEWD$daytype <- factor(weekdays(activityDataWEWD$date))
levels(activityDataWEWD$daytype)
levels(activityDataWEWD$daytype)<-list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                "Friday")
                                    ,weekend = c("Saturday", "Sunday"))
levels(activityDataWEWD$daytype)
str(activityDataWEWD)
#head(activityDataWEWD)
table(activityDataWEWD$daytype)

###2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
###interval (x-axis) and the average number of steps taken, averaged across all weekday days 
###or weekend days (y-axis). See the README file in the GitHub repository to see an example 
###of what this plot should look like using simulated data.

###Get the average of the interval across all weekday days & weekend days
activityStepsIntervalWEWD <- aggregate(steps ~ interval + daytype, activityDataWEWD, mean)
colnames(activityStepsIntervalWEWD) <- c("interval","daytype", "meanSteps")
head(activityStepsIntervalWEWD)

## plot time series
xyplot( meanSteps ~ interval | daytype,
        activityStepsIntervalWEWD,
        type = "l",
        layout = c(1,2),
        main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days",
        xlab = "5-minute interval",
        ylab = "Average Number of Steps Taken"
)

