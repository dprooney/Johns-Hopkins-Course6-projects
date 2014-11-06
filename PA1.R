# Loading and preprocessing

  rawData <- read.csv("activity.csv",stringsAsFactors = FALSE)
  
  # Change the date variable into a date class rather than character  
  rawData$date <- as.Date(rawData$date)   

  # Create a separate data set that strips the na rows from the raw data
  hasSteps <- rawData[!is.na(rawData$steps),]
    
  # Create a complementary data set of intervals lacking data
  gaps <- rawData[is.na(rawData$steps),c(2,3)]

  # Create a vector of interval identifiers
  intervals <- rawData$interval[1:288]
  
# Mean daily steps

  # Calculate the total number of steps per day
  dailySteps <- as.vector(tapply(hasSteps$steps,hasSteps$date,sum))

  # Plot the histogram
  library(ggplot2)
  dailyHist <- qplot(dailySteps, binwidth = 1000, geom="histogram")
  dailyHist <- dailyHist + labs(title="Number of steps per day")
  dailyHist <- dailyHist + labs(x="Number of steps", y="Count")
  
  # Calculate mean and median
  summary(dailySteps,digits=7)
  
# Daily activity

  # Calculate the average steps per interval
  intervalSteps <- aggregate(steps~interval,hasSteps,mean)

  # Plot the time series
  dailyAct <- qplot(intervals,steps,data=intervalSteps,geom="line")
  dailyAct <- dailyAct + labs(title="Daily activity pattern")
  dailyAct <- dailyAct + labs(x="Start of interval (in military time)", y="Average number of steps")
  
  # Find the maximum
  intervalSteps$interval[which.max(intervalSteps$steps)]
  max(intervalSteps$steps)
  
# Missing values

  # Calculate total number of NA's
  nrow(gaps)
  
  # Fill in gaps by using the means for each interval
  completedData <- rawData
  repIS <- rep(intervalSteps$steps,61)
  completedData$steps[is.na(completedData$steps)] <- repIS[is.na(completedData$steps)]

  # Calculate the new total number of steps
  completedSteps <- as.vector(tapply(completedData$steps,completedData$date,sum))
  
  # Plot the new histogram
  completedHist <- qplot(completedSteps, binwidth = 1000, geom="histogram")
  completedHist <- completedHist + labs(title="Number of steps per day")
  completedHist <- completedHist + labs(x="Number of steps", y="Count")
  
  # Calculate mean and median
  summary(completedSteps,digits=7)
  
# Weekdays and weekends

  # Create boolean variable indicating whether a date is on the weekend
  onWeekend <- weekdays(completedData$date) %in% c("Saturday","Sunday")
  dayOrEnd <- rep("Weekday",length(completedData$date))
  dayOrEnd[onWeekend] <- "Weekend"
  completedData <- cbind(completedData,dayOrEnd)
  
  # Find mean for intervals on weekdays and weekends
  DorEintervalSteps <- aggregate(steps~interval+dayOrEnd,completedData,mean)
  
  # Create panel plot separating weekend and weekday data
  deplot <- qplot(interval,steps,data=DorEintervalSteps,facets=.~dayOrEnd,geom="line")
  deplot <- deplot + labs(title="Weekday and weekend activity patterns")
  deplot <- deplot + labs(x="Start of interval (in military time)", y="Average number of steps")