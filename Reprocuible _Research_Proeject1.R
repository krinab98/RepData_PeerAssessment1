library(ggplot2)
#load the dataset
activity<-read.csv("activity.csv")
dim(activity)
summary(activity)
names(activity)
head(activity)

#total no of  steps per day
stepsperday <- aggregate(steps~date,activity,FUN=sum, na.rm=TRUE)
hist(stepsperday$steps)

#mean steps
meanStepsPerDay <- mean(stepsperday$steps)
meanStepsPerDay

#median steps
medianStepsPerDay <- median(stepsperday$steps)
medianStepsPerDay


#question 2 average daliy pattern
stepsPerInterval<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
stepsPerInterval
plot(steps~interval, data=stepsPerInterval, type="l",col="darkblue",lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps


#question 3
sum(is.na(activity$steps))



## values without NA are imputed in a new column

activity$CompleteSteps <- ifelse(is.na(activity$steps), round(stepsPerInterval$steps[match(activity$interval, stepsPerInterval$interval)],0), activity$steps)
# new dataset activityFull
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
# see first 10 values of the new dataset
head(activityFull, n=10)

# prepare data
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
# draw the histogram
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))

# Mean
mean(StepsPerDayFull$Steps)

#Median
median(StepsPerDayFull$Steps)

# Create variable with date in correct format
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")

# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activityFull, n=10)


# create table with steps per time across weekdaydays or weekend days
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTimeDT$time <- stepsPerInterval$interval/100
# draw the line plot
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
