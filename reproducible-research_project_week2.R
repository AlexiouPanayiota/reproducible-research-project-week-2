data=read.csv("activity.csv")
str(data)


data$date=as.Date.factor(data$date)

weekday<- weekdays(data$date)
activity <-cbind(data, weekday)

summary(data)

tsteps<- with(data, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))

names(tsteps)<- c("dates", "steps")

hist(tsteps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))

mean(tsteps$steps)
median(tsteps$steps)

average.daily.activity<- aggregate(data$steps, by= list(data$interval), FUN = mean , na.rm = TRUE)

names(average.daily.activity)<-c("interval", "mean")

plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")


average.daily.activity[which.max(average.daily.activity$mean),]$interval

#3

sum(is.na(data$steps))

clean.steps<- average.daily.activity$mean[match(data$interval,average.daily.activity$interval)]

data.clean <- transform(data, steps = ifelse(is.na(data$steps), yes = clean.steps, no = data$steps))

total.clean.steps<- aggregate(steps ~ date, data.clean, sum)

names(total.clean.steps)<- c("date", "daily.steps")

hist(total.clean.steps$daily.steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

mean(total.clean.steps$daily.steps)



median(total.clean.steps$daily.steps)
library(ggplot2)

#4
# Create variable with date in correct format
data.clean$RealDate <- as.Date(data.clean$date, format = "%Y-%m-%d")
# create a variable with weekdays name
data.clean$weekday <- weekdays(data.clean$RealDate)
# create a new variable indicating weekday or weekend
data.clean$DayType <- ifelse(data.clean$weekday=='Σάββατο' | data.clean$weekday=='Κυριακή', 'weekend','weekday')
# see first 10 values
head(data.clean, n=10)

# create table with steps per time across weekdaydays or weekend days
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=data.clean,FUN=mean,na.action=na.omit)
StepsPerTime <- aggregate(steps~interval,data=data,FUN=mean,na.action=na.omit)

StepsPerTime$time <- StepsPerTime$interval/100

# variable time (more comprensible for the graph axis)
StepsPerTimeDT$time <- StepsPerTime$interval/100
# draw the line plot
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+
  ggtitle("Average steps per time interval: weekdays vs. weekends")+
  xlab("Time")+ylab("Steps")+
  theme(plot.title = element_text(face="bold", size=12))+
  facet_grid(DayType ~ .)