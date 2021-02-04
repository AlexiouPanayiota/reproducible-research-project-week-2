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


#4


data$datetype <- sapply(data$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

activity.datetype<- aggregate(steps~interval+datetype, data,mean, na.rm =TRUE)
library(ggplot2)
ggplot(activity.datetype, aes(x = interval, y = steps, color = datetype))+ 
  geom_line() + labs(title = "Average daily steps by date type", x = "Interval", y = "Average number of steps") + facet_wrap(~datetype, ncol = 1, nrow = 2) 


