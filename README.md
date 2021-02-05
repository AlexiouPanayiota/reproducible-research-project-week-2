# Code to describe and show a strategy for imputing missing data
Check how many na's are in the steps of activity data 
sum(is.na(data$steps))

clean.steps<- average.daily.activity$mean[match(data$interval,average.daily.activity$interval)]
#clean the data from na's
#make a new data frame data.clean and put all the steps from activity data that aren't na's
data.clean <- transform(data, steps = ifelse(is.na(data$steps), yes = clean.steps, no = data$steps))
