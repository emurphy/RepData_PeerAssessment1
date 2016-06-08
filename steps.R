activities <- read.csv(unz('activity.zip', 'activity.csv'))

steps_per_day <- aggregate(activities$steps, by=list(activities$date), sum)

names(steps_per_day) = c("date", "steps") 
hist(steps_per_day$steps)

summary(steps_per_day)

avg_steps_per_interval <- aggregate(activities$steps, by=list(activities$interval), mean, na.rm=TRUE)
names(avg_steps_per_interval) = c("interval", "steps") 
plot(avg_steps_per_interval, type="l")

avg_steps_per_interval[which.max(avg_steps_per_interval$steps),]

sum(is.na(activities$steps))

library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
filled_activities <- ddply(activities, ~ interval, transform, steps = impute.mean(steps))

filled_activities <- filled_activities[order(filled_activities$date, filled_activities$interval), ] #plyr orders by group so we have to reorder
