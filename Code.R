# Load the data
dataActivity <- read.csv(file="activity.csv")
# Convert date field from factor to date
dataActivity$date <- as.Date(dataActivity$date)

library("dplyr")
library("ggplot2")
dayStepCount <- dataActivity %>% group_by(date) %>% summarise(day.count = sum(steps))

ggplot(dayStepCount) + geom_histogram(aes(x = day.count), binwidth = 1000) + 
        theme_bw() + ggtitle('Total number of steps per day') + xlab('Number of steps per day')

# Mean and median number of steps/day
meanStepCount <- dayStepCount %>% ungroup() %>% 
summarise(mean = mean(day.count, na.rm = T), median = median(day.count, na.rm = T))
meanStepCount

tidyData <- dataActivity[!is.na(dataActivity$date) & !is.na(dataActivity$steps),]
plotData <- tapply(tidyData$steps, tidyData$interval, mean)
plot(names(plotData), plotData,type="l", xlab = "Interval", ylab = "Average Steps")

summary(dataActivity)
# Mean step count per inteval
MeanActivity <- dataActivity %>% group_by(interval) %>% 
        summarise(meansteps = mean(steps, na.rm = T))
dataActivity.Imputed <- left_join(dataActivity, MeanActivity)
# If step is NA change to meansteps
dataActivity.Imputed <- dataActivity.Imputed %>% mutate(steps = ifelse(is.na(steps),round(meansteps,0),steps))

# Number of steps/day imputed
dayStepCount.imp <- dataActivity.Imputed %>% group_by(date) %>% 
        summarise(day.count = sum(steps))

# Mean and median number of steps/day imputed
meanStepCount.imp <- dayStepCount.imp %>% ungroup() %>% 
        summarise(mean = mean(day.count, na.rm = T), median = median(day.count, na.rm = T))

ggplot(dayStepCount.imp) + geom_histogram(aes(x = day.count), binwidth = 500) + 
        ggtitle('Histogram of total number of steps taken each day\nAfter imputation') + theme_bw() +
        xlab('Daily step count')

day.type <- function(date) {
        if (weekdays(date) %in% c('Saturday', 'Sunday')) {
                return('weekend')
        } else {
                return('weekday')
        }
}

# set the locale for english weekdays
Sys.setlocale("LC_TIME", "English")
#Make dates dateobjects and generate weekday field
dataActivity.wd <- dataActivity.Imputed %>% mutate(date = as.Date(date), weekDay = weekdays(date)) %>%
# Calculate if weekend or weekday to create factor
mutate(weekEnd.day = ifelse(weekDay %in% c('Saturday','Sunday'),'weekEnd','weekDay'))
# Reorder weekend/weekday factor
dataActivity.wd$weekEnd.day <- relevel(factor(dataActivity.wd$weekEnd.day), ref = 'weekEnd')
# Average number of steps during weekend/weekdays
dataActivity.wd.mean <- dataActivity.wd %>% group_by(interval,weekEnd.day) %>%
        summarise(mean.steps = mean(steps, na.rm = T))

ggplot(dataActivity.wd.mean) + geom_line(aes(x = interval, y = mean.steps)) + 
        facet_wrap(~weekEnd.day,nrow = 2) + theme_bw() + 
        ggtitle('WeekEnd & WeekDay Patterns') + 
        xlab('Interval') +
        ylab('Mean number of steps')
