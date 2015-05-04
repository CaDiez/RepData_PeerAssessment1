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
