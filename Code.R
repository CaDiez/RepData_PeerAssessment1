# Load the data
dataActivity <- read.csv(file="activity.csv")
# Convert date field from factor to date
dataActivity$date <- as.Date(dataActivity$date)

library("dplyr")
dayStepCount <- dataActivity %>% group_by(date) %>% summarise(day.count = sum(steps))

ggplot(dayStepCount) + geom_histogram(aes(x = day.count), binwidth = 1000) + 
        theme_bw() + ggtitle('Total number of steps per day') + xlab('Number of steps per day')

# Mean and median number of steps/day
meanStepCount <- dayStepCount %>% ungroup() %>% 
        summarise(mean = mean(day.count, na.rm = T), median = median(day.count, na.rm = T))
meanStepCount