#Script for assignament 1 Reproducible Data, there is a MKD file associated with this Sctipt


#Read data from working directory
data <- read.csv("activity.csv", sep=",",  header=TRUE)

#Processing  date
data$date <- as.Date(data$date, "%Y-%m-%d")

#Processing data

library(dplyr)
sumSteps <- data %>% group_by(date) %>% summarise(sum(steps))
colnames(sumSteps) <- c("Date", "Sum")

avgSteps <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm=TRUE))
colnames(avgSteps) <- c("Interval", "Avg")

avgDaily <- avgSteps$Avg
stepsTS <- ts(avgDaily)

plot.ts(stepsTS)

which(avgDaily == max(avgDaily))


#Creating histogram
br <- sumSteps$Date
png("histogram1.png")
hist(sumSteps$Sum, col="red", main= "Histogram of Total Steps per day", xlab="Total Daily Steps", breaks=10)
dev.off()


#Checking the amount of missing values

table(is.na(data))

#Which variable has mnissing values?
any(is.na(data$steps))
any(is.na(data$date))
any(is.na(data$interval))

######################

newData <- data

newData <- newData %>% 
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
        
#Creating histogram with new data set

newSumSteps <- newData %>% group_by(date) %>%  summarise(sum = sum(steps))



png("histogram2.png")
hist(newSumSteps$sum,
     col="blue", 
     main= "Histogram of Total Steps per day", 
     xlab="Total Daily Steps", 
     breaks=10)
dev.off()

# checking for weekends

Sys.setlocale("LC_TIME", "English")
newData <- newData %>% group_by(date) %>% mutate(weekend= isWeekend(as.timeDate(date)))

        
newData <- mutate(newData, day = ifelse(weekend, "Weekend", "Weekday"))

wplot <- newData %>%
        group_by(interval, day) %>%
        summarise(avgEnd = mean(steps, na.rm=TRUE))


library(lattice)

png("panelplot.png")
xyplot(avgEnd ~ interval | day, data = wplot, type = "l", layout = c(1,2))
dev.off()




