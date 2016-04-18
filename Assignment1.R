rm(list = setdiff(ls(), lsf.str())) #Clear out the global environment
library('knitr')

wdir <- readline(prompt = "Enter directory that contains (activity.csv) here: ")
setwd(wdir)

org_data <- read.csv('activity.csv')

org_data$date <- as.Date(org_data$date)

#Part1
#Summary of total number of steps taken per day
#Used frequency because labels look cleaner on the y-axis
sum_steps <- aggregate(x = org_data$steps,by = list(org_data$date),FUN = sum)
colnames(sum_steps) <- c('date','total_steps')
hist_total_steps <- hist(x = sum_steps$total_steps,xlab = 'Total Steps per Day',freq = TRUE,main = 'Histogram of Total Steps per Day')
summary_total_steps <- matrix(0,1,2)
summary_total_steps[1,] <- c(mean(sum_steps$total_steps,na.rm = TRUE),median(sum_steps$total_steps,na.rm = TRUE))
summary_total_steps <- as.data.frame(summary_total_steps)
colnames(summary_total_steps) <- c('mean','median')
print(summary_total_steps)

#Part2
unique_interval <- unique(org_data$interval)

#The following section of the script collects the average steps taken per interval recorded.
#This might not be the most efficient way of computing this information
mean_interval <- matrix(0,length(unique_interval),2)
mean_interval[,1] <- unique_interval

for(i in 1:length(unique_interval)){
  interval <- unique_interval[i]
  temp <- org_data[which(org_data$interval == interval),]
  ave <- mean(temp$steps,na.rm = TRUE)
  mean_interval[i,2] <- ave
}

mean_interval <- as.data.frame(mean_interval)
colnames(mean_interval) <- c('interval','mean')

plot(x = mean_interval$interval,y = mean_interval$mean,type = 'l',xlab = 'Interval',
     ylab = 'Average Number of Steps',main = 'Interval ~ Average Number of Steps', col = 'blue')

max_interval <- mean_interval[which(mean_interval$mean == max(mean_interval$mean)),]
colnames(max_interval) <- c('Interval_Max_Steps','Max_mean')
print(max_interval)


#Number of missing data
missing_data <- nrow(org_data[which(is.na(org_data$steps)),])
print('Number of Missing Data Points')
print(missing_data)

#Impute the missing values
impute_data <- org_data
for(interval in unique_interval){
  val <- round(mean_interval[which(mean_interval$interval == interval),2],0)
  impute_data[which(is.na(impute_data$steps) & impute_data$interval == interval),]$steps <- val  
}

#New histogram and summary stats with the imputed data set
new_sum_steps <- aggregate(x = impute_data$steps,by = list(impute_data$date),FUN = sum)
colnames(new_sum_steps) <- c('date','total_steps')
new_hist_total_steps <- hist(x = new_sum_steps$total_steps,xlab = 'Total Steps per Day',freq = TRUE,main = 'Histogram of Total Steps per Day')
new_summary_total_steps <- matrix(0,1,2)
new_summary_total_steps[1,] <- c(mean(new_sum_steps$total_steps),median(new_sum_steps$total_steps))
new_summary_total_steps <- as.data.frame(new_summary_total_steps)
colnames(new_summary_total_steps) <- c('mean','median')
print(new_summary_total_steps)

#Imputing seems to reduce mean and median. This might be due to the significant number of 
#intervals with low or zero activity 
impute_data$date <- as.Date(impute_data$date)
impute_data$day <- weekdays(impute_data$date)
impute_data$flag <- ifelse(impute_data$day == 'Saturday' | impute_data$day == 'Sunday', 'weekend','weekday')
impute_data$flag <- factor(impute_data$flag)

mean_w_interval <- aggregate(x = impute_data$steps, by = list(impute_data$flag,impute_data$interval),FUN = mean)
colnames(mean_w_interval) <- c('flag','interval','mean')

g <- ggplot(mean_w_interval,aes(x = interval,y = mean))
g + geom_line(aes(colour = flag)) + facet_grid(flag~.)

par(mfrow = c(2,1))
plot(x = mean_w_interval[mean_w_interval$flag == "weekday",]$interval,y = mean_w_interval[mean_w_interval$flag == "weekday",]$mean,
     type = 'l',col = 'blue',xlab = 'weekday interval',ylab = 'mean' )
plot(x = mean_w_interval[mean_w_interval$flag == "weekend",]$interval, y = mean_w_interval[mean_w_interval$flag == "weekend",]$mean,
     type = 'l',col = 'red',xlab = 'weekend interval',ylab = 'mean')
