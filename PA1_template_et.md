# PA1_template
Reproducible Research - Assignment 1 Walkthrough

Assuming the user has access to assignment data 'activity.csv' the code requires user to input the folder where the data lives


```r
library('ggplot2')
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
wdir <- 'C:/Users/i52062/Desktop/Stats/Reproducible Research_5/Assignment1'
setwd(wdir)
```

Upon entry of the file path, the folder which contains becomes the working directory. Data is loaded via:


```r
org_data <- read.csv('activity.csv')
org_data$date <- as.Date(org_data$date)
```
Second line converts character field to date for easy processing.

Part 1
The total number of steps is calculated using the following script


```r
sum_steps <- aggregate(x = org_data$steps,by = list(org_data$date),FUN = sum)
colnames(sum_steps) <- c('date','total_steps')
```

The first histogram is produced via:


```r
hist(x = sum_steps$total_steps,xlab = 'Total Steps per Day',freq = TRUE,main = 'Histogram of Total Steps per Day')
```

![](PA1_template_et_files/figure-html/unnamed-chunk-4-1.png)

Further processing to obtain summary stats of total number of steps per day:


```r
summary_total_steps <- matrix(0,1,2)
summary_total_steps[1,] <- c(mean(sum_steps$total_steps,na.rm = TRUE),median(sum_steps$total_steps,na.rm = TRUE))
summary_total_steps <- as.data.frame(summary_total_steps)
colnames(summary_total_steps) <- c('mean','median')
print(summary_total_steps)
```

```
##       mean median
## 1 10766.19  10765
```

The script reports
Mean of total steps per day: 10766.19
Median total steps per day: 10765

The dataframe that contains steps taken per interval averaged across days is computed through the following scripts:


```r
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
```

The resulting plot is obtained through:


```r
plot(x = mean_interval$interval,y = mean_interval$mean,type = 'l',xlab = 'Interval',
     ylab = 'Average Number of Steps',main = 'Interval ~ Average Number of Steps', col = 'blue')
```

![](PA1_template_et_files/figure-html/unnamed-chunk-7-1.png)

The maximum average steps taken per interval is computed here:


```r
max_interval <- mean_interval[which(mean_interval$mean == max(mean_interval$mean)),]
colnames(max_interval) <- c('Interval_Max_Steps','Max_mean')
print(max_interval)
```

```
##     Interval_Max_Steps Max_mean
## 104                835 206.1698
```

The number of rows that contains missing data is reported here:
Please note that the only field that contains missing data is the $steps field


```r
missing_data <- nrow(org_data[which(is.na(org_data$steps)),])
print(missing_data)
```

```
## [1] 2304
```

For data imputing, the intervals with missing data were populated by calculating the mean of the intervals across all days and replacing the NAs with the mean interval values.


```r
impute_data <- org_data
for(interval in unique_interval){
  val <- round(mean_interval[which(mean_interval$interval == interval),2],0)
  impute_data[which(is.na(impute_data$steps) & impute_data$interval == interval),]$steps <- val  
}
```

Here the variable impute_data is the new data frame with imputed values instead of missing values. 
The dimensions of the new data frame is same as the original loaded data.


```r
nrow(impute_data) - nrow(org_data)
```

```
## [1] 0
```

The following data frame is created to get the new histogram:


```r
new_sum_steps <- aggregate(x = impute_data$steps,by = list(impute_data$date),FUN = sum)
colnames(new_sum_steps) <- c('date','total_steps')
```

The second histogram is created via:


```r
hist(x = new_sum_steps$total_steps,xlab = 'Total Steps per Day',freq = TRUE,main = 'Histogram of Total Steps per Day')
```

![](PA1_template_et_files/figure-html/unnamed-chunk-13-1.png)

Further processing is done to compute the summarys statistics:


```r
new_summary_total_steps <- matrix(0,1,2)
new_summary_total_steps[1,] <- c(mean(new_sum_steps$total_steps),median(new_sum_steps$total_steps))
new_summary_total_steps <- as.data.frame(new_summary_total_steps)
colnames(new_summary_total_steps) <- c('mean','median')
print(new_summary_total_steps)
```

```
##       mean median
## 1 10765.64  10762
```

The reported mean and median
Mean of total steps per day (imputed data): 10765.64
Median total steps per day (imputed data): 10762
I concluded the reduction in the statistics happen due to introduction of low activity intervals that were previously omitted from.

The follwoing code introduces new fields to impute_data data frame to carry out further analysis:


```r
impute_data$date <- as.Date(impute_data$date)
impute_data$day <- weekdays(impute_data$date)
impute_data$flag <- ifelse(impute_data$day == 'Saturday' | impute_data$day == 'Sunday', 'weekend','weekday')
impute_data$flag <- factor(impute_data$flag)
```

The following set of instructions create line chart that plots average steps per interval for weekends and weekdays


```r
mean_w_interval <- aggregate(x = impute_data$steps, by = list(impute_data$flag,impute_data$interval),FUN = mean)
colnames(mean_w_interval) <- c('flag','interval','mean')

g <- ggplot(mean_w_interval,aes(x = interval,y = mean))
g + geom_line(aes(colour = flag)) + facet_grid(flag~.)
```

![](PA1_template_et_files/figure-html/unnamed-chunk-16-1.png)



