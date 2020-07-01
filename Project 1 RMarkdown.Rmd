---
title: "Project 1 RMarkdown"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Reading the data

```{r}
setwd("C:/Users/drggshetty/Documents/R/Reproducible Research/Project 1")
data <- read.csv("activity.csv")
```

What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day

```{r}
steps_by_day<-aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = ("Total Steps Each Day"), col="pink",xlab="Number of Steps")

steps_by_day_mean<-mean(steps_by_day$steps)
steps_by_day_mean
steps_by_day_median<-median(steps_by_day$steps)
steps_by_day_median
```

What is the average daily activity pattern?
Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval, main="Mean of number of steps in each interval in each day",
     ylab="Number of steps",xlab="Interval", 
     type="l",col="blue")

max_steps_by_interval<-steps_by_interval[which.max(steps_by_interval$steps),1]
max_steps_by_interval
```

Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
no_of_NA<-sum(is.na(data))
no_of_NA

steps_mean <- aggregate(steps ~ interval, data, mean)

NA_steps <- numeric()
for (i in 1:nrow(data)) 
{
  row_obs <- data[i, ]
  if (is.na(row_obs$steps)) 
  {
    steps <- subset(steps_mean, interval == row_obs$interval)$steps
  } 
  else 
  {
    steps <- row_obs$steps
  }
  NA_steps <- c(NA_steps, steps)
}

new_data <- data
new_data$steps <- NA_steps

Steps_Total <- aggregate(steps ~ date, new_data, sum)
hist(Steps_Total$steps, main = ("Total Steps Each Day"), col="pink", xlab="Number of Steps")
hist(steps_by_day$steps, main = ("Total Steps Each Day"), col="orange", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("pink", "orange"),lwd=5)

mean_total <- mean(Steps_Total$steps)
mean_total

median_total <- median(Steps_Total$steps)
median_total

mean_diff <- mean_total - steps_by_day_mean
mean_diff

median_diff <- median_total - steps_by_day_median
median_diff
```

Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
Weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

new_data$DoW <- ifelse(is.element(weekdays(as.Date(new_data$date)),Weekdays),"Weekday","Weekend")
Steps_Total <- aggregate(steps ~ interval + DoW, new_data, mean)

library(lattice)
xyplot(Steps_Total$steps ~ Steps_Total$interval|Steps_Total$DoW,
       main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
