library(ggplot2)
library(scales)
library(dplyr)
library(grid)

hoursToMinutes <- function(hours) {
    # converts character HH:MM data to total minutes
    # Args: 
    #   hours - a character vector in "HH:MM" format
    # Returns:
    #   elapsedMinutes - a numeric vector
    hoursMinutes <- strsplit(hours, split = ":")
    hours <- as.numeric(hoursMinutes[[1]][1])
    minutes <- as.numeric(hoursMinutes[[1]][2])
    
    elapsedMinutes <- hours * 60 + minutes
    elapsedMinutes
}
## PRE-PROCESSING

df <- read.csv("sleepdata.csv", sep = ";", stringsAsFactors = FALSE)
df <- df[1:4]
names(df) <- c("startTime", "endTime", "quality", "hours")
## convert variables to appropriate classes
df$startTime <- as.POSIXct(df$startTime)
df$endTime <- as.POSIXct(df$endTime)
df$quality <- as.numeric(gsub("%", "", df$quality))
df$minutes <- sapply(df$hours, hoursToMinutes, USE.NAMES = FALSE)
## remove observations before November 2012
df <- filter(df, format(startTime, "%Y-%m") > "2012-11")

## VISUALIZATIONS

## frequency of bedtime hours
bedtimeFreq <- table(format(df$start, "%H"))
bedtimeFreq <- c(bedtimeFreq[length(bedtimeFreq)], bedtimeFreq)
bedtimeFreq <- bedtimeFreq[-length(bedtimeFreq)]
timeLabels <- factor(names(bedtimeFreq), levels = names(bedtimeFreq))
bedtime <- data.frame(Count = bedtimeFreq, Time = timeLabels)

ggplot(bedtime, aes(Time, Count)) +
    geom_point(col = "#1f77b4", size = 4) +
    xlab("Bedtime Hour (24-hour clock)") +
    ggtitle("Frequency of Bedtime Hours") +
    theme_bw()

## frequency of sleep duration
ggplot(df, aes(minutes)) + 
    geom_histogram(fill = "#1f77b4") +
    ylab("Count") +
    xlab("Duration of Sleep (minutes)") +
    ggtitle("Histogram of Sleep Duration") +
    theme_bw()

## duration of sleep over time
ggplot(df, aes(startTime, minutes)) + 
    ## geoms
    geom_point(aes(col = quality),
               size = 3.5) + 
    geom_smooth(method = "loess",
                col = "black") +
    ## titles
    ylab("Duration of Sleep (minutes)") +
    xlab("Date") +
    ggtitle("Duration of Sleep over Time") +
    ## scales
    scale_color_gradient2(high = "#1f77b4",
                          mid = "light grey",
                          low = "#d62728",
                          midpoint = median(df$quality)) + 
    scale_x_datetime(breaks = "4 months", 
                     labels = date_format("%b %Y")) +
    theme_bw()

## sleep quality over time
ggplot(df, aes(startTime, quality)) + 
    ## geoms
    geom_point(col = "#1f77b4",
               alpha = 0.5, size = 3.5) + 
    geom_smooth(method = "loess",
                col = "black") +
    ## titles
    ylab("Sleep Quality (%)") +
    xlab("Date") +
    ggtitle("Sleep Quality over Time") +
    ## scales
    scale_x_datetime(breaks = "4 months", 
                     labels = date_format("%b %Y")) +
    theme_bw()

## relationship of sleep quality and duration
ggplot(df, aes(minutes, quality)) +
    geom_point(col = "#1f77b4" ) +
    stat_smooth(method = "lm",
                col = "black") +
    xlab("Sleep Quality (%)") +
    ylab("Sleep Duration (minutes)") +
    theme_bw()

## linear model of sleep quality on sleep duration
lmFit <- lm(quality ~ minutes, data = df)
summary(lmFit)
