#Load Packages
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rio)

#Task 3
happydata = read.csv('happiness.csv')
str(happydata)

#Task 5
#Mean
mean(happydata$Happiness.Score, na.rm = TRUE)

#Median
median(happydata$Happiness.Score, na.rm = TRUE)

#Mode
(modevector <- happydata$Happiness.Score)
(uniqvector <- unique(modevector))
(positions <- match(modevector,uniqvector))
(freq <- tabulate(positions))
(maxfreq <- which.max(freq))
(uniqvector[maxfreq])

m <- function(modevector){
  uniquevector <- unique(modevector)
  uniquevector[which.max(tabulate(match(modevector,uniquevector)))]
}

(m(modevector))

#Task 6
#Standard Deviation
sd(happydata$Happiness.Score)

#Variance
var(happydata$Happiness.Score)

#Min value
min(happydata$Happiness.Score)

#Max value
max(happydata$Happiness.Score)

#Range
range(happydata$Happiness.Score)

#Quantile
quantile(happydata$Happiness.Score)

#Task 7
sapply(happydata, sd, na.rm = TRUE)

sapply(happydata, mean, na.rm = TRUE)


#Task 8
summary(happydata)

#Task 9
happydata %>%
  group_by(Region) %>%
  get_summary_stats(Happiness.Score, type="mean_sd")

rm(list = ls())
