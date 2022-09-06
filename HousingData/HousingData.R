library(readr)
library(ggplot2)
library(knitr)
library(tidyverse)
library(caret)
library(leaps)
library(car)
library(mice)
library(scales)
library(RColorBrewer)
library(plotly)
library(nortest)
library(lmtest)
library(housingData)

setwd("C:/Users/jvicm/Desktop/New folder/UCR data 2017")
help(package = housingData)

head(fipsCounty)
head(housing)
housing_data = read_csv("housing.csv")

head(housing_data)

summary(housing_data)


plot_map = ggplot(housing_data, 
                  aes(x = longitude, y = latitude, color = median_house_value, 
                      hma = housing_median_age, tr = total_rooms, tb = total_bedrooms,
                      hh = households, mi = median_income)) +
  geom_point(aes(size = population), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Data Map - Longtitude vs Latitude and Associated Variables") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired", labels = comma) +
  labs(color = "Median House Value (in $USD)", size = "Population")
plot_map

#Missing Data

housing_data$ocean_proximity = as.factor(housing_data$ocean_proximity)
levels(housing_data$ocean_proximity)
ggplot(housing_data, aes(x = factor(ocean_proximity))) +
  geom_bar(stat = "count", color = "black", fill = "blue")

# Data cleaning 
sum(is.na(housing_data))

housing_data = housing_data[housing_data$ocean_proximity != "ISLAND", ]


sum(is.na(total_bedrooms))

total_bedrooms = housing_data$total_bedrooms

total_bedrooms = housing_data$total_bedrooms
sum(is.na(total_bedrooms))




bedroom_mean = mean(housing_data$total_bedrooms, na.rm=TRUE)
bedroom_median = median(housing_data$total_bedrooms, na.rm=TRUE)
ggplot(housing_data, aes(x = total_bedrooms)) +
  geom_histogram(bins = 40, color = "black", fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "Mean"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "Median"), lwd = 1.5) +
  xlab("Total Bedrooms") +
  ylab("Frequency") +
  ggtitle("Histogram of Total Bedrooms (noncontinuous variable)") +
  scale_color_manual(name = "Summary Stats", labels = c("Mean", "Median"), values = c("red", "green"))


#BoxPplot Code
boxplot(housing_data[1:9])

pnorm(q = 4, mean = 6, sd = 1.5)
pnorm(q = 7.5, mean = 6, sd = 1.5) - pnorm(q = 4, mean = 6, sd = 1.5)



#Frequency distribution 

library(summarytools)
freq(housing_data$median_income)
library(data.table)
data_table <- data.table(housing_data)

print(housing_data)
print (data_table)

freq <- table(data_table$median_income)
freq


print ("Modified Frequency Table")
print (freq)

print ("Cumulative Frequency Table")
cumsum <- cumsum(freq)
print (cumsum)

print ("Relative Frequency Table")
prob <- prop.table(freq)
print (prob)
