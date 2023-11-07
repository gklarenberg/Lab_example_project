# Geraldine Klarenberg
# University of Florida
# School of Forest, Fisheries and Geomatics Sciences
# Fall semester 2021

# Project example
# OSBS phenology data and temperature
# Cleaning script for temperature data

############# 1 Packages ####################
# Load required packages
library(tidyverse)
library(lubridate)

############# 2 Load data ####################
# Read in raw data
temp30 <- read_csv("1_Raw_data/SAAT_30min.csv")

temp30

############# 3 Clean ####################
# Check how many records have quality flags
sum(temp30$finalQF==1, na.rm=TRUE)

# Check for NAs
sum(is.na(temp30$tempSingleMean))

# Take out NAs
temp30_noNA <- temp30 %>%
  drop_na(tempSingleMean) 

# Look at the date range
range(temp30_noNA$startDateTime)

# Check format
str(temp30_noNA$startDateTime)
# Time format! read_csv did that automatically for use

############# 4 Check for outliers ##########
# Do a quick check for strange values and the distribution
ggplot(temp30_noNA, aes(x=tempSingleMean))+
  geom_histogram()

# Make a boxplots by months. First add a column with months
temp30_noNA <- temp30_noNA %>% 
  mutate(month = month(temp30_noNA$startDateTime))

ggplot(temp30_noNA, aes(x = month, y = tempSingleMean, group = month)) +
  geom_boxplot()
# Looks pretty good!

############# 5 Save cleaned data ####################
# Save data 
write_csv(temp30_noNA, "2_Clean_data/temp30_clean_OSBS.csv")

# also make daily data and save
temp30_daily <- temp30_noNA %>% 
  mutate(day = day(temp30_noNA$startDateTime), year = year(temp30_noNA$startDateTime)) %>% 
  group_by(year, month, day) %>% 
  summarize(daily_temp = mean(tempSingleMean)) # don't take NA out: if there is NA you can't make a good average
temp30_daily$date <- ymd(paste(temp30_daily$year, temp30_daily$month, temp30_daily$day, sep = "-"))

write_csv(temp30_daily, "2_Clean_data/temp_daily_clean_OSBS.csv")

############# 6 Make plots ####################
temp_plot1 <- ggplot(temp30_noNA, aes(x = startDateTime, y = tempSingleMean)) +
  geom_point(size = 0.1) +
  ggtitle("30-minute temperature") +
  xlab("Date") + ylab("Temperature (degrees Celsius)") +
  theme_bw()

temp_plot2 <- ggplot(temp30_daily, aes(x = date, y = daily_temp)) +
  geom_point(size = 0.1) +
  ggtitle("30-minute temperature") +
  xlab("Date") + ylab("Temperature (degrees Celsius)") +
  theme_bw()

############# 7 Save plots ####################
jpeg("5_Outputs/05_temp_plot.jpg", width = 720, height = 504)
temp_plot1
dev.off()

jpeg("5_Outputs/06_temp_daily_plot.jpg", width = 720, height = 504)
temp_plot2
dev.off()
