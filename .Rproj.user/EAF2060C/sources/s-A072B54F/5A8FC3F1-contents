
# Project example
# OSBS phenology data and temperature
# Cleaning script for temperature data

# Load libraries
library(tidyverse)
library(lubridate)

# Read in raw data
temp30 <- read_csv("Phenology_OSBS/1_Raw_data/SAAT_30min.csv")

temp30

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

############# Check for outliers ##########
# Do a quick check for strange values and the distribution
ggplot(temp30_noNA, aes(x=tempSingleMean))+
  geom_histogram()

# Make a boxplots by months. First add a column with months
temp30_noNA <- temp30_noNA %>% 
  mutate(month = month(temp30_noNA$startDateTime))

ggplot(temp30_noNA, aes(x = month, y = tempSingleMean, group = month)) +
  geom_boxplot()
# Looks pretty good!

############ Save data and make plots over time ##########
# Save data 
write.csv(temp30_noNA, "Phenology_OSBS/2_Clean_data/temp30_clean_OSBS.csv", row.names = FALSE)

# also make daily data and save
temp30_daily <- temp30_noNA %>% 
  mutate(day = day(temp30_noNA$startDateTime), year = year(temp30_noNA$startDateTime)) %>% 
  group_by(year, month, day) %>% 
  summarize(daily_temp = mean(tempSingleMean)) # don't take NA out: if there is NA you can't make a good average
temp30_daily$date <- ymd(paste(temp30_daily$year, temp30_daily$month, temp30_daily$day, sep = "-"))

write.csv(temp30_daily, "Phenology_OSBS/2_Clean_data/temp_daily_clean_OSBS.csv", row.names = FALSE)

# Plots
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

# Save plots
pdf("Phenology_OSBS/5_Outputs/temp_plot.pdf", width = 10, height = 7)
temp_plot1
dev.off()

pdf("Phenology_OSBS/5_Outputs/temp_daily_plot.pdf", width = 10, height = 7)
temp_plot2
dev.off()
