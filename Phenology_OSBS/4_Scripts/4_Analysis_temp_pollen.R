# Geraldine Klarenberg
# University of Florida
# School of Forest, Fisheries and Geomatics Sciences
# Fall semester 2021

# Project example
# OSBS phenology data and temperature
# Analysis of temperature and pine pollen phenophase

############# 1 Packages ####################
# Load required packages
library(tidyverse)
library(lubridate)
library(cowplot) # for combining ggplots together

############# 2 Load data ####################
# Load the clean data
phe_ind_OSBS <- read_csv("Phenology_OSBS/2_Clean_data/phenology_clean_OSBS.csv")
temp_daily <- read_csv("Phenology_OSBS/2_Clean_data/temp_daily_clean_OSBS.csv")

# Make sure there are dates
class(phe_ind_OSBS$date)
class(temp_daily$date)

############# 3 Explore phenophase and temp ####################
# Filter out only open pollen cones
phe_ind_OSBS_pollen <- phe_ind_OSBS %>% 
  filter(phenophaseName == "Open pollen cones",
         phenophaseStatus == "yes")
  
# Plot together
pheno_pollen <- ggplot(phe_ind_OSBS_pollen, aes(date)) +
  geom_bar() +
  ggtitle("Total individuals in 'open pollen' phenophase") +
  xlab("") + ylab("Number of individuals") +
  theme_bw()

temp_plot <- ggplot(temp_daily, aes(date, daily_temp)) +
  geom_point() +
  ggtitle("Mean daily air temperature") +
  xlab("Date") + ylab("Temp (C)") +
  theme_bw()

combined_plot <- plot_grid(pheno_pollen, temp_plot, ncol = 1)

jpeg("Phenology_OSBS/5_Outputs/07_pollen_temp.jpg", width = 720, height = 720)
combined_plot
dev.off()
# Should probably still be better aligned: not properly aligned
# right now because they're still two separate data sets
# Solution: join! Need to do that anyway for the analysis

############# 4 Join data ####################
# join datasets so we have the dates (temp) lining up
all_data <- left_join(phe_ind_OSBS, temp_daily) #only joins by date
# Remember: some days have NA for temp (because if there is any half hour of data missing,
# you can't make a proper average! So if that happened, the day also got NA.
# For a real project I would probably interpolate)

############# 5 Explore relationships ####################
# this is all the data (so you can also explore relationships with other
# phenopases)
all_data_pollen <- all_data %>% 
  filter(phenophaseName == "Open pollen cones")

##### Visual exploration
# Make a boxplot to see if pollen phenophase (yes or no) is differen
# at different temperatures
ggplot(all_data_pollen, aes(y = daily_temp, group = phenophaseStatus)) +
  geom_boxplot()
# Too many phases - should only be 2 (yes and no) - what happened? 
# There are also "missed" and "uncertain"!
all_data_pollen <- all_data_pollen %>% 
  filter(phenophaseStatus %in% c("yes", "no"))

ggplot(all_data_pollen, aes(y = daily_temp, group = phenophaseStatus, fill = phenophaseStatus)) +
  geom_boxplot()

### Hmmm. So higher temps = no pollen
# But maybe that is because this only happens in spring when temperatures rise (so
# not summer when they're at their highest)
ggplot(all_data_pollen, aes(y = daily_temp, group = phenophaseStatus, fill = phenophaseStatus)) +
  geom_boxplot() +
  facet_wrap(vars(month))
# Not much difference in temp per se: so probably mostly related with time of the year?
# months 1, 2, 3, 4 (kinda up to 7). Increase in daylight?
# Look at the numbers in phenophase per month?

all_data_pollen_count <- all_data_pollen %>% 
  group_by(month, phenophaseStatus) %>%
  summarize(pheno_count = n())

ggplot(all_data_pollen_count, aes(x = month, y = pheno_count, 
                                  group = phenophaseStatus, fill = phenophaseStatus)) +
  geom_bar(stat = "identity", position = "dodge")

