
# Project example
# OSBS phenology data and temperature
# Analysis of temperature and pine pollen phenophase

# Load required libraries
library(tidyverse)
library(lubridate)

# Load the clean data
phe_ind_OSBS <- read_csv("Phenology_OSBS/2_Clean_data/phenology_clean_OSBS.csv")
temp_daily <- read_csv("Phenology_OSBS/2_Clean_data/temp_daily_clean_OSBS.csv")

# Make sure there are dates
class(phe_ind_OSBS$date)
class(temp_daily$date)

# Filter out only open pollen cones
phe_ind_OSBS_pollen <- phe_ind_OSBS %>% 
  filter(phenophaseName == "Open pollen cones",
         phenophaseStatus == "yes")
  
# Plot together
pheno_pollen <- ggplot(phe_ind_OSBS_pollen, aes(date)) +
  geom_bar() +
  ggtitle("Total individuals in phenophases") +
  xlab("Date") + ylab("Number of individuals") +
  theme_bw()

temp_plot <- ggplot(temp_daily, aes(date, daily_temp)) +
  geom_point() +
  ggtitle("Ave Daily Air Temperature") +
  xlab("") + ylab("Temp (C)") +
  theme_bw()

library(cowplot)
combined_plot <- plot_grid(pheno_pollen, temp_plot, ncol = 1)

pdf("Phenology_OSBS/5_Outputs/Pollen_temp.pdf", width = 7, height =12)
combined_plot
dev.off()
# Should probably still be better aligned

############ Look at relationships ##############
# join datasets so we have the dates (temp) lining up
all_data <- left_join(phe_ind_OSBS, temp_daily) #only joins by date
# Remember: some days have NA for temp (because if there is any half hour of data missing,
# you can't make a proper average! So if that happened, the day also got NA.
# For a real project I would probably interpolate)

# this is all the data again (so you can also explore relationships with other
# phenopases)
all_data_pollen <- all_data %>% 
  filter(phenophaseName == "Open pollen cones")

########### Visual analysis of relationships ###########

ggplot(all_data_pollen, aes(y = daily_temp, group = phenophaseStatus)) +
  geom_boxplot()
# what happened? There is also "missed" and "uncertain"!
all_data_pollen <- all_data_pollen %>% 
  filter(phenophaseStatus %in% c("yes", "no"))

ggplot(all_data_pollen, aes(y = daily_temp, group = phenophaseStatus, fill = phenophaseStatus)) +
  geom_boxplot()

### Hmmm. But maybe that is because this only happens in spring when temperatures rise
ggplot(all_data_pollen, aes(y = daily_temp, group = phenophaseStatus, fill = phenophaseStatus)) +
  geom_boxplot() +
  facet_wrap(vars(month))
# Not much difference: so probably mostly related with time of the year?
# Look at the numbers in phenophase per month?

all_data_pollen_count <- all_data_pollen %>% 
  group_by(month, phenophaseStatus) %>%
  summarize(pheno_count = n())

ggplot(all_data_pollen_count, aes(x = month, y = pheno_count, 
                                  group = phenophaseStatus, fill = phenophaseStatus)) +
  geom_bar(stat = "identity", position = "dodge")

# next lab: make prettier
