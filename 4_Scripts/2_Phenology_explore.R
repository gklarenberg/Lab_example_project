# Geraldine Klarenberg
# University of Florida
# School of Forest, Fisheries and Geomatics Sciences
# Fall semester 2021

# Project example
# OSBS phenology data and temperature
# Explore phenology for pines

############# 1 Packages ####################
# Load required packages
library(tidyverse)

############# 2 Load data ####################
# Load data
phe_ind_OSBS <- read_csv("2_Clean_data/phenology_clean_OSBS.csv")

# make sure the dates are dates
class(phe_ind_OSBS$date)
#phe_ind_OSBS$date <- ymd(phe_ind_OSBS$date)

# NB don't focus on species. After merging files, turns out there is 
# phenophase data for times when there are no individual measurements
# (e.g. 2014)

############# 3 Explore pine phenophase stauses ####################

unique(phe_ind_OSBS$phenophaseName)

# Get only the ones that refer to pines
pine_phases <- c("Emerging needles", "Open pollen cones", "Young needles")
phe_ind_OSBS_pines <- filter(phe_ind_OSBS, phenophaseName %in% pine_phases)

#### Plot all statuses
# Check total in status by day
samp_size <- count(phe_ind_OSBS_pines, date)
# or 
samp_size <- phe_ind_OSBS_pines %>% 
  group_by(date) %>% 
  summarize(tot_count = n())

# count/tally the individuals in which status 
in_status <- phe_ind_OSBS_pines %>%
  group_by(date, phenophaseName, phenophaseStatus) %>%
  summarize(ind_count = n())

# join with the total count (so we can calculate percentages later)
in_status <- full_join(samp_size, in_status, by="date")

# Retain only Yes
in_status_yes <- filter(in_status, phenophaseStatus %in% "yes")

############# 4 Make plots ####################
# as a stacked barplot
pheno_stack <- ggplot(in_status_yes, aes(date, ind_count, fill = phenophaseName)) +
  geom_bar(stat="identity", position = "stack", na.rm = TRUE) +
  ggtitle("Total individuals in phenophases") +
  xlab("Date") + ylab("Number of individuals") +
  theme_bw()
  
# as a grouped barplot
pheno_group <- ggplot(in_status_yes, aes(date, ind_count, fill = phenophaseName)) +
  geom_bar(stat="identity", position = "dodge", na.rm = TRUE) +
  ggtitle("Total individuals in phenophases") +
  xlab("Date") + ylab("Number of individuals") +
  theme_bw()

# as points or lines
pheno_point <- ggplot(in_status_yes, aes(date, ind_count, group = phenophaseName, color = phenophaseName)) +
  geom_point() +
  #geom_line() +
  #geom_smooth()+
  ggtitle("Total individuals in phenophases") +
  xlab("Date") + ylab("Number of individuals") +
  theme_bw()

# group by month
ave_in_status_yes <- in_status_yes %>% 
  group_by(phenophaseName, year = year(date), month = month(date)) %>% 
  summarize(ind_count = mean(ind_count))

pheno_point2 <- ggplot(ave_in_status_yes, aes(mdy(paste(month, "01", year, paste = "/")), ind_count, group = phenophaseName, color = phenophaseName)) +
  geom_point() +
  geom_line() +
  #geom_smooth()+
  ggtitle("Mean individuals per month in phenophases") +
  xlab("Date") + ylab("Mean number of individuals") +
  theme_bw()+
  facet_wrap(~phenophaseName)+
  theme(legend.position = "none")
#### NB Open pollen cones seem to show far less of a pattern
# than young and emerging needles!!

############# 5 Make percentage plots ####################
# convert to percent
in_status_yes <- in_status_yes %>% 
  mutate(percent = (ind_count/tot_count)*100)
# or
# in_status_yes$percent<- ((in_status_yes$ind_count)/in_status_yes$tot_count)*100

pheno_percent_bar <- ggplot(in_status_yes, aes(date, percent, fill = phenophaseName)) +
  geom_bar(stat="identity", position = "stack", na.rm = TRUE) +
  ggtitle("Proportion in phenophases") +
  xlab("Date") + ylab("Percentage of individuals") +
  theme_bw()

# as points or lines
pheno_percent_points <- ggplot(in_status_yes, aes(date, percent, group = phenophaseName, color = phenophaseName)) +
  geom_point() +
  #geom_line() +
  #geom_smooth()+
  ggtitle("Proportion in phenophases") +
  xlab("Date") + ylab("Percent of individuals") +
  theme_bw()
### Not really more informative

############# 6 Save plots ####################

jpeg("5_Outputs/01_pheno_stack.jpg", width = 720, height = 510)
pheno_stack
dev.off()

jpeg("5_Outputs/02_pheno_group.jpg", width = 720, height = 510)
pheno_group
dev.off()

jpeg("5_Outputs/03_pheno_stack_percent.jpg", width = 720, height = 510)
pheno_percent_bar
dev.off()

jpeg("5_Outputs/04_pheno_point_month.jpg", width = 720, height = 510)
pheno_point2
dev.off()

# For further analyses, maybe only look at open pollen cones; link with temperature?





