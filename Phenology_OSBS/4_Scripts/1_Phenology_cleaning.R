# Geraldine Klarenberg
# University of Florida
# School of Forest, Fisheries and Geomatics Sciences
# Fall semester 2021

# Project example
# OSBS phenology data and temperature
# Cleaning script for phenology data

############# 1 Packages ####################
# Load required packages
library(tidyverse)
library(lubridate)

############# 2 Load data ####################
# Load raw data
ind_OSBS <- read_csv('Phenology_OSBS/1_Raw_data/phe_perindividual.csv')

status_OSBS <- read.csv('Phenology_OSBS/1_Raw_data/phe_statusintensity.csv')

# Will be joining data together, clean first.

############# 3 Clean ####################
# take out column uid, we don't need it. Also take out date for the individual, 
# as we don't want to merge using this
ind_OSBS <- select(ind_OSBS, -uid, -date)
status_OSBS <- select (status_OSBS, -uid) # for status we do want date, as this is
# a time series

# retain only unique columns (take duplicates out)
ind_noD_OSBS <- distinct(ind_OSBS)
status_noD_OSBS <- distinct(status_OSBS)

# rename some columns: when joining, it is based on common column names,
# but I want to keep some columns separate (so I give them a different name)
# where is there an intersection of names
same_name <- intersect(names(status_noD_OSBS), names(ind_noD_OSBS))
same_name

# rename(dataframe, new_name = old_name)
status_noD_OSBS <- rename(status_noD_OSBS, editedDateStat=editedDate, 
                     measuredByStat=measuredBy, recordedByStat=recordedBy, 
                     samplingProtocolVersionStat=samplingProtocolVersion, 
                     remarksStat=remarks, dataQFStat=dataQF)

# convert date to date type, use lubridate
status_noD_OSBS$date <- ymd(status_noD_OSBS$date)

# only take the last edited individuals 
ind_last_OSBS <- ind_noD_OSBS %>%
  group_by(individualID) %>%
  filter(editedDate==max(editedDate))

# just in case there are duplicate dates for an individual
ind_lastnoD_OSBS <- ind_last_OSBS %>%
  group_by(editedDate, individualID) %>%
  filter(row_number()==1)

# now join the two data frames
phe_ind_OSBS <- full_join(status_noD_OSBS, ind_lastnoD_OSBS) 

# select required columns
names(phe_ind_OSBS)
phe_ind_OSBS <- select(phe_ind_OSBS, plotID, date, phenophaseName,
                           phenophaseStatus, phenophaseIntensity, decimalLatitude,
                           decimalLongitude, elevation, taxonID, scientificName,
                           growthForm)

############# 4 Save cleaned data ####################
write_csv(phe_ind_OSBS, "Phenology_OSBS/2_Clean_data/phenology_clean_OSBS.csv")
