#
# Imports from jitney86's GitHub repository, which is public 
# as of Feb 2019
# https://github.com/jitney86/Baby-data-viz/
#
# Converts to the "standard long" format
# which has 3 columns:  
#    * startTime - date time of sleep interval start
#    * day - baby's age in calendar days since birth as of
#      startTime.  (The day of birth is day 1)
#    * duration - integer number of minutes of sleep
# The converted data is saved as an R object 
#
# See also 
# https://flowingdata.com/2017/08/08/14-months-of-sleep-and-breast-feeding
# https://www.reddit.com/r/dataisbeautiful/comments/6s0ba9/months_3_to_17_of_my_babys_sleep_and/
# https://www.reddit.com/r/dataisbeautiful/comments/6tz7rr/update_to_my_childs_sleep_and_feed_dataviz_oc/
#
# This baby is nicknamed "Red".
# A copy of the input will be saved to the 00_Data directory.
#

library(httr)
library(data.table)
library(tools)
library(dplyr)
library(lubridate)

# Input data source
kInputURL <- 'https://raw.githubusercontent.com/jitney86/Baby-data-viz/master/sleep-eat.csv'

# Script for the converson
source('00_Scripts/Reddit_to_Std_Long.R')

# Outputs
kDataDir <- '00_Data'
kOutName <- 'Red'
kOutPrefix <- 'STD_LONG_'
kOutRaw <- paste0(kDataDir, '/RAWDATA_', kOutName, '.csv')

#
# Import and save the raw data ----
#

# Get the data
sleep_data <- fread(kInputURL, header=TRUE,
                    colClasses = "character",
                    fill=TRUE)

# Save the raw version
fwrite(sleep_data, kOutRaw)

#
# Create a cleaned up reddit version ----
#

# Look for abnormal strings
sleep_data_clean1 <- sleep_data %>% 
  melt(id.vars=1, variable.name = 'day') %>% 
  mutate(value = toupper(value))

# Show strings
table(sleep_data_clean1$value)

# Reddit has 'eat' and 'sleep'.  Keep both for now
sleep_data_clean2 <- sleep_data_clean1 %>%
  mutate(sleep_ind = case_when(value %in% c('S', 'E') ~ value,
                               grepl('BOTTLE', value) ~ 'E',
                               grepl('FEED', value) ~ 'E',
                               TRUE ~ ''))

# Find days with no data
sleep_data_clean3 <- sleep_data_clean2 %>%
  group_by(day) %>%
  mutate(sleep_ind_max = max(sleep_ind, na.rm=T)) %>%
  ungroup() 

# Show bad "days"
sleep_data_clean3 %>%
  filter(sleep_ind_max == "") %>%
  pull(day) %>%
  unique()

# Drop bad "days"
sleep_data_clean4 <- sleep_data_clean3 %>%
  filter(sleep_ind_max != "") %>%
  dplyr::select(-sleep_ind_max, value)

# Reshape back to wide
sleep_data_clean5 <- sleep_data_clean4 %>%
  dplyr::select(time, day, sleep_ind) %>%
  arrange(time, day) %>%
  dcast(time ~ day, value.var='sleep_ind') %>%
  mutate(time = as.numeric(time))

#
# Convert to the "standard long" format ----
#

sleep_data_std_long <- sleep_data_clean5 %>%
  Reddit_to_Std_Long()

saveRDS(sleep_data_std_long, file=paste0(kDataDir, '/', kOutPrefix,
                    kOutName, '.rds'))

#
# Check ----
#

sd_melt <- sleep_data_clean5 %>%
  melt(id.var='time') 

# Get total sleep times (346305)
sd_melt %>%
  filter(value == 'S') %>%
  nrow() *15

# Make sure we recover the total duration
sum(sleep_data_std_long$duration)

