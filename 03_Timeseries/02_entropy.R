#
# Calculate and plot approximate entropy
#

library(ggplot2)
library(data.table)
library(zoo)
library(dplyr)
library(lubridate)
library(pracma)
library(stats)
library(entropy)

# Average days per month
kMonDays <- 30.5

# Input/output directories
kDataDir <- '00_Data/'
kImgDir <- '00_Images/'

# Timeseries directories to create
kTSData <- '00_Data/Timeseries/'
kTSImg <- '00_Images/Timeseries/'

# Pre-calculated 15 minute interval timeseries data
kInterval15 <- paste0(kDataDir, '/INTERVAL_15.rds')

source('00_Scripts/Baby_Name_From_File.R')
source('00_Scripts/Month_from_Day.R')
source('00_Scripts/Week_from_Day.R')
source('00_Scripts/Day_from_Week.R')
source('00_Scripts/theme_bd.R')

#
# Calculate baby data entropy ----
#

int_df <- readRDS(kInterval15) %>% 
  mutate(week = Week_from_Day(day)) 

# Restrict to completed weeks
complete_week_df <- int_df %>%
  group_by(baby_name, week, day) %>%
  do(head(., n=1)) %>%
  ungroup() %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(tot_days = n()) %>%
  ungroup() %>%
  dplyr::filter(tot_days == 7) %>%
  dplyr::select(baby_name, week)

# Calculate entropy for acceptable weeks
entropy_df <- int_df %>% 
  semi_join(complete_week_df, by=c('baby_name', 'week')) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(ap_en = approx_entropy(sleep_flag)) %>%
  ungroup()

# Save this data 
saveRDS(entropy_df, file=paste0(kTSData, '/Entropy_by_Week_All.rds'))

#
# Plot entropy by week ----
#

xmax_week <- entropy_df %>%
  pull(week) %>% max()

ent_plot <- entropy_df %>%
  mutate(month = Month_from_Day(Day_from_Week(week))) %>%
  ggplot(aes(x=month, y=ap_en, color=baby_name)) +
  geom_line(size=1) +
  theme_minimal() +
  labs(title= 'Approximate Entropy By Week',
       x = 'Month',
       y = 'ApEn',
       color="") +
  theme_bd +
  scale_x_continuous(limits=c(0, Month_from_Day(xmax_week*7)),
                     breaks=seq(0, Month_from_Day(xmax_week*7), 4)) +
  scale_y_continuous(limits=c(0, 0.5))

print(ent_plot)
ggsave(paste0(kTSImg, 'Entropy_Week_Plot.png'),
       ent_plot)
