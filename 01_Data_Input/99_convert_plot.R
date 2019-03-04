#
# Consolidates and converts baby sleep data, and creates
# standard plots of sleep history.
#   1.  Combines all input data sets into 1 long
#     "Standard long" format file.
#   2.  Converts all baby data to additional formats:
#     2a.  "Standard long 2" - similar to "Standard Long" but
#         with separate rows for sleep intervals spanning calendar days
#     2b.  "Interval_15" - 15 minute interval time series
#     2c.  "Interval_5" - 5 minute interval time series.
#  3. Plots sleep graphs for all babies.
#  
#

library(tools)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

kDataDir <- "00_Data/"
kImgDir <- "00_Images/"

kInPrefix <- 'STD_LONG_'
kStdFilename <- 'COMB_STD_LONG.rds'
kStd2Filename <- 'COMB_STD_LONG_2.rds'
kIntFilename_15 <- 'INTERVAL_15.rds'
kIntFilename_05 <- 'INTERVAL_05.rds'

source('00_Scripts/Std_Long_to_Std_Long2.R')
source('00_Scripts/Interval_from_Std_Long2.R')
source('00_Scripts/Baby_Name_From_File.R')
source("00_Scripts/Baby_Sleep_Plot.R")
source("00_Scripts/Name_to_RGB_Color.R")
source('00_Scripts/Factorize_Baby_Name.R')

#
# Consolidate data ----
# Find all "Standard Long" files and combine
# Look for CSVs in in the data dirs
#

pattern_str = paste0("^", kInPrefix, ".*(rds|RDS)$")
infile_list <- list.files(path=kDataDir, pattern = pattern_str)

# Final std long frame with all data
std_long <- data.frame()

for (fn in infile_list) {
  this_name <-Baby_Name_From_File(fn, kInPrefix)
  this_std_long <- readRDS(paste0(kDataDir, '/', fn)) %>%
    mutate(baby_name = this_name)
  
  std_long <- std_long %>%
    bind_rows(this_std_long)
}

# Save the combined data, factorizing the name
std_long <- std_long %>%
  mutate(baby_name = Factorize_Baby_Name(baby_name)) %>%
  arrange(baby_name, startTime)
std_long %>%
  saveRDS(file=paste0(kDataDir, '/', kStdFilename))

#
# Create additional formats ----
#

# "Standard long 2 - split across days

std_long_2 <- std_long %>%
  Std_Long_to_Std_Long2()

saveRDS(std_long_2, file=paste0(kDataDir, '/',
                                kStd2Filename))

# Timeseries - 2 intervals

interval_15 <- std_long_2 %>% 
  Interval_from_Std_Long2(interval_mins = 15) 

saveRDS(interval_15, file=paste0(kDataDir, '/',
                                 kIntFilename_15))

interval_05 <- std_long_2 %>% 
  Interval_from_Std_Long2(interval_mins = 5) 

saveRDS(interval_05, file=paste0(kDataDir, '/',
                                 kIntFilename_05))

#
# Check ----
#

std_long %>%
  group_by(baby_name) %>%
  summarize(sum(duration))

std_long_2 %>%
  group_by(baby_name) %>%
  summarize(sum(duration))

# Intervals should be close but may differ slightly.

interval_15 %>%
  group_by(baby_name) %>%
  summarize(15 * sum(sleep_flag))

interval_05 %>%
  group_by(baby_name) %>%
  summarize(5 * sum(sleep_flag))
  

#
# Do basic sleep plots ----
#

baby_list <- interval_15 %>% pull(baby_name) %>% unique()

for (baby in baby_list) {
  sleep_color = Name_to_RGB_Color(baby)
  
  this_data <- interval_15 %>%
    dplyr::filter(baby_name == baby)
  
  # Create the plot
  g_p <- this_data %>%
    Baby_Sleep_Plot(sleep_color = sleep_color)
  
  # Save a rescaled plot
  plotOutName <-paste0(kImgDir, '/plotStdSq_', 
                       baby, '.png')
  
  max_day <- max(this_data$day)
  width_scaled = 0.7*(max_day/31)
  
  # Adjust plot if too small
  if (width_scaled <= 2) {
    g_p <- g_p +
      theme(legend.position='none') 
    width_scaled = 2
  }
  ggsave(filename = plotOutName,
         plot=g_p,
         device='png',
         height=4,
         width=width_scaled,
         units='in')
  
}


