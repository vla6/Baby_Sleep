#
# Setup for comparison of baby sleep data to charts
# from Ferber, M. (2006). Solve Your Child’s Sleep Problems. 
# New York: Simon and Schuster 
# 
# Creates subdirectories for data and image files.
# Also creates data frames containing the Ferber standards,
# at daily resolution
#

library(dplyr)

kDataDir <- '00_Data/'
kImgDir <- '00_Images/'

kSubDirStr <- 'Ferber'

kOutputStdBase <- '/Ferber_Guidelines.rds'
kOutputStdExtended <- '/Ferber_Guidelines_Extended.rds'

source("00_Scripts/Max_Day_from_Month.R")

# Length of Ferber data to keep (days)
kMaxDay <- Max_Day_from_Month(24.5)

#
# Create sub-directories ----
#

kFerberData <- paste0(kDataDir, '/', kSubDirStr)
dir.create(kFerberData, showWarnings = FALSE)

kFerberImg <- paste0(kImgDir, '/', kSubDirStr)
dir.create(kFerberImg, showWarnings = FALSE)

#
# Get basic Ferber standards ----
#


# Ferber data from his figure 1 chart, up to 2 years
x_days <- c(1, 
            Max_Day_from_Month(1),
            Max_Day_from_Month(3),
            Max_Day_from_Month(6),
            Max_Day_from_Month(9),
            Max_Day_from_Month(12),
            Max_Day_from_Month(18),
            Max_Day_from_Month(24))

tot_hrs_min <- c(14, 12.5, 12, 11.5, 11.25, 11, 11, 11)
tot_hrs_max <- c(18, 15.5, 14, 13.5, 13.25, 12.5, 12.25, 12)
tot_hrs_mid <- c(16, 14, 13, 12.5, 12.25, 11.75, 11.625, 11.5)
num_naps_f1 <- c(5, 4, 3, 2, 2, 1, 1, 1)

# Ferber data from figure 2
night_sleep <- c(NA, NA, 8.5, 9.25, 9.5, 9.75, 9.625, 9.625)
day_sleep <- c(NA, NA, 4.25, 3.75, 2.75, 2, 2, 1.875)
num_naps_f2_min <- c(NA, NA, 3, 2, 2, 1, 1, 1)
num_naps_f2_max <- c(NA, NA, 4, 3, 2, 2, 1, 1)

# Combine all Ferber data and save
ferber_df <- data.frame(day = x_days, tot_hrs_min, tot_hrs_max,
                        tot_hrs_mid, num_naps_f1, night_sleep,
                        day_sleep, num_naps_f2_min, num_naps_f2_max) %>%
  mutate(day = as.integer(round(day)))

saveRDS(ferber_df, paste0(kFerberData, '/', kOutputStdBase))

#
# Get Ferber data with daily resolution ---
# Interpolate between Ferber reference points
#

# Get long format for interpolating each characteristic
ferber_long <- ferber_df %>%
  melt(id.var = 'day') %>%
  arrange(variable, day, value)

# Get all days and variables
all_days <- seq(1, kMaxDay) %>% 
  data.frame() %>%
  setNames(c('day'))
all_vars_all_days <- ferber_long %>%
  dplyr::select(variable) %>% 
  distinct() %>%
  crossing(all_days)


# Get the previous value by field
ferber_df_extend_1 <- ferber_long %>%
  rename(value_prev = value) %>%
  mutate(day_prev = day) %>%
  full_join(all_vars_all_days, by=c('variable', 'day')) %>%
  arrange(variable, day) %>%
  group_by(variable) %>%
  fill(value_prev, day_prev, .direction='down') %>%
  ungroup() %>%
  dplyr::filter(!is.na(value_prev))

# Get the next value by field
ferber_df_extend_2 <- ferber_long %>%
  rename(value_next = value) %>%
  mutate(day_next= day) %>%
  full_join(all_vars_all_days, by=c('variable', 'day')) %>%
  arrange(variable, day) %>%
  group_by(variable) %>%
  fill(value_next, day_next, .direction='up') %>%
  ungroup() %>%
  dplyr::filter(!is.na(value_next))

# Join and calculate interpolated value
ferber_df_extend_3 <- ferber_df_extend_1 %>%
  left_join(ferber_df_extend_2, by = c('variable', 'day')) %>%
  mutate(day_d = case_when(day_next == day_prev ~ 1,
                           TRUE ~  1 - (day - day_prev) / 
                             (day_next - day_prev)),
         value = day_d*value_prev + (1-day_d)*value_next)

# Convert back to wide format
ferber_df_extend <- ferber_df_extend_3 %>%
  dplyr::select(day, variable, value) %>%
  dcast(day ~ variable)

# Save extended Ferber standard data
saveRDS(ferber_df_extend, paste0(kFerberData, '/', kOutputStdExtended))


