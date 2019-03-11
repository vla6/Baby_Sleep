#
# Examine nap duration, time between
# naps and number of naps
#

library(plotly)
library(ggplot2)
library(data.table)
library(zoo)
library(dplyr)
library(lubridate)
library(tidyquant)

# For naps, interval to join sleeps
kWakeCollapse <- 10

# Input/output directories
kDataDir <- '00_Data/'
kImgDir <- '00_Images/'
kFerberData <- '00_Data/Ferber/'
kFerberImg <- '00_Images/Ferber'

# Base baby data files
kStdLong2 <- paste0(kDataDir, '/COMB_STD_LONG_2.rds')
kInterval5 <- paste0(kDataDir, '/INTERVAL_05.rds')

# Ferber guidelines
kFerberExt <- paste0(kFerberData, '/Ferber_Guidelines_Extended.rds')

# Baby-specific wake and bedtimes
kWakeFile <- paste0(kFerberData, '/Wake_Times_All.rds')

source('00_Scripts/Collapse_Long_Format.R')
source('00_Scripts/Week_from_Day.R')
source('00_Scripts/Day_from_Week.R')
source('00_Scripts/theme_bd.R')
source('00_Scripts/Factorize_Baby_Name.R')
source('00_Scripts/Month_from_Day.R')

#
# Get day only sleep data ----
#

# Get day boundaries for simplicity
wake_sleep_bound <- readRDS(kWakeFile)

# Get "standard long 2" format.  
std_long_2 <- readRDS(kStdLong2) %>%
  mutate(week = Week_from_Day(day)) 

# Restrict to completed weeks
complete_week_df <- std_long_2 %>%
  group_by(baby_name, week, day) %>%
    do(head(., n=1)) %>%
    ungroup() %>%
    group_by(baby_name, week) %>%
    dplyr::summarize(tot_days = n()) %>%
    ungroup() %>%
    dplyr::filter(tot_days == 7) %>%
    dplyr::select(baby_name, week)
  
# Limit sleep intervals to daytime hours
baby_week_bound_df <- std_long_2 %>%
  semi_join(complete_week_df, by=c('baby_name', 'week')) %>%
  left_join(wake_sleep_bound, by=c('baby_name', 'day')) %>% 
  arrange(baby_name, day, startTime) %>%
  group_by(baby_name) %>%
  fill(waketime_hr, bedtime_hr, .direction='up') %>% 
  ungroup() %>%
  mutate(this_date = floor_date(startTime, 'days'),
         day_start = this_date + minutes(60* waketime_hr),
         day_end = this_date + minutes(60*bedtime_hr),
         endTime = startTime + minutes(duration)) %>% 
  mutate(startTime = pmax(day_start, startTime),
         endTime = pmin(day_end, endTime),
         duration = as.integer(difftime(endTime, startTime, tz='GMT',
                                        units='mins'))) %>%
  dplyr::filter(startTime < endTime)
  
# Get day sleep naps, removing any that touch the boundaries
daily_sleep_collapsed_filt <- baby_week_bound_df %>%
  dplyr::filter(startTime > day_start &
                  startTime + minutes(duration) < day_end) %>%
  arrange(baby_name, day, startTime) %>%
  dplyr::filter(endTime > startTime) %>%
  dplyr::select(baby_name, day, startTime, duration)

# Collapse sleeps with short wakes
nap_col <- daily_sleep_collapsed_filt %>%
  group_by(baby_name) %>%
  Collapse_Long_Format(collapse_interval = kWakeCollapse) %>%
  ungroup() %>%
  mutate(month = Month_from_Day(day))

sum(daily_sleep_collapsed_filt$duration)
sum(nap_col$duration)

#
# Plot mean naps by day ----
# Do weekly means
#

# Get Ferber standards
ferber_df <- readRDS(kFerberExt)

# Expand Ferber nap info across all days
day_df <- nap_col %>% select(day) %>% distinct()

ferber_df_expand_nap_1 <- ferber_df %>%
  dplyr::select(day,num_naps_f2_min, num_naps_f2_max) %>%
  dplyr::rename(min1 = num_naps_f2_min, max1 = num_naps_f2_max) %>%
  mutate(ind_day_low = day) %>%
  full_join(day_df, by='day') %>%
  arrange(day) %>%
  fill(min1, max1, ind_day_low, .direction='down') %>%
  dplyr::filter(!is.na(min1))

ferber_df_expand_nap_2 <- ferber_df %>%
  dplyr::select(day,num_naps_f2_min, num_naps_f2_max) %>%
  dplyr::rename(min2 = num_naps_f2_min, max2 = num_naps_f2_max) %>%
  mutate(ind_day_high = day) %>%
  full_join(day_df, by='day') %>%
  arrange(day) %>%
  fill(min2, max2, ind_day_high, .direction='up') 

ferber_df_expand_nap <- ferber_df_expand_nap_1 %>%
  left_join(ferber_df_expand_nap_2, by='day') %>%
  mutate(day_d = case_when(ind_day_high == ind_day_low ~ 0.5,
                           TRUE ~  1 - (day - ind_day_low) / 
                             (ind_day_high - ind_day_low)),
         num_naps_f2_min = day_d*min1 + (1-day_d)*min2,
         num_naps_f2_max = day_d*max1 + (1-day_d)*max2) %>%
  mutate(week = Week_from_Day(day),
         month = Month_from_Day(day))

# Plot nap standard
ribbon_g <- ferber_df_expand_nap %>%
  ggplot(aes(x=month)) + 
  geom_ribbon(aes(ymin=num_naps_f2_min, ymax=num_naps_f2_max, 
                  fill = "Ferber's Normal Range"), alpha = 0.3) +
  scale_fill_manual(values=c("green"), name="") +
  geom_line(aes(y = num_naps_f2_min), color='green', alpha=0.2, size=2,
            show.legend= T) +
  geom_line(aes(y = num_naps_f2_max), color='green', alpha=0.3, size=2,
            show.legend= T)  +
  theme_bd +
  theme(legend.position='none') +
  labs(title = 'Normal Nap Range (Ferber)',
       y = '# Naps') +
  scale_x_continuous(limits=c(0,24), expand=expand_scale(0),
                     breaks=seq(0, 24, 4)) 

print(ribbon_g)
ggsave(paste0(kFerberImg, '/Nap_standard.png'),
       ribbon_g)

# Get weekly averages
nap_col_day <- nap_col %>%
  group_by(baby_name, day) %>%
  dplyr::summarize(num_naps = n(),
                   month = mean(month)) %>%
  ungroup()

nap_col_wk <- nap_col_day %>%
  mutate(week= Week_from_Day(day)) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(tot_days = n(),
                   mean_naps = mean(num_naps),
                   month = mean(month)) %>%
  ungroup() %>%
  dplyr::filter(tot_days == 7)

# Plot for each baby (daily)
baby_list <- nap_col %>% pull(baby_name) %>% unique()

for (baby in baby_list) {
  
  this_data <- nap_col_day %>% 
    dplyr::filter(baby_name == baby)
  
  x_max <- 1.1*(this_data %>% pull(month) %>% max())
  
  gp <- this_data %>% 
    ggplot() +
    geom_line(aes(x=month, y=num_naps), color='black') +
    geom_ribbon(data=ferber_df_expand_nap, aes(x=month,
                                               ymin=num_naps_f2_min, ymax=num_naps_f2_max, 
                                               fill = "Ferber's Normal Range"), alpha = 0.3) +
    scale_fill_manual(values=c("green"), name="") +
    geom_line(data=ferber_df_expand_nap, aes(x=month, y = num_naps_f2_min), 
              color='green', alpha=0.2, size=2, show.legend= T) +
    geom_line(data=ferber_df_expand_nap, aes(x = month, y = num_naps_f2_max), 
              color='green', alpha=0.3, size=2,show.legend= T)  +
    theme_bd +
    theme(legend.position='none') +
    labs(title =  paste0(baby, ' - Naps by Day'),
         y = '# Naps') +
    scale_x_continuous(limits=c(0,x_max), expand=expand_scale(0),
                       breaks=seq(0, 24, 4)) 
  
  print(gp)
  
  ggsave(paste0(kFerberImg, '/Nap_Test_Std_', baby, '.png'),
         gp, width=7, height=5)
  
}

# Plot all baby weekly averages
# with the standard

xmax_wk <- max(nap_col_wk$month) * 1.1

weekly_naps <- nap_col_wk %>% 
  mutate(baby_name = Factorize_Baby_Name(baby_name)) %>%
  ggplot() +
  geom_line(aes(x=month, y=mean_naps, color=baby_name), size=0.8) +
  geom_ribbon(data=ferber_df_expand_nap, aes(x=month,
                                             ymin=num_naps_f2_min, ymax=num_naps_f2_max, 
                                             fill = "Normal"), alpha = 0.2) +
  scale_fill_manual(values=c("green"), name="") +
  theme_bd +
  theme(legend.position='right') +
  labs(title ='Mean Naps by Week',
       y = '# Naps', color='') +
  scale_x_continuous(expand=expand_scale(0), limits=c(0,xmax_wk)) +
  scale_y_continuous(limits=c(1,5))

print(weekly_naps)
ggsave(paste0(kFerberImg, '/Nap_Week_Test_All.png'),
       weekly_naps, width=8, height=5)

#
# Nap duration ----
#

nap_dur_day <- nap_col %>%
  group_by(baby_name, day) %>%
  dplyr::summarize(duration = mean(duration),
                   month = mean(month)) %>%
  ungroup()

nap_dur_wk <- nap_dur_day %>%
  mutate(week= Week_from_Day(day)) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(tot_days = n(),
                   duration  = mean(duration),
                   month = mean(month)) %>%
  ungroup() %>%
  dplyr::filter(tot_days == 7) %>%
  mutate(duration_hrs = duration/60)

nap_dur_wk %>%
  ggplot(aes(x=month, y=duration_hrs, color=baby_name)) +
  geom_line()

xmax_dur <- max(nap_dur_wk$month)
xmax_dur <- 5 * ceiling(xmax_dur/5)

weekly_duration <- nap_dur_wk %>% 
  mutate(baby_name = Factorize_Baby_Name(baby_name)) %>%
  ggplot() +
  geom_line(aes(x=month, y=duration_hrs, color=baby_name), size=0.7) +
  theme_bd +
  theme(legend.position='right') +
  labs(title ='Mean Nap Length by Week',
       y = 'Length (hours)', color='') +
  scale_x_continuous(expand=expand_scale(0),limits=c(0,xmax_dur)) +
  coord_cartesian(ylim=c(0,2))

print(weekly_duration)

ggsave(paste0(kFerberImg, '/Nap_Week_Duration_All.png'),
       weekly_duration, width=8, height=5)

#
# Waketime between naps ----
# Group by baby and day
#

# Get wake times *overlapping* day'night boundaries
# Save for later use in autocorrelation

day_sleep_overlap <- baby_week_bound_df  %>% 
  dplyr::select(startTime, day, duration, baby_name)

saveRDS(day_sleep_overlap, paste0(kFerberData, '/Std_Long2_Day_Only.rds'))

# Collapse short wakes

nap_ol <- day_sleep_overlap %>%
  arrange(baby_name, startTime, day) %>%
  group_by(baby_name) %>%
  Collapse_Long_Format(collapse_interval = kWakeCollapse) %>%
  ungroup() %>%
  mutate(month = Month_from_Day(day))

sum(day_sleep_overlap$duration)
sum(nap_ol$duration)

# Get wake intervals
wake_int_1 <- nap_ol %>%
  group_by(baby_name, day) %>%
  mutate(endTime = startTime + minutes(duration),
         wake_interval = as.integer(difftime(startTime, lag(endTime), tz='GMT',
                                             units='mins')),
         wake_interval_hr = wake_interval/60) %>%
  ungroup() %>%
  mutate(week = Week_from_Day(day),
         month = Month_from_Day(day))

# Daily averages
wake_int_day <-wake_int_1 %>%
  dplyr::filter(!is.na(wake_interval)) %>%
  group_by(baby_name, day) %>%
  dplyr::summarize(mean_int = mean(wake_interval_hr),
                   tot_wakes = n(),
                   week = first(week),
                   month = first(month)) %>%
  ungroup() 

wake_int_week <-  wake_int_1 %>%
  dplyr::filter(!is.na(wake_interval)) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(mean_int = mean(wake_interval_hr),
                   tot_wakes = n(),
                   month = mean(month)) %>%
  ungroup() 


# Plot all baby weekly averages

xmax_gap <- max(wake_int_week$month)
xmax_gap <- 5 * ceiling(xmax_gap/5)

weekly_nap_gp <- wake_int_week %>% 
  mutate(baby_name = Factorize_Baby_Name(baby_name)) %>%
  ggplot(aes(x=month, y=mean_int, color=baby_name)) +
  geom_line(size=0.8) +
  theme_bd +
  theme(legend.position='right') +
  labs(title ='Mean Waketimes by Week',
       y = 'Waketime Duration (hours)', color='') +
  scale_x_continuous(expand=expand_scale(0), limits=c(0,xmax_gap))

print(weekly_nap_gp)
ggsave(paste0(kFerberImg, '/Nap_Week_Waketimes_All.png'),
       weekly_nap_gp, width=8, height=5)
