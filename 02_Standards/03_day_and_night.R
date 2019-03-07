#
# Examine sleep occurring during the "day" vs.
# "night". Examine 2 definitions of "night" -
# 7 PM to 7 AM, and a derived night based on babies'
# sleep patterns.
#
# Resuls are compared to information from
# Ferber, M. (2006). Solve Your Child’s Sleep Problems. 
# New York: Simon and Schuster 
#

library(plotly)
library(ggplot2)
library(data.table)
library(zoo)
library(dplyr)
library(lubridate)

# Smoothing duration for probabilities (days)
kSmoothDur <- 21

# Probability threshold for finding a wake
kProbThresh <- 0.3

# Input/output directories
kDataDir <- '00_Data/'
kImgDir <- '00_Images/'
kFerberData <- '00_Data/Ferber/'
kFerberImg <- '00_Images/Ferber'

# Extended Ferber Guidline
kInputFerber <- paste0(kFerberData, '/Ferber_Guidelines_Extended.rds')

# Standard long 2 data for all babies
kInputStdLong2 <- "00_Data/COMB_STD_LONG_2.rds"

# Timeseries data (15 min intervals)
kInputTS <- paste0(kDataDir, '/INTERVAL_15.rds')

# Will create a subdirectory for many probability plots
kFerberImgProb <- paste0(kFerberImg, '/','Probs/')

source('00_Scripts/Baby_Sleep_Plot.R')
source('00_Scripts/Week_from_Day.R')
source('00_Scripts/Day_from_Week.R')
source('00_Scripts/Max_Day_from_Month.R')
source('00_Scripts/Month_from_Day.R')
source('00_Scripts/theme_bd.R')

# Min days for baby-specific sleep window
kSleepWinMin <- Max_Day_from_Month(3)

#
# Read and Plot Ferber Guidelines ----
# Get fields needed.
# Plot expected "% night"
#

# Read data and calcualte percent night
ferber_df_expanded_night <- readRDS(kInputFerber) %>%
  dplyr::select(day, night_sleep, day_sleep) %>%
  mutate(percent_night_sleep = night_sleep/(day_sleep + night_sleep)) %>%
  arrange(day) %>%
  mutate(month = Month_from_Day(day)) %>%
  filter(!is.na(percent_night_sleep))

# Percent night sleep plot
ferber_night <- ferber_df_expanded_night %>%
  ggplot(aes(x=month, y=percent_night_sleep)) +
  geom_line(color='darkgreen', size=2) +
  theme_minimal() +
  labs(title='Normal Sleep Guidelines',
       x = 'Month',
       y = 'Percent Night Sleep') +
  theme(plot.title = element_text(size=20),
        axis.title.x = element_text(size=18),
        axis.title.y= element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.text = element_text(size=14)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1,
                                                   trim=T),
                     limits=c(0.4, 1.0), breaks = seq(0.4, 1, 0.2)) +
  scale_x_continuous(limits=c(0, 24))

print(ferber_night)

ggsave(filename = paste0(kFerberImg, '/Ferber_Guide_Night_Percent.png'),
       device='png', plot=ferber_night, height = 6, width =10)


#
# 7 AM/PM night ----
# Define night as 7 AM to 7 PM.
# Get the percent night sleep by baby over time.
#

std_long2 <- readRDS(kInputStdLong2)
  
# Get day/night intervals and overlaps
day_night_info <- std_long2 %>%
  mutate(
    endTime = startTime + minutes(duration),
    night1_start = floor_date(startTime, 'day'),
    night1_end = night1_start + hours(7) - minutes(1),
    night2_start = night1_start + hours(19),
    night2_end = night1_start+ hours(24) - minutes(1),
    night1_dur = as.integer(1 + difftime(pmin(night1_end, endTime),
                                         pmax(night1_start, startTime),
                                         tz = 'GMT', units='mins')),
    night1_dur = case_when(night1_dur > 0 ~ night1_dur,
                           TRUE ~ as.integer(0)),
    night2_dur = as.integer(1+ difftime(pmin(night2_end, endTime),
                                        pmax(night2_start, startTime),
                                        tz = 'GMT', units='mins')),
    night2_dur = case_when(night2_dur > 0 ~ night2_dur,
                           TRUE ~ as.integer(0)),
    night_duration = night1_dur + night2_dur) 

# Get total night and day portions of sleep
day_night_info_daily <- day_night_info %>%
  group_by(baby_name, day) %>%
  dplyr::summarize(total_sleep_hrs = sum(duration, na.rm=T)/ 60,
                   night_sleep_hrs = sum(night_duration, na.rm = T) /60) %>%
  ungroup() %>%
  mutate(fract_night_sleep = night_sleep_hrs/total_sleep_hrs)

# Get weekly averages
day_night_info_weekly <- day_night_info_daily %>%
  mutate(week = Week_from_Day(day)) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(fract_night_sleep = mean(fract_night_sleep, na.rm=T),
                   tot_days = n()) %>%
  ungroup() %>%
  dplyr::filter(tot_days == 7) %>%
  dplyr::select(week, baby_name, fract_night_sleep)


# Plot the combined weekly data

xmax_week <- day_night_info_weekly %>%
  pull(week) %>% max()

comb_nd_plot <- day_night_info_weekly %>%
  mutate(month = Month_from_Day(Day_from_Week(week))) %>%
  ggplot(aes(x=month, y=fract_night_sleep, color=baby_name)) +
  geom_line(size=0.5) +
  geom_line(aes(x=month, y=percent_night_sleep),
            data = ferber_df_expanded_night,
            color='darkgreen', size=2) +
  labs(title='Percent Night Sleep - Weekly Averages',
       subtitle='Night defined as 7 PM through 7 AM',
       x = 'Month',
       y = 'Percent Night Sleep',
       color="") +
  theme_bd + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1,
                                                   trim=T),
                     limits=c(0.4, 1.0), breaks = seq(0.4, 1, 0.2)) +
  scale_x_continuous(limits=c(0, Month_from_Day(xmax_week * 7)),
                     breaks=seq(0, Month_from_Day(xmax_week * 7), 4))

print(comb_nd_plot)
ggsave(filename = paste0(kFerberImg, '/Night_Percent_All.png'),
       device='png', plot=comb_nd_plot, height = 6, width =10)

#
# Baby's morning and evening ----
# For calculating baby-specific nights.  Get the
# probability of sleep per 15 minutes by day,
# smoothing over 14 days.  
# Will allow finding the morning/night boundaries
#

# Start with the 15 minute timeseries data
ts_data <- readRDS(kInputTS) %>%
  arrange(baby_name, interval, day)

# Get rolling sleep probabilities
ts_data2 <- ts_data %>%
  arrange(baby_name, interval, day) %>%
  group_by(baby_name) %>%
  mutate(sleep_prob = rollmean(sleep_flag, 
                               k=kSmoothDur, 
                               fill=c('extend', NA,
                                      'extend'))) %>%
  ungroup() %>%
  arrange(baby_name, interval_start)

# Filter out babies who don't have data
# in a valid time period
sleep_probability_df <- ts_data2 %>%
  group_by(baby_name) %>%
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  dplyr::filter(max_day >= kSleepWinMin) %>%
  dplyr::select(-max_day)

# Get the morning wake time by day
wake_df <- sleep_probability_df %>%
  filter(sleep_prob <= kProbThresh &
           day >= kSleepWinMin ) %>%
  arrange(baby_name, day, interval_start) %>%
  group_by(baby_name, day) %>%
  do(head(., n=1)) %>%
  ungroup() %>%
  mutate(waketime_hr = interval/60)

# Get the evening sleep time by day
sleep_df <- sleep_probability_df %>%
  filter(sleep_prob <= kProbThresh &
           day >= kSleepWinMin) %>%
  arrange(baby_name, day, interval_start) %>%
  group_by(baby_name, day) %>%
  do(tail(., n=1)) %>%
  ungroup() %>%
  mutate(bedtime_hr = interval/60)

sleep_wake_time_df <- wake_df %>%
  dplyr::select(baby_name, day, waketime_hr) %>%
  left_join(sleep_df %>%
              dplyr::select(baby_name, day, bedtime_hr),
            by=c('baby_name', 'day')) %>%
  dplyr::select(day, baby_name, waketime_hr, bedtime_hr)

# Save data in Ferber directory - wake times
saveRDS(sleep_wake_time_df, 
        file=paste0(kFerberData, '/Wake_Times_All.rds'))

#
# Plot bounaries ----
# Show the wake and bedtimes on a sleep
# history chart for audit
#

# Get the boundaries with the full data
int_plot_data <- sleep_probability_df %>%
  left_join(sleep_wake_time_df, by=c('baby_name', 'day')) %>%
  mutate(sleep_flag = factor(sleep_flag),
         waketime_min = waketime_hr * 60,
         bedtime_min= bedtime_hr * 60) %>%
  arrange(baby_name, interval_start) 

# Plot sleep boundaries for each baby
baby_names <- int_plot_data %>% pull(baby_name) %>% unique() 
for (baby in baby_names) {
  int_plot <- int_plot_data %>%
    mutate(month = Month_from_Day(day)) %>%
    dplyr::filter(baby_name == baby) %>%
    ggplot(aes(x=month, y=interval, fill=sleep_flag)) + 
    geom_tile() +
    geom_line(aes(y=waketime_min), color='black', size=2) +
    geom_line(aes(y=bedtime_min), color='black', size=2) +
    labs(title=baby) +
    theme(axis.line=element_blank(),
          axis.text.x=element_text(size=12),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) +
    scale_x_continuous(expand = expand_scale(0)) +
    scale_y_continuous(expand = expand_scale(0)) +
    scale_fill_manual(breaks=c("0", "1"),
                      values=c("white", "blue"))
  
  print(int_plot)
  ggsave(filename = paste0(kFerberImg, '/Wake_Bed_Check_',
                           baby, '.png'),
         device='png', plot=int_plot, height = 4, width =10)
}

#
# Plot probability curves ----
#

# Create a directory for these plots, if needed
dir.create(kFerberImgProb, showWarnings = FALSE)

# Function for plotting a probability curve
prob_curve_plot <- function(data) {
  
  gr_title_str <- paste0(data$baby_name[[1]], '_Day_', 
                         prettyNum(data$day[[1]]))
  gp <- data %>%
    mutate(int_hr = interval/60) %>%
    ggplot(aes(x=int_hr, y=sleep_prob)) +
    geom_line() +
    theme(plot.title = element_text(size=20),
          axis.title.x = element_text(size=18),
          axis.title.y= element_text(size=18),
          axis.text.x  = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.text = element_text(size=14)) +
    scale_y_continuous(limits=c(0,1)) +
    scale_x_continuous(limits=c(0,24),
                       breaks = seq(0, 24, 4)) +
    labs(title=paste0('Sleep Probability By Hour - ',
                      gr_title_str),
         x = 'Hour of Day',
         y = 'Probability')
  
  print(gp)
  ggsave(filename=paste0(kFerberImgProb, 'Prob_Still_',
                         gr_title_str, '.png'), plot=gp,
         device='png', width = 8, height = 6)
  
  # Return empty frame for easy loop
  return(data.frame())
  
}

# Stills to save (days, if available)
kSelDays <- c(7, 14, 31, 61, 91, 150, 200, 250, 
              300, 350, 400, 450, 500, 550, 600)

# Do the plotting
int_plot_data %>%
  dplyr::filter(day %in% kSelDays) %>%
  group_by(baby_name, day) %>%
  do(prob_curve_plot(.))

#
# Percent night, baby-specific ----
#

# Get the % night sleep by this defnition
night_day_sleep_baby_plot_df <- int_plot_data %>%
  dplyr::filter(sleep_flag == 1 &
                  !is.na(waketime_min)) %>%
  mutate(night_flag = ifelse(interval < waketime_min |
                               interval >= bedtime_min, 1, 0)) %>%
    group_by(baby_name, day) %>%
    dplyr::summarize(fract_night_sleep = mean(night_flag)) %>%
    ungroup() %>%
    mutate(week = Week_from_Day(day)) %>%
    group_by(baby_name, week) %>%
    dplyr::summarize(fract_night_sleep = mean(fract_night_sleep, na.rm=T),
                     tot_days = n()) %>%
    ungroup() %>%
    dplyr::filter(tot_days == 7) %>%
    dplyr::select(baby_name, week, fract_night_sleep)
  

# Plot the combined weekly percent night sleep data
xmax_week <- night_day_sleep_baby_plot_df %>%
  pull(week) %>% max()

comb_nd_plot_baby <- night_day_sleep_baby_plot_df %>%
  mutate(month =  Month_from_Day(Day_from_Week(week))) %>%
  ggplot(aes(x=month, y=fract_night_sleep, color=baby_name)) +
  geom_line() +
  geom_line(aes(x=month, y=percent_night_sleep),
            data = ferber_df_expanded_night,
            color='darkgreen', size=2) +
  theme_minimal() +
  labs(title='Percent Night Sleep - Weekly Averages',
       subtitle='Baby-specific night definition',
       x = 'Month',
       y = 'Percent Night Sleep',
       color="") +
  theme(plot.title = element_text(size=20),
        axis.title.x = element_text(size=18),
        axis.title.y= element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.text = element_text(size=14)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1,
                                                   trim=T),
                     limits=c(0.4, 1.0), breaks = seq(0.4, 1, 0.2)) +
  scale_x_continuous(limits=c(0, Month_from_Day(xmax_week * 7)),
                     breaks=seq(0, Month_from_Day(xmax_week * 7), 4))

print(comb_nd_plot_baby)

ggsave(filename = paste0(kFerberImg, '/Night_Percent_All_BS.png'),
       device='png', plot=comb_nd_plot_baby, 
       height = 6, width =10)

#
# Length of night and percent of night asleep ----
#

sleep_wake_time_df
ts_data

# Combine the interval data with the wake and sleep times.
# Fill toward early times

sleep_with_int_day <- sleep_wake_time_df %>%
  right_join(ts_data, by=c('day', 'baby_name')) %>%
  fill(waketime_hr, bedtime_hr, .direction='up') %>%
  fill(waketime_hr, bedtime_hr, .direction='down') %>%
  dplyr::filter(!is.na(waketime_hr)) %>%
  mutate(night1_flag = ifelse((interval + 8)/60  < waketime_hr, 
                              1, 0),
         night2_flag = ifelse((interval+ 8)/60 >= bedtime_hr, 
                              1, 0)) %>%
  arrange(baby_name, day, interval) %>%
  group_by(baby_name, day, night1_flag, night2_flag) %>%
  summarize(interval = first(interval),
            tot_sleep = 15 * sum(sleep_flag),
            tot_time = n() * 15) %>%
  ungroup() %>%
  arrange(baby_name, day, interval) %>%
  group_by(baby_name) %>%
  mutate(join_grp = (night1_flag == 1 &
                       (lag(night2_flag, default=1) == 1)),
         tot_sleep = ifelse(join_grp, tot_sleep +
                              lag(tot_sleep, default=0), 
                            tot_sleep),
         tot_time = ifelse(join_grp, tot_time +
                             lag(tot_time, default=0), 
                           tot_time)) %>%
  ungroup() %>%
  dplyr::filter(!night2_flag) %>%
  dplyr::select(-join_grp, -night2_flag) %>%
  rename(night_flag = night1_flag) %>%
  mutate(tot_hrs = tot_time/60,
         fract_sleep = tot_sleep/tot_time) %>%
  dplyr::select(baby_name, day, 
                night_flag, tot_hrs, fract_sleep,
                tot_time, tot_sleep)

# Get weekly night length and sleep time
sleep_with_int_week <- sleep_with_int_day %>%
  mutate(week = Week_from_Day(day)) %>%
  group_by(baby_name, week, night_flag) %>%
  summarize(tot_sleep = mean(tot_sleep, na.rm=T),
            tot_time = mean(tot_time, na.rm=T),
            tot_days = n()) %>%
  ungroup() %>%
  dplyr::filter(tot_days == 7) %>%
  mutate(fract_sleep = tot_sleep/tot_time,
         tot_hrs = tot_time/60,
         sleep_hrs = tot_sleep/60) %>%
  dplyr::select(baby_name, week, 
                night_flag, tot_hrs, fract_sleep)

saveRDS(sleep_with_int_week, 
        file=paste0(kFerberData, 'Tot_Sleep_Day_Night_Weekly.rds'))

saveRDS(sleep_with_int_day, 
        file=paste0(kFerberData, 'Tot_Sleep_Day_Night_Daily.rds'))

sleep_with_int_day %>%
  filter(baby_name == 'Red' & night_flag == 1) %>%
  ggplot(aes(x=day, y=tot_hrs)) +
  geom_line()

# Function to plot the day/night spans
Plot_Day_Night <- function(data,
                           night_plot=T,
                           y_var = tot_hrs,
                           title_str = '', 
                           subtitle_str = '',
                           y_lab = '',
                           month_days = Max_Day_from_Month(1),
                           y_min=0, y_max = 14) {
  y_quo = enquo(y_var)
  
  # Resolve plot labe strings
  if (is.null(title_str) | title_str == '') {
    title_str = paste0('Sleep Span Plot  - ',
                       ifelse(night_plot, 'night', 'day'),
                       ' - ',
                       quo_name(y_quo)) 
  }
  if (is.null(y_lab) | y_lab == '') {
    y_lab = quo_name(y_quo)
  }
  
  # Modify data
  data2 <- data %>%
    mutate(month = Day_from_Week(week)/month_days,
           response = (!!y_quo))  %>%
    dplyr::filter(night_flag == ifelse(night_plot, 1, 0))
  
  # Get max x range
  xmax <- data2 %>%
    pull(month) %>% max()
  
  span_plot_baby <- data2 %>%
    ggplot(aes(x=month, y=response, color=baby_name)) +
    geom_line(size=1) +
    theme_minimal() +
    labs(title= title_str,
         subtitle= subtitle_str,
         x = 'Month',
         y = y_lab,
         color="") +
    theme(plot.title = element_text(size=20),
          axis.title.x = element_text(size=18),
          axis.title.y= element_text(size=18),
          axis.text.x  = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.text = element_text(size=14)) +
    scale_y_continuous(limits=c(y_min, y_max), 
                       breaks = seq(y_min, y_max, 2)) +
    scale_x_continuous(limits=c(0, 1.1*xmax),
                       breaks=seq(0, xmax, 4))
  
  print(span_plot_baby)
  
  return(span_plot_baby)
}

# Plot night sleep durations and hours
gp_night_hr <- sleep_with_int_week %>%
  mutate(night_hrs= tot_hrs * fract_sleep) %>%
  Plot_Day_Night(night_plot=T, y_var = night_hrs,
                 title_str = 'Hours of Sleep in Night',
                 subtitle_str = 'Baby-specific night definition',
                 y_lab = 'Sleep hours (weekly average)')
ggsave(filename = paste0(kFerberImg, '/Night_Sleep_Hours.png'),
       device='png', plot=gp_night_hr, height = 7, width =10)

gp_night_tot <- sleep_with_int_week %>%
  Plot_Day_Night(night_plot=T, y_var = tot_hrs,
                 title_str = 'Length of Night',
                 subtitle_str = 'Baby-specific night definition',
                 y_lab = 'Tot hours (weekly average)')
ggsave(filename = paste0(kFerberImg, '/Night_Duration_Hours.png'),
       device='png', plot=gp_night_tot, height = 7, width =10)

# Plot day sleep durations and hours
gp_day_hr <- sleep_with_int_week %>%
  mutate(day_hrs= tot_hrs * fract_sleep) %>%
  Plot_Day_Night(night_plot=F, y_var = day_hrs,
                 title_str = 'Hours of Sleep in Day',
                 subtitle_str = 'Baby-specific night definition',
                 y_lab = 'Sleep hours (weekly average)',
                 y_min=0, y_max=14)
ggsave(filename = paste0(kFerberImg, '/Day_Sleep_Hours.png'),
       device='png', plot=gp_day_hr, height = 7, width =10)

gp_day_tot <- sleep_with_int_week %>%
  Plot_Day_Night(night_plot=F, y_var = tot_hrs,
                 title_str = 'Length of Day',
                 subtitle_str = 'Baby-specific night definition',
                 y_lab = 'Tot hours (weekly average)',
                 y_min=0, y_max=14)
ggsave(filename = paste0(kFerberImg, '/Day_Duration_Hours.png'),
       device='png', plot=gp_day_tot, height = 7, width =10)

