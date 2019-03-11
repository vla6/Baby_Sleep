#
# Autocorrelation
#

library(ggplot2)
library(data.table)
library(stringr)
library(zoo)
library(dplyr)
library(lubridate)
library(entropy)
library(quantmod)
library(stats)

# Interval around 24 hours to look for peak (hrs)
kPeakIntDay <- 2

# Input/output directories
kDataDir <- '00_Data/'
kImgDir <- '00_Images/'

# Timeseries directories 
kTSData <- '00_Data/Timeseries/'
kTSImg <- '00_Images/Timeseries/'

# Image subdirectory to create
kTSImgSub <- '00_Images/Timeseries/ACF_week'

# 5 minute interval data 
kInterval5 <- paste0(kDataDir, '/INTERVAL_05.rds')

# Day sleep only file
kFerberData <- '00_Data/Ferber/'
kDaySleepAll <- paste0(kFerberData, '/Std_Long2_Day_Only.rds')

source('00_Scripts/Week_from_Day.R')
source('00_Scripts/Day_from_Week.R')
source('00_Scripts/Month_from_Day.R')
source('00_Scripts/theme_bd.R')
source('00_Scripts/Interval_from_Std_Long2.R')

#
# Dir create for saving some files ----
#

dir.create(kTSImgSub, showWarnings = FALSE)

#
# ACF ----
# weekly
#

# Function to get the acf, without plots etc.
acf_simple <- function(data, hrs_conv = 5/60,
                       lag.max = 650) {
  acf_obj <-acf(data$sleep_flag, plot=FALSE,
                lag.max = lag.max, 
                demean=F)
  
  acf_data <- data.frame(tau = acf_obj$lag, acf = acf_obj$acf) %>%
    mutate(tau_hrs = tau * hrs_conv) 
  
  return(acf_data %>% dplyr::select(tau_hrs, acf))
}

# Import 5 minute timeseries
int_data <- readRDS(kInterval5) %>%
  mutate(week = Week_from_Day(day),
         hour = interval/60) 

# Get full weeks
full_week_df <- int_data %>%
  distinct(baby_name, week, day) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(num_days = n()) %>%
  ungroup() %>%
  dplyr::filter(num_days == 7) %>%
  dplyr::select(baby_name, week)

# Get autocorrelation by baby and week
acf_data_full <- int_data %>%
  semi_join(full_week_df, by=c('baby_name', 'week')) %>%
    group_by(baby_name, week) %>%
    do(acf_simple(.)) %>%
    ungroup() 

# Save autocorr data
saveRDS(acf_data_full, file=paste0(kTSData, '/ACF_all_comp.rds'))

#
# ACF Plots ----
#

# Function to plot an ACF (simple)
simple_acf_plot <- function(data, trans_name = 'identity',
                            savedir = NULL,
                            accuracy = 1,
                            plot_filename_prefix = 'ACF_SELWEEK_') {
  
  plot_head_week = data$week[[1]]
  plot_head_name = data$baby_name[[1]]
  plot_trans = case_when(trans_name == 'identity' ~ '',
                         TRUE ~ paste0('(', trans_name, ')'))
  
  plot_head_str = paste('ACF', plot_head_name,
                        'Week', plot_head_week, plot_trans)
  
  gp <- data %>% 
    ggplot(aes(x=tau_hrs, y=acf)) + 
    geom_line() +
    theme_bd +
    scale_x_continuous(labels=scales::number_format(accuracy = accuracy),
                       trans= trans_name) +
    scale_y_continuous(limits=c(0,1)) + 
    labs(title = plot_head_str,
         y = 'acf', 
         x = 'tau (hours)')
  
  if (!is.null(savedir)) {
    file_str = paste0(plot_filename_prefix, trans_name, '_',
                      plot_head_name,'_WK_', 
                      str_pad(plot_head_week, 3, pad='0'))
    ggsave(paste0(savedir, '/', file_str, '.png'),
           gp, width=6, height=5)
  }
  
  print(gp)
  
  # Return null data frame as noop
  return(data.frame())
}

# Test function
acf_data_full %>%
  dplyr::filter(baby_name == 'Ryan' & week == 8) %>%
  simple_acf_plot()

# Get some characteristic plots per baby
acf_select_plot <- function(data, n_weeks = 10, savedir = NULL,
                            trans_name = 'identity',
                            accuracy = 1,
                            plot_filename_prefix = 'ACF_SELWEEK_') {
  
  # Get selected weeks - evenly spaced
  week_sel <- data %>%
    dplyr::select(baby_name, week) %>%
    distinct() %>%
    arrange(baby_name, week) %>%
    group_by(baby_name) %>%
    mutate(this_int = floor(n() / (n_weeks)),
           first_week = first(week),
           sel_plot = case_when((week - first_week) %% 
                                  this_int == 0 ~ TRUE,
                                TRUE ~ FALSE))  %>%
    ungroup() %>% 
    dplyr::filter(sel_plot == TRUE) %>%
    dplyr::select(baby_name, week)
  
  data %>%
    semi_join(week_sel, by=c('baby_name', 'week')) %>%
    group_by(baby_name, week) %>%
    do(simple_acf_plot(., savedir = savedir,
                       trans_name = trans_name,
                       accuracy = accuracy,
                       plot_filename_prefix = plot_filename_prefix)) %>%
    ungroup()
  
}

# Linear plot
# for all babies
acf_data_full %>%
  acf_select_plot(savedir = kTSImgSub, n_weeks=10)

# Log plot
# for all babies
acf_data_full %>%
  acf_select_plot(savedir = kTSImgSub, n_weeks=10,
                  trans_name = 'log10',
                  accuracy = 0.1)

#
# ACF, days only----
# Create weekly plots, which are limited in time
#

# Get day-only sleep, calculated in a previous script
day_sleep_file <- readRDS(kDaySleepAll) %>%
  mutate(week = Week_from_Day(day))

# Get interval, limiting to full weeks
this_int_filt <- day_sleep_file %>% 
  semi_join(full_week_df, by=c('baby_name', 'week')) %>%
  Interval_from_Std_Long2(interval_mins = 5) 

# Get the autocorrelation by week
acf_data_full_filt <- this_int_filt %>% 
  group_by(baby_name, day) %>%
  do(acf_simple(., lag.max=88)) %>%
  ungroup() %>%
  mutate(week = Week_from_Day(day)) %>%
  group_by(baby_name, week, tau_hrs) %>%
  summarize(acf = mean(acf, na.rm=T))  %>%
  ungroup()

# Save autocorr data
saveRDS(acf_data_full_filt, file=paste0(kTSData, '/ACF_days_comp.rds'))

table(acf_data_full_filt$baby_name)

acf_data_full_filt %>% filter(baby_name == 'Ryan' & week == 3) %>%
  ggplot(aes(x=tau_hrs, y=acf)) +
  geom_line()

# Linear plot
# for all babies
acf_data_full_filt %>%
  acf_select_plot(savedir = kTSImgSub, n_weeks=10,
                  plot_filename_prefix = 'ACF_DAY_')

# Log plot
# for all babies
acf_data_full_filt %>%
  acf_select_plot(savedir = kTSImgSub, n_weeks=10,
                  trans_name = 'log10',
                  accuracy = 0.1,
                  plot_filename_prefix = 'ACF_DAY_')

#
# Wake times ----
# Get first peak location in the day data
#

# function to get peaks in an ACF curve
# Returns the data frame with a peak ind appended
data_with_peaks <- function(data) {
  
  # Add a row id to the data and sort
  data_mod <- data %>%
    arrange(tau_hrs) %>%
    mutate(row_id = seq_len(n()))
  
  peak_list <- data_mod %>%
    pull(acf) %>%
    findPeaks()
  
  data_with_peak_ind <- data_mod %>%
    mutate(peak = case_when((row_id + 1) %in% peak_list ~ TRUE,
                            TRUE ~ FALSE))
  return(data_with_peak_ind)
  
}

# Function to plot the acf with peaks
peak_plot <- function(data, title_str = '') {
  gp <- data %>%
    ggplot(aes(x=tau_hrs, y=acf)) +
    geom_line() +
    geom_point(aes(color=peak, size=peak)) +
    theme_bd +
    scale_colour_manual(values = c("transparent", "orange")) + 
    scale_size_manual(values =c(0, 7)) +
    theme(legend.position='none') +
    labs(title = title_str,
         x = 'tau (hours)')
  
  print(gp)
  return(gp)
}

# Create a couple example plots

peak_plot_8 <- acf_data_full_filt %>% 
  dplyr::filter(baby_name == 'Ryan' & 
                  week == 8) %>%
  do(data_with_peaks(.)) %>%
  peak_plot(title_str = 'Ryan week 8')

ggsave(paste0(kTSImgSub, '/ACF_peak_ex_Ryan_08.png'),
       peak_plot_8)

peak_plot_62 <- acf_data_full_filt %>% 
  dplyr::filter(baby_name == 'Ryan' & 
                  week == 62) %>%
  do(data_with_peaks(.)) %>%
  peak_plot(title_str = 'Ryan week 62')

ggsave(paste0(kTSImgSub, '/ACF_peak_ex_Ryan_62.png'),
       peak_plot_62)

# Waketimes for all babies
# Get the first peak

acf_data_full_filt_peak <- acf_data_full_filt %>%
  group_by(baby_name, week) %>%
  do(data_with_peaks(.)) %>% 
  ungroup()

table(acf_data_full_filt_peak$peak)

# Get first peak by baby and week
acf_first_peak <- acf_data_full_filt_peak %>%
  arrange(baby_name, week, tau_hrs) %>%
  dplyr::filter(peak == TRUE) %>%
  group_by(baby_name, week) %>%
  do(head(., n=1)) %>%
  ungroup() %>%
  mutate(month = Month_from_Day(Day_from_Week(week)))

xmax_dur <- max(acf_first_peak$month)
xmax_dur <- 5 * ceiling(xmax_dur/5)

peak_plot <- acf_first_peak %>%
  ggplot(aes(x=month, y=tau_hrs, color=baby_name)) +
  geom_line(size=0.8) +
  theme_bd +
  theme(legend.position='right') +
  labs(title = 'Waketime - First Nap Interval',
       x = 'Month',
       y = 'Lag to First Nap ACF Peak (hrs)',
       color = '') +
  scale_x_continuous(limits=c(0,xmax_dur)) +
  scale_y_continuous(limits=c(0, 1.1*max(acf_first_peak$tau_hrs))) 

print(peak_plot)
ggsave(paste0(kTSImg, '/Wake_Time_All.png'),
       peak_plot)

#
# Slope init ----
# Get initial ACF slope by week
# Use an exponential fit
#

# Function to perform the nonlinear least squares fit.
# Returns fit object and data in list
sleep_nls <- function(data) {
  
  
  # Get the valley
  mod_data <- data %>%
    mutate(id = seq_len(nrow(.)))
  
  valley_1 <- tryCatch({
    (mod_data  %>%
       pull(acf) %>%
       findValleys())[[1]]},
    error = function(cond) {NULL})
  
  if (is.null(valley_1)) {
    return(NULL)
  }
  
  data_bef_valley <- mod_data %>%
    dplyr::filter(id <= valley_1)
  
  # Get thresh val
  thresh <-  mod_data %>%
    dplyr::filter(id > valley_1) %>%
    pull(tau_hrs) %>% min()
  
  # Get the exponenial fit
  tryCatch({
    nfit <- nls(acf ~ (1-b)* exp(-tau_hrs/a)  +  b,
                data=data_bef_valley, start=list(a=0.5,
                                                 b=0.02))
    
    return(list(nfit= nfit, data = data_bef_valley, thresh=thresh))},
    error=function(cond) {NULL})
}


# Function to return a data frame using the nls fit
acf_init_slope <- function(data) {
  
  # Do the fit
  nlist <- data %>% sleep_nls()
  
  nfit <- nlist[['nfit']]
  thresh <- nlist[['thresh']]
  
  if(is.null(nfit)) {
    return(data.frame())
  }
  
  # Build a data framf from the results
  sum_nfit <- summary(nfit)
  sum_nfit_param <- sum_nfit$parameters
  
  sum_df <- sum_nfit_param %>% 
    as.data.frame() %>%
    bind_cols(param = rownames(sum_nfit_param),
              thresh = rep(thresh, nrow(sum_nfit_param)),
              sigma= rep(sum_nfit$sigma, nrow(sum_nfit_param)))
  
  return(sum_df)
  
}

# Get the initial slopes for the days only                                             vc

initial_slopes_filt <- acf_data_full_filt %>% 
  group_by(baby_name, week) %>%
  do(acf_init_slope(.)) %>%
  ungroup() %>%
  mutate(day= Day_from_Week(week),
         month = Month_from_Day(day))

saveRDS(initial_slopes_filt, file=paste0(kTSData, '/Init_Slope_filt.rds'))

initial_slopes_filt %>%
  dplyr::filter(param == 'a') %>%
  ggplot(aes(x=week, y=sigma, color=baby_name)) +
  geom_line()


initial_slopes_filt %>%
  dplyr::filter(param == 'a') %>%
  ggplot(aes(x=week, y=thresh, color=baby_name)) +
  geom_line()

initial_slopes_filt %>%
  dplyr::filter(param == 'b') %>%
  ggplot(aes(x=week, y=Estimate, color=baby_name)) +
  geom_line()

x_max_comb <- 5 * ceiling(max(initial_slopes_filt$month)/5)

init_slope_comb <- initial_slopes_filt %>%
  dplyr::filter(param == 'a') %>%
  ggplot(aes(x=month, y=Estimate, color=baby_name)) +
  geom_line(size=0.8) +
  coord_cartesian(ylim=c(0, 3.5)) +
  theme_bd +
  scale_y_continuous(expand = expand_scale(0)) +
  scale_x_continuous(expand = expand_scale(0), limits=c(0,x_max_comb)) +
  labs(title= 'ACF Low-Tau Exp Fit Timescale',
       y = 'Timescale Param (hours)',
       color='',
       x = 'Month')

print(init_slope_comb)

ggsave(paste0(kTSImg, '/Autocor_Slope_fits.png'),
       init_slope_comb)

# 
# Plot exponential fits ----
# Selected
#

data <- acf_data_full_filt %>% 
  filter(baby_name == 'Ryan'&  week == wk) 

title_str = ''
# Function to return a data frame using the nls fit
acf_fit_and_plot <- function(data, title_str = '') {
  
  # Do the fit
  nlist <- data %>% sleep_nls()
  
  nfit <- nlist[['nfit']]
  thresh <- nlist[['thresh']]
  data_mod <- nlist[['data']]
  
  if(!is.null(nfit)) {
    
    sum_nfit <- summary(nfit)
    sum_nfit_param <- sum_nfit$parameters
    
    data_mod$pred <- predict(nfit)
  } else {
    data_mod <- data
    data_mod$pred = NA
  }
  
  data_plot <- data_mod %>%
    dplyr::select(tau_hrs, acf, pred) %>%
    melt(id.vars='tau_hrs')
  
  gp <- data_plot %>% 
    ggplot(aes(x=tau_hrs, y=value, color=variable)) +
    geom_line() + 
    theme_bd +
    scale_y_continuous(expand = expand_scale(0)) +
    scale_x_continuous(expand = expand_scale(0)) +
    labs(title= title_str,
         y = 'acf',
         color='',
         x = 'Tau (hours)') +
    scale_color_manual(labels = c("acf", "fit"), values = c("black", "red"))
  
  return(gp)
  
}

wk_sel <- c(2, 12, 30, 60)

for (wk in wk_sel) {
  gp <- acf_data_full_filt %>% 
    filter(baby_name == 'Ryan'&  week == wk) %>%
    acf_fit_and_plot(title_str = paste0('Ryan week ',
                                        prettyNum(wk)))
  
  print(gp)
  ggsave(paste0(kTSImgSub, '/Expfit_example_Ryan_', prettyNum(wk),
                '.png'), gp)
  
}

#
# Night existence ----
# ACF at 24 h
#

# Flag peaks
acf_data_full_peak <- acf_data_full %>%
  group_by(baby_name, week) %>%
  do(data_with_peaks(.)) %>% 
  ungroup()

peak_24h_df <- acf_data_full_peak %>%
  dplyr::filter(tau_hrs >= 24 - kPeakIntDay &
                  tau_hrs <= 24 + kPeakIntDay)

peak_24h_df_agg <- peak_24h_df %>%
  arrange(baby_name, week, desc(acf)) %>%
  group_by(baby_name, week) %>%
  do(head(., n=1)) %>%
  ungroup() %>%
  group_by(baby_name) %>%
  mutate(n_wk = n()) %>%
  ungroup() %>%
  dplyr::filter(n_wk >= 10) %>%
  mutate(month = Month_from_Day(Day_from_Week(week)))

print_peak_24 <- function(data, trans= 'identity',
                          outfile=NULL,
                          legend.position='right',
                          title_str = '24h ACF Peak Height vs Time') {
  
  lim_high <- 5*ceiling(max(data$month)/5)
  
  if (trans == 'identity') {
    lim_low = 0
  } else {
    lim_low= 0.1
  }
  
  n_units <- data %>% dplyr::select(baby_name) %>% 
    distinct() %>% nrow()
  
  if (n_units > 1) {
    gp <- data %>%
      ggplot(aes(x=month, y=acf, color=baby_name)) 
  } else {
    gp <- data %>%
      ggplot(aes(x=month, y=acf)) 
  }
  
  gp <- gp +
    geom_line(size=0.8) +
    theme_bd +
    theme(legend.position=legend.position) +
    scale_y_continuous(expand = expand_scale(0), trans=trans,
                       limits=c(lim_low,1)) +
    scale_x_continuous(expand = expand_scale(0),
                       limits=c(lim_low, lim_high), trans=trans) +
    labs(title= title_str,
         y = 'acf(24)',
         color='',
         x = 'Month') 
  
  print(gp)
  return(gp)
  
}

peak_24_g <- peak_24h_df_agg %>%
  print_peak_24()

ggsave(paste0(kTSImg, '/Autocor_peak_height_24h.png'),
       peak_24_g)

# Same but log
peak_24_g_log <- peak_24h_df_agg %>%
  print_peak_24(trans='log10',
                title_str= '24h ACF Peak Height vs Time (Log/Log)')

ggsave(paste0(kTSImg, '/Autocor_peak_height_24h_log.png'),
       peak_24_g_log)

# Do each baby separately
bnames <- peak_24h_df_agg %>% pull(baby_name) %>% unique()

for (baby in bnames) {
  baby_gr <- peak_24h_df_agg %>%
    filter(baby_name == baby) %>%
    print_peak_24(trans='identity',
                  title_str= paste0('24h ACF Peak v Time - ', baby))
  
  ggsave(paste0(kTSImg, '/Autocor_peak_height_24h_', baby, '.png'),
         baby_gr)
}


