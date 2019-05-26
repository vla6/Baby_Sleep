#
# Create a timeseries from "Standard Long 2"
# data.  The output data set has an indicator field
# which is 1 for sleep, 0 for wake
#
require(tidyr)

Interval_from_Std_Long2 <- function(data, interval_mins = 15,
                                             thresh_pct = 0.5) {
  
  # Create a frame with enough time intervals for 1 day.
  int_df <- data.frame(interval = seq(0, 24 * 60, 
                                        interval_mins))
  
  # Get init days by baby
  init_days <- data %>%
    dplyr::select(day, startTime, baby_name) %>%
    dplyr::filter(!(is.na(day) | is.na(startTime))) %>%
    group_by(baby_name) %>%
    do(head(., n=1)) %>%
    ungroup() %>%
    mutate(init_day = floor_date(startTime - days(day))) %>%
    dplyr::select(baby_name, init_day)
  
  data_int <- data %>%
    crossing(int_df) %>%
    mutate(interval_start = floor_date(startTime, unit='day') +
             minutes(interval),
           interval_stop = interval_start + minutes(interval_mins - 1),
           overlap_time = as.integer(difftime(pmin(startTime + minutes(duration), 
                                          interval_stop),
                                   pmax(interval_start, startTime), tz='GMT',
                                     units='min')) + as.integer(1),
           sleep_flag = as.integer(case_when(overlap_time >=
                                               (thresh_pct * interval_mins) ~ 1, 
                                  TRUE ~ 0)))

  timeseries <- data_int %>%
    group_by(baby_name, interval_start) %>%
    dplyr::summarize(sleep_flag = max(sleep_flag, na.rm=T),
                     interval = first(interval)) %>%
    ungroup() %>%
    left_join(init_days, by = 'baby_name') %>%
    
    mutate(day = as.integer(round(difftime(floor_date(interval_start, 'day'), 
                                           init_day, tz='GMT',
                                           units='days'))),
           interval = case_when(interval == 1440 ~ 0,
                                TRUE ~ interval)) %>%
    dplyr::select(baby_name, day, interval, interval_start, sleep_flag)
  
  return(timeseries)
}
