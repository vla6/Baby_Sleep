#
# Convert "standard long" format to
# type 2 standard long (split days)
#

require(tools)
require(data.table)
require(lubridate)
require(dplyr)

#
# Set a variable for the start and end days.
# The end day is NA the same as start
#

EndDayCond <- function(startDay, endTime) {
  endDay <- floor_date(endTime, "day")
  return(case_when(as.integer(difftime(endDay, startDay, 'days')) != 0 ~ endDay,
                   TRUE ~ as.POSIXct(NA, tz='GMT')))
}

Std_Long_to_Std_Long2 <- function(data) {
  
  # Copy and modify frame.  Round start times
  sleepData <- data %>%
    as.data.frame() %>%
    copy() %>%
    mutate(startTime = as.POSIXct(round.POSIXt(startTime, units='mins'),
                                  tz='GMT'),
           endTime = startTime + minutes(duration),
           startDay = floor_date(startTime, "day"),
           endDay = EndDayCond(startDay, endTime))
  
  sleepDataLong <- sleepData %>%
    melt(id.vars =c('baby_name', 'startTime', 'endTime', 'day', 'duration'),
         measure.vars = c("startDay", "endDay"), na.rm=T,
         value.name = 'indDay') %>%
    arrange(baby_name, startTime) %>%
    dplyr::select(baby_name, startTime, endTime, day, duration, indDay, variable) %>%
    mutate(day = as.integer(day))
           
  # Adjust start and end times
  sleepDataLong2 <- sleepDataLong %>%
    rename(startTime_orig = startTime,
           endTime_orig = endTime,
           duration_orig = duration,
           day_orig = day) %>%
    group_by(baby_name) %>%
    mutate(startTime = pmax(startTime_orig, indDay, na.rm=T),
           endTime = pmin(endTime_orig, indDay + days(1), na.rm=T),
           day = case_when(variable == 'endDay' ~ as.integer(day_orig + 1),
                           TRUE ~ day_orig),
           duration = as.integer(difftime(endTime, startTime, 
                                          tz='GMT', units='mins'))) %>%
    ungroup() %>%
    arrange(baby_name, startTime)
  
  stdLong2 <- sleepDataLong2 %>%
    select(baby_name, startTime, day, duration)
  
  return(stdLong2)
}

