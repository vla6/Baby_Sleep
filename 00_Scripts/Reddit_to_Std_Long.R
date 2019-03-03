#
# Convert "reddit format" data frame to long format data
# frame.  
# Expects a baby's birth date as input.  This may be an
# arbtirary date if unkown (defaults to '2016-01-01').
# Note - the last day's sleep will be truncated at midnight.
# The sum of total sleep will be the same in the input and 
# output.
#

require(dplyr)
require(tidyr)
require(lubridate)

source('00_Scripts/Collapse_Long_Format.R')

Reddit_to_Std_Long <- function(data,
                               birth_day_str = '2016-01-01',
                               interval_base = 15,
                               collapse_interval = 1) {

  # Get the birthday as POSIXct
  start_dttm <- as.POSIXct(birth_day_str, tz='GMT',
                           format='%Y-%m-%d')
  
  # Convert wide to long with standard variable names
  long_format <- data %>%
    rename_at(1, ~ 'time') %>%
    melt(id.vars="time", variable.name = 'day',
          value.name='sleep_ind') %>%
    mutate(time = as.numeric(as.character(time)),
           day = as.integer(as.character(day)))
  
  # Get sleep rows only and calculate expected fields
  long_format2 <- long_format %>%
    filter(sleep_ind %in% c('S', 's')) %>%
    mutate(startTime = start_dttm + days(day) + 
             minutes(time * 60),
           duration = as.integer(interval_base))
  
  # Collapse adjoining times
  long_format3 <- long_format2 %>%
    Collapse_Long_Format(collapse_interval = collapse_interval) %>%
    dplyr::select(startTime, day, duration) 
  
  return(long_format3)
}

