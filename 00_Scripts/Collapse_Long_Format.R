#
# Collapses a "long format" data frame, joining sleep intervals
# with wake separations less than a threshold value.
#  Expects a data frame with the following fields:
#     startTime - POSIXct
#     duration - integer
# Any other fields will be passed through, with only values
# on the earliest time within a sleep group returned.
#
# Variable name order is retained but the data frame is sorted
# by start time
#

require(dplyr)
require(lubridate)
require(tidyr)


Collapse_Long_Format <- function(data,
                                 collapse_interval = 1) {
  
  # Get data frame names to retain order
  data_names <- names(data)#[!names(data) %in% c('startTime', 'duration')]
  
  # Find rows with short gaps and collapse
  flagged_data <- data %>%
    arrange(startTime) %>%
    mutate(endTime = startTime + minutes(duration) - seconds(1),
           finalEndTime = case_when(difftime(lead(startTime),
                                             endTime, units='mins') >= 
                                      collapse_interval ~ endTime,
                                    TRUE ~ as.POSIXct(NA, tz='GMT'))) %>%
    fill(finalEndTime, .direction='up') %>%
    mutate(finalEndTime = case_when(is.na(finalEndTime) ~ max(endTime),
                                    TRUE ~ finalEndTime)) %>%
    dplyr::select(-endTime, -duration) %>%
    group_by(finalEndTime) %>%
    summarize_all(funs(first(.))) %>%
    ungroup() %>%
    mutate(duration = 1+ as.integer(difftime(finalEndTime, startTime,
                                             units='mins'))) %>%
    select(data_names)
  
  return(flagged_data)

}

