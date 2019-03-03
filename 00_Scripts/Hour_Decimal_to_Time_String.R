#
# Converts an hour decimal to a time string, 
# e.g. 1.25 to 01:15
#

require(stringr)

Hour_Decimal_to_Time_String <- function(hour_decimal) {
  return(paste0(str_pad(floor(hour_decimal), width=2, pad = '0'), 
                ":", str_pad(60* (hour_decimal %% 1), 
                             width = 2, pad = '0')))
}
