#
# Calculate the week from the day.
# Assume the day is numeric
#


Week_from_Day <- function(day_num)  {
  return(1 + floor(day_num/7))
}