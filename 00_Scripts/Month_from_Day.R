#
# Calculate the month from the day.
# Assume the day is numeric.
# Use 30.5 days per month.
# Return a floating point number
#


Month_from_Day <- function(day_num)  {
  return(day_num/30.5)
}