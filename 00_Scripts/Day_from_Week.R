#
# Get a relevant day during a given week.
# Select the 3rd day
# The week is numeric.
#

Day_from_Week <- function(week_num)  {
  return((week_num - 1) * 7 + 3)
}
