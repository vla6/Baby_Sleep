#
# Imports data from the mammababy app format
# See http://mammababy.lifenstats.com/home
#
# Converts to the "standard long" format
# which has 3 columns:  
#    * startTime - date time of sleep interval start
#    * day - baby's age in calendar days since birth as of
#      startTime.  (The day of birth is day 1)
#    * duration - integer number of minutes of sleep
# The converted data is saved as an R object 
#
# Will attempt to convert .csv files with a prefix of 
# "RAWDATA_MAMMABABY_" found in the data directory.
# For successful import, the filename must be of the format
#    RAWDATA_MAMMABABY_<id_str>_<dob>.csv
# For example, RAWDATA_MAMMABABY_Jane_20151122.csv
# Dates have been masked (shifted) in included data.
#
# The <id_str> should be 8 characters or fewer and uniquely
# identify the baby ("Red" and "Ryan" are reserved by the input 
# data).  These will be case-sensitive in final analyses.
# The date ofbirth should be in YYYYMMDD format.
#
# All timezones are converted to GMT - daylight savings is ignored.
#

library(data.table)
library(lubridate)
library(dplyr)
library(tools)

# Input directory, relative to project head
kDataDir <- "00_Data/"

# Prefix string to match for Mammababy data
kPatternStr <- "RAWDATA_MAMMABABY_"

# Output file prefix
kOutPrefix <- 'STD_LONG_'

#
# Import the data.  ----
# Find matching Mammababy files and import in a loop
#

# Find "Mammababy" input files
pattern_str <- paste0("^", kPatternStr, ".*(csv|CSV)$")
mammababy_file_list <- list.files(path=kDataDir, pattern = pattern_str)

# Import and convert each file
for (fn in mammababy_file_list) {

  fn_basestr <- gsub(kPatternStr, "", file_path_sans_ext(fn))
  fn_split_list <- gregexpr('_',fn_basestr)
  
  # Get name from file.  Restrict to 8 characters
  fn_baby_name <- substring(substring(fn_basestr, 1, 
                                      fn_split_list[[length(fn_split_list)]] -1),
                            1, 8)
  #Get DOB string and convert to date
  fn_dob_str <- substring(fn_basestr, fn_split_list[[length(fn_split_list)]] +1)
  fn_dob <- as.POSIXct(fn_dob_str, tz='GMT', format="%Y%m%d")
  
  print(paste('MB Conv', fn_baby_name, fn_dob_str))
  
  # Import data
  mb_data <- fread(paste0(kDataDir, '/', fn),
                     col.names = c("dateStr", "duration"),
                     colClasses = c("character", "integer"))
  
  # Convert the start time to POSIXct, and add days of life
  mb_data_mod <- mb_data %>%
    mutate(startTime = as.POSIXct(dateStr, tz='GMT',
                                  format="%Y-%m-%d %I:%M:%OS %p"),
           day = as.integer(1 + floor(as.numeric(difftime(startTime, fn_dob, 
                                                          tz='GMT',units='days'))))) %>%
    arrange(startTime)
  
  # Save as R object
  mb_data_mod %>%
    dplyr::select(startTime, day, duration) %>%
    saveRDS(file=paste0(kDataDir, '/', kOutPrefix,
                        fn_baby_name, '.rds'))
}

