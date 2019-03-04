00_Data directory

Expected to be a subdirectory under the main project directory.
For relative paths, open the R project in RStudio.

This directory contains 1 initial input file.
Additional files will be written here by various scripts.
Subdirectories will be created.

TO INCLUDE ADDITIONAL BABIES IN ANALYSIS
----------------------------------------

MAMMABABY FORMAT:  To include a new MamaBaby (see http://mammababy.lifenstats.com/), simply rename the file and copy the file to the "00_Data" directory. The file must be named with the following pattern:
     RAWDATA_MAMMABABY_<id_str>_<dob>.csv
For example, RAWDATA_MAMMABABY_Jane_20151122.csv
  The date of birth (<dob>) must be in YYYYMMDD format, and be the baby's birth date.  The <id_str> should be 8 characters or fewer and uniquely identify the baby ("Red" and "Ryan" are reserved by the input data).  These will be case-sensitive in final analyses.
  With this naming convention, a file found in 00_Data/ will be automatically imported and included in all other analyses.

OTHER FORMATS:  To include any other format, you may add a script to the "01_Data_Input/" directory.  The script should output a .rds file to "00_Data/", with the following naming pattern:
    STD_LONG_<id_str>.rds
For example, STD_LONG_Liam.rds.  
  The <id_str> should be 8 characters or fewer and uniquely identify the baby ("Red" and "Ryan" are reserved by the input data).  These will be case-sensitive in final analyses.
  The file must contain the following fields:
    * startTime - date time of sleep interval start
    * day - baby's age in calendar days since birth as of startTime.  
      (The day of birth is day 1)
    * duration - integer number of minutes of sleep
  Files of this format with this naming convention will be automatically incorporated into all subsequent analyses.

TO CUSTOMIZE COLORS FOR NEW BABIES:  Edit the files:  
    00_Scripts/Name_to_RGB_Color.R
    00_Scripts/Factorize_Baby_Name.R
 