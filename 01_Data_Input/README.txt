01_Data_Input directory

This directory contains a series of scripts to input baby sleep data, and do an initial plot for each baby.  The scripts are as follows:

  00_install_packages.R - Installs any packages needed for the project.  Packages already installed are NOT updated.

  01_input_mammababy.R - Imports any "MammaBaby" format files found in the "00_Data" directory.  One file is included by default; see below for more information.  Saves the data file(s) in "Standard Long" format in the "00_Data" directory.

  02_input_red.R - Imports another baby's sleep data from a public GitHub repository.  Downloads the data from the web. Saves the data in "Standard Long" format in the "00_Data" directory.

  99_convert_plot.R - Converts the initial "Standard Long" formats to several additional formats, and creates a sleep history plot for each baby.

00_Data_Import is expected to be a subdirectory under the main project directory.
For relative paths, open the R project in RStudio.

Additional scripts can be included to import alternative formats.  Any "standard long" files following the expected naming conventions (see below) will be gathered and analysed starting with 99_convert_plot.R.


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
 