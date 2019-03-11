#
# Setup for timeseries scripts. 
# Creates subdirectories for data and image files.
#

library(dplyr)

# Input/output directories
kDataDir <- '00_Data/'
kImgDir <- '00_Images/'

# Timeseries directory names
kSubDirStr <- 'Timeseries'

#
# Create sub-directories ----
#

kFerberData <- paste0(kDataDir, '/', kSubDirStr)
dir.create(kFerberData, showWarnings = FALSE)

kFerberImg <- paste0(kImgDir, '/', kSubDirStr)
dir.create(kFerberImg, showWarnings = FALSE)
