#
# Installs all required packages.
# Installs only if packages are not already installed.
# Does not update or check versions, etc.
#

required_packages <- c('data.table',
                       'lubridate',
                       'dplyr',
                       'httr',
                       'tools',
                       'jpeg',
                       'ggplot2',
                       'plotly',
                       'zoo',
                       'tidyquant',
                       'TeachingDemos',
                       'pracma',
                       'stats',
                       'entropy',
                       'stringr',
                       'quantmod',
                       'tidyr',
                       'fasttime',
                       'reshape2')

# Find required packages not already there
non_installed_required <- required_packages[!required_packages %in%
                                              installed.packages()]

# Install new packages
if (length(non_installed_required) > 0) {
  install.packages(non_installed_required)
}
