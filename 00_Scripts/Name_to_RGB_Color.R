#
# Function to return an RGB color associated
# with a short baby ID string.  Case insensitive.
# If no baby match, gray is returned
#

Name_to_RGB_Color <- function(name_str) {
  name_ary <- c('Ryan' = '#53868E', # Dark blue-green
                'WP_P' = '#375e3c',  # Dark true green
                'BSS' = '#5e3c37',  # Dark brick red
                'Red' = '#53698e', # Dark gray-blue
                'WP_Z' = '#50375e')  # Eggplant
  names(name_ary) = toupper(names(name_ary))
  name_str = toupper(as.character(name_str))
  return(ifelse(name_str %in% names(name_ary),
                as.character(name_ary[name_str]), '#202020'))
    
}

