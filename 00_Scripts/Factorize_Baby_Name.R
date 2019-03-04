#
# Function to turn baby names into a factor
# for preserving plot colors
#

Factorize_Baby_Name <- function(name_var) {
  exp_name_vec <- c('BSS', 'Red', 'Ryan', 'WP_P', 'WP_Z')
  all_name_vec <- c(exp_name_vec, 
                           sort(setdiff(unique(name_var), exp_name_vec)))
  return(factor(name_var, levels=rev(all_name_vec)))
}
