#
# Function to create a standard plot showing
# the full history of baby sleep.
# Assumes data from just 1 baby input
#

require(dplyr)
require(ggplot2)

source('00_Scripts/Hour_Decimal_to_Time_String.R')
source('00_Scripts/Month_from_Day.R')

Baby_Sleep_Plot <- function(data,
                        sleep_color = '#838383',
                        wake_color= '#FFFBE9',
                        x_tick_interval = 2) {
  
  # Modify data for plot, including getting time string
  data_mod <- data %>%
    mutate(month = Month_from_Day(day),
           time = format(interval_start, "%H:%M"),
           sleep_flag = factor(sleep_flag, levels=c(0,1))) %>%
    arrange(day, time)
  
  # y labels
  y_breaks <- seq(0, 22, 2)
  y_labels <- sapply(y_breaks, Hour_Decimal_to_Time_String)
  
  # x labels
  max_tick = x_tick_interval * 
    floor(max(data_mod$month, na.rm=T) / x_tick_interval)
  if (max_tick <= 0) {
    max_tick = max(1, max(data_mod$month, na.rm=T))
    min_tick = 0
    tick_int = 0.25
  } else {
    min_tick = x_tick_interval * 
      ceiling(min(data_mod$month, na.rm=T) / x_tick_interval)
    tick_int = x_tick_interval
  }
  x_br = seq(min_tick, max_tick, tick_int)
  
  # Get baby's name
  var_name <- data_mod$baby_name[[1]]
  
  # Plot the data
  g <- data_mod %>%
    ggplot(aes(x=month, y=time, fill=sleep_flag)) +
    geom_tile() +
    scale_fill_manual(values=c(wake_color, sleep_color),
                      labels=c('Wake', 'Sleep')) +
    scale_y_discrete(breaks=y_labels,
                      expand=expand_scale(0)) +
    scale_x_continuous(breaks = x_br, expand=expand_scale(0)) +
    labs(x='Month', y = 'Time',
         title=var_name, fill="") +
    guides(fill = guide_legend(direction = "horizontal")) +
    theme(panel.background = element_blank(),
          legend.position=c(1, 1.05),
          legend.justification="right",
          axis.text=element_text(size=14),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12),
          title=element_text(size=16),
          legend.key = element_rect(colour = 'gray',
                                    fill = "transparent"))
  
  print(g)
  return(g)
}  

