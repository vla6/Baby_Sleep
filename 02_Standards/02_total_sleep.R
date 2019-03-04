#
# Compare total sleep to Ferber standards for
# all babies.  Create ribbon plots for comparisons.
# Also examine day-of-week differences (daycare effects),
# specifically for 'Ryan'
#
# Standards from Ferber, M. (2006). Solve 
# Your Child’s Sleep Problems. New York: Simon and Schuster 
# (Data frame created upstream)
#

library(plotly)
library(ggplot2)
library(data.table)
library(zoo)
library(dplyr)
library(lubridate)
library(tidyquant)
library(TeachingDemos)

kFerberData <- '00_Data/Ferber/'
kFerberImg <- '00_Images/Ferber/'

# Standard long 2 data for all babies
kInputStdLong2 <- "00_Data/COMB_STD_LONG_2.rds"

# Extended Ferber Guidline
kInputFerber <- paste0(kFerberData, '/Ferber_Guidelines_Extended.rds')

source('00_Scripts/Name_to_RGB_Color.R')
source('00_Scripts/Week_from_Day.R')
source('00_Scripts/Month_from_Day.R')
source('00_Scripts/theme_bd.R')
source("00_Scripts/Max_Day_from_Month.R")

#
# Plot Ferber guidelines only ----
# Ribbon plot
#

ferber_extended_df <- readRDS(kInputFerber)

# Ribbon alone
ribbon_g <- ferber_extended_df %>%
  mutate(month = Month_from_Day(day)) %>%
  ggplot(aes(x=month)) + 
  geom_ribbon(aes(ymin=tot_hrs_min, ymax=tot_hrs_max, 
                  fill = "Ferber's Normal Range"), alpha = 0.3) +
  geom_line(aes(y = tot_hrs_mid), color='darkgreen',
            show.legend= T) +
  scale_fill_manual(values=c("green"), name="") +
  labs(y='Total Sleep (hours)', x='Month', 
       title= 'Normal Sleep Guidelines',
       color = "") +
  scale_x_continuous(limits=c(0,24), expand=expand_scale(0),
                     breaks=seq(0, 24, 4)) +
  ylim(8,18) +
  theme_minimal() +
  theme_bd +
  theme(legend.position = c(0.75, 0.1))

print(ribbon_g)
ggsave(filename = paste0(kFerberImg, '/Ferber_Guide.png'),
       device='png', plot=ribbon_g, height = 4, width =10)

#
# Ribbon plot with data function ----
# Function for ribbon plot with baby data.
# Expects the following columns: 
#  month
#  tot_hrs_min
#  tot_hrs_max
#  tot_hrs
#

# Function to plot
Ferber_Ribbon_Plot <- function(data, limit_months = 24,
                               line_color = "#202020",
                               title_str= "") {
  
  # Get the x max
  x_max_data <- data %>%
    dplyr::filter(!is.na(tot_hrs)) %>%
    pull(month) %>% 
    max(na.rm=T)
  x_max <- pmin(limit_months, round(x_max_data*1.1))
  
  gp <- data %>%
    ggplot() + 
    geom_line(aes(x=month, y=tot_hrs), color=line_color) +
    geom_ribbon(aes(ymin=tot_hrs_min, ymax=tot_hrs_max, x=month, 
                    fill = "green"), alpha = 0.3) +
    scale_fill_manual(values=c("green"), name="fill") +
    theme(legend.position = "none") +
    labs(y='Total Sleep (hours)', x='Month',
         title=title_str) +
    ylim(8,18) +
    scale_x_continuous(limits=c(0,x_max), expand=expand_scale(0)) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size=20),
          axis.title.x = element_text(size=18),
          axis.title.y= element_text(size=18),
          axis.text.x  = element_text(size=14),
          axis.text.y  = element_text(size=14),
          legend.text = element_text(size=14)) 
  
  print(gp)
  return(gp)
}

#
# Plot daily total sleep for all babies ----
# Plot each baby by day individually.
#

std_long_2 <- readRDS(kInputStdLong2)

# Get daily total sleep for all babies
total_sleep_df <- std_long_2 %>%
  group_by(baby_name, day) %>%
  summarize(tot_hrs = sum(duration, na.rm=T)/60,
            startDay = first(floor_date(startTime, unit='days'))) %>%
  ungroup() %>%
  mutate(month = Month_from_Day(day))

# Plot for each baby
baby_names <- total_sleep_df %>%
  pull(baby_name) %>%
  unique()

for (baby in baby_names) {
  
  this_data <- total_sleep_df %>%
    dplyr::filter(baby_name == baby) %>%
    left_join(ferber_extended_df, by='day')

  # Use standard baby colors for plot
  this_color = Name_to_RGB_Color(baby)
  
  g <- this_data %>%
    Ferber_Ribbon_Plot(title_str = baby,
                       line_color = this_color)
  ggsave(filename=paste0('Total_Sleep_Daily_', baby, '.png'), 
         device="png",path=kFerberImg, plot=g)
}

#
# Kernel density plot ----
#

dplot_baby_labs <- total_sleep_df %>% 
  pull(baby_name) %>%
  unique()

dplot <- total_sleep_df %>%
  ggplot(aes(tot_hrs, fill = baby_name, colour = baby_name)) +
  geom_density(alpha = 0.1) +
  xlim(c(5, 22)) + 
  labs(title = 'Sleep Ranges',
       x='Hours of Total Sleep',
       y = 'Density',
       fill="", color="") +
  theme_minimal() +
  scale_color_discrete(labels = as.vector(dplot_baby_labs)) +
  scale_fill_discrete(labels = as.vector(dplot_baby_labs))  +
  theme(legend.position=c(0.9, 0.7),
        plot.title = element_text(size=20),
        axis.title.x = element_text(size=18),
        axis.title.y= element_text(size=18),
        axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.text = element_text(size=14)) 

print(dplot)
ggsave(filename='Total_KDF_all.png',
       device="png",path=kFerberImg, plot=dplot)

#
# Plot weekly average sleep for all babies ----
# 1 plot with all data
#

smoothed_data <- total_sleep_df %>%
  mutate(week = Week_from_Day(day)) %>%
  group_by(baby_name, week) %>%
  dplyr::summarize(tot_days = n(),
                   tot_sleep_avg = mean(tot_hrs),
                   tot_sleep_sd = sd(tot_hrs),
                   month = mean(month)) %>%
  ungroup() %>%
  dplyr::filter(tot_days == 7)  %>%
  arrange(baby_name, week)

ferber_df_smooth <- ferber_extended_df %>%
  mutate(month = Month_from_Day(day)) %>%
  mutate(week = Week_from_Day(day)) %>%
  group_by(week) %>%
  dplyr::summarize(tot_hrs_min = mean(tot_hrs_min),
                   tot_hrs_max = mean(tot_hrs_max),
                   tot_hrs_mid = mean(tot_hrs_mid),
                   month = mean(month))%>%
  ungroup() 

max_x <- smoothed_data %>%
  pull(month) %>%
  max() * 1.1

plot_all <- smoothed_data %>%
  ggplot() + 
  geom_line(aes(x=month, y=tot_sleep_avg, color=baby_name), size=1) +
  geom_ribbon(data =ferber_df_smooth,
              aes(ymin=tot_hrs_min, ymax=tot_hrs_max, x=month, 
                  fill = "Normal"), alpha = 0.1) +
  scale_fill_manual(values=c("green"), name="") +
  #scale_color_discrete(labels=dplot_baby_labs) +
  #theme(legend.position = "none") +
  labs(y='Total Sleep (hours)', x='Month',
       title='Total Sleep - Weekly Averages',
       color='') +
  scale_y_continuous(limits=c(8,18),
                     breaks=seq(8,18,1)) +
  scale_x_continuous(limits=c(0,max_x), expand=expand_scale(0)) +
  theme_bd

print(plot_all)

ggsave(filename='Total_Sleep_Weekly_all.png',
       device="png",path=kFerberImg, plot=plot_all)

#
# Plot std dev w ranges ----
#

ferber_df_smooth <- ferber_df_smooth %>%
  mutate(ferber_range_max = (tot_hrs_max - tot_hrs_min)/2,
         ferber_range_0 = 0)

plot_sd_all <-smoothed_data %>%
  ggplot() + 
  geom_line(aes(x=month, y=tot_sleep_sd, color=baby_name), size=1) +
  geom_ribbon(data =ferber_df_smooth,
              aes(ymin=ferber_range_0, ymax=ferber_range_max, x=month, 
                  fill = "Normal"), alpha = 0.1) +
  scale_fill_manual(values=c("green"), name="") +
  scale_color_discrete(labels=dplot_baby_labs) +
  theme(legend.position = "none") +
  labs(y='SD(Total Sleep)', x='Month',
       title='Total Sleep - Standard Deviation',
       color='baby') +
  coord_cartesian(ylim=c(0, 2.5)) +
  #scale_y_continuous(limits=c(0,3)) +
  scale_x_continuous(limits=c(0,max_x), expand=expand_scale(0)) +
  theme_bd 

print(plot_sd_all)

ggsave(filename='Total_Sleep_Week_SD_all.png',
       device="png",path=kFerberImg, plot=plot_all)

#
# Compare variation between, within babies ----
#

smoothed_sd_data <- smoothed_data %>%
  group_by(week) %>%
  dplyr::summarize(month = first(month),
                   within_baby_sd = mean(tot_sleep_sd),
                   between_baby_sd = sd(tot_sleep_avg),
                   tot_babies = n()) %>%
  ungroup() %>%
  #filter(tot_babies >= 3) %>%  # Used when more babies available
  melt(id.vars=c('month', 'week', 'tot_babies'))

x_max_baby <- smoothed_sd_data %>%
  pull(month) %>% max() * 1.1

sd_comp_data <- smoothed_sd_data %>%
  ggplot() + 
  geom_line(aes(x=month, y=value, color=variable),
            show.legend=T) +
  geom_line(data =ferber_df_smooth, aes(x=month, 
                                        y=ferber_range_max,
                                        color="black"),
            color='black', size=1.5, linetype='dashed') +
  #scale_color_discrete(labels=dplot_baby_labs) +
  theme(legend.position = "none") +
  labs(y='SD(Total Sleep)', x='Month',
       title='Total Sleep - Standard Deviation',
       color="") +
  scale_color_discrete(labels=c('within baby SD', 'between baby SD', 'f')) +
  scale_x_continuous(limits=c(0,x_max_baby), expand=expand_scale(0)) +
  theme_bd +
  theme(legend.position = c(0.8, 0.8))

print(sd_comp_data)

ggsave(filename='Total_Sleep_Week_SD_within_between_all.png',
       device="png",path=kFerberImg, plot=sd_comp_data)

#
# Weekday box plot ----
# Does sleep vary by day of week?
# Box plots of sleep by quarter/day
#

# Get weekday info from the data frames
sleep_baby_long <- total_sleep_df %>%
  select(day, baby_name, tot_hrs) %>%
  dplyr::filter(!is.na(tot_hrs)) %>%
  mutate(baby_quarter = 1 + floor(day/(Max_Day_from_Month(3)))) %>%
  group_by(baby_name, baby_quarter) %>%
  mutate(tot_days = n()) %>%
  ungroup() %>%
  dplyr::filter(tot_days >= 80) %>% 
  dplyr::select(-tot_days)

weekday_baby_long <- total_sleep_df %>%
  select(day, baby_name, startDay) %>%
  dplyr::filter(!is.na(startDay)) %>%
  mutate(weekday = weekdays(startDay))

baby_quarter_num <-seq(1, 18, 1)
baby_quarter_str <- sapply(baby_quarter_num,
                           function(x) paste0('Months ',
                                              as.character(x*3-2), ' - ',
                                              as.character(x*3)))

sleep_baby_join <- sleep_baby_long %>%
  left_join(weekday_baby_long, 
            by=c('day', 'baby_name')) %>%
  mutate(weekday = factor(weekday, 
                          levels=c('Sunday', 'Monday',
                                   'Tuesday', 'Wednesday',
                                   'Thursday', 'Friday', 
                                   'Saturday')),
         baby_quarter = factor(baby_quarter,
                               levels = baby_quarter_num,
                               labels = baby_quarter_str))

baby_names <- sleep_baby_join %>%
  pull(baby_name) %>% unique()

for (n in baby_names) {
  
  bp <- sleep_baby_join %>%
    filter(baby_name == n) %>%
    ggplot(aes(x=weekday, y=tot_hrs)) +
    geom_boxplot() + 
    facet_wrap(~baby_quarter) +
    theme_tq() +
    labs(y = 'Total Sleep (hours)',
         title = paste0(n, ' - Sleep by Day of Week')) +
    scale_y_continuous(limits=c(8,18), breaks=seq(8,16,2)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12),
          plot.title = element_text(size=18),
          axis.title.x = element_blank(),
          axis.title.y= element_text(size=16),
          axis.text.y  = element_text(size=14),
          legend.text = element_text(size=12),
          strip.text.x = element_text(size=12)) 
  
  print(bp)
  
  ggsave(filename=paste0('Total_Sleep_Weekday_Box_', n, 
                         '.png'),
         device="png",path=kFerberImg, plot=bp)
  
}

#
# ANOVA by baby/weekday ----
# See a "daycare effect" for Ryan
#

# Export the results 
txtStart(file=paste0(kFerberData,
                     '/Tot_Sleep_Weekday_ANOVA.txt'))

baby_names <- sleep_baby_join %>%
  pull(baby_name) %>% unique()

anova_p <- c()

for (n in baby_names) {
  
  print(paste0('ANOVA for ', n))
  
  anova_data <- sleep_baby_join %>%
    dplyr::filter(baby_name == n)
  
  anova_lm <- lm(tot_hrs ~ weekday, data = anova_data)
  summary(anova_lm)
  anova_obj <- anova(anova_lm)
  print(anova_obj)
  anova_obj_p <- anova_obj$`Pr(>F)`[[1]]
  print(anova_obj_p)
  anova_p[n] = anova_obj_p
}

print(anova_p)
txtStop()

fwrite(as.data.frame(anova_p), 
       file = paste0(kFerberData,
                     '/Tot_Sleep_Weekday_ANOVA_pval.csv'))

#
# Ryan daycare compare  ----
#

ryan_daycare_sleep_comp <- sleep_baby_join %>%
  dplyr::filter(baby_name == 'Ryan') %>%
  mutate(week = Week_from_Day(day),
         daycare_day = case_when(weekday %in% c('Wednesday',
                                                'Thursday',
                                                'Friday') ~ 1,
                                 TRUE ~ 0),
         daycare_day = factor(daycare_day, levels=c(0,1),
                              labels=c('Other Sat-Tues', 
                                       'Daycare Wed-Fri'))) %>%
  group_by(week, daycare_day) %>%
  dplyr::summarize(mean_sleep = mean(tot_hrs),
                   tot_days = n(),
                   day= mean(day)) %>%
  ungroup()  %>%
  dplyr::filter(tot_days >= 3) %>%
  mutate(month = Month_from_Day(day))

rd_g <- ryan_daycare_sleep_comp %>%
  ggplot(aes(x=month, y=mean_sleep, color=daycare_day)) +
  geom_line() +
  labs(title= 'Daycare Sleep Effect - Ryan',
       subtitle = 'Weekly Averages',
       y = 'Mean Total Sleep (hours)',
       x = 'Month',
       color = "") +
  theme_minimal() +
  theme(legend.position=c(0.8, 0.2),
        plot.title = element_text(size=18),
        axis.title.x = element_text(size=16),
        axis.title.y= element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        legend.text = element_text(size=14))

print(rd_g)

ggsave(filename='Total_Sleep_Daycare_Ryan.png', 
       device="png",path=kFerberImg, plot=rd_g)

