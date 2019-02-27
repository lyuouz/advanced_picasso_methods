setwd('/Users/liusiliang/downloads/lab 5/')

library(tidyverse)
library(lubridate)
library(stringr)

nyc_data <- read_csv('1629160.csv') %>% 
  janitor::clean_names() %>% 
  select(-station, -(latitude:elevation)) %>% 
  mutate(
    tmax = tmax/10,
    tmin = tmin/10,
    date = ymd(date)
  )

nyc_data %>% 
  group_by(date) %>% 
  summarize(
    avg_tmax = mean(tmax),
    avg_tmin = mean(tmin)
  ) %>% 
  gather(key = temp, value = value, avg_tmax:avg_tmin) %>%
  ggplot(aes(x = date, y = value, color = temp)) + geom_line() +
  labs(
    title = "Trends in daily temperature in New York City, 2017-2019",
    x = "Date",
    y = "Maximum or minimum temperature"
  ) + scale_color_hue(name = "Temperature", 
                      labels = c('Maximum', 'Minimum'))
  
nyc_data %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>% 
  summarize(
    avg_tmax = mean(tmax),
    avg_tmin = mean(tmin)
  ) %>% 
  gather(key = temp, value = value, avg_tmax:avg_tmin) %>% 
  ggplot(aes(x = month, y = value, color = temp)) + geom_line() +
  labs(
    title = "Trends in monthly temperature in New York City, 2017-2019",
    x = "Month",
    y = "Maximum or minimum temperature"
  ) + scale_color_hue(name = "Temperature", 
                     labels = c('Maximum', 'Minimum')) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))





