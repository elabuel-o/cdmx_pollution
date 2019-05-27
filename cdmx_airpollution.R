

############################################
#### Hourly analysis of PM 2.5 
#### Analysed period: May 2019
#### Author: Armando E. 
#### Based in the package by Diego Valle-Jones
#### superb 'rsinaica'! 
############################################

rm(list = ls())

## Loading packages... 
library(rsinaica)
library(tidyverse)
library(lubridate)
library(zoo)
library(gghighlight)

####################################################
#### PM 2.5 levels in Mexico City
#### Download data from sinaica (near real time)
####################################################

#########################
### 1. Downloading data
#########################

## rsinaica function for getting PM2.5 param for all Mexico's monitoring stations
## Two-weeks analysis in the peak of the crisis! 
pm25_2019 <- sinaica_param_data(
    "PM2.5",
    "2019-05-01",
    "2019-05-17",
    "Crude"
)

##########################
### 2. Data wrangling
##########################

### Filtering data (at least 2 weeks of reports and no NAs)
### Also choosing only "Valle de México" monitoring stations (network name)
pm25 <- pm25_2019 %>% 
    mutate(value = ifelse(value < 1, NA_real_, value)) %>%
    group_by(network_name) %>% 
    filter(!is.na(value)) %>%
    mutate(nweeks = n_distinct(week(date))) %>% 
    filter(nweeks >= 2) %>% 
    select(-nweeks) %>% 
    ungroup() %>% 
    filter(network_name == "Valle de México") %>% 
    group_by(date, hour) %>% 
    summarise(avg_level = mean(value),
              max_level = max(value), 
              count = n())

rm(pm25_2019)

########################
### 3. A pair of plots!
########################

### Plot 1: Average values of PM 2.5 in the middle of the crisis
### Deptics the average values across CDMX monitoring stations which report
### enough data. 
### 
### Highlighted (with deprecated highlight_line() ---works better!)
gghighlight_line(pm25, aes(x = hour, y = avg_level, group = date, colour = date), 
                 size = .7, 
                 max(avg_level), max_highlight = 3) + 
    theme_light() + 
    ggtitle("Promedio por hora del día. CDMX") + 
    xlab("Hora del día") +
    ylab(expression(paste("Concentración de PM2.5", " ", "(",mu,"g/", m^3,")")))

### Plot 2: Maximum values of PM 2.5 in the middle of the crisis
### Depicts the maximum value from all monitoring stations wich report
### enough data
### 
gghighlight_line(pm25, aes(x = hour, y = max_level, group = date, colour = date),
                 size = .7,
                 max(max_level), max_highlight = 3) + 
    theme_light() + 
    ggtitle("Niveles máximos por hora del día. CDMX") + 
    xlab("Hora del día") + 
    ylab(expression(paste("Concentración de PM2.5", " ", "(",mu,"g/", m^3, ")")))


########################
### 4. Final data file
########################
### WARNING:
### This is not a good practice, since other people must be able to download
### original data file and wrangle it from scratch. 
### In case of impossibility of such task, please use the "pm25.csv" file

write.csv(pm25, file = "pm25.csv")

