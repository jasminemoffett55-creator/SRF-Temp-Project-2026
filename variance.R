require(tidyverse)
require(lubridate)
require(janitor)
require(dplyr)
require(zoo)
require(ggpubr)

setwd("C:/Users/asher.wescott/Desktop/srfdata")

WQ2014.raw <- read.csv("WQ2014_clean_FINAL.csv") 

WQ2014 <- WQ2014.raw %>%  
  clean_names() %>%
  dplyr::select( timestamp, location, id, date, temperature_c) %>%
  rename(temp = temperature_c) %>%
  mutate( 
    datetime = mdy_hms(timestamp), #make datetime object out of timestamp  
    location = str_trim(location), #get rid of spacing inconsistencies in location names
    date=as.Date(datetime, format = "%m/%d/%Y") # make date var which+ gets rid of time and only keeps date
  ) %>%
  filter(!is.na(datetime)) #only keep rows where datetime isnt missing

mean <- WQ2014 %>% #creates dataframe with mean temps 
  group_by(location, date) %>% #split data into groups based on unique combo of loc/date
  summarise(daily_mean_temp = mean(temp, na.rm=TRUE)) %>%
  ungroup()


max <- WQ2014 %>%
  group_by(location, date) %>%
  summarise(daily_max_temp = max(temp, na.rm = TRUE)) 

alldata <- left_join(mean, max, by = c("location", "date"))

meanvarbyloc  <- alldata %>%
  group_by(location) %>%
  summarise(meanvar = (var = var(daily_mean_temp, na.rm = TRUE))) %>%
  ungroup()

maxvarbyloc  <- alldata %>%
  group_by(location) %>%
  summarise(meanvar = (var = var(daily_max_temp, na.rm = TRUE))) %>%
  ungroup()



