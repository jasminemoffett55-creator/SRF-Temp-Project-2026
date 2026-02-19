require(tidyverse)
require(lubridate)
require(janitor)
require(dplyr)
require(zoo)

setwd("C:/Users/asher.wescott/Desktop/srfdata")

WQ2014.raw <- read.csv("WQ2014_clean_FINAL.csv") 

WQ2014 <- WQ2014.raw %>%  
  clean_names() %>%
  dplyr::select( timestamp, location, id, date, temperature_c) %>%
  rename(temp = temperature_c)

WQ2014 <- WQ2014 %>%
  mutate(datetime = mdy_hms(timestamp))

WQ2014_16deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  filter(day.min <= 16 & day.max >= 16) %>%
  distinct(date, .keep_all = T)
  

nrow(WQ2014_16deg)  


WQ2014_18deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  filter(day.min <= 18 & day.max >= 18) %>%
  distinct(date, .keep_all = T)

nrow(WQ2014_18deg)  


WQ2014_20deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  filter(day.min <= 20 & day.max >= 20) %>%
  distinct(date, .keep_all = T)

nrow(WQ2014_20deg)  




WQ2014_abv16deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  distinct(date, .keep_all = T) %>%
  mutate(threshold = day.min <= 16 & day.max >= 16) # Create a logical vector
  
WQ2014_abv16deg$consec <- with(rle(WQ2014_abv16deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))

WQ2014_abv16deg %>%
  group_by(location) %>%
  summarize(max_value = max(consec, ma.rm = TRUE))


WQ2014_abv18deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  distinct(date, .keep_all = T) %>%
  mutate(threshold = day.min <= 18 & day.max >= 18) # Create a logical vector

WQ2014_abv18deg$consec <- with(rle(WQ2014_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))

WQ2014_abv18deg %>%
  group_by(location) %>%
  summarize(max_value = max(consec, ma.rm = TRUE))


WQ2014_abv20deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  distinct(date, .keep_all = T) %>%
  mutate(threshold = day.min <= 20 & day.max >= 20) # Create a logical vector

WQ2014_abv20deg$consec <- with(rle(WQ2014_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))

WQ2014_abv20deg %>%
  group_by(location) %>%
  summarize(max_value = max(consec, ma.rm = TRUE))





