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

WQ16_summary <- WQ2014_abv16deg %>%
  group_by(location) %>%
  summarize(count_16deg = sum(threshold, na.rm = TRUE))

WQ16 <- WQ16_summary %>%
  mutate(location = fct_reorder(location, count_16deg, .desc = TRUE)) %>%
  ggplot(WQ_summary, mapping = aes(x=location, y=count_16deg, fill=location, label = count_16deg)) +
    geom_text(vjust = -0.5, size = 3.5) +
    geom_col(stat = "identity") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal() +
    guides(fill = "none") +
   labs(title = "Days over 16 per Location",
       x = "Location",
       y = "Number of Days")


  
  
WQ2014_abv16deg$consec <- with(rle(WQ2014_abv16deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))

WQ16con <- WQ2014_abv16deg %>%
  group_by(location) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(location = fct_reorder(location, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=location, y=max_value, fill=location, label = max_value)) +
    geom_text(vjust = -0.5, size = 3.5) +
    geom_col(stat = "identity") +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_minimal() +
    guides(fill = "none") +
   labs(title = "Maximum number of consecutive days over 16",
       x = "Location",
       y = "Max Number of Consecutive Days")




WQ2014_abv18deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  distinct(date, .keep_all = T) %>%
  mutate(threshold = day.min <= 18 & day.max >= 18) # Create a logical vector


WQ18_summary <- WQ2014_abv18deg %>%
  group_by(location) %>%
  summarize(count_18deg = sum(threshold, na.rm = TRUE))

WQ18 <- WQ18_summary %>%
  mutate(location = fct_reorder(location, count_18deg, .desc = TRUE)) %>%
  ggplot(WQ_summary, mapping = aes(x=location, y=count_18deg, fill=location, label = count_18deg)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Days over 18 per Location",
       x = "Location",
       y = "Number of Days")

WQ2014_abv18deg$consec <- with(rle(WQ2014_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


WQ18con <- WQ2014_abv18deg %>%
  group_by(location) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(location = fct_reorder(location, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=location, y=max_value, fill=location, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")




WQ2014_abv20deg <- WQ2014 %>%
  arrange(location, date) %>%
  group_by(date, location) %>%
  mutate(day.min = min(temp)) %>%
  mutate(day.max = max(temp)) %>%
  distinct(date, .keep_all = T) %>%
  mutate(threshold = day.min <= 20 & day.max >= 20) # Create a logical vector

WQ20_summary <- WQ2014_abv20deg %>%
  group_by(location) %>%
  summarize(count_20deg = sum(threshold, na.rm = TRUE))

WQ20 <- WQ20_summary %>%
  mutate(location = fct_reorder(location, count_20deg, .desc = TRUE)) %>%
  ggplot(WQ_summary, mapping = aes(x=location, y=count_20deg, fill=location, label = count_20deg)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Days over 20 per Location",
       x = "Location",
       y = "Number of Days")


WQ2014_abv20deg$consec <- with(rle(WQ2014_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))

WQ20con <- WQ2014_abv20deg %>%
  group_by(location) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(location = fct_reorder(location, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=location, y=max_value, fill=location, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



totaldaypanel <- ggarrange(WQ16, WQ18, WQ20)
totaldaypanel

consecdaypanel <- ggarrange(WQ16con, WQ18con, WQ20con)
consecdaypanel






