library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(zoo)
library(tidyr)
library(slider)

setwd("C:/Users/asher.wescott/Desktop/srfdata")

#read in csv, clean names, and set datetime with only valid datetime 
massive <- readRDS("SRF_datasetUSCcomb.rds") %>%#read in csv
  clean_names() %>% #clean/standardize col names
  dplyr::select(timestamp, site, temperature) %>%
  mutate( 
    site = str_trim(site), #get rid of spacing inconsistencies in location names
    date=as.Date(timestamp, format = "%Y-%m-%d %HH:MM:SS") # make sure date var is registering as date
  ) 


  

yes <- "prefire"
no <- "postfire"
massive <- massive %>%
  mutate(pre_fire = ifelse(year(timestamp) <= 2020, yes, no)) %>%
  filter(!is.na(temperature))

unique(massive$site)

sites_keep <- c("Weir", "Puma", "USC Comb")
massive <- massive %>%
  filter(site %in% sites_keep)  

unique(massive$site)
##############

WQ2014_abv18deg <- massive %>%
  filter(date >= "2014-06-1" & timestamp <= "2014-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2014_abv18deg$consec <- with(rle(WQ2014_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2014 <- WQ2014_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2014
###############

WQ2015_abv18deg <- massive %>%
  filter(date >= "2015-06-1" & timestamp <= "2015-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2015_abv18deg$consec <- with(rle(WQ2015_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2015 <- WQ2015_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2015
##################

WQ2016_abv18deg <- massive %>%
  filter(date >= "2016-06-1" & timestamp <= "2016-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2016_abv18deg$consec <- with(rle(WQ2016_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2016 <- WQ2016_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2016

#####################
WQ2017_abv18deg <- massive %>%
  filter(date >= "2017-06-1" & timestamp <= "2017-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2017_abv18deg$consec <- with(rle(WQ2017_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2017 <- WQ2017_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2017


#######################




WQ2018_abv18deg <- massive %>%
  filter(date >= "2018-06-1" & timestamp <= "2018-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2018_abv18deg$consec <- with(rle(WQ2018_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2018 <- WQ2018_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2018

###################
WQ2019_abv18deg <- massive %>%
  filter(date >= "2019-06-1" & timestamp <= "2019-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2019_abv18deg$consec <- with(rle(WQ2019_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2019 <- WQ2019_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")

abv18con2019
#############################
WQ2020_abv18deg <- massive %>%
  filter(date >= "2020-06-1" & timestamp <= "2020-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2020_abv18deg$consec <- with(rle(WQ2020_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2020 <- WQ2020_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2020
##################
WQ2021_abv18deg <- massive %>%
  filter(date >= "2021-06-1" & timestamp <= "2021-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2021_abv18deg$consec <- with(rle(WQ2021_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2021 <- WQ2021_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2021
#####################
WQ2022_abv18deg <- massive %>%
  filter(date >= "2022-06-1" & timestamp <= "2022-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2022_abv18deg$consec <- with(rle(WQ2022_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2022 <- WQ2022_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2022
#######################
WQ2023 <- massive %>%
  filter(date >= "2023-06-1" & timestamp <= "2023-10-31") %>%
  filter(site == "Puma")


########################
WQ2023_abv18deg <- massive %>%
  filter(date >= "2023-06-1" & timestamp <= "2023-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)

unique(WQ2023_abv18deg$site)


WQ2023_abv18deg$consec <- with(rle(WQ2023_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2023 <- WQ2023_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2023
##############
WQ2024_abv18deg <- massive %>%
  filter(date >= "2024-06-1" & timestamp <= "2024-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2024_abv18deg$consec <- with(rle(WQ2024_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2024 <- WQ2024_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2024
###########
WQ2025_abv18deg <- massive %>%
  filter(date >= "2025-06-1" & timestamp <= "2025-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 18 & day.max >= 18) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)

unique(WQ2023_abv18deg$site)


WQ2025_abv18deg$consec <- with(rle(WQ2025_abv18deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv18con2025 <- WQ2025_abv18deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 18",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv18con2025
######################
WQ2014_abv20deg <- massive %>%
  filter(date >= "2014-06-1" & timestamp <= "2014-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2014_abv20deg$consec <- with(rle(WQ2014_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2014 <- WQ2014_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2014
#################
WQ2015_abv20deg <- massive %>%
  filter(date >= "2015-06-1" & timestamp <= "2015-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2015_abv20deg$consec <- with(rle(WQ2015_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2015 <- WQ2015_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2015
##############
WQ2016_abv20deg <- massive %>%
  filter(date >= "2016-06-1" & timestamp <= "2016-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2016_abv20deg$consec <- with(rle(WQ2016_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2016 <- WQ2016_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2016






################
WQ2017_abv20deg <- massive %>%
  filter(date >= "2017-06-1" & timestamp <= "2017-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2017_abv20deg$consec <- with(rle(WQ2017_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2017 <- WQ2017_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2017



###################



WQ2018_abv20deg <- massive %>%
  filter(date >= "2018-06-1" & timestamp <= "2018-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2018_abv20deg$consec <- with(rle(WQ2018_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2018 <- WQ2018_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2018

###################
WQ2019_abv20deg <- massive %>%
  filter(date >= "2019-06-1" & timestamp <= "2019-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2019_abv20deg$consec <- with(rle(WQ2019_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2019 <- WQ2019_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")

abv20con2019
#############################
WQ2020_abv20deg <- massive %>%
  filter(date >= "2020-06-1" & timestamp <= "2020-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2020_abv20deg$consec <- with(rle(WQ2020_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2020 <- WQ2020_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2020
##################
WQ2021_abv20deg <- massive %>%
  filter(date >= "2021-06-1" & timestamp <= "2021-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2021_abv20deg$consec <- with(rle(WQ2021_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2021 <- WQ2021_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2021
#####################
WQ2022_abv20deg <- massive %>%
  filter(date >= "2022-06-1" & timestamp <= "2022-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2022_abv20deg$consec <- with(rle(WQ2022_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2022 <- WQ2022_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2022
#######################
WQ2023 <- massive %>%
  filter(date >= "2023-06-1" & timestamp <= "2023-10-31") %>%
  filter(site == "Puma")


########################
WQ2023_abv20deg <- massive %>%
  filter(date >= "2023-06-1" & timestamp <= "2023-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)

unique(WQ2023_abv20deg$site)


WQ2023_abv20deg$consec <- with(rle(WQ2023_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2023 <- WQ2023_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2023
##############
WQ2024_abv20deg <- massive %>%
  filter(date >= "2024-06-1" & timestamp <= "2024-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)


WQ2024_abv20deg$consec <- with(rle(WQ2024_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))


abv20con2024 <- WQ2024_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2024
###########
WQ2025_abv20deg <- massive %>%
  filter(date >= "2025-06-1" & timestamp <= "2025-10-31") %>%
  filter(!is.na(temperature)) %>% 
  group_by(site, date) %>%
  mutate(
    day.min = min(temperature, na.rm = TRUE),
    day.max = max(temperature, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(threshold = day.min <= 20 & day.max >= 20) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  arrange(site, date)

unique(WQ2023_abv20deg$site)


WQ2025_abv20deg$consec <- with(rle(WQ2025_abv20deg$threshold), unlist(Map(`*`, sapply(lengths, seq.int), values)))



abv20con2025 <- WQ2025_abv20deg %>%
  group_by(site) %>%
  summarize(max_value = max(consec, na.rm = TRUE)) %>%
  mutate(site = fct_reorder(site, max_value, .desc = TRUE, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=site, y=max_value, fill=site, label = max_value)) +
  geom_text(vjust = -0.5, size = 3.5) +
  geom_col(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "Maximum number of consecutive days over 20",
       x = "Location",
       y = "Max Number of Consecutive Days")



abv20con2025

  
