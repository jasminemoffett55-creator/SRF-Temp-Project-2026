require(tidyverse)
require(lubridate)
require(janitor)
require(dplyr)
require(zoo)
require(ggpubr)

setwd("C:/Users/asher.wescott/Desktop/srfdata")

WQ2014.raw <- read.csv("WQ2014_clean_FINAL.csv") 

WQloc <- WQ2014.raw %>%
  distinct(location)

target_locations <- c("Big Creek", "Mill Creek", "Puma", "Weir", "Upper Scott Creek", "SC798L72", "Manta")
  
WQ2014 <- WQ2014.raw %>%  
  filter(location %in% target_locations) %>%
  clean_names() %>%
  dplyr::select( timestamp, location, id, date, temperature_c) %>%
  rename(temp = temperature_c)

  
WQ2014$date <- mdy(WQ2014$date)

gap_report <- WQ2014 %>%
  # 1. Start with your specific locations (Optional)
  # filter(location %in% c("Big Creek", "Stone River")) %>% 
  
  group_by(location) %>%
  
  # 2. Fill the timeline per location
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  
  # 3. Identify missing temperature values
  mutate(is_missing = is.na(temp)) %>%
  
  # 4. Create a unique ID for each continuous "run" of NAs
  mutate(gap_id = cumsum(is_missing != lag(is_missing, default = first(is_missing)))) %>%
  
  # 5. Filter for only the missing rows
  filter(is_missing) %>%
  
  # 6. CRITICAL: Group by BOTH location and gap_id
  group_by(location, gap_id) %>%
  summarize(
    gap_start = min(date),
    gap_end = max(date),
    days_missing = n(),
    .groups = "drop" # Keeps the output clean
  ) %>%
  
  # 7. Remove the helper ID
  dplyr::select(-gap_id)

print(gap_report)

