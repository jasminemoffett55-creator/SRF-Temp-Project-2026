library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(zoo)
library(tidyr)
library(slider)

setwd("C:/Users/asher.wescott/Desktop/srfdata")

massive <- readRDS("SRF_dataset5.rds") %>%#read in csv
  clean_names() %>% #clean/standardize col names
  dplyr::select(timestamp, site, temperature) %>%
  mutate( 
    site = str_trim(site), #get rid of spacing inconsistencies in location names
    date=as.Date(timestamp, format = "%Y-%m-%d %HH:MM:SS") # make sure date var is registering as date
  ) 

massive <- massive %>%
  mutate(site = case_match (site, "US Efish" ~ "USC Comb", .default = site))
massive <- massive %>%
  mutate(site = case_match (site, "Upper Scott Creek" ~ "USC Comb", .default = site))
unique(massive$site)

saveRDS(massive, "SRF_datasetUSCcomb.rds")
