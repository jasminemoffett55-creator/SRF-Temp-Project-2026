install.packages(c("tidyverse", "lubridate", "janitor", "dplyr"))
require(tidyverse)
require(lubridate)
require(janitor)
require(dplyr)
require(zoo)

setwd("C:/Users/asher.wescott/Desktop/srfdata")

WQ2014.raw <- read.csv("WQ2014_clean_FINAL.csv") 
  
WQ2014 <- WQ2014.raw %>%  
  clean_names() %>%
  dplyr::select( location, id, date, temperature_c) %>%
  rename(temp = temperature_c)

WQ2014 <- WQ2014 %>%
  mutate(datetime = mdy_hms(timestamp))

range.Date(WQ2014$date)

daily_temp_2014$year <- c("2014")


daily_temp_2014 <- WQ2014 %>%  #creating new dataframe which is daily_temp using wq2014 and...
  mutate(date = as.Date(datetime)) %>%   # extract just the day
  group_by(date) %>% #group temp values by date
  summarise(
    daily_mean_temp = mean(temp, na.rm = TRUE) #ignore missing temp readings and calc mean 
  ) %>%
  ungroup() #this avoids grouping things on accident later on, should use ungroup() after summarise() unless you want the group for all further operations 



#calc MWAT
daily_temp_2014 <- daily_temp_2014 %>% #add following code to daily temp dataframe
  arrange(date) %>% #sort daily temp in chronological order from earliest to latest date
  mutate( #any new column crewated with mutate is a col in dataframe 
    rolling_7d_mean= rollapply( #create new column in dailytemp called rolling_7d_mean which calculates 7 day rolling mean
      daily_mean_temp, #using the daily-meantemp
      width= 7, #7 days
      FUN = mean, #take the mean
      align = "right", #avg ends on current day 
      fill = NA, # put NA in rows where rolling avg cannot be computed due to lack of 7 previous days
      na.rm= TRUE #ignore missing temp values 
    )
  )
daily_temp_2014$year <- c("2014")

daily_temp_2014$md <- format(daily_temp_2014$date, "%m-%d")


WQ2015.raw <- read.csv("WQ2015_clean_FINAL.csv")
  
WQ2015 <- WQ2015.raw %>%
  clean_names() %>%
  dplyr::select(timestamp, date, time_stamp, location, id, date, temperature_c) %>%
  rename(temp = temperature_c)
  
WQ2015$timestamp <- mdy_hms(paste(WQ2015$date, WQ2015$time_stamp))

WQ2015 <- WQ2015 %>%
  mutate(datetime = ymd_hms(timestamp))



daily_temp_2015 <- WQ2015 %>%  #creating new dataframe which is daily_temp using wq2014 and...
  mutate(date = as.Date(datetime)) %>%   # extract just the day
  group_by(date) %>% #group temp values by date
  summarise(
    daily_mean_temp = mean(temp, na.rm = TRUE) #ignore missing temp readings and calc mean 
  ) %>%
  ungroup() #this avoids grouping things on accident later on, should use ungroup() after summarise() unless you want the group for all further operations 



#calc MWAT
daily_temp_2015 <- daily_temp_2015 %>% #add following code to daily temp dataframe
  arrange(date) %>% #sort daily temp in chronological order from earliest to latest date
  mutate( #any new column crewated with mutate is a col in dataframe 
    rolling_7d_mean= rollapply( #create new column in dailytemp called rolling_7d_mean which calculates 7 day rolling mean
      daily_mean_temp, #using the daily-meantemp
      width= 7, #7 days
      FUN = mean, #take the mean
      align = "right", #avg ends on current day 
      fill = NA, # put NA in rows where rolling avg cannot be computed due to lack of 7 previous days
      na.rm= TRUE #ignore missing temp values 
    )
  )

daily_temp_2015$year <- c("2015")

daily_temp_2015$md <- format(daily_temp_2015$date, "%m-%d")

mwatdiff <- inner_join(daily_temp_2014, daily_temp_2015, by = "md") %>%
  group_by(md) %>%
  mutate(diff = rolling_7d_mean.y - rolling_7d_mean.x)
  ungroup()


  diff.plot <- ggplot(mwatdiff, aes(x = date.x, y = diff)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_date(
      date_labels = "%b %d",
      date_breaks = "30 days"
    ) +
    annotate("text", as.Date(x = "2014-03-30"), y = 3, label = "2015 warmer", size = 5, color = "blue", fontface = "italic") +
    annotate("text", as.Date(x = "2014-08-30"), y = -3, label = "2015 cooler", size = 5, color = "blue", fontface = "italic")
  

  
diff.plot  
  
class(mwatdiff$md)
mwatdiff$md <- as.Date(mwatdiff$md, format = "%m-%d")

