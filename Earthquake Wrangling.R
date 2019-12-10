library(tidyverse)
library(lubridate)
library(Hmisc)

# Read in and join data
eq_carib <- read_csv("query_carib.csv") %>% 
  select(time, depth, mag) %>% 
  mutate(place = "carib")

eq_hawaii <- read_csv("query_hawaii.csv") %>% 
  select(time, depth, mag) %>% 
  mutate(place = "hawaii")

eq <- full_join(eq_carib, eq_hawaii)

# Aggreate rows by week 

eq <- eq %>% 
  group_by(week = cut(time, "week", start.on.monday = FALSE), place) %>% 
  mutate(depth = impute(depth, mean)) %>%
  summarise(depth = mean(depth), 
            mag = mean(mag), 
            count = n()) %>% 
  na.omit()

# Add empty weeks 
week <- seq(7,25535,7)

w <- c()

for (i in week) {
  
  newdate <- as.Date("2019-11-24") - i
  w <- c(w, as.Date(newdate))
  
}

weeks <- as.data.frame(as.Date(w, origin = "1970-01-08")) %>% 
  mutate(week = as.factor(as.Date(w, origin = "1970-01-08"))) %>% 
  select(week)

eq_full <- left_join(weeks, eq) 

eq_full[is.na(eq_full)] <- 0

write.csv(eq_full, "Carribbean-Hawaii Earthquakes 1950-2019.csv")
