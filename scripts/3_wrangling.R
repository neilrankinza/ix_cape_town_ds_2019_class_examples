# METADATA ====
# Description: Wrangling data
# Created: 2019-06-30 (Neil Rankin)
# Last updated: 2019-06-30 (Neil Rankin)
# Last reviewed: NA

# SUMMARY: An introduction following https://r4ds.had.co.nz/transform.html


# INITIALISE ====


#> setup environment ----
options(digits=2)

#> additional libraries ----

library(nycflights13)
library(tidyverse)


# TUTORIAL ====

flights


# filter flights for only Jan 1st
jan1 <- filter(flights, month == 1, day == 1)


# arrange
# flights with longest departure delay
arrange(flights, desc(dep_delay))

# select specific columns
select(flights, year:day)


# use mutate to add a new variable
# talk through this code
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

# introduce the pipe operator

flights_sml <- flights_sml  %>% 
  mutate(gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)


# Doing operations acorss groups
# Summarise (making a 'smaller' data set)

# average departure delay by day
by_day <- flights %>% 
  group_by(year, month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))


# per group metrics
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365) %>% 
  ungroup()

# keeping datasets 'grouped' can lead to problems later (mostly when you forget about grouping)
# It is safest to always ungroup

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


# work through the chapter or use it as a reference when we start working on the Employment Challenge



