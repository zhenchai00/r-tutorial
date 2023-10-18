install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
# Error in library(nycflights13): there is no package called 'nycflights13'
install.packages("nycflights13")



library(tidyverse)
library(magrittr)
library(dplyr)
library(nycflights13)



data(flights, package="nycflights13")
nycflights13::flights
flights

# 1. Find all flights that had an arrival delay of three or more hours.
flight <- flights %>%
  filter(arr_delay >= 180)
flight

# 2. Find all flights that arrived more than three hours late but didn’t leave late.
flight <- flights %>%
  filter(arr_delay > 180, dep_delay <= 0)
flight

# 3. Find all flights that were delayed by at least an hour but made up over 50 minutes in flight.
flight <- flights %>%
  filter(dep_delay >= 60, air_time > 50)
flight

# 4. Find all flights that departed between midnight and 5am.
flight <- flights %>%
  filter(dep_time >= 0, dep_time <= 500)
flight

# 5. Find the most delayed flights.
flight <- flights %>%
  filter(dep_delay == max(dep_delay, na.rm = TRUE))
flight

# 6. Find the flights travelled the farthest. Hint: use air_time column.
flight <- flights %>%
  filter(air_time == max(air_time, na.rm = TRUE))
flight

# 7. Find the flights travelled the shortest.
flight <- flights %>%
  filter(air_time == min(air_time, na.rm = TRUE))
flight

# 8. Show the following details for each flight: flight number, origin and destination.
flight <- flights %>%
  select(flight, origin, dest)
flight

# 9. Show the following details for each flight travelled in June 2013: flight number, Origin and destination.
flight <- flights %>%
  filter(month == 6, year == 2013) %>%
  select(flight, origin, dest)
flight

# 10. Find the carrier that has the maximum departure delay on 1st January 2013.
flight <- flights %>%
  filter(month == 1, day == 1, year == 2013) %>%
  group_by(carrier) %>%
  summarize(max_delay = max(dep_delay, na.rm = TRUE)) %>%
  filter(max_delay == max(max_delay, na.rm = TRUE))
flight