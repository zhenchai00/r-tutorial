sapply(flights, function(x) sum(is.na(x)))
sapply(flights, function(x) mean(is.na(x)))

install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("nycflights13")

library(tidyverse)
library(magrittr)
library(dplyr)
library(nycflights13)

data(flights, package="nycflights13")
nycflights13::flights
view(flights)
flights

str(flights)
dim(flights)
head(flights, 13)
tail(flights, 13)
summary(flights)
View(flights)
anyNA(flights)

sum(is.na(flights))

anyColNA = sapply(flights, anyNA)
anyColNA[1:19]

print(as_tibble(flights), n=40)
str(flights)


# Q1 Find all flights that had an arrival delay of three or more hours. 
dim(flights)
flightsQ1 = flights %>% 
    filter(arr_delay >= 180)
dim(flightsQ1)
print(flightsQ1)

flightQ11 = sum(flights$arr_delay >= 180, na.rm=TRUE)
(flightQ11)


# Q2 Find all flights that arrived more than three hours late but didn’t leave late. 
flightsQ2 = flights %>% 
    filter(arr_delay >= 180) %>% 
    filter(dep_delay <= 0)
dim(flightsQ2)
view(flightsQ2)


# Q3 Find all flights that were delayed by at least an hour but made up over 50 minutes in flight.
# (dep_delay - arr_delay) > 50
flightsQ3 = flights %>% 
    mutate(uptime = dep_delay - arr_delay) %>% 
    filter(dep_delay >= 1) %>% 
    filter(uptime > 50)
print(flightsQ3)
view(flightsQ3)

flights %>% 
    filter(dep_delay >= 60, dep_delay - arr_delay > 50) %>%
    view()



# Q4 Find all flights that departed between midnight and 5am.
flightsQ4 = flights %>% 
    filter(dep_time >= 0000) %>% 
    filter(dep_time <= 0500)
print(flightsQ4)
view(flightsQ4)



# Q5 Find the most delayed flights. 
flightsQ5 = flights %>% 
    filter(dep_delay == max(dep_delay, na.rm = TRUE))
print(flightsQ5)
view(flightsQ5)

flightsQ5 = flights %>% 
    arrange(desc(dep_delay))
print(flightsQ5)
view(flightsQ5)


# Q6 Find the flights travelled the farthest. Hint: use air_time column.
flightsQ6 = flights %>% 
    filter(air_time == max(air_time, na.rm = TRUE))
print(flightsQ6)
view(flightsQ6)

flightsQ6 = flights %>% 
    arrange(desc(air_time))
print(flightsQ6)
view(flightsQ6)


# Q7 Find the flights travelled the shortest.
flightsQ7 = flights %>% 
    filter(air_time == min(air_time, na.rm = TRUE))
print(flightsQ7)
view(flightsQ7)

flightsQ7 = flights %>% 
    arrange(air_time)
print(flightsQ7)
view(flightsQ7)


# Q8 Show the following details for each flight: flight number, origin and destination. 
flightsQ8 = flights %>% 
    select(flight, origin, dest)
print(flightsQ8)
view(flightsQ8)



# Q9 Show the following details for each flight travelled in June 2013: flight number, Origin and destination. 
flightsQ9 = flights %>% 
    filter(month == 6, year == 2013) %>% 
    select(flight, origin, dest)
print(flightsQ9)
view(flightsQ9)



# Q10 Find the carrier that has the maximum departure delay on 1st January 2013. 
flightsQ10 = flights %>% 
    filter(day == 1, month == 1, year == 2013) %>% 
    filter(dep_delay == max(dep_delay, na.rm = TRUE))
print(flightsQ10)
view(flightsQ10)

flightsQ10 = flights %>% 
    filter(day == 1, month == 1, year == 2013) %>% 
    arrange(desc(dep_delay)) %>% 
    select(carrier)
print(flightsQ10)
view(flightsQ10)