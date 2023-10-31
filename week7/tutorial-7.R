# Data Visualization 

library(ggplot2)
library(tidyverse)
library(datetime)
library(corrplot)

# q1
# economics dataset from ggplot2
ecodataset = economics
ecodataset

# data exploration
dim(ecodataset)
str(ecodataset)

is.data.frame(ecodataset)

# q1a Plot a line for the total population (pop), where the x-axis in the plot displays the month of data collection (date)
q1a = ecodataset %>% 
    select(date, pop)
q1a

ggplot(q1a, aes(x = as.Date(date), y = pop)) +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")

# q1b Plot a line for the total population (pop) after 2005-01-01. Hint: use filter function in dplyr package.
q1b = ecodataset %>% 
    filter(date > "2004-12-31")
q1b

ggplot(q1b, aes(x = as.Date(date), y = pop)) +
    geom_line() 


# q2
# flights dataset from ggplot2
flydataset = flights
flydataset

# data exploration
dim(flydataset)
str(flydataset)

is.data.frame(flydataset)

# q2a Use a proper plot to display the data in carrier name column (carrier). 
q2a = flydataset %>% 
    select(carrier)

ggplot(data = q2a, aes(carrier)) +
    geom_bar()

# q2b Use proper plots to display the data in departure delays (dep_delay). 
q2b = flydataset %>% 
    select(dep_delay)

ggplot(data = q2b, aes(dep_delay)) +
    geom_bar()

# q2c Use proper plots to display the data in departure delays (arr_delay).
q2c = flydataset %>% 
    select(arr_delay)

ggplot(data = q2c, aes(arr_delay)) +
    geom_bar()

# q2d Use a proper plot to display the co-variation between departure delays (dep_delay) and arrival delays (arr_delay). 
q2d = flydataset %>% 
    select(arr_delay, dep_delay)
q2d

#scatter plot method
ggplot(q2d, aes(x = dep_delay, y = arr_delay)) +
    geom_point()

# q2e Use a proper plot to display the co-variation between origin (origin) and destination (dest). 
q2e = flydataset %>% 
    select(origin, dest)
q2e

#scatter plot method
ggplot(q2e, aes(x = origin, y = dest)) +
    geom_count()

# q2f Use a proper plot to display the co-variation between origin (origin) and departure delays (dep_delay) 
q2f = flydataset %>% 
    select(origin, dep_delay)
q2f

ggplot(q2f, aes(x = origin, y = dep_delay)) +
    geom_count()

# q2g Try grouping the plots used to answer section (b) by carrier name (carrier). Hint: use color parameter in aes function
q2g = flydataset %>% 
    select(dep_delay, carrier)

ggplot(data = q2g, aes(dep_delay, colour = carrier)) +
    geom_bar()

# q2h Try facetting the plots used to answer section (b) by carrier name (carrier). 
q2h = flydataset %>% 
    select(dep_delay, carrier)

ggplot(data = q2h, aes(dep_delay, colour = carrier)) +
    geom_bar() + 
    facet_wrap(~carrier)