library(tidyverse)
library(nycflights13)

jan1 <- flights %>% filter(month == 1, day == 1)

(nov_dec <- flights %>% filter(month %in% c(11, 12)))

#Task 1
# Arrival delay of two or more hours
flights %>% filter(arr_delay >= 120)

# Flew to Houston
Houston <- flights %>% filter(dest == "IAH" | dest == "HOU")

# Were operated by United, American or Delta
flights %>% filter(carrier %in% c("UA", "DL", "AA"))

# Departede in summer months
(summer <- flights %>% filter(month %in% c(7,8,9)))

# Arrived more than two hours late, but left on time
flights %>% filter(dep_delay <= 0 & arr_delay >= 120)

# Made up over 30 minutes in flight
Gain <- flights %>% filter(dep_delay >= 60 & (dep_delay - arr_delay) > 30)

# Departed betwden midnight and 6 AM
night_dep <- flights %>% filter(dep_time >= 0 & dep_time <= 0600)

?between

# Task 3
# missing dep_time
missing_dep <- sum(is.na(flights$dep_time)) #8255 flights have missing values

col_na <- flights %>% 
  select(where(~any(is.na(.)))) %>% 
  names() # In total, 6 variables have missing NA values

# ARRANGE-exercises
# Missing values to start
NA_on_top <- flights %>% arrange(desc(is.na(dep_time)))

most_delayed <- flights %>% arrange(desc(dep_delay))
left_earliest <- flights %>% arrange(dep_delay)
highest_speed <- flights %>% mutate(speed = distance/air_time) %>% 
  arrange(desc(speed)) %>% View()
farthest <- flights %>% arrange(desc(distance))
shortest <- flights %>% arrange(distance)

# SELECT-exercises
#selecting variable numerous times over, nothing happens
flights %>% select(air_time, everything()) %>% 
  select(air_time, air_time, everything())

# MUTATE-exercises
# task 1
flights %>% mutate(dep_time = (floor(dep_time/100)*60 + (dep_time %% 100)), 
                   sched_dep_time = floor(sched_dep_time/100)*60 + (sched_dep_time %% 100))

#task 2
flights %>% mutate(diff = arr_time - dep_time) %>% select(air_time, diff) #must change
flights_new <- flights %>% mutate(arr_time = (floor(arr_time/100)*60 + 
                                                (arr_time %% 100)), 
                   dep_time = floor(dep_time/100)*60 + (dep_time %% 100))

flights_new %>% mutate(diff = arr_time - dep_time) %>% select(air_time, diff)
# still not seeing that the numbers match, will maybe try to investigate more 
# before deeadline date

# most delayed
flights %>% arrange(desc(dep_delay)) %>% filter(dense_rank(desc(dep_delay)) <= 10)


