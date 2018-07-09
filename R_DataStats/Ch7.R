library(tidyverse)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = z), binwidth = 0.5)

ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)+
  coord_cartesian(xlim = c(0, 1))

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x=sched_dep_time, y=..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

#Use geom_tile() together with dplyr to explore how average flight 
#delays vary by destination and month of year. What makes the plot 
#difficult to read? How could you improve it?

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bin2d(mapping = aes(group= cut_number(carat,20)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bin2d(mapping = aes(group= cut_width(carat,0.1)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bin2d(mapping = aes(group= cut_number(price,100)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bin2d(mapping = aes(group= cut_width(price,100)))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
