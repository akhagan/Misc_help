flights <- nycflights13::flights

#flights that flew into houston
filter(flights, dest %in% c("IAH", "HOU"))

#flights delayed by 2hr+
filter(flights, arr_delay >= 120)

#operated by united, american or delta
filter(flights, carrier %in% c("UA", "AA", "DL"))

#departed in july, aug & sept
filter(flights, month %in% c(07, 08, 09))

#arrived 2hr late but didn't leave late
filter(flights, arr_delay > 120, dep_delay <= 0)

#delayed 1hr+ but made up 30+ min in flight
filter(flights, dep_delay >= 60 & arr_delay)
       
flights$dep_time[between(flights$dep_time, 0, 600)]

filter(flights, is.na(dep_time))

transmute(flights, dep_time, 
          hour= dep_time %/% 100,
          minute = dep_time %% 100, 
          dep_time_min = (hour*60)+minute)

calc.airtime <- 
  transmute(flights, arr_time - dep_time)

mean_dep <- aggregate(x=flights$dep_delay, by = list(flights$carrier), FUN=function(x) mean(x, na.rm=T))

flights$air_time == calc.airtime

flights %>%
  select (dep_delay, carrier) %>%
  filter(!is.na(dep_delay)) %>%
  group_by (carrier) %>%
summarise (mean_dep = mean(dep_delay),
           sd_dep= sd(dep_delay))