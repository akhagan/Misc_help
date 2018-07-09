library(lubridate)
library(nycflights13)

#Compare air_time with the duration between the departure and arrival. 
#Explain your findings. (Hint: consider the location of the airport.)

flights %>%
  select(dep_time, arr_time, air_time, dest) %>%
  rename(faa = dest) %>%
  left_join(airports, by="faa")%>%
  select(-c(name, lat, lon, alt, dst, tzone))%>%
  mutate(tz_nyc = tz+5, 
         nyc_arr_time = arr_time + tz_nyc) %>%
  mutate(calc_air_time=nyc_arr_time-dep_time)%>%
  View()
