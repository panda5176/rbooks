library(nycflights13)
library(tidyverse)

View(flights)

filter(flights, month == 1, day == 1)
filter(flights, month %in% c(11, 12))
filter(flights, between(month, 5, 6))
filter(flights, is.na(dep_time))

arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, contains("TIME"))
select(flights, time_hour, air_time, everything())

rename(flights, tail_num = tailnum)

mutate(
  flights,
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)
transmute(
  flights,
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
group_by(flights, year, month, day) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
flights %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")
ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1 / 3) +
  geom_smooth(se = FALSE)
