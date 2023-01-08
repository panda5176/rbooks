library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()
as_datetime(today())
as_date(now())

ymd("2017-01-31")
mdy("Jan 31st, 2017")
dmy(31012017)
ymd_hms("2017-01-31 20:11:59")

flights %>%
  select(year, month, day, hour, minute) %>%
  mutate(departure = make_datetime(year, month, day, hour, minute))

datetime <- ymd_hms("2017-01-31 20:11:59")
year(datetime)
mday(datetime)
wday(datetime)
month(datetime, label = TRUE, abbr = FALSE)

year(datetime) <- 2020
update(datetime, year = 2020)

today() - ymd(19791014)
as.duration(today() - ymd(19791014))
now() + dseconds(15)
today() + ddays(0:5)
today() + dyears(1)
now() + seconds(15)
today() + days(0:5)
today() + years(1)

ymd_hms("2015-06-01 12:00:00", tz = "UTC")
x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York")
x2 <- ymd_hms("2015-06-01 12:00:00", tz = "Asia/Seoul")
x2 - x1
