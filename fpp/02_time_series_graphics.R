library(tidyverse)
library(fpp2)

y <- ts(c(123, 39, 78, 52, 110), start = 2012)

autoplot(melsyd[, "Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

ggseasonplot(a10, polar = TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

# qplot(Temperature, Demand, data = as.data.frame(elecdemand)) +
ggplot(as.data.frame(elecdemand), aes(x = Temperature, y = Demand)) +
  geom_point() +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

beer2 <- window(ausbeer, start = 1992)
gglagplot(beer2)

ggAcf(beer2)
