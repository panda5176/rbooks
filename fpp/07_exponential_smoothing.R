library(tidyverse)
library(fpp2)

oildata <- window(oil, start = 1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")
oildata <- window(oil, start = 1996)
fc <- ses(oildata, h = 5)
autoplot(fc) +
  autolayer(fitted(fc), series = "Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

air <- window(ausair, start = 1990)
fc <- holt(air, h = 15)
fc2 <- holt(air,
            damped = TRUE,
            phi = 0.9,
            h = 15)
autoplot(air) +
  autolayer(fc, series = "Holt's method", PI = FALSE) +
  autolayer(fc2, series = "Damped Holt's method", PI = FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

aust <- window(austourists, start = 2005)
fit1 <- hw(aust, seasonal = "additive")
fit2 <- hw(aust, seasonal = "multiplicative")
autoplot(aust) +
  autolayer(fit1, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit2, series = "HW multiplicative forecasts",
            PI = FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour = guide_legend(title = "Forecast"))

aust <- window(austourists, start = 2005)
fit <- ets(aust)
summary(fit)
autoplot(fit)
fit %>% forecast(h = 8) %>%
  autoplot() +
  ylab("International visitor night in Australia (millions)")
