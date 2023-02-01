library(tidyverse)
library(fpp2)

Box.test(goog200, lag = 10, type = "Ljung-Box")
Box.test(diff(goog200), lag = 10, type = "Ljung-Box")

cbind(
  "Sales ($million)" = a10,
  "Monthly log sales" = log(a10),
  "Annual change in log sales" = diff(log(a10), 12)
) %>%
  autoplot(facets = TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")

library(urca)
goog %>% ur.kpss() %>% summary()
goog %>% diff() %>% ur.kpss() %>% summary()

fit <- auto.arima(uschange[, "Consumption"], seasonal = FALSE)
fit %>% forecast(h = 10) %>% autoplot(include = 80)

lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales" = lh02) %>%
  autoplot(facets = TRUE) + xlab("Year") + ylab("")
lh02 %>% diff(lag = 12) %>%
  ggtsdisplay(xlab = "Year",
              main = "Seasonally differenced H02 scripts")
(fit <- Arima(
  h02,
  order = c(3, 0, 1),
  seasonal = c(0, 1, 2),
  lambda = 0
))
h02 %>%
  Arima(order = c(3, 0, 1),
        seasonal = c(0, 1, 2),
        lambda = 0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")
