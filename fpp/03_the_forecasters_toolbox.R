library(tidyverse)
library(fpp2)

# Set training data from 1992 to 2007
beer2 <- window(ausbeer, start = 1992, end = c(2007, 4))
# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h = 11),
            series = "Mean", PI = FALSE) +
  autolayer(naive(beer2, h = 11),
            series = "Naïve", PI = FALSE) +
  autolayer(snaive(beer2, h = 11),
            series = "Seasonal naïve", PI = FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title = "Forecast"))

autoplot(goog200) +
  autolayer(meanf(goog200, h = 40),
            series = "Mean", PI = FALSE) +
  autolayer(rwf(goog200, h = 40),
            series = "Naïve", PI = FALSE) +
  autolayer(rwf(goog200, drift = TRUE, h = 40),
            series = "Drift",
            PI = FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour = guide_legend(title = "Forecast"))

(lambda <- BoxCox.lambda(elec))
autoplot(BoxCox(elec, lambda))

res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
Box.test(res, lag = 10, fitdf = 0)
Box.test(res,
         lag = 10,
         fitdf = 0,
         type = "Lj")
checkresiduals(naive(goog200))

googfc1 <- meanf(goog200, h = 40)
googfc2 <- rwf(goog200, h = 40)
googfc3 <- rwf(goog200, drift = TRUE, h = 40)
googtest <- window(goog, start = 201, end = 240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

sqrt(mean(tsCV(
  goog200, rwf, drift = TRUE, h = 1
) ^ 2, na.rm = TRUE))
sqrt(mean(residuals(rwf(goog200, drift = TRUE)) ^ 2, na.rm = TRUE))

autoplot(naive(goog200, bootstrap = TRUE))
