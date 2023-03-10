library(tidyverse)
library(fpp2)

autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, 5), series = "5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(
    values = c("Data" = "grey50", "5-MA" = "red"),
    breaks = c("Data", "5-MA")
  )

autoplot(elecequip, series = "Data") +
  autolayer(ma(elecequip, 12), series = "12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(
    values = c("Data" = "grey", "12-MA" = "red"),
    breaks = c("Data", "12-MA")
  )

elecequip %>% decompose(type = "multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical equipment index")

elecequip %>%
  stl(t.window = 13,
      s.window = "periodic",
      robust = TRUE) %>%
  autoplot()

fit <- stl(elecequip,
           t.window = 13,
           s.window = "periodic",
           robust = TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method = "naive") %>%
  autoplot() + ylab("New orders index")
