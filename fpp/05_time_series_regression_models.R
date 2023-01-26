library(tidyverse)
library(fpp2)

fit.consMR <-
  tslm(Consumption ~ Income + Production + Unemployment + Savings,
       data = uschange)
summary(fit.consMR)

autoplot(uschange[, 'Consumption'], series = "Data") +
  autolayer(fitted(fit.consMR), series = "Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour = guide_legend(title = " "))

CV(fit.consMR)
