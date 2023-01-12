stop_dist_model = lm(dist ~ speed, data = cars)
summary(stop_dist_model)$coefficients
confint(stop_dist_model, level = 0.99)

speed_grid = seq(min(cars$speed), max(cars$speed), by = 0.01)
dist_ci_band = predict(
  stop_dist_model,
  newdata = data.frame(speed = speed_grid),
  interval = "confidence",
  level = 0.99
)
dist_pi_band = predict(
  stop_dist_model,
  newdata = data.frame(speed = speed_grid),
  interval = "prediction",
  level = 0.99
)

plot(
  dist ~ speed,
  data = cars,
  xlab = "Speed (in Miles Per Hour)",
  ylab = "Stopping Distance (in Feet)",
  main = "Stopping Distance vs Speed",
  pch  = 20,
  cex  = 2,
  col  = "grey",
  ylim = c(min(dist_pi_band), max(dist_pi_band))
)
abline(stop_dist_model, lwd = 5, col = "darkorange")

lines(
  speed_grid,
  dist_ci_band[, "lwr"],
  col = "dodgerblue",
  lwd = 3,
  lty = 2
)
lines(
  speed_grid,
  dist_ci_band[, "upr"],
  col = "dodgerblue",
  lwd = 3,
  lty = 2
)
lines(
  speed_grid,
  dist_pi_band[, "lwr"],
  col = "dodgerblue",
  lwd = 3,
  lty = 3
)
lines(
  speed_grid,
  dist_pi_band[, "upr"],
  col = "dodgerblue",
  lwd = 3,
  lty = 3
)
points(mean(cars$speed),
       mean(cars$dist),
       pch = "+",
       cex = 3)

# ANOVA
summary(stop_dist_model)$fstatistic
anova(stop_dist_model)
