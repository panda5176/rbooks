plot(
  dist ~ speed,
  data = cars,
  xlab = "Speed (in Miles Per Hour)",
  ylab = "Stopping Distance (in Feet)",
  main = "Stopping Distance vs Speed",
  pch = 20,
  cex = 2,
  col = "grey"
)

x = cars$speed
y = cars$dist

Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x)) ^ 2)
Syy = sum((y - mean(y)) ^ 2)

beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)

## interpolation
beta_0_hat + beta_1_hat * 21.5

## residual
cars[which(cars$speed == 8),]$dist - (beta_0_hat + beta_1_hat * 8)

## residual variance
y_hat = beta_0_hat + beta_1_hat * x
e = y - y_hat
n = length(e)
s2_e = sum(e ^ 2) / (n - 2)
s2_e

## standard error
s_e = sqrt(s2_e)
s_e

SST = sum((y - mean(y)) ^ 2)
SSReg = sum((y_hat - mean(y)) ^ 2)
SSE = sum((y - y_hat) ^ 2)
c(SST, SSReg, SSE)
s2_e == SSE / (n - 2)

## coefficient of determination(r2)
R2 = SSReg / SST
R2

stop_dist_model = lm(dist ~ speed, data = cars)
stop_dist_model
plot(
  dist ~ speed,
  data = cars,
  xlab = "Speed (in Miles Per Hour)",
  ylab = "Stopping Distance (in Feet)",
  main = "Stopping Distance vs Speed",
  pch  = 20,
  cex  = 2,
  col  = "grey"
)
abline(stop_dist_model, lwd = 3, col = "darkorange")
summary(stop_dist_model)
summary(stop_dist_model)$sigma
predict(stop_dist_model, newdata = data.frame(speed = c(8, 21, 50)))
