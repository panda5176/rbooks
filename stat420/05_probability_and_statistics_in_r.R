dnorm(x = 3, mean = 2, sd = 5)
pnorm(q = 3, mean = 2, sd = 5)
qnorm(p = 0.975, mean = 2, sd = 5)
rnorm(n = 10, mean = 2, sd = 5)
dbinom(x = 6, size = 10, prob = 0.75)

# t-test
capt_crisp = data.frame(weight = c(15.5, 16.2, 16.1, 15.8, 15.6, 16.0, 15.8, 15.9, 16.2))
x_bar = mean(capt_crisp$weight)
s = sd(capt_crisp$weight)
mu_0 = 16
n = 9
t = (x_bar - mu_0) / (s / sqrt(n))
t
p_value = pt(t, df = n - 1)
p_value

capt_test_results = t.test(
  x = capt_crisp$weight,
  mu = 16,
  alternative = c("two.sided"),
  # alternative = c("less"),
  conf.level = 0.95,
)
capt_test_results
names(capt_test_results)
capt_test_results$conf.int

critical_value = qt(0.975, df = 8)
ci = c(x_bar - critical_value * s / sqrt(9),
       x_bar + critical_value * s / sqrt(9))
ci

# paired t-test
x = c(70, 82, 78, 74, 94, 82)
n = length(x)
y = c(64, 72, 60, 76, 72, 80, 84, 68)
m = length(y)
x_bar = mean(x)
s_x   = sd(x)
y_bar = mean(y)
s_y   = sd(y)

s_p = sqrt(((n - 1) * s_x ^ 2 + (m - 1) * s_y ^ 2) / (n + m - 2))
s_p
t = ((x_bar - y_bar) - 0) / (s_p * sqrt(1 / n + 1 / m))
t
p_value = 1 - pt(t, df = n + m - 2)
p_value

t.test(x, y, alternative = c("greater"), var.equal = TRUE)

t_test_data = data.frame(values = c(x, y),
                         group  = c(rep("A", length(x)), rep("B", length(y))))
t.test(
  values ~ group,
  data = t_test_data,
  alternative = c("greater"),
  var.equal = TRUE
)
