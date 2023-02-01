library(tidyverse)
library(tidymodels)

data(crickets, package = "modeldata")
names(crickets)

# Plot the temperature on the x-axis, the chirp rate on the y-axis. The plot
# elements will be colored differently for each species:
ggplot(crickets,
       aes(
         x = temp,
         y = rate,
         color = species,
         pch = species,
         lty = species
       )) +
  # Plot points for each data point and color by species
  geom_point(size = 2) +
  # Show a simple linear model fit created separately for each species:
  geom_smooth(method = lm, se = FALSE, alpha = 0.5) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")

interaction_fit <-  lm(rate ~ (temp + species) ^ 2, data = crickets)
# To print a short summary of the model:
interaction_fit

# Place two plots next to one another:
par(mfrow = c(1, 2))
# Show residuals vs predicted values:
plot(interaction_fit, which = 1)
# A normal quantile plot on the residuals:
plot(interaction_fit, which = 2)

# Fit a reduced model:
main_effect_fit <-  lm(rate ~ temp + species, data = crickets)
# Compare the two:
anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)
predict(main_effect_fit, new_values)

corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)
corr_res %>%
  # Convert each to a tidy format; `map_dfr()` stacks the data frames
  map_dfr(tidy, .id = "predictor") %>%
  ggplot(aes(x = fct_reorder(predictor, estimate))) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with mpg")

split_by_species <-
  crickets %>%
  group_nest(species)
split_by_species

model_by_species <-
  split_by_species %>%
  mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))
model_by_species
model_by_species %>%
  mutate(coef = map(model, tidy)) %>%
  select(species, coef) %>%
  unnest(cols = c(coef))
