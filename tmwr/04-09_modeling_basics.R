library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# library(modeldata) # This is also loaded by the tidymodels package
# data(ames)
# or, in one line:
data(ames, package = "modeldata")
dim(ames)

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white") +
  scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# Set the random number stream using `set.seed()`
# so that the results can be reproduced later.
set.seed(501)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
# or initial_time_split() for time series
ames_split
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
dim(ames_train)

lm_model <- linear_reg() %>% set_engine("lm")

lm_form_fit <-
  lm_model %>%
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <-
  lm_model %>%
  fit_xy(x = ames_train %>% select(Longitude, Latitude),
         y = ames_train %>% pull(Sale_Price))

lm_form_fit
lm_xy_fit

rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger", verbose = TRUE) %>%
  set_mode("regression") %>%
  translate()

lm_form_fit %>% extract_fit_engine()
model_res <-
  lm_form_fit %>%
  extract_fit_engine() %>%
  summary()
# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
param_est

tidy(lm_form_fit)

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)
ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(lm_form_fit, ames_test_small)) %>%
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

tree_model <-
  decision_tree(min_n = 2) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_fit <-
  tree_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(tree_fit, ames_test_small))

# lm_wflow <-
#   workflow() %>%
#   add_model(lm_model)
lm_wflow <-
  lm_wflow %>%
  add_formula(Sale_Price ~ Longitude + Latitude)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))
lm_fit %>% update_formula(Sale_Price ~ Longitude)

lm_wflow <-
  lm_wflow %>%
  remove_formula() %>%
  add_variables(outcome = Sale_Price,
                predictors = c(Longitude, Latitude))
lm_wflow

simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  # all_numeric_predictors(), all_numeric(), all_predictors(), and all_outcomes()
  step_dummy(all_nominal_predictors())
simple_ames

lm_wflow <-
  lm_wflow %>%
  remove_variables() %>%
  add_recipe(simple_ames)
lm_wflow
lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:3))
# Get the recipe after it has been estimated:
lm_fit %>% extract_recipe(estimated = TRUE)

ames_rec <-
  recipe(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
      Latitude + Longitude,
    data = ames_train
  ) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ Gr_Liv_Area:starts_with("Bldg_Type_")) %>%
  step_ns(Latitude, Longitude, deg_free = 20)
tidy(ames_rec)

lm_wflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_recipe(ames_rec)
lm_fit <- fit(lm_wflow, ames_train)

ames_test_res <-
  predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res <-
  bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)
ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

conf_mat(two_class_example, truth = truth, estimate = predicted)
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve
roc_auc(two_class_example, truth, Class1)
autoplot(two_class_curve) # gain_curve(), lift_curve(), and pr_curve()

sensitivity(hpc_cv, obs, pred, estimator = "macro")
sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")
sensitivity(hpc_cv, obs, pred, estimator = "micro")

roc_auc(hpc_cv, obs, VF, F, M, L, estimator = "macro_weighted")
hpc_cv %>%
  group_by(Resample) %>%
  roc_curve(obs, VF, F, M, L) %>%
  autoplot()
