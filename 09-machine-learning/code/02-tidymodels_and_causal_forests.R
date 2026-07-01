###
# 02 - tidymodels Pipeline and Causal Forests
# 260226
###

# This script covers two modern ML workflows:
#
# Part A: The tidymodels pipeline (recipe -> workflow -> tune -> finalize)
#         applied to LASSO on the ISLR2::Wage dataset.
#
# Part B: Causal forests (grf package) for heterogeneous treatment effect
#         estimation on simulated RCT data.
#
# No external data required.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ISLR2)
p_load(tidymodels)
p_load(glmnet)
p_load(grf)
p_load(magrittr)
p_load(data.table)
p_load(ggplot2)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
set.seed(1234)

# =========================================================================
# PART A: tidymodels pipeline
# =========================================================================

# 1 - tidymodels pipeline ----

# prepare the Wage data
data(Wage)
Wage <- as.data.table(Wage)
Wage[, c("wage", "region") := NULL]
Wage <- droplevels(Wage)

# split into train/test
wage_split <- initial_split(Wage, prop = 0.75, strata = logwage)
train_data <- training(wage_split)
test_data  <- testing(wage_split)

# define a recipe: normalize numeric predictors, dummy-encode factors
rec <- recipe(logwage ~ ., data = train_data) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

# define a LASSO model specification with tunable penalty
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

# bundle recipe + model into a workflow
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(lasso_spec)

# set up 10-fold cross-validation
folds <- vfold_cv(train_data, v = 10)

# create a grid of penalty values
lambda_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 50)

# tune the penalty over the grid
tune_results <- tune_grid(wf, resamples = folds, grid = lambda_grid)

# Interpretation:
# tidymodels separates the three concerns: preprocessing (recipe),
# model specification (parsnip), and fitting strategy (workflow + tune).
# This makes it easy to swap models or change preprocessing steps.

# 2 - evaluate and finalize ----

# visualize CV results
cv_metrics <- collect_metrics(tune_results)
cv_rmse <- cv_metrics[cv_metrics$`.metric` == "rmse", ]

p_tune <- ggplot(as.data.table(cv_rmse),
                 aes(x = penalty, y = mean)) +
  geom_ribbon(aes(ymin = mean - std_err, ymax = mean + std_err),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue") +
  scale_x_log10() +
  labs(title = "LASSO Tuning: RMSE vs Penalty",
       x = expression(lambda ~ "(log scale)"), y = "RMSE (CV)") +
  theme_minimal()
ggsave("output/figures/260226_tidymodels_tune.png", p_tune,
       width = 7, height = 5, dpi = 300)

# select best penalty and finalize
best_penalty <- select_best(tune_results, metric = "rmse")
cat("Best penalty:", best_penalty$penalty, "\n")

final_wf <- finalize_workflow(wf, best_penalty)
final_fit <- fit(final_wf, data = train_data)

# evaluate on held-out test set
test_preds <- predict(final_fit, new_data = test_data)
test_rmse <- sqrt(mean((test_data$logwage - test_preds$.pred)^2))
cat("Test RMSE:", round(test_rmse, 4), "\n")

# predicted vs actual
pred_dt <- data.table(actual = test_data$logwage,
                      predicted = test_preds$.pred)

p_pred <- ggplot(pred_dt, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.2, color = "grey50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Log Wage (Test Set)",
       x = "Actual", y = "Predicted") +
  coord_equal() +
  theme_minimal()
ggsave("output/figures/260226_tidymodels_pred_actual.png", p_pred,
       width = 6, height = 6, dpi = 300)

rm(wf, tune_results, cv_metrics, cv_rmse, p_tune, pred_dt, p_pred)

# Questions:
# - How would you switch from LASSO to ridge in this pipeline?
#   (Hint: change mixture from 1 to 0.)
# - What happens if you remove step_normalize() from the recipe?

# =========================================================================
# PART B: Causal forests
# =========================================================================

# 3 - causal forests setup ----

# simulate an RCT with heterogeneous treatment effects
n <- 2000

# covariates
X <- data.table(
  age       = runif(n, 25, 65),
  income    = rnorm(n, 50000, 15000),
  education = sample(1:5, n, replace = TRUE),
  urban     = rbinom(n, 1, 0.6)
)

# treatment assignment (random)
W <- rbinom(n, 1, 0.5)

# heterogeneous treatment effect: larger for younger, urban individuals
tau_true <- 2 + 3 * (X$age < 40) + 2 * X$urban

# outcome
Y <- 10 + 0.5 * X$age + 0.001 * X$income + tau_true * W + rnorm(n, sd = 3)

# Interpretation:
# We simulate a setting where the treatment effect varies across
# individuals.  Younger urban participants have a CATE of 2+3+2 = 7,
# while older rural participants have a CATE of 2.

# 4 - estimate CATE ----

# fit a causal forest
cf <- causal_forest(
  X = as.matrix(X),
  Y = Y,
  W = W,
  num.trees = 2000
)

# individual treatment effect predictions
tau_hat <- predict(cf)$predictions

# compare estimated vs true CATE
cate_dt <- data.table(
  tau_true = tau_true,
  tau_hat  = tau_hat,
  age      = X$age,
  urban    = factor(X$urban, labels = c("Rural", "Urban"))
)

p_cate <- ggplot(cate_dt, aes(x = tau_true, y = tau_hat)) +
  geom_point(alpha = 0.15, color = "grey50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ urban) +
  labs(title = "Causal Forest: Estimated vs True CATE",
       x = "True Treatment Effect", y = "Estimated Treatment Effect") +
  theme_minimal()
ggsave("output/figures/260226_cate_scatter.png", p_cate,
       width = 9, height = 5, dpi = 300)

# CATE by age
p_cate_age <- ggplot(cate_dt, aes(x = age, y = tau_hat, color = urban)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  geom_hline(yintercept = c(2, 4, 5, 7), linetype = "dotted", color = "grey50") +
  labs(title = "Estimated CATE by Age and Urban Status",
       x = "Age", y = "Estimated CATE", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("output/figures/260226_cate_by_age.png", p_cate_age,
       width = 7, height = 5, dpi = 300)

rm(p_cate, p_cate_age)

# 5 - variable importance and ATE ----

# average treatment effect with standard error
ate <- average_treatment_effect(cf)
cat("ATE:", round(ate[1], 3), "  SE:", round(ate[2], 3), "\n")

# variable importance: which covariates drive heterogeneity?
vi <- variable_importance(cf)
vi_dt <- data.table(
  variable   = colnames(X),
  importance = as.vector(vi)
)
vi_dt <- vi_dt[order(-importance)]

p_vi <- ggplot(vi_dt, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Causal Forest: Variable Importance",
       subtitle = "Which covariates drive treatment-effect heterogeneity?",
       x = NULL, y = "Importance") +
  theme_minimal()
ggsave("output/figures/260226_cf_variable_importance.png", p_vi,
       width = 7, height = 4, dpi = 300)

rm(p_vi, vi_dt)

# Interpretation:
# Age and urban status should show the highest importance, since
# those are the variables that drive the true heterogeneity in our
# simulation.  Income and education are noise dimensions.

# 6 - exercises ----

# 1. Modify the tidymodels pipeline to use elastic net (mixture = 0.5)
#    instead of LASSO.  Does the test RMSE improve?
# 2. In the causal forest simulation, add a continuous interaction
#    (e.g. tau depends linearly on income) and check if the forest
#    picks it up in variable importance.
# 3. Estimate the CATE separately for education levels 1-5 using
#    predict(cf, newdata = ...) and plot the results.
# 4. Try increasing num.trees to 5000.  Does the ATE estimate change?
#    What about computational time?

# 7 - cleanup ----

rm(X, Y, W, tau_true, tau_hat, cf, cate_dt, ate)
rm(Wage, wage_split, train_data, test_data)
rm(final_wf, final_fit, best_penalty, lasso_spec, rec, folds, lambda_grid)
gc()
