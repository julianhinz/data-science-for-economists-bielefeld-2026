###
# 01 - Model Selection and Regularization
# 260226
###

# This script covers the core machine-learning workflow for economists:
#
# 1. Explore the ISLR2::Wage dataset
# 2. Bias-variance trade-off (simulation)
# 3. Variance explosion as p grows (Monte Carlo)
# 4. Best subset selection with diagnostic plots
# 5. Cross-validation for model selection
# 6. Ridge regression
# 7. LASSO regression
# 8. Elastic net
# 9. Side-by-side comparison
#
# All models predict log(wage) from the Wage dataset.  No external
# data is required.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ISLR2)
p_load(leaps)
p_load(glmnet)
p_load(MASS)
p_load(magrittr)
p_load(data.table)
p_load(ggplot2)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
set.seed(1234)

# 1 - explore data ----

data(Wage)
Wage <- as.data.table(Wage)
str(Wage)
summary(Wage[, .(wage, logwage, age, education, jobclass)])

# drop single-level factors and raw wage (we predict logwage)
Wage <- Wage[, -c("wage", "region")]
Wage <- droplevels(Wage)

# quick look at outcome vs key predictors
p_age <- ggplot(Wage, aes(x = age, y = logwage)) +
  geom_point(alpha = 0.15, color = "grey50") +
  geom_smooth(method = "loess", color = "steelblue", se = FALSE) +
  labs(title = "Log Wage vs Age", x = "Age", y = "Log Wage") +
  theme_minimal()
ggsave("output/figures/260226_wage_vs_age.png", p_age,
       width = 7, height = 5, dpi = 300)

p_edu <- ggplot(Wage, aes(x = education, y = logwage, fill = education)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Log Wage by Education Level", x = NULL, y = "Log Wage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("output/figures/260226_wage_by_education.png", p_edu,
       width = 7, height = 5, dpi = 300)

rm(p_age, p_edu)

# 2 - bias-variance trade-off (simulation) ----

# illustrate how bias, variance, and test MSE change with flexibility
flexibility <- seq(0.5, 10, by = 0.1)
bias2 <- (10 - flexibility)^2 / 10
variance <- flexibility
irred <- 2
test_mse <- bias2 + variance + irred

bv_dt <- data.table(flexibility, bias2, variance, test_mse)
bv_long <- melt(bv_dt, id.vars = "flexibility",
                variable.name = "component", value.name = "error")

p_bv <- ggplot(bv_long, aes(x = flexibility, y = error, color = component,
                             linetype = component)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c(bias2 = "blue", variance = "red",
                                test_mse = "black"),
                     labels = c("Bias\u00b2", "Variance", "Test MSE")) +
  scale_linetype_manual(values = c(bias2 = "dashed", variance = "dashed",
                                   test_mse = "solid"),
                        labels = c("Bias\u00b2", "Variance", "Test MSE")) +
  labs(title = "Bias-Variance Trade-Off",
       x = "Model Flexibility", y = "Error",
       color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = c(0.5, 0.9), legend.direction = "horizontal")
ggsave("output/figures/260226_bias_variance.png", p_bv,
       width = 7, height = 5, dpi = 300)

rm(bv_dt, bv_long, p_bv)

# Interpretation:
# As flexibility increases, bias falls but variance rises.  Test MSE
# is minimized at an intermediate level -- the sweet spot where the
# model is complex enough to capture signal but simple enough to avoid
# fitting noise.

# 3 - variance explosion (Monte Carlo) ----

# show that OLS test MSE grows with p when n is fixed
n_train <- 50
n_test <- 200
p_seq <- seq(5, 45, by = 5)

mc_results <- rbindlist(lapply(p_seq, function(p) {
  beta <- rep(c(1, 0), length.out = p)
  Sigma <- matrix(0.9, p, p) + diag(p) * 0.1

  mse_vec <- replicate(200, {
    X_train <- mvrnorm(n_train, rep(0, p), Sigma)
    y_train <- X_train %*% beta + rnorm(n_train)
    X_test  <- mvrnorm(n_test, rep(0, p), Sigma)
    y_test  <- X_test %*% beta + rnorm(n_test)
    y_hat   <- X_test %*% coef(lm(y_train ~ X_train))[-1] +
               coef(lm(y_train ~ X_train))[1]
    mean((y_test - y_hat)^2)
  })

  data.table(p = p, mse = mean(mse_vec), se = sd(mse_vec) / sqrt(200))
}))

p_mc <- ggplot(mc_results, aes(x = p, y = mse)) +
  geom_ribbon(aes(ymin = mse - 1.96 * se, ymax = mse + 1.96 * se),
              fill = "steelblue", alpha = 0.2) +
  geom_point(color = "steelblue") +
  geom_line(color = "steelblue") +
  labs(title = "OLS Test Error Grows with p (n = 50)",
       x = "Number of Predictors (p)", y = "Test MSE") +
  theme_minimal()
ggsave("output/figures/260226_variance_explosion.png", p_mc,
       width = 7, height = 5, dpi = 300)

rm(mc_results, p_mc)

# Questions:
# - What happens to the curve if you increase n_train to 200?
# - At what p/n ratio does OLS break down completely?

# 4 - best subset selection ----

regfit <- regsubsets(logwage ~ . + I(age^2), data = Wage, nvmax = 24)
reg_sum <- summary(regfit)

# diagnostic plots
diag_dt <- data.table(
  p     = seq_along(reg_sum$bic),
  RSS   = reg_sum$rss,
  AdjR2 = reg_sum$adjr2,
  Cp    = reg_sum$cp,
  BIC   = reg_sum$bic
)
diag_long <- melt(diag_dt, id.vars = "p",
                  variable.name = "criterion", value.name = "value")

best_bic <- which.min(reg_sum$bic)

p_diag <- ggplot(diag_long, aes(x = p, y = value)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ criterion, scales = "free_y") +
  geom_vline(xintercept = best_bic, linetype = "dashed", color = "red") +
  labs(title = "Best Subset Selection: Diagnostic Criteria",
       x = "Number of Predictors", y = NULL) +
  theme_minimal()
ggsave("output/figures/260226_subset_diagnostics.png", p_diag,
       width = 9, height = 6, dpi = 300)

# best model coefficients (by BIC)
best_coefs <- coef(regfit, best_bic)
cat("Best subset model (BIC) selects", length(best_coefs) - 1, "predictors:\n")
print(best_coefs)

rm(diag_dt, diag_long, p_diag)

# Interpretation:
# BIC penalizes complexity more heavily than AIC/Cp.  The selected model
# typically includes age, education, and a handful of other predictors.

# 5 - cross-validation ----

# K-fold CV to estimate test error for each model size
K <- 10
n <- nrow(Wage)
folds <- sample(rep(1:K, length.out = n))

cv_mse <- matrix(NA, nrow = K, ncol = best_bic)

for (k in 1:K) {
  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)

  fit_k <- regsubsets(logwage ~ . + I(age^2), data = Wage[train_idx],
                      nvmax = best_bic)

  X_test <- model.matrix(logwage ~ . + I(age^2), data = Wage[test_idx])

  for (p in 1:best_bic) {
    coefs_p <- coef(fit_k, id = p)
    preds <- X_test[, names(coefs_p), drop = FALSE] %*% coefs_p
    cv_mse[k, p] <- mean((Wage$logwage[test_idx] - preds)^2)
  }
}

cv_dt <- data.table(
  p   = rep(1:best_bic, each = K),
  mse = as.vector(cv_mse)
)
cv_mean <- cv_dt[, .(mean_mse = mean(mse)), by = p]

p_cv <- ggplot(cv_dt, aes(x = p, y = mse)) +
  geom_jitter(width = 0.15, alpha = 0.3, color = "grey50") +
  geom_line(data = cv_mean, aes(x = p, y = mean_mse),
            color = "steelblue", linewidth = 0.8) +
  geom_point(data = cv_mean, aes(x = p, y = mean_mse),
             color = "steelblue", size = 2) +
  labs(title = "10-Fold CV: Test MSE by Model Size",
       x = "Number of Predictors", y = "Test MSE") +
  theme_minimal()
ggsave("output/figures/260226_cv_model_size.png", p_cv,
       width = 7, height = 5, dpi = 300)

rm(cv_mse, cv_dt, cv_mean, p_cv)

# 6 - ridge regression ----

X <- model.matrix(logwage ~ . + I(age^2), data = Wage)[, -1]
y <- Wage$logwage

# fit ridge over a lambda grid
ridge_fit <- glmnet(X, y, alpha = 0)

# coefficient path plot
ridge_coefs <- as.data.table(as.matrix(t(coef(ridge_fit)[-1, ])))
ridge_coefs[, log_lambda := log(ridge_fit$lambda)]
ridge_long <- melt(ridge_coefs, id.vars = "log_lambda",
                   variable.name = "predictor", value.name = "coefficient")

p_ridge_path <- ggplot(ridge_long,
                       aes(x = log_lambda, y = coefficient, color = predictor)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Ridge Coefficient Paths",
       x = expression(log(lambda)), y = "Coefficient") +
  theme_minimal()
ggsave("output/figures/260226_ridge_path.png", p_ridge_path,
       width = 8, height = 5, dpi = 300)

# cross-validation for best lambda
cv_ridge <- cv.glmnet(X, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min

# CV error plot
cv_ridge_dt <- data.table(
  log_lambda = log(cv_ridge$lambda),
  mse        = cv_ridge$cvm,
  se         = cv_ridge$cvsd
)

p_cv_ridge <- ggplot(cv_ridge_dt, aes(x = log_lambda, y = mse)) +
  geom_ribbon(aes(ymin = mse - se, ymax = mse + se),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = log(best_lambda_ridge),
             linetype = "dashed", color = "red") +
  labs(title = "Ridge Regression: CV Error",
       x = expression(log(lambda)), y = "Mean Squared Error") +
  theme_minimal()
ggsave("output/figures/260226_ridge_cv.png", p_cv_ridge,
       width = 7, height = 5, dpi = 300)

cat("Ridge best lambda:", best_lambda_ridge, "\n")

rm(ridge_coefs, ridge_long, cv_ridge_dt, p_ridge_path, p_cv_ridge)

# 7 - lasso regression ----

lasso_fit <- glmnet(X, y, alpha = 1)

# coefficient path plot
lasso_coefs <- as.data.table(as.matrix(t(coef(lasso_fit)[-1, ])))
lasso_coefs[, log_lambda := log(lasso_fit$lambda)]
lasso_long <- melt(lasso_coefs, id.vars = "log_lambda",
                   variable.name = "predictor", value.name = "coefficient")

p_lasso_path <- ggplot(lasso_long,
                       aes(x = log_lambda, y = coefficient, color = predictor)) +
  geom_line(show.legend = FALSE) +
  labs(title = "LASSO Coefficient Paths",
       x = expression(log(lambda)), y = "Coefficient") +
  theme_minimal()
ggsave("output/figures/260226_lasso_path.png", p_lasso_path,
       width = 8, height = 5, dpi = 300)

# cross-validation for best lambda
cv_lasso <- cv.glmnet(X, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min

# how many non-zero coefficients?
lasso_coef <- coef(cv_lasso, s = "lambda.min")
cat("LASSO selects", sum(lasso_coef != 0) - 1, "predictors (excl. intercept)\n")

rm(lasso_coefs, lasso_long, p_lasso_path)

# Interpretation:
# LASSO sets some coefficients to exactly zero (variable selection).
# Ridge shrinks all coefficients toward zero but never to exactly zero.

# 8 - elastic net ----

# alpha = 0.5 blends ridge (alpha=0) and lasso (alpha=1)
cv_enet <- cv.glmnet(X, y, alpha = 0.5)
best_lambda_enet <- cv_enet$lambda.min

enet_coef <- coef(cv_enet, s = "lambda.min")
cat("Elastic net selects", sum(enet_coef != 0) - 1, "predictors\n")
cat("Elastic net best lambda:", best_lambda_enet, "\n")

# Questions:
# - How does the number of selected predictors change as alpha varies
#   from 0 to 1?
# - When might elastic net be preferred over pure LASSO?

# 9 - comparison ----

# coefficient comparison across all three methods
ridge_coef <- coef(cv_ridge, s = "lambda.min")

coef_comp <- data.table(
  variable = rownames(ridge_coef)[-1],
  Ridge    = as.vector(ridge_coef)[-1],
  LASSO    = as.vector(lasso_coef)[-1],
  ElasticNet = as.vector(enet_coef)[-1]
)

coef_long <- melt(coef_comp, id.vars = "variable",
                  variable.name = "method", value.name = "coefficient")

p_coef <- ggplot(coef_long,
                 aes(x = coefficient,
                     y = reorder(variable, coefficient),
                     fill = method)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(Ridge = "steelblue", LASSO = "tomato",
                               ElasticNet = "darkorange")) +
  labs(title = "Coefficient Comparison: Ridge vs LASSO vs Elastic Net",
       x = "Coefficient Value", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("output/figures/260226_coef_comparison.png", p_coef,
       width = 9, height = 7, dpi = 300)

# CV error comparison via 10-fold CV with RMSE
rmse <- function(pred, truth) sqrt(mean((pred - truth)^2))

K <- 10
folds <- sample(rep(1:K, length.out = nrow(Wage)))
cv_errors <- data.table(fold = integer(), method = character(), rmse = numeric())

for (k in 1:K) {
  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)

  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test  <- X[test_idx, ]
  y_test  <- y[test_idx]

  # subset selection
  fit_sub <- regsubsets(logwage ~ . + I(age^2), data = Wage[train_idx],
                        nvmax = best_bic)
  coefs_sub <- coef(fit_sub, id = best_bic)
  X_test_sub <- model.matrix(logwage ~ . + I(age^2),
                             data = Wage[test_idx])
  pred_sub <- X_test_sub[, names(coefs_sub), drop = FALSE] %*% coefs_sub
  rmse_sub <- rmse(pred_sub, y_test)

  # ridge
  cv_r <- cv.glmnet(X_train, y_train, alpha = 0)
  pred_r <- predict(cv_r, s = cv_r$lambda.min, newx = X_test)
  rmse_r <- rmse(pred_r, y_test)

  # lasso
  cv_l <- cv.glmnet(X_train, y_train, alpha = 1)
  pred_l <- predict(cv_l, s = cv_l$lambda.min, newx = X_test)
  rmse_l <- rmse(pred_l, y_test)

  # elastic net
  cv_e <- cv.glmnet(X_train, y_train, alpha = 0.5)
  pred_e <- predict(cv_e, s = cv_e$lambda.min, newx = X_test)
  rmse_e <- rmse(pred_e, y_test)

  cv_errors <- rbind(cv_errors, data.table(
    fold   = k,
    method = c("Subset", "Ridge", "LASSO", "Elastic Net"),
    rmse   = c(rmse_sub, rmse_r, rmse_l, rmse_e)
  ))
}

p_box <- ggplot(cv_errors, aes(x = method, y = rmse, fill = method)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c(Subset = "grey70", Ridge = "steelblue",
                               LASSO = "tomato", `Elastic Net` = "darkorange")) +
  labs(title = "10-Fold CV: RMSE Comparison",
       x = NULL, y = "Root Mean Squared Error") +
  theme_minimal()
ggsave("output/figures/260226_cv_comparison.png", p_box,
       width = 7, height = 5, dpi = 300)

cat("\nMean RMSE by method:\n")
print(cv_errors[, .(mean_rmse = mean(rmse)), by = method])

rm(coef_comp, coef_long, p_coef, cv_errors, p_box)

# 10 - exercises ----

# 1. Vary the elastic-net mixing parameter alpha over a grid from 0 to 1
#    (e.g. seq(0, 1, 0.1)) and plot the CV error vs alpha.  Which alpha
#    minimizes error?
# 2. Replace logwage with wage as the outcome.  How do the best-subset
#    and LASSO results change?
# 3. Add interaction terms (e.g. age:education) to the model matrix.
#    Does LASSO select any interactions?
# 4. Repeat the Monte Carlo simulation in section 3 but use ridge
#    regression instead of OLS.  Does ridge tame the variance explosion?

# 11 - cleanup ----

rm(X, y, Wage, regfit, reg_sum, ridge_fit, lasso_fit)
rm(cv_ridge, cv_lasso, cv_enet)
rm(ridge_coef, lasso_coef, enet_coef)
gc()
