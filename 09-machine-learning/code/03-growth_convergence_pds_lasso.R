###
# 03 - Growth and Convergence with PDS-LASSO
# 260610
###

# Empirical application for the afternoon class.  We replicate the
# Belloni-Chernozhukov-Hansen (2014) PDS-LASSO analysis of the
# Barro-Lee growth regressions, and then extend it to a modern
# panel built from Penn World Table 10.01 and World Bank WDI.
#
# Story arc:
#
#   Solow predicts beta-convergence -- poor countries grow faster.
#   Bivariate OLS on the data: no convergence.  Puzzle!
#   Barro (1991): add controls -> "conditional" convergence emerges.
#   But with 60 candidate controls and 90 countries OLS is hopeless.
#   PDS-LASSO (Belloni-Chernozhukov-Hansen 2014) picks controls in a
#   data-driven way and gives valid inference on the convergence rate.
#
# Part A: Barro-Lee 1960-1985, hdm::GrowthData (the classic).
# Part B: PWT + WDI 1995-2019 (does the result still hold?).

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(hdm)
p_load(glmnet)
p_load(pwt10)
p_load(WDI)
p_load(data.table)
p_load(magrittr)
p_load(ggplot2)
p_load(patchwork)

# 0 - settings ----
setwd("09-machine-learning")
getwd()
dir.create("output/figures",
           showWarnings = FALSE, recursive = TRUE)
dir.create("input",
           showWarnings = FALSE, recursive = TRUE)
set.seed(1234)

theme_set(theme_minimal(base_size = 12))

# =========================================================================
# PART A: Classical Barro-Lee (1960-1985)
# =========================================================================

# 1 - load classical Barro-Lee data ----

data(GrowthData)
growth <- as.data.table(GrowthData)
cat("GrowthData dimensions:", dim(growth), "\n")

# Outcome  : annualized growth rate of GDP per capita 1960-1985
# gdpsh465 : log GDP per capita in 1960 (the convergence regressor)
# 60 other columns: human-capital, demographic, fiscal, openness, ... controls

# 2 - bivariate convergence: the puzzle ----

# unconditional OLS: does growth fall with initial GDP?
fit_bi <- lm(Outcome ~ gdpsh465, data = growth)
summary(fit_bi)$coefficients["gdpsh465", ]

# Plot it
p_puzzle <- ggplot(growth, aes(gdpsh465, Outcome)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  labs(title = "The convergence puzzle (Barro-Lee 1960-1985)",
       subtitle = "Bivariate regression: poor countries did NOT systematically grow faster",
       x = "log GDP per capita in 1960",
       y = "Annualized GDP growth rate 1960-1985")

ggsave("output/figures/260610_convergence_puzzle_classical.png",
       p_puzzle, width = 7, height = 4.5, dpi = 150)

# Interpretation: the coefficient on initial GDP is essentially zero
# (and even slightly positive).  Solow's prediction of unconditional
# convergence is rejected.

# 3 - full OLS with all controls ----

# Drop the intercept column (lm adds its own)
growth_x <- growth[, -c("intercept")]

fit_full <- lm(Outcome ~ ., data = growth_x)
coef_full <- summary(fit_full)$coefficients
coef_full["gdpsh465", ]

# Interpretation: with 60 controls and 90 observations we are nearly
# saturated.  The coefficient flips sign but standard errors explode.
# We can't tell whether convergence is there or not.

# 4 - Ridge and LASSO paths ----

y <- growth$Outcome
d <- growth$gdpsh465
X <- as.matrix(growth[, -c("Outcome", "intercept", "gdpsh465")])

# Ridge: shrinks all coefficients, keeps gdpsh465 in
ridge <- cv.glmnet(cbind(d, X), y, alpha = 0, standardize = TRUE)
coef_ridge_d <- coef(ridge, s = "lambda.min")["d", 1]

# LASSO: many coefficients shrink to exactly zero
lasso <- cv.glmnet(cbind(d, X), y, alpha = 1, standardize = TRUE)
coef_lasso_d <- coef(lasso, s = "lambda.min")["d", 1]
n_selected <- sum(coef(lasso, s = "lambda.min")[, 1] != 0) - 1  # exclude intercept

cat("Ridge coef on initial GDP:", round(coef_ridge_d, 4), "\n")
cat("LASSO coef on initial GDP:", round(coef_lasso_d, 4),
    "  (selected:", n_selected, "vars)\n")

# Coefficient path for LASSO (which variables enter as lambda shrinks)
lasso_path <- glmnet(cbind(d, X), y, alpha = 1, standardize = TRUE)
plot(lasso_path, xvar = "lambda", label = FALSE,
     main = "LASSO coefficient path (Barro-Lee data)")

# show the variables that enter at the lambda.min step
selected_vars <- rownames(coef(lasso, s = "lambda.min"))[coef(lasso, s = "lambda.min")[, 1] != 0]
cat("LASSO selected variables at lambda.min:\n")
print(selected_vars)  

# Note: Ridge and LASSO are *prediction* tools.  They do not give us
# valid inference on the coefficient of interest -- both shrink it.

# 5 - PDS-LASSO: data-driven controls + valid inference ----

# Belloni-Chernozhukov-Hansen (2014):
#   1. LASSO of Outcome on X     -> selected controls X_Y
#   2. LASSO of gdpsh465 on X    -> selected controls X_D
#   3. X_S = X_Y union X_D
#   4. OLS of Outcome on (gdpsh465, X_S) -> valid SE on coefficient of d

fit_pds <- rlassoEffect(x = X, y = y, d = d, method = "double selection")
pds_summary <- summary(fit_pds)$coefficients
print(pds_summary)

# show selected controls
selY <- rlasso(X, y)$index
selD <- rlasso(X, d)$index
union_sel <- which(selY | selD)
cat("\nPDS-LASSO selected", length(union_sel),
    "variables out of", ncol(X), "\n")
selected_names <- colnames(X)[union_sel]
print(selected_names)

# 6 - side-by-side comparison ----

estimates_classical <- data.table(
  method  = c("Bivariate OLS", "Full OLS (60 controls)",
              "Ridge", "LASSO", "PDS-LASSO"),
  est     = c(coef(fit_bi)["gdpsh465"],
              coef(fit_full)["gdpsh465"],
              coef_ridge_d,
              coef_lasso_d,
              pds_summary[1, "Estimate."]),
  se      = c(summary(fit_bi)$coefficients["gdpsh465", "Std. Error"],
              summary(fit_full)$coefficients["gdpsh465", "Std. Error"],
              NA_real_,  # CV-glmnet doesn't give SEs
              NA_real_,
              pds_summary[1, "Std. Error"]),
  p_value = c(summary(fit_bi)$coefficients["gdpsh465", "Pr(>|t|)"],
              summary(fit_full)$coefficients["gdpsh465", "Pr(>|t|)"],
              NA_real_, NA_real_,
              pds_summary[1, "Pr(>|t|)"])
)
estimates_classical[, period := "1960-1985 (Barro-Lee)"]
print(estimates_classical)

# Visualisation
p_compare_classical <- ggplot(estimates_classical[!is.na(se)],
       aes(x = est,
           y = factor(method, levels = rev(method)),
           xmin = est - 1.96 * se, xmax = est + 1.96 * se)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(color = "steelblue", size = 0.8) +
  labs(title = "Convergence coefficient: Barro-Lee 1960-1985",
       subtitle = "PDS-LASSO recovers significant conditional convergence",
       x = "Estimated coefficient on log(initial GDP per capita)",
       y = NULL)

ggsave("09-machine-learning/figures/260610_convergence_compare_classical.png",
       p_compare_classical, width = 8, height = 3.5, dpi = 150)

# Punchline: bivariate ~ 0 (no convergence), full OLS unstable,
#            PDS-LASSO ~ -0.05 and significant -> conditional
#            convergence at roughly 5% per year, after data-driven
#            selection of human-capital, fiscal, and openness controls.

# =========================================================================
# PART B: Modern extension, 1995-2019
# =========================================================================

# The Barro-Lee dataset stops in 1985.  Does the convergence story
# still hold for the past 25 years?  We rebuild the same regression
# from Penn World Table + WDI.

# 7 - build the modern cross-section (or load from cache) ----

modern_cache <- "09-machine-learning/input/growth_modern_1995_2019.csv"

if (file.exists(modern_cache)) {

  modern <- fread(modern_cache)
  cat("Loaded cached modern dataset:", nrow(modern), "countries.\n")

} else {

  cat("Building modern dataset from PWT and WDI...\n")

  # 7a) GDP per capita and basic shares from PWT 10.01
  data(pwt10.01)
  pwt <- as.data.table(pwt10.01)

  start_year <- 1995
  end_year   <- 2019
  n_years    <- end_year - start_year

  # initial conditions (1995) and end (2019)
  pwt_init <- pwt[year == start_year,
                  .(isocode, country,
                    gdp_pc_init = rgdpo / pop,
                    pop_init   = pop,
                    hc_init    = hc,
                    inv_share  = csh_i,
                    gov_share  = csh_g,
                    exp_share  = csh_x,
                    imp_share  = csh_m)]

  pwt_end  <- pwt[year == end_year,
                  .(isocode, gdp_pc_end = rgdpo / pop)]

  panel <- merge(pwt_init, pwt_end, by = "isocode")
  panel[, gdpsh_init := log(gdp_pc_init)]
  panel[, growth     := (log(gdp_pc_end) - log(gdp_pc_init)) / n_years]
  panel[, openness   := exp_share + imp_share]

  # 7b) Barro-style controls from WDI (initial-year values)
  # Note: we use PWT's hc index for human capital -- WDI's schooling
  # indicators have poor coverage in 1995.
  wdi_inds <- c(
    life_exp      = "SP.DYN.LE00.IN",
    fertility     = "SP.DYN.TFRT.IN",
    inflation     = "FP.CPI.TOTL.ZG",
    pop_growth    = "SP.POP.GROW",
    pop_under14   = "SP.POP.0014.TO.ZS",
    urban_share   = "SP.URB.TOTL.IN.ZS",
    fdi_in        = "BX.KLT.DINV.WD.GD.ZS"
  )

  wdi_raw <- as.data.table(WDI(indicator = wdi_inds,
                               start = start_year, end = start_year,
                               extra = TRUE))
  wdi_raw <- wdi_raw[region != "Aggregates",
                     c("iso3c", names(wdi_inds)), with = FALSE]
  setnames(wdi_raw, "iso3c", "isocode")

  modern <- merge(panel, wdi_raw, by = "isocode")

  # 7c) drop columns/rows with too many missings
  ctrl_cols <- c("hc_init", "inv_share", "gov_share", "exp_share",
                 "imp_share", "openness", names(wdi_inds))

  # need gdp and growth to be available
  modern <- modern[!is.na(growth) & !is.na(gdpsh_init)]

  # keep countries with at most 2 missing controls
  modern[, n_miss := rowSums(is.na(.SD)), .SDcols = ctrl_cols]
  modern <- modern[n_miss <= 2]
  modern[, n_miss := NULL]

  # mean-impute remaining missings (column-wise)
  for (col in ctrl_cols) {
    modern[is.na(get(col)),
           (col) := mean(modern[[col]], na.rm = TRUE)]
  }

  fwrite(modern, modern_cache)
  cat("Saved", nrow(modern), "countries to", modern_cache, "\n")
}

# 8 - bivariate convergence on modern data ----

fit_bi_m <- lm(growth ~ gdpsh_init, data = modern)
summary(fit_bi_m)$coefficients["gdpsh_init", ]

p_puzzle_m <- ggplot(modern, aes(gdpsh_init, growth)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  labs(title = "Convergence puzzle, modern data (PWT 1995-2019)",
       subtitle = paste0(nrow(modern), " countries, bivariate OLS"),
       x = "log GDP per capita in 1995",
       y = "Annualized GDP growth rate 1995-2019")

ggsave("output/figures/260610_convergence_puzzle_modern.png",
       p_puzzle_m, width = 7, height = 4.5, dpi = 150)

# 9 - build a richer control matrix and run PDS-LASSO ----

# base controls
base_ctrls <- c("hc_init", "inv_share", "gov_share", "openness",
                "life_exp", "fertility", "inflation",
                "pop_growth", "pop_under14", "urban_share", "fdi_in")

# expand with squares and pairwise interactions -> high-dimensional X
make_high_dim_X <- function(dt, cols) {
  X_base <- as.matrix(dt[, ..cols])
  stopifnot(!anyNA(X_base))
  X_sq   <- X_base^2
  colnames(X_sq) <- paste0(cols, "_sq")
  # pairwise interactions: i < j
  pairs  <- combn(cols, 2, simplify = FALSE)
  X_int  <- sapply(pairs, function(p) X_base[, p[1]] * X_base[, p[2]])
  colnames(X_int) <- vapply(pairs, function(p) paste0(p[1], "_x_", p[2]), "")
  cbind(X_base, X_sq, X_int)
}

X_modern <- make_high_dim_X(modern, base_ctrls)
y_modern <- modern$growth
d_modern <- modern$gdpsh_init

# Standardise to keep the LASSO penalty meaningful
X_modern <- scale(X_modern)

cat("Modern design: n =", nrow(X_modern),
    "  p =", ncol(X_modern), "\n")

fit_full_m <- lm(y_modern ~ d_modern + X_modern)
coef_full_m <- summary(fit_full_m)$coefficients

# Ridge / LASSO
ridge_m <- cv.glmnet(cbind(d_modern, X_modern), y_modern,
                     alpha = 0, standardize = FALSE)
lasso_m <- cv.glmnet(cbind(d_modern, X_modern), y_modern,
                     alpha = 1, standardize = FALSE)

coef_ridge_m <- coef(ridge_m, s = "lambda.min")[2, 1]  # row 2 is "d_modern"
coef_lasso_m <- coef(lasso_m, s = "lambda.min")[2, 1]

# PDS-LASSO
fit_pds_m <- rlassoEffect(x = X_modern, y = y_modern, d = d_modern,
                          method = "double selection")
pds_summary_m <- summary(fit_pds_m)$coefficients

estimates_modern <- data.table(
  method  = c("Bivariate OLS", "Full OLS",
              "Ridge", "LASSO", "PDS-LASSO"),
  est     = c(coef(fit_bi_m)["gdpsh_init"],
              coef_full_m["d_modern", "Estimate"],
              coef_ridge_m, coef_lasso_m,
              pds_summary_m[1, "Estimate."]),
  se      = c(summary(fit_bi_m)$coefficients["gdpsh_init", "Std. Error"],
              coef_full_m["d_modern", "Std. Error"],
              NA_real_, NA_real_,
              pds_summary_m[1, "Std. Error"]),
  p_value = c(summary(fit_bi_m)$coefficients["gdpsh_init", "Pr(>|t|)"],
              coef_full_m["d_modern", "Pr(>|t|)"],
              NA_real_, NA_real_,
              pds_summary_m[1, "Pr(>|t|)"])
)
estimates_modern[, period := "1995-2019 (PWT + WDI)"]
print(estimates_modern)

# 10 - one combined comparison plot ----

estimates_all <- rbind(estimates_classical, estimates_modern)

p_compare_all <- ggplot(estimates_all[!is.na(se)],
       aes(x = est,
           y = factor(method, levels = rev(unique(method))),
           xmin = est - 1.96 * se, xmax = est + 1.96 * se,
           color = period)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.7) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(title = "The convergence coefficient across methods and periods",
       subtitle = "Negative = poor countries catching up",
       x = "Coefficient on log(initial GDP per capita)",
       y = NULL, color = NULL) +
  theme(legend.position = "bottom")

ggsave("output/figures/260610_convergence_compare_all.png",
       p_compare_all, width = 9, height = 4, dpi = 150)

# 11 - which controls did PDS-LASSO pick? ----

# rerun the two LASSO steps explicitly to inspect selection
selY <- rlasso(X_modern, y_modern)$index
selD <- rlasso(X_modern, d_modern)$index
union_sel <- which(selY | selD)
cat("\nPDS-LASSO selected", length(union_sel),
    "variables out of", ncol(X_modern), "\n")
selected_names <- colnames(X_modern)[union_sel]
print(selected_names)

# 12 - exercises ----

# 1. Re-run Part B over a different window: 1990-2019, or 2000-2019.
#    Does the convergence rate change?
# 2. Swap the LASSO step for an Elastic Net (alpha = 0.5) inside the
#    rlassoEffect-style pipeline.  Hint: write the three steps yourself
#    using cv.glmnet().
# 3. Add regional dummies (Africa, Asia, ...) as additional candidate
#    controls -- do they survive selection?
# 4. Run the modern analysis on log(GDP per worker) instead of log(GDP
#    per capita) using PWT's emp variable.  Does the story change?

# 13 - cleanup ----

rm(list = ls())
gc()
