###
# 01 - Event Study: Brexit and Trade
# Student TODO version - tidyverse syntax
# 260226
###

# Learning goals:
#
# 1. Build simple analysis datasets from a clean bilateral trade database.
# 2. Visualise UK exports around the Brexit transition period.
# 3. Compare UK exports to EU27 with comparator exporters.
# 4. Estimate a simple Difference-in-Differences model.
# 5. Interpret a log-point coefficient as an approximate percentage effect.

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse)
p_load(lubridate)
p_load(countrycode)
p_load(fixest)
p_load(scales)

# Helper: TODO placeholders stop execution until replaced.
TODO <- function(message = "replace this TODO with your code") {
  stop(paste("TODO:", message), call. = FALSE)
}

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

source("code/00-build_brexit_trade_data.R")

trade_total <- build_clean_trade_database(force_rebuild = FALSE) |>
  as_tibble()

event_date <- ymd("2020-12-31")

EU27 <- c("DEU", "AUT", "BEL", "DNK", "FIN", "FRA", "GRC", "IRL",
          "ITA", "LUX", "NLD", "PRT", "ESP", "SWE", "MLT", "CYP",
          "EST", "LTU", "LVA", "CZE", "HUN", "BGR", "ROU", "POL",
          "SVK", "SVN", "HRV")

comparators <- c("GBR", "IRL", "ISL", "SWE", "NOR", "CHE")

# =============================================================================
# 1 - inspect the clean bilateral trade database ----
# =============================================================================

# Exercise 1.1:
# Inspect the first rows and structure of trade_total.

TODO("show the first rows of trade_total")
TODO("inspect the structure of trade_total")

# Exercise 1.2:
# Summarise the number of origins, destinations, months, and date range.

trade_summary <- trade_total |>
  summarise(
    n_origins = TODO("number of distinct origins"),
    n_destinations = TODO("number of distinct destinations"),
    n_months = TODO("number of distinct months"),
    first_month = TODO("first month in the data"),
    last_month = TODO("last month in the data")
  )

print(trade_summary)

# Questions:
# - What is the unit of observation in trade_total?
# - Why do we work with TOTAL exports rather than all HS2 products here?

# =============================================================================
# 2 - build analysis datasets from trade_total ----
# =============================================================================

# Exercise 2.1:
# Build monthly total UK exports to all destinations.

uk_exports <- trade_total |>
  filter(TODO("keep only UK exports: origin == 'GBR'")) |>
  group_by(TODO("group by month/date")) |>
  summarise(
    value = TODO("sum export value"),
    .groups = "drop"
  )

# Exercise 2.2:
# Build monthly exports to EU27 by origin for the UK and comparator countries.

comp_eu <- trade_total |>
  filter(
    TODO("keep origins in comparators"),
    TODO("keep destinations in EU27")
  ) |>
  group_by(TODO("group by date and origin")) |>
  summarise(
    value = TODO("sum export value"),
    .groups = "drop"
  ) |>
  mutate(
    country = TODO("convert origin ISO3 code to country name")
  )

# Exercise 2.3:
# Build monthly exports to all destinations by origin for the UK and comparator countries.
# This is not used in the main DiD below, but it is useful for comparison.

comp_all <- trade_total |>
  filter(TODO("keep origins in comparators")) |>
  group_by(TODO("group by date and origin")) |>
  summarise(
    value = TODO("sum export value"),
    .groups = "drop"
  ) |>
  mutate(
    country = TODO("convert origin ISO3 code to country name")
  )

# Inspect the datasets.

TODO("show first rows of uk_exports")
TODO("show first rows of comp_eu")
TODO("show first rows of comp_all")

# Questions:
# - What is the unit of observation in uk_exports?
# - What is the unit of observation in comp_eu?
# - Why might exports to EU27 be the relevant outcome for a Brexit application?

# =============================================================================
# 3 - descriptive plot: UK exports ----
# =============================================================================

# Exercise 3.1:
# Plot total UK exports over time.
# Add vertical lines for:
# - 2020-01-31: formal UK withdrawal from the EU
# - 2020-12-31: end of the transition period

p_uk <- ggplot(uk_exports, aes(x = TODO("date"), y = TODO("exports in million USD"))) +
  geom_line() +
  geom_vline(xintercept = ymd("2020-01-31"), linetype = "dashed") +
  geom_vline(xintercept = event_date, linetype = "dashed") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Total Value of UK Exports",
    subtitle = "Monthly exports, TOTAL product category",
    x = NULL,
    y = "Exports (mn USD)"
  ) +
  theme_minimal()

print(p_uk)

ggsave(
  "output/figures/260226_uk_exports.png",
  p_uk,
  width = 8,
  height = 5,
  dpi = 300
)

# Questions:
# - What happens to UK exports around early 2020?
# - Why is a simple before/after comparison not enough?
# - What other shocks might affect trade around this period?

# =============================================================================
# 4 - country comparison plot ----
# =============================================================================

# Exercise 4.1:
# Normalize each country's exports to EU27 by its own 2019 average.
#
# Hint:
# group_by(origin) |>
# mutate(value_norm = value / mean(value[year(date) == 2019], na.rm = TRUE))

comp_dt <- comp_eu |>
  group_by(TODO("origin")) |>
  mutate(
    value_norm = TODO("value divided by 2019 average within origin")
  ) |>
  ungroup()

# Exercise 4.2:
# Plot normalized exports to EU27 by country.

p_comp <- ggplot(comp_dt, aes(x = TODO("date"), y = TODO("value_norm"), color = TODO("country"))) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = event_date, linetype = "dashed", color = "black") +
  scale_color_manual(values = c(
    "United Kingdom" = "#D55E00",
    "Ireland" = "#009E73",
    "Iceland" = "#0072B2",
    "Norway" = "#CC79A7",
    "Sweden" = "#E69F00",
    "Switzerland" = "#56B4E9"
  )) +
  labs(
    title = "Exports to EU27 Relative to 2019 Average",
    subtitle = "UK vs comparator exporters",
    x = NULL,
    y = "Normalized exports (2019 = 1)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_comp)

ggsave(
  "output/figures/260226_country_comparison.png",
  p_comp,
  width = 8,
  height = 5,
  dpi = 300
)

# Questions:
# - Do the UK and comparator countries move similarly before the event?
# - Does the UK visibly diverge after the transition period?
# - What makes a good comparator country in this setting?

# =============================================================================
# 5 - Difference-in-Differences ----
# =============================================================================

# Exercise 5.1:
# Define treatment variables.
#
# treated: UK exporter
# post: after end of transition period
# treatment: treated x post

reg_dt <- comp_eu |>
  mutate(
    treated = TODO("origin == 'GBR'"),
    post = TODO("date > event_date"),
    treatment = TODO("treated * post")
  )

# Exercise 5.2:
# Estimate a Difference-in-Differences model:
#
# log(exports_it) = beta * treatment_it + date FE + origin FE + error_it
#
# Hint:
# feols(log(value) ~ treatment | date + origin, data = reg_dt)

reg_did <- TODO("estimate the DiD model with feols")

etable(reg_did)

# Exercise 5.3:
# Interpret the coefficient as a percentage effect.

did_coef <- TODO("extract the treatment coefficient from reg_did")
did_approx_percent <- TODO("100 * did_coef")
did_exact_percent <- TODO("100 * (exp(did_coef) - 1)")

did_approx_percent
did_exact_percent

# Interpretation questions:
# - What does the treatment coefficient compare?
# - What do date fixed effects absorb?
# - What do origin fixed effects absorb?
# - What is the parallel-trends assumption here?
# - Why is the exact percentage effect 100 * (exp(beta) - 1) rather than 100 * beta?

# =============================================================================
# 6 - optional extension ----
# =============================================================================

# Try one of the following:
#
# 1. Replace comp_eu with comp_all. What changes?
# 2. Change event_date to ymd("2020-01-31"). What changes?
# 3. Remove one comparator country and rerun the DiD.
# 4. Add a country to comparators and rebuild comp_eu.
