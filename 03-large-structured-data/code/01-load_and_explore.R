###
# 01 - Load and Explore Firm-Level Trade Data
# 2026-02-26
#
# This script:
# - sets the working directory to 03-large-structured-data/
# - loads the filtered Colombian import data from the USA
# - checks that key variables exist
# - creates readable plots for highly skewed firm-level trade data
#
# Recommended way to run:
#
#   source("code/01-load-and-explore.R")
#
# or, from inside the code/ folder:
#
#   source("01-load-and-explore.R")
###

# 0 - packages ----

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(data.table)
p_load(ggplot2)
p_load(scales)
p_load(lubridate)

# 1 - set working directory ----

# If R was opened from the main course folder, move into 03-large-structured-data/.
if (dir.exists("03-large-structured-data")) {
  setwd("03-large-structured-data")
}

# If R was opened from the code/ folder, move one level up.
if (basename(getwd()) == "code") {
  setwd("..")
}

# Check that we are now in the correct folder.
if (!dir.exists("temp/imports_usa")) {
  stop(
    "Could not find temp/imports_usa/. ",
    "Please start R from the main course folder, ",
    "from 03-large-structured-data/, or from 03-large-structured-data/code/. ",
    "Also make sure you ran the filtering script first."
  )
}

cat("Working directory:\n")
cat(getwd(), "\n\n")

# 2 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 3 - load data ----

files = list.files(
  "temp/imports_usa",
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop(
    "No CSV files found in temp/imports_usa/. ",
    "Did you run the filtering script first?"
  )
}

cat("Number of files found:", length(files), "\n\n")

data = rbindlist(
  lapply(files, function(f) {
    cat("Reading:", basename(f), "\n")

    dt = fread(
      f,
      showProgress = FALSE,
      fill = TRUE,
      colClasses = list(
        character = c("FECH", "NIT", "LUIN")
      )
    )

    # Clean column names before combining files.
    names(dt) = trimws(gsub('"', "", names(dt)))

    # Keep the source file name for debugging.
    dt[, source_file := basename(f)]

    return(dt)
  }),
  use.names = TRUE,
  fill = TRUE
)

# Check for duplicated column names.
duplicated_names = names(data)[duplicated(names(data))]

if (length(duplicated_names) > 0) {
  print(duplicated_names)
  stop("Duplicated column names found after loading.")
}

cat("\nData dimensions:\n")
print(dim(data))

cat("\nVariable names:\n")
print(names(data))

# Check that the variables we need exist.
required_vars = c("FECH", "NIT", "VADUA", "VAFODO", "LUIN")

missing_vars = setdiff(required_vars, names(data))

if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing: ",
    paste(missing_vars, collapse = ", ")
  )
}

# 4 - clean variables ----

# Convert trade values to numeric.
data[, VAFODO := as.numeric(VAFODO)]
data[, VADUA  := as.numeric(VADUA)]

# Clean string variables.
data[, NIT := trimws(as.character(NIT))]
data[, LUIN := trimws(as.character(LUIN))]
data[, FECH := trimws(as.character(FECH))]

# Remove observations with missing or non-positive FOB values.
data = data[!is.na(VAFODO) & VAFODO > 0]

# Create log value for plots.
data[, log_VAFODO := log10(VAFODO)]

cat("\nRows after cleaning:\n")
print(nrow(data))

cat("\nSummary of VAFODO:\n")
print(summary(data$VAFODO))

# 5 - size distribution by firm ----

plot_size = data[
  ,
  .(VADUA = sum(VADUA, na.rm = TRUE)),
  by = .(NIT)
]

plot_size = plot_size[!is.na(VADUA) & VADUA > 0]
plot_size[, log_VADUA := log10(VADUA)]

p_size = ggplot(plot_size, aes(x = VADUA)) +
  geom_histogram(bins = 50) +
  scale_x_log10(
    "Total value of imports in 2018",
    labels = label_dollar()
  ) +
  scale_y_continuous("Number of firms") +
  labs(
    title = "Size distribution of Colombian importing firms",
    subtitle = "Firm-level imports from the USA, log-scaled x-axis"
  ) +
  theme_minimal()

ggsave(
  "output/figures/size_distribution_firms.png",
  p_size,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_size, plot_size)

# 6 - imports over time ----

plot_time = data[
  ,
  .(VADUA = sum(VADUA, na.rm = TRUE)),
  by = .(FECH)
]

plot_time = plot_time[!is.na(FECH)]

# FECH is coded as YYMM, for example 1808 = August 2018.
plot_time[, date := ymd(paste0("20", FECH, "01"))]

plot_time = plot_time[!is.na(date)]
setorder(plot_time, date)

p_time = ggplot(plot_time, aes(x = date, y = VADUA)) +
  geom_line() +
  geom_point() +
  scale_x_date("Date") +
  scale_y_continuous(
    "Total value of imports in 2018",
    labels = label_dollar()
  ) +
  labs(
    title = "Total imports from the USA over time",
    subtitle = "Monthly imports, Colombia 2018"
  ) +
  theme_minimal()

ggsave(
  "output/figures/imports_over_time.png",
  p_time,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_time, plot_time)

# 7 - top importing cities ----

plot_city = data[
  !is.na(LUIN) & LUIN != "",
  .(VADUA = sum(VADUA, na.rm = TRUE)),
  by = .(LUIN)
]

plot_city = plot_city[!is.na(VADUA) & VADUA > 0]
setorder(plot_city, -VADUA)

plot_city_top = plot_city[1:min(.N, 15)]

p_city = ggplot(plot_city_top, aes(x = reorder(LUIN, VADUA), y = VADUA)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    "Total value of imports in 2018",
    labels = label_dollar()
  ) +
  labs(
    title = "Top Colombian cities importing from the USA",
    subtitle = "Top 15 cities by total import value",
    x = "City"
  ) +
  theme_minimal()

ggsave(
  "output/figures/top_importing_cities.png",
  p_city,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_city, plot_city, plot_city_top)

# 8 - arithmetic-scale histogram, zoomed ----

# On the full arithmetic scale, the plot is hard to read because the data
# are highly skewed. We therefore zoom to the bottom 99% of transactions.
q99 = quantile(data$VAFODO, 0.99, na.rm = TRUE)

p_arith_zoom = ggplot(data[VAFODO <= q99], aes(x = VAFODO)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(
    "Import value, FOB dollars",
    labels = label_dollar()
  ) +
  scale_y_continuous("Number of transactions") +
  labs(
    title = "Distribution of Import Values",
    subtitle = "Arithmetic scale, excluding the top 1% for readability"
  ) +
  theme_minimal()

ggsave(
  "output/figures/arithmetic_scale_zoomed.png",
  p_arith_zoom,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_arith_zoom, q99)

# 9 - log-transformed histogram ----

# This is usually better than putting both axes of a histogram on log scales.
# It avoids warnings from log10(0) densities.
p_log = ggplot(data, aes(x = log_VAFODO)) +
  geom_histogram(bins = 100) +
  scale_y_continuous("Number of transactions") +
  labs(
    title = "Distribution of Import Values",
    subtitle = "Log-transformed import values",
    x = "log10(Import value, FOB dollars)"
  ) +
  theme_minimal()

ggsave(
  "output/figures/log_import_values.png",
  p_log,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_log)

# 10 - CCDF: complementary cumulative distribution ----

# The CCDF is P(X >= x).
# It is useful for visualizing the upper tail of highly skewed data.
ccdf = data[
  ,
  .(value = sort(VAFODO, decreasing = TRUE))
]

ccdf[, rank := .I]
ccdf[, ccdf := rank / .N]

p_ccdf = ggplot(ccdf, aes(x = value, y = ccdf)) +
  geom_point(alpha = 0.3, size = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "CCDF of Import Values",
    subtitle = "Log-log scale",
    x = "Import value, log scale",
    y = "P(X >= x), log scale"
  ) +
  theme_minimal()

ggsave(
  "output/figures/ccdf.png",
  p_ccdf,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_ccdf, ccdf)

# 11 - CCDF restricted to the upper tail ----

# For a descriptive tail fit, it is better to focus on the upper tail,
# not the full distribution. Here we use the top 10% of observations.
tail_cutoff = quantile(data$VAFODO, 0.90, na.rm = TRUE)

ccdf_tail = data[
  VAFODO >= tail_cutoff,
  .(value = sort(VAFODO, decreasing = TRUE))
]

ccdf_tail[, rank := .I]
ccdf_tail[, ccdf := rank / .N]

# Fit a simple descriptive line in log-log space.
# This is useful for visualization, but it is not a formal Pareto estimator.
fit = lm(log10(ccdf) ~ log10(value), data = ccdf_tail)

alpha_hat = -coef(fit)[2]

p_ccdf_tail = ggplot(ccdf_tail, aes(x = value, y = ccdf)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_abline(
    intercept = coef(fit)[1],
    slope = coef(fit)[2],
    linetype = "dashed"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "CCDF of Import Values",
    subtitle = paste0(
      "Top 10%; descriptive tail slope: ",
      round(alpha_hat, 2)
    ),
    x = "Import value, log scale",
    y = "P(X >= x), log scale"
  ) +
  theme_minimal()

ggsave(
  "output/figures/ccdf_top10.png",
  p_ccdf_tail,
  width = 8,
  height = 5,
  dpi = 300
)

rm(p_ccdf_tail, ccdf_tail, fit, alpha_hat, tail_cutoff)

# 12 - cleanup ----

rm(data)
gc()

cat("\nScript completed successfully.\n")