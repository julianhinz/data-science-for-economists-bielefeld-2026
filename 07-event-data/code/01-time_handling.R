###
# 01 - Time Handling in R: lubridate and zoo
# Student TODO version
# 260226
###

# This script introduces two main approaches to working with dates and time series in R:
#
# 1. lubridate -- parse, extract, and manipulate dates and date-times
# 2. zoo -- ordered observations for rolling statistics and time-series plots
#
# No external data is required; all examples are self-contained.
#
# Instructions:
# - Work through the script section by section.
# - Replace TODO(...) calls with the appropriate R code.
# - Answer the interpretation questions in comments.
# - Run each section after completing it.

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(lubridate)
p_load(zoo)
p_load(magrittr)
p_load(data.table)
p_load(ggplot2)

# Helper: TODO placeholders stop execution until you replace them.
TODO <- function(message = "replace this TODO with your code") {
  stop(paste("TODO:", message), call. = FALSE)
}

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1 - lubridate basics ----
# =============================================================================

# Exercise 1.1: parse character strings into Date or POSIXct objects.
# Hint: use ymd() and ymd_hms().

d1 <- TODO("parse '2025-05-26' as a Date")
t1 <- TODO("parse '2025-05-26 14:30:05' as a date-time in Europe/Berlin")

# Check the classes.
class(d1)
class(t1)

# Exercise 1.2: parse dates in different formats.
# Hint: dmy(), mdy(), ymd().

date_dmy <- TODO("parse '26-05-2025'")
date_mdy <- TODO("parse '05/26/2025'")
date_numeric <- TODO("parse 20250526")

# Exercise 1.3: answer by running the code.
# What happens if you call ymd('26-05-2025')? Why?
TODO("try ymd('26-05-2025') and write a short comment explaining the result")

# What class is returned by Sys.time() vs Sys.Date()?
TODO("check class(Sys.time()) and class(Sys.Date())")

# =============================================================================
# 2 - extracting and modifying components ----
# =============================================================================

# Exercise 2.1: extract components from t1.
# Hint: year(), month(), wday(), quarter().

t1_year <- TODO("extract the year from t1")
t1_month_number <- TODO("extract the month number from t1")
t1_month_label <- TODO("extract the month label from t1")
t1_weekday <- TODO("extract the weekday label from t1; set week_start = 1")
t1_quarter <- TODO("extract the quarter from t1")

# Print your answers.
t1_year
t1_month_number
t1_month_label
t1_weekday
t1_quarter

# Exercise 2.2: modify components in place.
# Change the year to 2026 and the month to December.

t1_modified <- t1
TODO("change the year of t1_modified to 2026")
TODO("change the month of t1_modified to 12")
t1_modified

# Question:
# After modifying year and month, what day of the week is t1_modified?
TODO("extract the weekday of t1_modified")

# =============================================================================
# 3 - durations vs periods ----
# =============================================================================

# Exercise 3.1: periods -- human-readable calendar units.

d1 <- ymd("2025-01-31")

# Add one calendar month to d1.

# Add one calendar month and then one day to d1.
d1_plus_one_month_one_day <- TODO("add one calendar month and one day to d1")

# Exercise 3.2: durations -- exact elapsed seconds.
# Add exactly 30 days to d1.
d1_plus_30_days_exact <- TODO("add exactly 30 days using ddays()")

# Compare outputs.
d1_plus_one_month
d1_plus_one_month_one_day
d1_plus_30_days_exact

# Question:
# Why are d1 + months(1) and d1 + ddays(30) not the same idea?
# Write your answer in a comment here.

# Exercise 3.3: intervals -- anchored spans between two time points.

event <- ymd_hms("2025-02-20 09:30:00", tz = "UTC")
span <- TODO("create an interval between event and now(tzone = 'UTC')")

# How many days and weeks since the event?
days_since_event <- TODO("divide the interval by days(1)")
weeks_since_event <- TODO("divide the interval by dweeks(1)")

days_since_event
weeks_since_event

# =============================================================================
# 4 - rounding and alignment ----
# =============================================================================

t1 <- ymd_hms("2025-05-26 14:30:05", tz = "Europe/Berlin")

# Exercise 4.1: round, floor, and ceiling dates.

t1_nearest_hour <- TODO("round t1 to the nearest hour")
t1_start_day <- TODO("floor t1 to the start of the day")
t1_next_month <- TODO("ceil t1 to the start of the next month")

t1_nearest_hour
t1_start_day
t1_next_month

# Exercise 4.2: aggregate irregular dates to months.

dates <- ymd(c("2025-01-03", "2025-01-15", "2025-02-07", "2025-02-20"))
month_bins <- TODO("map all dates to the first day of their month")
month_bins

# Question:
# Why is floor_date(..., 'month') useful before grouping data by month?
# Write your answer in a comment here.

# =============================================================================
# 5 - zoo basics ----
# =============================================================================

# zoo ('Z's ordered observations') stores data with an ordered time index.

# Exercise 5.1: create a monthly time series of random values.

set.seed(42)

months_seq <- TODO("create a monthly sequence from 2020-01-01 to 2023-12-01")
values <- TODO("create a cumulative sum of random values with mean = 0.5 and sd = 2")
ts_zoo <- TODO("create a zoo object with values ordered by months_seq")

# Inspect the zoo object.
head(ts_zoo)
index(ts_zoo)[1:5]
coredata(ts_zoo)[1:5]

# Exercise 5.2: compute rolling statistics.
# Hint: rollmean() and rollapply().

# 3-month rolling mean, right-aligned, with NA where unavailable.
ts_roll3 <- TODO("compute a 3-month rolling mean")

# 6-month rolling standard deviation, right-aligned, with NA where unavailable.
ts_rollsd <- TODO("compute a 6-month rolling standard deviation")

# Exercise 5.3: combine into a data.table for ggplot.

plot_dt <- data.table(
  date    = TODO("extract dates from the zoo index"),
  raw     = TODO("extract the raw values"),
  roll3   = TODO("extract the rolling mean values"),
  roll_sd = TODO("extract the rolling sd values")
)

head(plot_dt)

# Exercise 5.4: plot raw series + rolling mean.
# Fill in the missing y variables and labels.

p_zoo <- ggplot(plot_dt, aes(x = date)) +
  geom_line(aes(y = TODO("raw series column")), color = "grey60") +
  geom_line(aes(y = TODO("rolling mean column")), color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Simulated Monthly Series with 3-Month Rolling Mean",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal()

print(p_zoo)

ggsave(
  "output/figures/260226_zoo_rolling_mean.png",
  p_zoo,
  width = 8,
  height = 5,
  dpi = 300
)

# Exercise 5.5: plot rolling volatility.

p_vol <- ggplot(plot_dt[!is.na(roll_sd)], aes(x = date, y = TODO("rolling sd column"))) +
  geom_area(fill = "tomato", alpha = 0.3) +
  geom_line(color = "tomato") +
  labs(
    title = "6-Month Rolling Standard Deviation",
    x = NULL,
    y = "Rolling SD"
  ) +
  theme_minimal()

print(p_vol)

ggsave(
  "output/figures/260226_zoo_rolling_sd.png",
  p_vol,
  width = 8,
  height = 5,
  dpi = 300
)

# Questions:
# - What happens to rollmean() output when you change align to 'center'?
# - Why might you prefer a wider rolling window? What is the trade-off?
# Write your answers in comments.

# =============================================================================
# 6 - mini challenge: irregular observations to monthly series ----
# =============================================================================

# In applied work, dates are often irregular. Here is a small example.

irregular_dt <- data.table(
  date = ymd(c("2025-01-03", "2025-01-14", "2025-02-02", "2025-02-20", "2025-03-05")),
  value = c(10, 14, 9, 18, 20)
)

# Exercise 6.1: create a month column using floor_date().
irregular_dt[, month := TODO("floor date to month")]

# Exercise 6.2: compute average value by month.
monthly_dt <- irregular_dt[, .(
  mean_value = TODO("mean of value")
), by = month]

monthly_dt

# Exercise 6.3: convert monthly_dt to a zoo object.
monthly_zoo <- TODO("create a zoo object from monthly_dt$mean_value ordered by monthly_dt$month")
monthly_zoo

# =============================================================================
# 7 - comparison: when to use what ----
# =============================================================================

# Fill in the comparison in your own words.
#
# lubridate is useful for:
#   1.
#   2.
#   3.
#
# zoo is useful for:
#   1.
#   2.
#   3.
#
# In practice, when would you use both together?

# =============================================================================
# 8 - cleanup ----
# =============================================================================

# Uncomment after finishing the exercise.
# rm(plot_dt, ts_zoo, ts_roll3, ts_rollsd, p_zoo, p_vol)
# gc()
