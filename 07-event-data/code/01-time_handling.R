###
# 01 - Time Handling in R: lubridate and zoo
# 260226
###

# This script introduces the two main approaches to working with dates
# and time series in R:
#
# 1. lubridate -- parse, extract, and manipulate dates and date-times
# 2. zoo -- ordered observations for rolling statistics and time-series plots
#
# No external data is required; all examples are self-contained.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(lubridate)
p_load(zoo)
p_load(magrittr)
p_load(data.table)
p_load(ggplot2)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 1 - lubridate basics ----

# parse character strings into Date or POSIXct objects
d1 <- ymd("2025-05-26")
t1 <- ymd_hms("2025-05-26 14:30:05", tz = "Europe/Berlin")

class(d1)   # "Date"
class(t1)   # "POSIXct"

# different input formats -- lubridate figures them out
dmy("26-05-2025")
mdy("05/26/2025")
ymd(20250526)

# Questions:
# - What happens if you call ymd("26-05-2025")?  Why?
# - What class is returned by Sys.time() vs Sys.Date()?

# 2 - extracting and modifying components ----

year(t1)
month(t1)
month(t1, label = TRUE)
wday(t1, label = TRUE, week_start = 1)   # Monday = 1

# modify components in place
year(t1) <- 2026
month(t1) <- 12
t1

# Questions:
# - After modifying year and month, what day of the week is t1?
# - How could you get the quarter from a date?  (Hint: quarter())

# 3 - durations vs periods ----

# periods -- human-readable calendar units (variable length)
d1 <- ymd("2025-01-31")
d1 + months(1)           # 2025-02-28 (adjusts for short month)
d1 + months(1) + days(1) # 2025-03-01

# durations -- exact number of seconds
d1 + ddays(30)           # exactly 30 * 86400 seconds later

# intervals -- anchored spans between two time points
event <- ymd_hms("2025-02-20 09:30:00", tz = "UTC")
span <- interval(event, now(tzone = "UTC"))
span / days(1)           # how many days since the event?
span / dweeks(1)         # how many weeks?

# Interpretation:
# Use periods when you want calendar-aware arithmetic (e.g. "one month
# from today").  Use durations when you need exact elapsed seconds
# (e.g. timing an experiment).  Intervals store start and end points.

# 4 - rounding and alignment ----

t1 <- ymd_hms("2025-05-26 14:30:05", tz = "Europe/Berlin")

round_date(t1, "hour")      # nearest hour
floor_date(t1, "day")       # start of the day
ceiling_date(t1, "month")   # start of next month

# useful for aggregating irregular data to regular intervals
dates <- ymd("2025-01-03", "2025-01-15", "2025-02-07", "2025-02-20")
floor_date(dates, "month")  # all mapped to first of their month

# 5 - zoo basics ----

# zoo ("Z's ordered observations") stores data with an ordered time index

# create a monthly time series of random values
set.seed(42)
months_seq <- seq(ymd("2020-01-01"), ymd("2023-12-01"), by = "month")
values <- cumsum(rnorm(length(months_seq), mean = 0.5, sd = 2))
ts_zoo <- zoo(values, order.by = months_seq)

# rolling mean (3-month window)
ts_roll3 <- rollmean(ts_zoo, k = 3, align = "right", fill = NA)

# rolling standard deviation with rollapply
ts_rollsd <- rollapply(ts_zoo, width = 6, FUN = sd, align = "right", fill = NA)

# combine into a data.table for ggplot
plot_dt <- data.table(
  date     = index(ts_zoo),
  raw      = coredata(ts_zoo),
  roll3    = coredata(ts_roll3),
  roll_sd  = coredata(ts_rollsd)
)

# plot raw series + rolling mean
p_zoo <- ggplot(plot_dt, aes(x = date)) +
  geom_line(aes(y = raw), color = "grey60") +
  geom_line(aes(y = roll3), color = "steelblue", linewidth = 0.8) +
  labs(title = "Simulated Monthly Series with 3-Month Rolling Mean",
       x = NULL, y = "Value") +
  theme_minimal()
ggsave("output/figures/260226_zoo_rolling_mean.png", p_zoo,
       width = 8, height = 5, dpi = 300)

# plot rolling volatility (6-month window)
p_vol <- ggplot(plot_dt[!is.na(roll_sd)], aes(x = date, y = roll_sd)) +
  geom_area(fill = "tomato", alpha = 0.3) +
  geom_line(color = "tomato") +
  labs(title = "6-Month Rolling Standard Deviation",
       x = NULL, y = "Rolling SD") +
  theme_minimal()
ggsave("output/figures/260226_zoo_rolling_sd.png", p_vol,
       width = 8, height = 5, dpi = 300)

# Questions:
# - What happens to rollmean() output when you change align to "center"?
# - Why might you prefer a wider rolling window?  What is the trade-off?

# 6 - comparison: when to use what ----

# Interpretation:
#
# lubridate:
#   - Parsing dates from messy text
#   - Calendar arithmetic (add months, extract weekdays)
#   - Works with data.table / data.frame columns
#
# zoo:
#   - Rolling statistics (mean, sd, custom functions)
#   - Time-aligned merging of irregular series
#   - Foundation for xts and other finance-oriented packages
#
# In practice you often use both: lubridate to parse and manipulate date
# columns, zoo/rollapply for rolling-window calculations.

# 7 - cleanup ----

rm(plot_dt, ts_zoo, ts_roll3, ts_rollsd, p_zoo, p_vol)
gc()
