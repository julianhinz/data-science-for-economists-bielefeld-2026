###
# 01 - Load and Explore Firm-Level Trade Data
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(ggplot2)
p_load(scales)
p_load(lubridate)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 1 - load data (chunk and pull) ----

# list all csv files in the imports_usa folder
files = list.files("temp/imports_usa", pattern = "\\.csv$", full.names = TRUE)

# read and bind all chunks
data = rbindlist(lapply(files, function(f) {
  cat("Reading:", basename(f), "\n")
  fread(f)
}))

gc()

# 2 - firm-level distributions ----

# convert trade value to numeric
data[, VAFODO := as.numeric(VAFODO)]

# remove zero or missing values
data = data[!is.na(VAFODO) & VAFODO > 0]

# 3 - size distribution ----

plot_size = data[, .(VADUA = sum(VADUA)), by = .(NIT)]
plot = ggplot() +
  theme_minimal() +
  geom_histogram(data = plot_size, aes(x = VADUA)) +
  scale_x_log10("Total value of imports in 2018", labels = scales::label_dollar()) +
  scale_y_continuous("Number of firms") +
  ggtitle("Size distribution of Colombian firms")
ggsave("output/figures/size_distribution.png", plot, width = 6, height = 4)
rm(plot, plot_size)

# 4 - imports over time ----

plot_time = data[, .(VADUA = sum(VADUA)), by = .(FECH)]
plot_time = plot_time[!is.na(FECH)]
plot_time[, date := ymd(paste0(FECH, "01"))]

plot = ggplot() +
    theme_minimal() +
    geom_line(data = plot_time, aes(x = date, y = VADUA)) +
    scale_x_date("Date") +
    scale_y_continuous("Total value of imports in 2018", labels = scales::label_dollar()) +
    ggtitle("Total imports over time")
ggsave("output/figures/imports_over_time.png", plot, width = 6, height = 4)
rm(plot, plot_time)

# 5 - size distribution by city ----

plot_city = data[, .(VADUA = sum(VADUA)), by = .(LUIN)]
plot = ggplot() +
  theme_minimal() +
  geom_histogram(data = plot_city, aes(x = VADUA)) +
  scale_x_log10("Total value of imports in 2018", labels = scales::label_dollar()) +
  scale_y_continuous("Number of firms") +
  ggtitle("Size distribution of Colombian firms by city")
ggsave("output/figures/size_distribution_city.png", plot, width = 6, height = 4)
rm(plot, plot_city)

# 6 - histogram on arithmetic scale (VAFODO) ----

p_arith = ggplot(data, aes(x = VAFODO)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "steelblue") +
  labs(title = "Distribution of Export Values (Arithmetic Scale)",
       x = "Export Value (VAFODO)",
       y = "Density") +
  theme_minimal()
ggsave("output/figures/260226_arithmetic_scale.png", p_arith,
       width = 8, height = 5, dpi = 300)
rm(p_arith)

# 7 - histogram on log-log scale ----

p_loglog = ggplot(data, aes(x = VAFODO)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "steelblue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Distribution of Export Values (Log-Log Scale)",
       x = "Export Value (log scale)",
       y = "Density (log scale)") +
  theme_minimal()
ggsave("output/figures/260226_log_log_scale.png", p_loglog,
       width = 8, height = 5, dpi = 300)
rm(p_loglog)

# 8 - CCDF (complementary cumulative distribution) ----

# the CCDF is P(X >= x), a cleaner way to check for power-law tails
ccdf = data[, .(value = sort(VAFODO, decreasing = TRUE))] %>%
  .[, rank := .I] %>%
  .[, ccdf := rank / .N]

p_ccdf = ggplot(ccdf, aes(x = value, y = ccdf)) +
  geom_point(alpha = 0.3, size = 0.5, color = "steelblue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "CCDF of Export Values (Log-Log Scale)",
       subtitle = "Straight line = power-law tail",
       x = "Export Value (log scale)",
       y = "P(X >= x) (log scale)") +
  theme_minimal()
ggsave("output/figures/260226_ccdf.png", p_ccdf,
       width = 8, height = 5, dpi = 300)
rm(p_ccdf, ccdf)

# 9 - CCDF restricted to top 50% ----

median_val = median(data$VAFODO)

ccdf_top = data[VAFODO >= median_val, .(value = sort(VAFODO, decreasing = TRUE))] %>%
  .[, rank := .I] %>%
  .[, ccdf := rank / .N]

# fit a line in log-log space to estimate the Pareto exponent
fit = lm(log10(ccdf) ~ log10(value), data = ccdf_top)
alpha_hat = -coef(fit)[2]

p_ccdf_top = ggplot(ccdf_top, aes(x = value, y = ccdf)) +
  geom_point(alpha = 0.3, size = 0.5, color = "steelblue") +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],
              color = "red", linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "CCDF of Export Values (Top 50%)",
       subtitle = paste0("Estimated Pareto exponent: ", round(alpha_hat, 2)),
       x = "Export Value (log scale)",
       y = "P(X >= x) (log scale)") +
  theme_minimal()
ggsave("output/figures/260226_ccdf_top50.png", p_ccdf_top,
       width = 8, height = 5, dpi = 300)
rm(p_ccdf_top, ccdf_top, fit)

# 10 - cleanup ----

rm(data)
gc()
