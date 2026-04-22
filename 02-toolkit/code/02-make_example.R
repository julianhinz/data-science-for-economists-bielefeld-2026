###
# 02 - Makefile Example: Simple Figure
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(ggplot2)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 1 - data ----

set.seed(42)
months = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
dt = data.table(
  month = months,
  value = cumsum(rnorm(length(months), mean = 2, sd = 1))
)

# 2 - plot ----

p = ggplot(dt, aes(x = month, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Makefile Demo: Monthly Series",
       x = NULL, y = "Value") +
  theme_minimal()

fig_path = "output/figures/260226_make_example.png"
ggsave(fig_path, p, width = 7, height = 5, dpi = 300)

# 3 - cleanup ----

rm(months, dt, p, fig_path)
gc()
