###
# 03 - Tidyverse vs data.table: Side-by-Side Comparison
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(dplyr)
p_load(ggplot2)
p_load(microbenchmark)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 1 - sample data ----

# use the starwars dataset (ships with dplyr)
data(starwars, package = "dplyr")
sw_tbl = starwars
sw_dt = as.data.table(starwars)

# 2 - filter ----

# tidyverse
sw_tbl %>%
  filter(species == "Human", height >= 190)

# data.table
sw_dt[species == "Human" & height >= 190]

# 3 - arrange ----

# tidyverse
sw_tbl %>%
  arrange(birth_year)

# data.table
sw_dt[order(birth_year)]

# by reference (modifies in place, no copy)
# setorder(sw_dt, birth_year, na.last = TRUE)

# 4 - select ----

# tidyverse
sw_tbl %>%
  select(name, height, mass, homeworld)

# data.table
sw_dt[, .(name, height, mass, homeworld)]

# 5 - mutate ----

# tidyverse
sw_tbl %>%
  select(name, birth_year) %>%
  mutate(dog_years = birth_year * 7)

# data.table (creates column by reference)
sw_dt[, dog_years := birth_year * 7]

# 6 - summarise / group_by ----

# tidyverse
sw_tbl %>%
  group_by(species) %>%
  summarise(
    mean_height = mean(height, na.rm = TRUE),
    n = n()
  )

# data.table
sw_dt[, .(mean_height = mean(height, na.rm = TRUE),
          n = .N),
      by = species]

# 7 - chaining multiple operations ----

# tidyverse
sw_tbl %>%
  filter(species == "Human") %>%
  group_by(homeworld) %>%
  summarise(mean_height = mean(height, na.rm = TRUE)) %>%
  arrange(desc(mean_height))

# data.table with pipes
sw_dt[species == "Human"] %>%
  .[, .(mean_height = mean(height, na.rm = TRUE)), by = homeworld] %>%
  .[order(-mean_height)]

# 8 - .SD: apply functions across columns ----

# tidyverse
sw_tbl %>%
  group_by(species) %>%
  summarise(across(c(height, mass, birth_year), mean, na.rm = TRUE))

# data.table
sw_dt[, lapply(.SD, mean, na.rm = TRUE),
      .SDcols = c("height", "mass", "birth_year"),
      by = species]

# 9 - benchmarking ----

# use the storms dataset for a larger comparison
data(storms, package = "dplyr")
storms_dt = as.data.table(storms)

bench = microbenchmark(
  dplyr = {
    storms %>%
      group_by(name, year, month, day) %>%
      summarise(
        wind = mean(wind),
        pressure = mean(pressure),
        .groups = "drop"
      )
  },
  data.table = {
    storms_dt[, .(wind = mean(wind),
                  pressure = mean(pressure)),
              by = .(name, year, month, day)]
  },
  times = 10
)
print(bench)

# 10 - combining data.table with ggplot2 ----

storms_dt[, .(wind = mean(wind),
              pressure = mean(pressure),
              category = first(category)),
          by = .(name, year, month, day)] %>%
  ggplot(aes(x = pressure, y = wind, color = category)) +
  geom_point(alpha = 0.3) +
  labs(title = "Storm Pressure vs Wind Speed",
       x = "Pressure (mbar)",
       y = "Wind Speed (knots)") +
  theme_minimal()

# 11 - cleanup ----

rm(sw_tbl, sw_dt, storms_dt, bench)
gc()
