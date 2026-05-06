###
# 03 - Test the Law of One Price: IKEA Billy Bookcase
# 260501
###

# 0 - packages ----

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(data.table)
p_load(httr2)
p_load(jsonlite)
p_load(ggplot2)
p_load(ggrepel)
p_load(countrycode)


# 1 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)


# 2 - load IKEA prices ----

# This file is produced by 02b-scrape_ikea_jsonld.R

dt = fread("temp/ikea_billy_prices_jsonld.csv")

# Map IKEA country/language codes to ISO2 country codes

country_map = data.table(
  country = c(
    "de/de", "fr/fr", "it/it", "es/es", "pl/pl", "nl/nl", "cz/cs",
    "dk/da", "fi/fi", "no/no", "se/sv", "hu/hu", "ro/ro",
    "us/en", "gb/en", "ie/en", "at/de", "ch/fr",
    "au/en", "ca/en", "sg/en", "jp/ja", "kr/ko", "in/en"
  ),
  iso2 = c(
    "DE", "FR", "IT", "ES", "PL", "NL", "CZ",
    "DK", "FI", "NO", "SE", "HU", "RO",
    "US", "GB", "IE", "AT", "CH",
    "AU", "CA", "SG", "JP", "KR", "IN"
  )
)

dt = merge(dt, country_map, by = "country", all.x = TRUE)

# Add readable country names

dt[, country_name := countrycode(iso2, "iso2c", "country.name")]


# 3 - get exchange rates ----

# API key should be saved in .Renviron as:
# EXCHANGE_API_KEY=your_api_key_here

api_key = Sys.getenv("exchangerate-api")

if (api_key == "") {
  stop("API key not found. Set EXCHANGE_API_KEY in your .Renviron file.")
}

rates_resp = request("https://v6.exchangerate-api.com") |>
  req_url_path_append("v6", api_key, "latest", "USD") |>
  req_retry(max_tries = 3) |>
  req_throttle(rate = 10 / 60) |>
  req_error(is_error = \(resp) FALSE) |>
  req_perform()

if (resp_status(rates_resp) != 200) {
  stop("Exchange-rate API error: ",
       resp_status(rates_resp), " ",
       resp_status_desc(rates_resp))
}

rates_json = rates_resp |> 
  resp_body_json()

rates_dt = data.table(
  currency     = names(rates_json$conversion_rates),
  rate_per_usd = as.numeric(unlist(rates_json$conversion_rates))
)

# Convert USD-based exchange rates into EUR-based exchange rates.
#
# rate_per_usd = units of currency X per 1 USD
# spot         = units of currency X per 1 EUR

eur_per_usd = rates_dt[currency == "EUR", rate_per_usd]

rates_dt[, spot := rate_per_usd / eur_per_usd]

# For EUR countries, spot should be exactly 1.

rates_dt[currency == "EUR", spot := 1]


# 4 - merge prices with exchange rates ----

dt = merge(
  dt,
  rates_dt[, .(currency, spot)],
  by = "currency",
  all.x = TRUE
)

# Check if any IKEA currencies did not match the exchange-rate data

missing_fx = dt[is.na(spot), .(country, country_name, currency, price)]

if (nrow(missing_fx) > 0) {
  print(missing_fx)
  warning("Some countries have missing exchange rates.")
}


# 5 - compute Law of One Price variables ----

# Germany is the reference country.
# Since Germany is priced in EUR, its price is the benchmark.

price_de = dt[country == "de/de", price]

if (length(price_de) != 1 || is.na(price_de)) {
  stop("Could not identify a unique German benchmark price.")
}

# Relative price:
# local IKEA price divided by German IKEA price.
#
# Example:
# if Germany price = 49.99 EUR
# and France price = 59.99 EUR,
# price_rel = 59.99 / 49.99

dt[, price_rel := price / price_de]

# Law of One Price prediction:
#
# If the Law of One Price holds:
# price_local = price_Germany_EUR * spot
#
# Therefore:
# price_rel = spot
#
# LoP ratio:
# ratio = price_rel / spot
#
# ratio = 1 means price parity with Germany
# ratio > 1 means more expensive than Germany after exchange-rate adjustment
# ratio < 1 means cheaper than Germany after exchange-rate adjustment

dt[, ratio := price_rel / spot]

# Keep only observations with valid ratios for plotting

plot_data = dt[!is.na(ratio)]

print(
  dt[order(ratio),
     .(country_name, country, price, currency, spot, price_rel, ratio)]
)


# 6 - diagnostic checks ----

# Which countries are included in the plots?

print(plot_data[, .N, by = currency][order(currency)])

# Which countries are excluded, if any?

excluded = dt[is.na(ratio)]

if (nrow(excluded) > 0) {
  print(excluded[, .(country_name, country, price, currency, spot, price_rel, ratio)])
}


# 7 - plot: relative price vs exchange rate ----

plot_scatter = ggplot(plot_data, aes(x = spot, y = price_rel)) +
  theme_minimal() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_point(aes(color = ratio), size = 3) +
  geom_text_repel(aes(label = country_name), size = 3) +
  scale_color_viridis_c(name = "LoP ratio") +
  scale_x_log10("Exchange rate: local currency per EUR") +
  scale_y_log10("Relative price: local price / German price")

ggsave(
  plot_scatter,
  filename = "output/figures/260501_billy_loop_scatter.png",
  width = 20,
  height = 16,
  units = "cm"
)


# 8 - plot: LoP ratio by country ----

plot_bar = ggplot(
  plot_data,
  aes(x = reorder(country_name, ratio), y = ratio, fill = ratio)
) +
  theme_minimal() +
  geom_col() +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_c(name = "LoP ratio") +
  xlab(NULL) +
  ylab("LoP ratio: 1 = price parity with Germany") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave(
  plot_bar,
  filename = "output/figures/260501_billy_loop_bar.png",
  width = 20,
  height = 14,
  units = "cm"
)


# 9 - interpretation notes ----

# Interpretation:
#
# ratio = 1:
#   The IKEA price is exactly what exchange rates predict relative to Germany.
#
# ratio > 1:
#   The IKEA price is higher than exchange rates predict.
#   The product is relatively expensive compared with Germany.
#
# ratio < 1:
#   The IKEA price is lower than exchange rates predict.
#   The product is relatively cheap compared with Germany.
#
# Deviations from 1 may reflect:
# - VAT and local taxes
# - trade costs
# - transport costs
# - local pricing strategies
# - market power
# - promotions or temporary discounts
# - product comparability problems


# 10 - cleanup ----

rm(
  dt, rates_dt, rates_resp, rates_json, plot_data,
  plot_scatter, plot_bar, country_map, price_de,
  eur_per_usd, missing_fx, excluded
)

gc()