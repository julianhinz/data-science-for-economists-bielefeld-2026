###
# 01 - APIs and httr2
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(httr2)
p_load(jsonlite)
p_load(ggplot2)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("temp", showWarnings = FALSE, recursive = TRUE)

# 1 - API basics: World Bank ----

# the World Bank API is free and requires no key
# fetch GDP data for Germany
resp = request("https://api.worldbank.org/v2") %>%
  req_url_path_append("country", "DEU", "indicator", "NY.GDP.MKTP.CD") %>%
  req_url_query(format = "json", per_page = 50, date = "2000:2023") %>%
  req_perform()

# parse the JSON response
body = resp %>% resp_body_json()

# the World Bank API returns a list: [[1]] is metadata, [[2]] is data
gdp_raw = body[[2]]

# extract into a data.table
gdp = rbindlist(lapply(gdp_raw, function(x) {
  data.table(
    country = x$country$value,
    year = as.integer(x$date),
    gdp = as.numeric(x$value)
  )
}))

print(gdp[order(year)])

# quick plot
ggplot(gdp[!is.na(gdp)], aes(x = year, y = gdp / 1e12)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(title = "Germany: GDP (current USD)",
       x = "Year", y = "GDP (trillions USD)",
       caption = "Source: World Bank API") +
  theme_minimal()


# 2 - httr2: retry and rate limiting ----

# httr2 has built-in retry logic and rate limiting
resp = request("https://api.worldbank.org/v2") %>%
  req_url_path_append("country", "all", "indicator", "NY.GDP.MKTP.CD") %>%
  req_url_query(format = "json", per_page = 100, date = "2022") %>%
  req_retry(max_tries = 3) %>%   # retry up to 3 times
  req_throttle(rate = 10 / 60) %>%               # max 10 requests per minute
  req_perform()

resp_status(resp)

# 3 - httr2: HTTP status vs API error ----

# Here we deliberately use an invalid country code.
# The goal is to show that HTTP status 200 does not always mean "we got valid data".

resp = request("https://api.worldbank.org/v2") %>%
  req_url_path_append("country", "INVALID", "indicator", "NY.GDP.MKTP.CD") %>%
  req_url_query(format = "json") %>%
  req_error(is_error = \(resp) FALSE) %>%  # do not stop automatically
  req_perform()

# i. Check the HTTP status code
status = resp_status(resp)

cat("HTTP status:", status, resp_status_desc(resp), "\n")

# ii. Parse the JSON body returned by the API
body = resp %>% 
  resp_body_json()


# iii. Interpret what happened
if (status != 200) {
  
  message("HTTP error: ", status, " ", resp_status_desc(resp))
  
} else if (!is.null(body[[1]]$message)) {
  
  message("HTTP request worked, but the API returned an error:")
  message("API error key: ", body[[1]]$message[[1]]$key)
  message("API error value: ", body[[1]]$message[[1]]$value)
  
} else {
  
  message("Request successful: data returned by the API.")
  
}
# 4 - fetching multiple countries in a loop ----

countries = c("DEU", "FRA", "USA", "CHN", "BRA", "IND", "JPN", "GBR")

results = list()
for (cc in countries) {
  cat("Fetching:", cc, "\n")

  tryCatch({
    resp = request("https://api.worldbank.org/v2") %>%
      req_url_path_append("country", cc, "indicator", "NY.GDP.MKTP.CD") %>%
      req_url_query(format = "json", per_page = 30, date = "2000:2023") %>%
      req_retry(max_tries = 3) %>%
      req_throttle(rate = 10 / 60) %>%
      req_perform()

    body = resp %>% resp_body_json()

    if (length(body) >= 2 && !is.null(body[[2]])) {
      results[[cc]] = rbindlist(lapply(body[[2]], function(x) {
        data.table(
          country = x$country$id,
          country_name = x$country$value,
          year = as.integer(x$date),
          gdp = as.numeric(x$value)
        )
      }))
    }
  }, error = function(e) {
    message("Failed for ", cc, ": ", e$message)
  })

  Sys.sleep(runif(1, 0.5, 1.5))  # polite delay
}

all_gdp = rbindlist(results)
print(all_gdp[order(country, year)])

# 5 - API key management : request API KEY: https://v6.exchangerate-api.com----

# never hardcode API keys in scripts!
# store them in .Renviron (loaded automatically at R startup)

# edit your .Renviron file:
# usethis::edit_r_environ()
#
# add a line like:
# OPENROUTER_API_KEY=sk-or-v1-abc123...
#
# then access it in R:
# api_key = Sys.getenv("OPENROUTER_API_KEY")

api_key <- Sys.getenv("exchangerate-api")

if (api_key == "") {
  stop("API key not found. Please set EXCHANGE_API_KEY in your .Renviron file.")
}
# if (api_key == "") stop("Set OPENROUTER_API_KEY in .Renviron")

# 6 - exchange rates API (JSON) ----

rates_resp <- request("https://v6.exchangerate-api.com") %>%
  req_url_path_append("v6", api_key, "latest", "USD") %>%
  req_retry(max_tries = 3) %>%
  req_throttle(rate = 10 / 60) %>%
  req_error(is_error = \(resp) FALSE) %>%
  req_perform()

if (resp_status(rates_resp) == 200) {
  rates_json <- rates_resp %>% resp_body_json()

  rates_dt <- data.table(
    currency = names(rates_json$conversion_rates),
    rate = unlist(rates_json$conversion_rates)
  )

  print(rates_dt[currency %in% c("EUR", "GBP", "JPY", "CNY", "BRL")])

} else {
  message("HTTP error: ", resp_status(rates_resp), " ", resp_status_desc(rates_resp))
}



# free exchange rate API, no key required
rates_resp = request("https://open.er-api.com/v6/latest/USD") %>%
  req_perform()

rates_json = rates_resp %>% resp_body_json()
rates_dt = data.table(
  currency = names(rates_json$rates),
  rate = unlist(rates_json$rates)
)
print(rates_dt[currency %in% c("EUR", "GBP", "JPY", "CNY", "BRL")])

# 8 - cleanup ----

rm(gdp, all_gdp, rates_dt)
gc()
