###
# 02b - Scrape IKEA Billy Bookcase Prices
# Visible HTML vs JSON-LD approach
# 260501
###

# 0 - packages ----

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(data.table)
p_load(rvest)
p_load(stringr)
p_load(jsonlite)

# 1 - settings ----

dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("temp", showWarnings = FALSE, recursive = TRUE)

# 2 - download one IKEA product page ----

country = "de/de"

url = str_c(
  "https://www.ikea.com/",
  country,
  "/p/billy-buecherregal-weiss-00263850/"
)

page = tryCatch(
  read_html(url),
  error = function(e) {
    message("Failed to read page: ", e$message)
    NULL
  }
)

if (is.null(page)) stop("Page could not be downloaded.")


# 3 - approach 1: scrape the visible HTML price ----

# In the browser:
# right-click on the visible price -> Inspect
#
# We see a class such as:
# class="pipcom-price"
#
# This is the visible price shown to users.

price_text = page |>
  html_element(".pipcom-price") |>
  html_text2()

price_text

# The extracted text may contain duplicated information, for example:
# "49.99€Preis 49.99€"
#
# So we clean it and keep the first price-like number.

price_num = price_text |>
  str_extract("[0-9]+[,.][0-9]+") |>
  str_replace(",", ".") |>
  as.numeric()

price_num

# Teaching point:
# This works, but it depends on IKEA keeping the same CSS class name.
# If the website design changes, ".pipcom-price" may stop working.


# 4 - inspect JSON-LD structured data ----

# Many e-commerce sites include machine-readable product data inside:
#
# <script type="application/ld+json">
#
# This data is often used for search engines and is more structured than
# the visible HTML.

jsonld_blocks = page |>
  html_elements("script[type='application/ld+json']") |>
  html_text()

# How many JSON-LD blocks are on the page?

length(jsonld_blocks)

# Print all JSON-LD blocks so students can inspect them.
# Ask students to search for:
# - offers
# - price
# - priceCurrency

for (i in seq_along(jsonld_blocks)) {
  cat("\n\n============================\n")
  cat("JSON-LD block", i, "\n")
  cat("============================\n\n")
  
  pretty_block = tryCatch(
    prettify(jsonld_blocks[[i]]),
    error = function(e) jsonld_blocks[[i]]
  )
  
  cat(pretty_block)
}

# Teaching point:
# We look for a field called "offers" because product price information
# is usually stored there.


# 4b - manually extract price from the JSON-LD block ----

# First, find which JSON-LD block contains the word "offers".

has_offers = str_detect(jsonld_blocks, '"offers"')

has_offers

# Keep the first JSON-LD block that contains "offers".

product_block = jsonld_blocks[has_offers][[1]]

# Show this block in a readable format.

cat(prettify(product_block))

# Now parse the JSON text into an R list.

product_json = fromJSON(product_block, simplifyVector = FALSE)

# Inspect the top-level fields in the JSON-LD object.

names(product_json)

# Extract the part called "offers".

offers = product_json[["offers"]]

offers

# The "offers" part contains the price and the currency.

price_jsonld = as.numeric(offers[["price"]])
currency_jsonld = offers[["priceCurrency"]]

price_jsonld
currency_jsonld

# Put the result into a small table.

data.table(
  price = price_jsonld,
  currency = currency_jsonld
)

# Teaching point:
# Now we know the logic:
#
# 1. Find all JSON-LD blocks.
# 2. Find the block containing "offers".
# 3. Parse it as JSON.
# 4. Extract offers$price and offers$priceCurrency.
#
# Next, we wrap this logic into a function.


# 5 - helper function: extract price from JSON-LD ----

extract_price_jsonld = function(page) {
  
  scripts = page |>
    html_elements("script[type='application/ld+json']") |>
    html_text()
  
  for (s in scripts) {
    
    parsed = tryCatch(
      fromJSON(s, simplifyVector = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(parsed)) next
    
    offers = parsed[["offers"]]
    
    if (!is.null(offers)) {
      return(list(
        price    = as.numeric(offers[["price"]]),
        currency = offers[["priceCurrency"]]
      ))
    }
  }
  
  list(price = NA_real_, currency = NA_character_)
}

# Test the function on the German page.

result = extract_price_jsonld(page)

cat("JSON-LD price:", result$price, result$currency, "\n")


# 6 - scrape multiple country pages ----

countries = c(
  "de/de", "fr/fr", "it/it", "es/es", "pl/pl", "nl/nl", "cz/cs",
  "dk/da", "fi/fi", "no/no", "se/sv", "hu/hu", "ro/ro",
  "us/en", "gb/en", "ie/en", "at/de", "ch/fr",
  "au/en", "ca/en", "sg/en", "jp/ja", "kr/ko", "in/en"
)

# For some country websites, the same BILLY URL does not work.
# We therefore use an alternative product page.
#
# This is a comparability issue and should be mentioned in the analysis.

alt_product_countries = c("in/en", "sg/en", "jp/ja", "kr/ko")

dt = data.table(
  country     = character(),
  price       = numeric(),
  currency    = character(),
  product_id  = character(),
  url         = character()
)

for (cc in countries) {
  
  if (cc %in% alt_product_countries) {
    
    product_id = "00522047"
    
    url = str_c(
      "https://www.ikea.com/",
      cc,
      "/p/billy-bookcase-white-00522047/"
    )
    
  } else {
    
    product_id = "00263850"
    
    url = str_c(
      "https://www.ikea.com/",
      cc,
      "/p/billy-buecherregal-weiss-00263850/"
    )
  }
  
  cat("Scraping:", url, "\n")
  
  page = tryCatch(
    read_html(url),
    error = function(e) {
      message("  Failed: ", e$message)
      NULL
    }
  )
  
  if (is.null(page)) next
  
  result = tryCatch(
    extract_price_jsonld(page),
    error = function(e) {
      message("  Parse error for ", cc, ": ", e$message)
      NULL
    }
  )
  
  if (!is.null(result)) {
    
    dt = rbind(
      dt,
      data.table(
        country    = cc,
        price      = result$price,
        currency   = result$currency,
        product_id = product_id,
        url        = url
      )
    )
    
    cat("  ->", result$price, result$currency, "\n")
  }
  
  # Polite delay between requests.
  # This avoids sending many requests too quickly.
  
  Sys.sleep(1 + runif(1, 0, 2))
}

print(dt)


# 7 - save results ----

fwrite(dt, "temp/ikea_billy_prices_jsonld.csv")


# 8 - cleanup ----

rm(page, result)
gc()