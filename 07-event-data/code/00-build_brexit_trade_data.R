###
# 00 - Build Clean Brexit Trade Database
# Data Science for Economists
# 260226
###

# Common data-preparation file.
#
# Purpose:
# - Read the raw BACI monthly HS2 files.
# - Keep exports and the TOTAL product category.
# - Convert country names to ISO3 codes.
# - Save one clean bilateral monthly dataset:
#
#     data/trade_total_exports.rds
#
# This file is intentionally limited. All analysis choices -- UK exports,
# exports to EU27, comparator countries, treatment variables, DiD, event study,
# and gravity regressions -- are done in 01.

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table)
p_load(stringr)
p_load(lubridate)
p_load(countrycode)

# -----------------------------------------------------------------------------
# Helper: convert country names to ISO3 codes
# -----------------------------------------------------------------------------

to_iso3 <- function(x) {
  x_chr <- str_squish(as.character(x))

  out <- countrycode(
    x_chr,
    origin = "country.name",
    destination = "iso3c",
    warn = FALSE
  )

  # If a value is already an ISO3 code, keep it.
  already_iso3 <- is.na(out) & str_detect(x_chr, "^[A-Z]{3}$")
  out[already_iso3] <- x_chr[already_iso3]

  out
}

# -----------------------------------------------------------------------------
# Helper: read one BACI monthly file
# -----------------------------------------------------------------------------

read_one_baci_file <- function(file) {
  message("Reading: ", basename(file))

  dt <- fread(file, showProgress = FALSE)

  required_cols <- c(
    "Period", "Reporter", "Partner", "Commodity Code",
    "Trade Flow Code", "Trade Value (US$)"
  )

  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ", basename(file), ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Keep exports and TOTAL product only.
  dt <- dt[
    `Trade Flow Code` == 2 &
      as.character(`Commodity Code`) == "TOTAL"
  ]

  if (nrow(dt) == 0) {
    return(data.table(
      date = as.Date(character()),
      origin = character(),
      destination = character(),
      value = numeric()
    ))
  }

  clean <- dt[, .(
    date = ymd(str_c(Period, "01")),
    origin = to_iso3(Reporter),
    destination = to_iso3(Partner),
    value = as.numeric(`Trade Value (US$)`)
  )]

  # Convert YYYYMM to the last day of the month.
  clean[, date := ceiling_date(date, "month") - days(1)]

  clean <- clean[
    !is.na(date) &
      !is.na(origin) &
      !is.na(destination) &
      !is.na(value)
  ]

  # Collapse any duplicates within a file.
  clean <- clean[, .(
    value = sum(value, na.rm = TRUE)
  ), by = .(date, origin, destination)]

  clean[]
}

# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

build_clean_trade_database <- function(
    input_dir = "input/monthly_hs2",
    output_file = "data/trade_total_exports.rds",
    force_rebuild = FALSE
) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

  if (file.exists(output_file) && !force_rebuild) {
    message("Loading existing file: ", output_file)
    return(readRDS(output_file))
  }

  if (!dir.exists(input_dir)) {
    stop(
      "Input directory not found: ", input_dir, "\n",
      "Expected raw BACI monthly files in input/monthly_hs2/."
    )
  }

  files <- list.files(input_dir, full.names = TRUE)
  files <- files[str_detect(basename(files), "\\.csv(\\.gz)?$")]

  if (length(files) == 0) {
    stop("No .csv or .csv.gz files found in ", input_dir)
  }

  trade_total <- rbindlist(
    lapply(files, read_one_baci_file),
    use.names = TRUE,
    fill = TRUE
  )

  trade_total <- trade_total[, .(
    value = sum(value, na.rm = TRUE)
  ), by = .(date, origin, destination)]

  setorder(trade_total, date, origin, destination)

  saveRDS(trade_total, output_file)
  fwrite(trade_total, sub("\\.rds$", ".csv.gz", output_file))

  message("Saved clean database to: ", output_file)
  message("Rows: ", format(nrow(trade_total), big.mark = ","))

  trade_total
}

# If students run this file directly, build the database.
if (sys.nframe() == 0) {
  trade_total <- build_clean_trade_database(force_rebuild = FALSE)
}
