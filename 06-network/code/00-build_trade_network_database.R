###
# 00 - Build Clean BACI Trade Data
# Data Science for Economists - Networks application
# March 2026
###

# Common file for everyone: run/source this file to generate the same clean
# BACI trade dataset.
#
# Important:
# This file only prepares the data.
# Network construction is deliberately left for the student lab.
#
# Required raw files, expected in 06-network/data/:
#   - BACI_HS07_Y2017_V202001_p1.csv.gz
#   - BACI_HS07_Y2017_V202001_p2.csv.gz
#   - country_codes_cepii_V2021.csv.gz
#
# Output:
#   - 06-network/data/clean_baci_y2017.rds
#   - optionally: 06-network/data/clean_baci_y2017.csv
#
# Data: CEPII BACI
# https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(
  data.table,
  tidyverse,
  here
)

# -----------------------------------------------------------------------------
# Helper: locate BACI files
# -----------------------------------------------------------------------------

find_baci_files <- function(data_dir, year) {
  pattern <- paste0("BACI_HS.*Y", year, ".*\\.csv(\\.gz)?$")

  files <- list.files(
    path = data_dir,
    pattern = pattern,
    full.names = TRUE
  )

  files <- sort(files)

  if (length(files) == 0) {
    stop(
      "Could not find BACI files for year ", year, " in ", data_dir, ".\n",
      "Expected files like: BACI_HS07_Y2017_V202001_p1.csv.gz",
      call. = FALSE
    )
  }

  files
}

# -----------------------------------------------------------------------------
# Helper: locate CEPII country-code file
# -----------------------------------------------------------------------------

find_country_file <- function(data_dir) {
  files <- list.files(
    path = data_dir,
    pattern = "country_codes_cepii.*\\.csv(\\.gz)?$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop(
      "Could not find CEPII country-code file in ", data_dir, ".\n",
      "Expected file like: country_codes_cepii_V2021.csv.gz",
      call. = FALSE
    )
  }

  files[1]
}

# -----------------------------------------------------------------------------
# Main function: build clean BACI data
# -----------------------------------------------------------------------------

build_clean_baci <- function(data_dir = here("06-network", "data"),
                             output_dir = here("06-network", "data"),
                             year = 2017,
                             force_rebuild = FALSE,
                             save_csv = FALSE) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  rds_path <- file.path(output_dir, paste0("clean_baci_y", year, ".rds"))
  csv_path <- file.path(output_dir, paste0("clean_baci_y", year, ".csv"))

  if (file.exists(rds_path) && !force_rebuild) {
    message("Loading existing clean BACI file: ", rds_path)
    return(readRDS(rds_path))
  }

  # ---------------------------------------------------------------------------
  # 1. Read BACI raw files
  # ---------------------------------------------------------------------------

  baci_files <- find_baci_files(data_dir = data_dir, year = year)

  message("Reading BACI files:")
  message("  - ", paste(baci_files, collapse = "\n  - "))

  baci_raw <- rbindlist(
    lapply(baci_files, fread),
    use.names = TRUE,
    fill = TRUE
  )

  required_baci_cols <- c("t", "i", "j", "k", "v")

  if (!all(required_baci_cols %in% names(baci_raw))) {
    stop(
      "BACI files must contain columns: ",
      paste(required_baci_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Read CEPII country-code conversion file
  # ---------------------------------------------------------------------------

  country_file <- find_country_file(data_dir = data_dir)

  message("Reading country codes:")
  message("  - ", country_file)

  country_codes <- fread(country_file)

  required_country_cols <- c("country_code", "iso_3digit_alpha")

  if (!all(required_country_cols %in% names(country_codes))) {
    stop(
      "Country-code file must contain columns: ",
      paste(required_country_cols, collapse = ", "),
      call. = FALSE
    )
  }

  conversion <- country_codes |>
    select(
      country_code,
      iso3 = iso_3digit_alpha
    ) |>
    filter(
      !is.na(iso3),
      iso3 != ""
    )

  # ---------------------------------------------------------------------------
  # 3. Clean BACI and attach ISO3 exporter/importer codes
  # ---------------------------------------------------------------------------

  clean_baci <- baci_raw |>
    transmute(
      year = as.integer(t),
      exporter_code = i,
      importer_code = j,
      product_code = str_pad(as.character(k), width = 6, pad = "0"),
      trade_value = as.numeric(v)
    ) |>
    filter(
      year == !!year,
      !is.na(trade_value),
      trade_value > 0,
      !str_detect(product_code, "^(98|99)")
    ) |>
    left_join(conversion, by = c("exporter_code" = "country_code")) |>
    rename(exp = iso3) |>
    left_join(conversion, by = c("importer_code" = "country_code")) |>
    rename(imp = iso3) |>
    filter(
      !is.na(exp),
      !is.na(imp),
      exp != imp
    ) |>
    select(
      year,
      exp,
      imp,
      product_code,
      trade_value
    ) |>
    arrange(exp, imp, product_code)

  # ---------------------------------------------------------------------------
  # 4. Save clean data
  # ---------------------------------------------------------------------------

  saveRDS(clean_baci, rds_path)

  if (save_csv) {
    write_csv(clean_baci, csv_path)
  }

  message("Saved clean BACI data to:")
  message("  - ", rds_path)

  if (save_csv) {
    message("  - ", csv_path)
  }

  message("Rows: ", nrow(clean_baci))
  message("Exporters: ", n_distinct(clean_baci$exp))
  message("Importers: ", n_distinct(clean_baci$imp))
  message("Products: ", n_distinct(clean_baci$product_code))

  return(clean_baci)
}

# -----------------------------------------------------------------------------
# Backward-compatible alias
# -----------------------------------------------------------------------------
# This lets old lab files still run if they call build_trade_network_database().
# It returns clean BACI data, not a network object.

build_trade_network_database <- build_clean_baci

# -----------------------------------------------------------------------------
# Run directly
# -----------------------------------------------------------------------------

if (sys.nframe() == 0) {
  clean_baci <- build_clean_baci(force_rebuild = TRUE)
  print(glimpse(clean_baci))
}