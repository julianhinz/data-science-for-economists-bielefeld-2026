###
# 02 - DuckDB and Parquet Workflows
# Colombian Import Data - IMPO 2018
# 260226
#
# Goal:
# 1. Start from many Colombian import CSV files
# 2. Clean the text encoding
# 3. Convert the CSV files into one Parquet file
# 4. Compare CSV vs Parquet reading
# 5. Query Parquet directly with DuckDB
###

if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(data.table)
p_load(dplyr)
p_load(arrow)
p_load(duckdb)
p_load(DBI)

# 0 - settings ----

dir.create("temp", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

csv_dir      <- "temp/Impo_2018"
parquet_file <- "temp/colombia_imports_2018.parquet"
duckdb_file  <- "temp/colombia_imports.duckdb"

files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
files <- sort(files)

cat("Working directory:", getwd(), "\n")
cat("CSV directory:", csv_dir, "\n")
cat("Number of CSV files found:", length(files), "\n")

if (length(files) == 0) {
  cat("\nNo CSV files found. Here are CSV files I can find under the current project:\n")
  print(list.files(".", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE))
  stop("Fix csv_dir before continuing.")
}

# Columns we keep for the class example
keep_cols <- c(
  "FECH",     # date / period
  "ADUA",     # customs office
  "PAISGEN",  # country of origin
  "PAISPRO",  # country of provenance
  "PAISCOM",  # country of purchase
  "VAFODO",   # FOB value in USD
  "FLETE",    # freight
  "VACID",    # CIF value in USD
  "VACIP",    # CIF value in Colombian pesos
  "NIT",      # importer tax ID
  "DIGV",     # verification digit
  "RZIMPO"    # importer name
)

# 1 - helper functions ----

# The raw CSV files contain Latin-1 / Windows-1252 text.
# Example: CORPORACI\xD3N should become CORPORACIÓN.
to_utf8 <- function(x) {
  iconv(x, from = "WINDOWS-1252", to = "UTF-8", sub = "")
}

# Safely quote file paths inside SQL strings
qstr <- function(con, x) {
  as.character(DBI::dbQuoteString(con, x))
}

# Guess whether a CSV file uses comma or semicolon as separator
guess_delim <- function(file) {
  header <- readLines(file, n = 1, warn = FALSE)
  
  n_semicolon <- lengths(regmatches(header, gregexpr(";", header, fixed = TRUE)))
  n_comma     <- lengths(regmatches(header, gregexpr(",", header, fixed = TRUE)))
  
  if (n_semicolon > n_comma) ";" else ","
}

# 2 - read one CSV file ----

read_import_csv <- function(file) {
  
  delim <- guess_delim(file)
  
  dt <- fread(
    file,
    sep = delim,
    fill = TRUE,
    colClasses = "character",
    encoding = "Latin-1",
    showProgress = FALSE
  )
  
  # Clean column names
  names(dt) <- to_utf8(names(dt))
  names(dt) <- trimws(sub("^\ufeff", "", names(dt)))
  
  # If the file was accidentally read as one giant column, stop early
  if (ncol(dt) == 1 && grepl(";|,", names(dt)[1])) {
    stop("File appears to have been read as one column. Check delimiter: ", basename(file))
  }
  
  # Convert all character columns to valid UTF-8
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  dt[, (char_cols) := lapply(.SD, to_utf8), .SDcols = char_cols]
  
  # If some expected columns are missing, create them as NA
  missing_cols <- setdiff(keep_cols, names(dt))
  
  for (x in missing_cols) {
    dt[, (x) := NA_character_]
  }
  
  # Keep only the columns we need, in a fixed order
  dt <- dt[, ..keep_cols]
  
  dt
}

# 3 - CSV to Parquet conversion ----

cat("\nReading Colombian import CSV files...\n")

imports <- rbindlist(
  lapply(files, function(f) {
    cat("Reading:", basename(f), "\n")
    read_import_csv(f)
  }),
  use.names = TRUE,
  fill = TRUE
)

cat("\nImported data dimensions:\n")
print(dim(imports))

cat("\nImported data columns:\n")
print(names(imports))

if (nrow(imports) == 0) {
  stop("No rows were read from the CSV files. Parquet file will not be written.")
}

if (ncol(imports) == 0) {
  stop("No columns were read from the CSV files. Parquet file will not be written.")
}

if (!all(keep_cols %in% names(imports))) {
  stop("Some expected columns are missing from the imported data.")
}

cat("\nWriting Parquet file...\n")

# Delete old Parquet file first, in case an earlier run wrote a broken file
unlink(parquet_file)

arrow::write_parquet(imports, parquet_file)

cat("\nParquet columns:\n")
print(names(arrow::open_dataset(parquet_file)))

rm(imports)
gc()

# 4 - compare CSV and Parquet reading ----

cat("\nCSV read time, one file:\n")

system.time(
  one_csv <- read_import_csv(files[1])
)

cat("\nParquet read time, selected columns only:\n")

system.time(
  parquet_small <- arrow::read_parquet(
    parquet_file,
    col_select = c("VAFODO", "RZIMPO")
  )
)

rm(one_csv, parquet_small)
gc()

# 5 - query Parquet with DuckDB SQL ----
# con <- dbConnect(...)     # open DuckDB
# dbGetQuery(con, "...")    # query DuckDB
# dbDisconnect(con)         # close DuckDB

con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_file)

# Colombian numeric values often use comma decimals:
# Example: "10588,49"
#
# DuckDB expects decimal points:
# Example: "10588.49"
#
# This expression converts VAFODO safely to numeric.
# It also removes possible thousands separators written as dots.
value_expr <- "TRY_CAST(REPLACE(REPLACE(VAFODO, '.', ''), ',', '.') AS DOUBLE)"

cat("\nDuckDB query on Parquet file:\n")

summary_stats <- dbGetQuery(con, paste0("
  SELECT
    COUNT(*) AS n,
    AVG(", value_expr, ") AS mean_fob_value_usd,
    MEDIAN(", value_expr, ") AS median_fob_value_usd
  FROM read_parquet(", qstr(con, parquet_file), ")
  WHERE ", value_expr, " IS NOT NULL
"))

print(summary_stats)

# 6 - top Colombian importers by FOB value ----

cat("\nTop Colombian importers by FOB value:\n")

top_importers <- dbGetQuery(con, paste0("
  SELECT
    RZIMPO AS importer_name,
    SUM(", value_expr, ") AS total_fob_value_usd
  FROM read_parquet(", qstr(con, parquet_file), ")
  WHERE ", value_expr, " IS NOT NULL
  GROUP BY RZIMPO
  ORDER BY total_fob_value_usd DESC
  LIMIT 20
"))

print(top_importers)

# 7 - inspect a few rows ----

cat("\nExample rows:\n")

example_rows <- dbGetQuery(con, paste0("
  SELECT
    FECH,
    NIT,
    DIGV,
    RZIMPO,
    VAFODO,
    ", value_expr, " AS VAFODO_numeric
  FROM read_parquet(", qstr(con, parquet_file), ")
  WHERE VAFODO IS NOT NULL
  LIMIT 10
"))

print(example_rows)

# 8 - cleanup ----

dbDisconnect(con, shutdown = TRUE)
gc()