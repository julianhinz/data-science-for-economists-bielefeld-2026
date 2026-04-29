###
# 02 - DuckDB and Parquet Workflows
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(duckdb)
p_load(DBI)
p_load(arrow)
p_load(duckplyr)
p_load(ggplot2)

# 0 - settings ----

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("temp", showWarnings = FALSE, recursive = TRUE)

# 1 - CSV to Parquet conversion ----

# list all CSV files
files = list.files("temp/imports_usa", pattern = "\\.csv$", full.names = TRUE)

# read all CSVs and write as a single Parquet file
data = rbindlist(lapply(files, fread))
arrow::write_parquet(data, "temp/imports_usa.parquet")

rm(data)
gc()

# 2 - reading Parquet vs CSV ----

# read only the columns you need -- much faster than CSV
system.time(
  data_parquet <- arrow::read_parquet(
    "temp/imports_usa.parquet",
    col_select = c("VAFODO", "RAZSOCIAL")
  )
)
setDT(data_parquet)

# compare to reading all columns from CSV
system.time(
  data_csv <- fread(files[1])
)

rm(data_parquet, data_csv)
gc()

# 3 - DuckDB: connect and load ----

con = dbConnect(duckdb::duckdb(), dbdir = "temp/imports_colombia.db")

# DuckDB can read CSV files directly into a table
dbExecute(con, "DROP TABLE IF EXISTS imports")

# load all CSV files with auto-detection
for (f in files) {
  cat("Loading:", basename(f), "\n")

  delim = if (grepl("Diciembre|Enero|Febrero|M10418|Marzo|Mayo|Octubre|Septiembre", f)) {
    ";"
  } else {
    ","
  }

  tryCatch({
    if (!dbExistsTable(con, "imports")) {
      dbExecute(con, paste0(
        "CREATE TABLE imports AS SELECT * FROM read_csv_auto('", f,
        "', delim='", delim, "', quote='\"', escape='\"',",
        " strict_mode=false, ignore_errors=true, null_padding=true, sample_size=10000)"
      ))
    } else {
      dbExecute(con, paste0(
        "INSERT INTO imports SELECT * FROM read_csv_auto('", f,
        "', delim='", delim, "', quote='\"', escape='\"',",
        " strict_mode=false, ignore_errors=true, null_padding=true, sample_size=10000)"
      ))
    }
  }, error = function(e) message("Failed: ", basename(f), " -- ", e$message))
}

# 4 - DuckDB: SQL queries ----

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) AS n FROM imports")

# aggregate directly in DuckDB (no R memory needed)
top_firms = dbGetQuery(con, "
  SELECT RAZSOCIAL, SUM(CAST(VAFODO AS DOUBLE)) AS total_value
  FROM imports
  WHERE VAFODO IS NOT NULL
  GROUP BY RAZSOCIAL
  ORDER BY total_value DESC
  LIMIT 20
")
setDT(top_firms)
print(top_firms)

# 5 - DuckDB queries on Parquet files (no table needed) ----

# DuckDB can query Parquet directly without loading into a table
parquet_result = dbGetQuery(con, "
  SELECT COUNT(*) AS n,
         AVG(CAST(VAFODO AS DOUBLE)) AS mean_value,
         MEDIAN(CAST(VAFODO AS DOUBLE)) AS median_value
  FROM read_parquet('temp/imports_usa.parquet')
  WHERE VAFODO IS NOT NULL
")
print(parquet_result)

# 6 - duckplyr: dplyr syntax with DuckDB speed ----

# read Parquet into a duckplyr-backed data frame
df = arrow::read_parquet("temp/imports_usa.parquet")

# use familiar dplyr verbs -- DuckDB runs under the hood
result = df %>%
  dplyr::mutate(VAFODO = as.numeric(VAFODO)) %>%
  dplyr::filter(!is.na(VAFODO), VAFODO > 0) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_value = mean(VAFODO, na.rm = TRUE),
    median_value = median(VAFODO, na.rm = TRUE)
  )
print(result)

rm(df, result)

# 7 - benchmarking: CSV vs Parquet read times ----

system.time(fread(files[1]))
system.time(arrow::read_parquet("temp/imports_usa.parquet"))

# 8 - cleanup ----

dbDisconnect(con, shutdown = TRUE)
gc()
