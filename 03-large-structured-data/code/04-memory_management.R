###
# 04 - Memory Management in R
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)

# 0 - settings ----

dir.create("temp", showWarnings = FALSE, recursive = TRUE)

# 1 - object sizes ----

# different types have different memory footprints
chr_vect = c("12", "11", "33")
dbl_vect = c(12, 11, 33)
int_vect = c(12L, 11L, 33L)

object.size(chr_vect) # character
object.size(dbl_vect) # double
object.size(int_vect) # integer

# 2 - factor vs character encoding ----

# for small vectors, factors have overhead
gender_small = c("female", "male", "other")
object.size(gender_small)
object.size(as.factor(gender_small))

# for repeated values, factors save memory
gender_large = rep(c("female", "male", "other"), 1000)
object.size(gender_large)             # character: many copies
object.size(as.factor(gender_large))  # factor: integer + levels

rm(chr_vect, dbl_vect, int_vect, gender_small, gender_large)

# 3 - memory profile ----

# see what types consume the most memory in the current session
memory.profile()

# 4 - rm() and gc() ----

# create a large object
big_data = data.table(
  x = rnorm(1e6),
  y = rnorm(1e6),
  z = sample(letters, 1e6, replace = TRUE)
)
object.size(big_data)

# check memory before cleanup
gc()

# remove and reclaim
rm(big_data)
gc()  # forces garbage collection, reclaims freed memory

# 5 - read only needed columns ----

# when working with large files, read only the columns you need
# fread's select argument avoids loading unnecessary columns

# example: if you only need two columns from a large file
# dt = fread("temp/large_file.csv", select = c("firm_id", "export_value"))

# arrow::read_parquet also supports column selection
# dt = arrow::read_parquet("temp/large_file.parquet",
#                          col_select = c("firm_id", "export_value"))

# 6 - remove redundant columns ----

dt = data.table(
  id = 1:1000,
  name = paste0("firm_", 1:1000),
  value = rnorm(1000),
  temp_calc = rnorm(1000),  # intermediate column, no longer needed
  debug_flag = TRUE          # leftover from debugging
)

object.size(dt) # before

# remove columns by reference (no copy!)
dt[, c("temp_calc", "debug_flag") := NULL]

object.size(dt) # after
rm(dt)

# 7 - chunk-and-pull pattern ----

# when data is too large to load entirely, process in chunks:
#
# 1. Split: break file into manageable pieces (e.g. by year, in shell)
# 2. Process: write a function that reads and processes one chunk
# 3. Combine: apply to all chunks and bind results

process_chunk = function(file) {
  chunk = fread(file)
  result = chunk[, .(n = .N, mean_val = mean(as.numeric(VAFODO), na.rm = TRUE))]
  result[, file := basename(file)]
  return(result)
}

# apply to all chunks
files = list.files("temp/imports_usa", pattern = "\\.csv$", full.names = TRUE)
if (length(files) > 0) {
  results = rbindlist(lapply(files, function(f) {
    tryCatch(process_chunk(f), error = function(e) NULL)
  }))
  print(results)
  rm(results)
}

# 8 - monitoring memory usage ----

# total memory used by R session
gc()  # returns a summary table

# pryr::mem_used() gives a single number (requires pryr package)
# p_load(pryr)
# mem_used()

# lobstr::obj_size() is more accurate than object.size()
# p_load(lobstr)
# obj_size(my_data)

# 9 - cleanup ----

gc()
