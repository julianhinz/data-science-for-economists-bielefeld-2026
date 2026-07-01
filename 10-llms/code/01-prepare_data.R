###
# 01 - Prepare Data for NLP Analysis
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(lubridate)
p_load(ggplot2)

# 0 - settings ----

dir.create("input", showWarnings = FALSE, recursive = TRUE)
dir.create("temp", showWarnings = FALSE, recursive = TRUE)

# 1 - download data ----

# download the ECB speeches dataset
download.file(
  url = "https://www.ecb.europa.eu/press/key/shared/data/all_ECB_speeches.csv",
  destfile = "input/all_ECB_speeches.csv",
  method = "curl"
)

# read the dataset
ecb_speeches <- fread("input/all_ECB_speeches.csv", sep = "|", quote = "")
str(ecb_speeches)

# some basic stats
ecb_speeches[, .N, by = speakers][order(-N)]

ecb_speeches[, year := year(date)]
ecb_speeches[, .N, by = year][order(-N)]

# distribution of length of speeches
ecb_speeches[, length := nchar(contents)]

# count words
ecb_speeches[, word_count := sapply(strsplit(contents, "\\s+"), length)]

ggplot(ecb_speeches, aes(x = word_count)) +
  geom_histogram(bins = 100) +
  labs(title = "Distribution of Length of ECB Speeches",
       x = "Length (characters)",
       y = "Frequency") +
  theme_minimal()

ecb_speeches[word_count == 0]

# restrict the dataset to speeches with more than 100 and less than 10000 words
ecb_speeches <- ecb_speeches[word_count > 100 & word_count < 10000]

# save the cleaned dataset
fwrite(ecb_speeches, "temp/ecb_speeches_cleaned.csv")
