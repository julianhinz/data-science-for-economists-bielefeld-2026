###
# Text as Data: Preferential Trade Agreements
# Data Science for Economists - afternoon application
# March 2026
###

# This script uses the common database-builder file:
#   00-build_text_database.R
#
# Students generate the same clean dataset themselves, but the XML parsing lives
# in the common 00 file. The lab below focuses on text-as-data choices:
# tokenisation, tf-idf, dictionaries, validation, and similarity.

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse)
p_load(tidytext)
p_load(textstem)
p_load(ggplot2)
p_load(quanteda)
p_load(quanteda.textstats)
p_load(here)

# Build or load the clean TOTA text database.
# The first run downloads and processes the raw XML files.
source(here("05-text-as-data", "code", "00-build_text_database.R"))

dir.create(here("05-text-as-data", "output"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("05-text-as-data", "output", "figures"), showWarnings = FALSE, recursive = TRUE)

tota_texts <- build_tota_text_database(force_rebuild = FALSE)

# =============================================================================
# STUDENT LAB VERSION
# =============================================================================

# How to use this file:
# - Work through the exercises in order.
# - Replace TODO(...) with your own code.
# - The XML helper functions live in 00-build_text_database.R.
# - You only need to work with the clean one-row-per-agreement dataset below.

TODO <- function(...) {
  stop("Replace TODO(...) with your own code before running this exercise.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# Exercise 1. Language detection — keep English-only treaties
# -----------------------------------------------------------------------------

p_load(cld2)

# Detect language from the first 2000 characters of each treaty.
tota_texts <- tota_texts |>
  mutate(lang = cld2::detect_language(str_sub(text, 1, 2000)))

cat("Treaties before language filter:", nrow(tota_texts), "\n")

# 1a. Which non-English languages appear, and how often?
tota_texts |>
  filter(TODO("condition: lang is not 'en' or is NA")) |>
  count(lang, sort = TRUE) |>
  print()

# 1b. Show 10 example non-English agreements.
tota_texts |>
  filter(TODO("same condition as above")) |>
  select(agreement_id, parties, year, lang) |>
  slice_head(n = 10) |>
  print()

# 1c. Keep only English treaties.
tota_texts <- tota_texts |>
  filter(TODO("keep rows where lang == 'en'"))

cat("Treaties after keeping English only:", nrow(tota_texts), "\n")

# -----------------------------------------------------------------------------
# Exercise 2. Inspect the corpus
# -----------------------------------------------------------------------------

# One row = one trade agreement. The column `text` contains the full treaty text.
glimpse(tota_texts)

# 2a. Complete the summary.
corpus_summary <- tota_texts |>
  summarise(
    n_agreements = TODO("count agreements"),
    first_year   = TODO("earliest year"),
    last_year    = TODO("latest year"),
    mean_words   = TODO("mean n_words"),
    median_words = TODO("median n_words")
  )

print(corpus_summary)

# 2b. Which agreement is longest?
tota_texts |>
  arrange(TODO("sort by n_words, descending")) |>
  select(agreement_id, year, parties, n_words) |>
  slice_head(n = 5)

# 2c. Treaty length as a simple proxy for agreement depth.
# Complete the plot: average treaty length by year.
p_depth <- tota_texts |>
  filter(!is.na(year)) |>
  group_by(TODO("year")) |>
  summarise(mean_words = TODO("mean n_words"), .groups = "drop") |>
  ggplot(aes(x = TODO("year"), y = TODO("mean_words"))) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(
    title = "Trade agreements became longer over time",
    subtitle = "Word count is a simple proxy for agreement depth",
    x = "Year signed", y = "Mean number of words"
  ) +
  theme_minimal()

print(p_depth)

# -----------------------------------------------------------------------------
# Exercise 3. Regular expressions: warm-up
# -----------------------------------------------------------------------------

example_text <- c(
  "Tariffs shall be reduced by 10 percent.",
  "The parties agree on sanitary and phytosanitary measures.",
  "Most-favoured-nation treatment applies.",
  "Article 12: Safeguards and countervailing duties."
)

# 3a. Detect lines that contain tariff-related language (tariff, duty, or duties).
str_detect(str_to_lower(example_text), TODO("regex pattern"))

# 3b. Extract numbers from the text.
str_extract(example_text, TODO("regex pattern for one or more digits"))

# 3c. Replace "Most-favoured-nation" with "MFN".
str_replace_all(example_text, TODO("pattern"), TODO("replacement"))

# 3d. Extract words starting with p or P.
words <- c("Policy", "trade", "production", "data", "protection")
words[str_detect(words, TODO("regex pattern"))]

# -----------------------------------------------------------------------------
# Exercise 4. Tokenise, remove stop words, lemmatise
# -----------------------------------------------------------------------------

# Step 1: raw tokenisation — one token per row, no cleaning yet.
tota_raw <- tota_texts |>
  select(agreement_id, parties, year, text) |>
  unnest_tokens(output = TODO("name the new column 'word'"), input = TODO("text column"))

cat("Raw tokens:", nrow(tota_raw), "| Unique types:", n_distinct(tota_raw$word), "\n")

# The most common tokens are stopwords — linguistically uninformative.
tota_raw |> count(word, sort = TRUE) |> slice_head(n = 10)

# Step 2: remove English stopwords.
tota_nostop <- tota_raw |>
  anti_join(TODO("stop-word table from tidytext"), by = "word")

cat("After stopword removal:", nrow(tota_nostop), "tokens\n")

# Numbers are now prominent — also not useful for content analysis.
tota_nostop |> count(word, sort = TRUE) |> slice_head(n = 10)

# Step 3: remove pure numbers.
tota_nonums <- tota_nostop |>
  filter(!str_detect(word, TODO("regex matching pure digit strings")))

cat("After removing numbers:", nrow(tota_nonums), "| Unique types:", n_distinct(tota_nonums$word), "\n")

# Step 4: lemmatise — collapse inflected forms to their base form.
# "parties" -> "party", "agreements" -> "agreement", "applied" -> "apply"
tota_words <- tota_nonums |>
  mutate(word = lemmatize_words(word))

# Show some examples where lemmatisation changed the token.
tota_nonums |>
  slice_sample(n = 2000) |>
  mutate(lemma = lemmatize_words(word)) |>
  filter(word != lemma) |>
  distinct(word, lemma) |>
  slice_head(n = 12) |>
  print()

# -----------------------------------------------------------------------------
# Exercise 4b. Collocations: capturing meaningful multi-word expressions
# -----------------------------------------------------------------------------

# Build a quanteda token object applying the same preprocessing steps.
tota_toks <- corpus(tota_texts, text_field = "text") |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en")) |>
  tokens_select(min_nchar = 2)

tota_toks <- as.tokens(lapply(as.list(tota_toks), lemmatize_words))

# 4b-i. Detect statistically significant bigrams and trigrams.
# min_count filters rare pairs; the z-score measures statistical significance.
collocations <- textstat_collocations(tota_toks,
                                      size     = TODO("2:3 for bigrams and trigrams"),
                                      min_count = TODO("try 50"))
print(head(collocations, 20))

# 4b-ii. Compound significant collocations back into the token stream so
# "most_favoured_nation" or "intellectual_property" travel as single tokens.
tota_toks_comp <- tokens_compound(tota_toks,
                                  pattern = collocations[TODO("condition: z > 3"), ])

# What are the most frequent tokens after compounding?
dfm(tota_toks_comp) |>
  topfeatures(20) |>
  print()

# Convert back to tidy format so all downstream steps use the compounded vocabulary.
tota_words <- dfm(tota_toks_comp) |>
  tidy() |>
  rename(agreement_id = document, word = term) |>
  left_join(tota_texts |> select(agreement_id, parties, year),
            by = "agreement_id") |>
  uncount(count)

# 4b-iii. What are the 30 most frequent words after the full pipeline?
top_words <- tota_words |>
  count(word, sort = TRUE) |>
  slice_head(n = 30)

print(top_words)

p_load(wordcloud2)
wordcloud2(top_words, color = 'random-light', backgroundColor = "#152238")

p_top_words <- top_words |>
  mutate(word = fct_reorder(word, n)) |>
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most frequent non-stop words", x = NULL, y = "Count") +
  theme_minimal()

print(p_top_words)
ggsave(here("05-text-as-data", "output", "figures", "top_words.png"),
       p_top_words, width = 8, height = 6, dpi = 300)

# Interpretation question:
# Which high-frequency words are substantively meaningful? Which are generic
# legal words that might be less useful? Do any multi-word expressions appear?

# -----------------------------------------------------------------------------
# Exercise 5. TF-IDF: distinctive words by treaty
# -----------------------------------------------------------------------------

# 5a. Compute tf-idf for each word in each agreement.
treaty_tfidf <- tota_words |>
  count(TODO("agreement_id"), TODO("parties"), TODO("year"), TODO("word"), sort = TRUE) |>
  bind_tf_idf(term = TODO("word"), document = TODO("agreement_id"), n = TODO("count variable")) |>
  arrange(desc(tf_idf))

# 5b. Show the 8 most distinctive words for each treaty.
tfidf_top_by_treaty <- treaty_tfidf |>
  group_by(agreement_id, parties, year) |>
  slice_max(TODO("tf_idf"), n = 8, with_ties = FALSE) |>
  ungroup()

print(tfidf_top_by_treaty |> select(agreement_id, year, parties, word, tf_idf) |> head(40))

# 5c. Pick one treaty and interpret its most distinctive words.
one_treaty_id <- treaty_tfidf$agreement_id[1]

treaty_tfidf |>
  filter(agreement_id == one_treaty_id) |>
  arrange(desc(tf_idf)) |>
  select(parties, year, word, n, tf, idf, tf_idf) |>
  slice_head(n = 15)

# Interpretation question:
# Do the top tf-idf terms tell you something specific about this agreement?

# -----------------------------------------------------------------------------
# Exercise 6. Build your own dictionary
# -----------------------------------------------------------------------------

# Below is a starter dictionary. Add at least three terms to one category.
# Think carefully: dictionary choices are research choices.

trade_dictionary <- list(
  agriculture    = c("agric", "farm", "crop"),
  services       = c("service", "financial", "telecommunication"),
  protection     = c("safeguard", "anti-dumping", "quota"),
  liberalisation = c("liberal", "free trade", "market access")
)

# 6a. Add terms to the dictionary above.
# Example: trade_dictionary$agriculture <- c(trade_dictionary$agriculture, "livestock")

# 6b. Complete this helper function.
count_dictionary_category <- function(text, patterns) {
  pattern <- paste(patterns, collapse = "|")
  str_count(str_to_lower(text), regex(TODO("pattern variable"), ignore_case = TRUE))
}

# 6c. Count dictionary matches by agreement.
dictionary_counts <- tota_texts |>
  mutate(
    agriculture    = map_int(text, count_dictionary_category, patterns = TODO("agriculture patterns")),
    services       = map_int(text, count_dictionary_category, patterns = TODO("services patterns")),
    protection     = map_int(text, count_dictionary_category, patterns = TODO("protection patterns")),
    liberalisation = map_int(text, count_dictionary_category, patterns = TODO("liberalisation patterns")),
    agriculture_per_1000    = 1000 * agriculture    / n_words,
    services_per_1000       = 1000 * services       / n_words,
    protection_per_1000     = 1000 * protection     / n_words,
    liberalisation_per_1000 = 1000 * liberalisation / n_words
  )

# 6d. Which agreements score highest on agriculture?
dictionary_counts |>
  arrange(desc(TODO("agriculture_per_1000"))) |>
  select(agreement_id, year, parties, agriculture, agriculture_per_1000) |>
  head(10)

# 6e. Which agreements score highest on services?
dictionary_counts |>
  arrange(desc(TODO("services_per_1000"))) |>
  select(agreement_id, year, parties, services, services_per_1000) |>
  head(10)

# -----------------------------------------------------------------------------
# Exercise 7. Validate the dictionary measure
# -----------------------------------------------------------------------------

# Text measures are constructed measures. We now create a small sample for
# manual validation of the agriculture dictionary.

set.seed(42)

high_agriculture_sample <- dictionary_counts |>
  arrange(desc(agriculture_per_1000)) |>
  slice_head(n = 5) |>
  mutate(machine_agriculture = TRUE, sample_type = "high agriculture score")

zero_agriculture_sample <- dictionary_counts |>
  filter(agriculture == 0) |>
  slice_sample(n = 5) |>
  mutate(machine_agriculture = FALSE, sample_type = "zero agriculture score")

validation_sample <- bind_rows(high_agriculture_sample, zero_agriculture_sample) |>
  mutate(
    excerpt           = str_squish(str_sub(text, 1, 900)),
    human_agriculture = NA,
    correct           = NA
  ) |>
  select(agreement_id, year, parties, sample_type, machine_agriculture,
         agriculture, agriculture_per_1000, excerpt, human_agriculture, correct)

write_csv(validation_sample,
          here("05-text-as-data", "output", "agriculture_validation_template.csv"))

# 7a. Open output/agriculture_validation_template.csv.
# 7b. Read each excerpt and fill in human_agriculture as TRUE/FALSE.
# 7c. Save the completed file as output/agriculture_validation_coded.csv.
# 7d. Then complete the precision/recall/accuracy formulas below.

if (file.exists(here("05-text-as-data", "output", "agriculture_validation_coded.csv"))) {
  validation_coded <- read_csv(
    here("05-text-as-data", "output", "agriculture_validation_coded.csv"),
    show_col_types = FALSE
  ) |>
    mutate(
      human_agriculture = as.logical(human_agriculture),
      machine_agriculture = as.logical(machine_agriculture)
    )

  validation_summary <- validation_coded |>
    summarise(
      TP        = sum( machine_agriculture &  human_agriculture, na.rm = TRUE),
      FP        = sum( machine_agriculture & !human_agriculture, na.rm = TRUE),
      FN        = sum(!machine_agriculture &  human_agriculture, na.rm = TRUE),
      TN        = sum(!machine_agriculture & !human_agriculture, na.rm = TRUE),
      precision = TODO("TP / (TP + FP)"),
      recall    = TODO("TP / (TP + FN)"),
      accuracy  = TODO("(TP + TN) / (TP + FP + FN + TN)")
    )

  print(validation_summary)
}

# Discussion question:
# Does the dictionary capture substantive agriculture provisions, or does it
# sometimes pick up generic legal language?

# -----------------------------------------------------------------------------
# Exercise 8. Chi-square comparison of treaty content
# -----------------------------------------------------------------------------

# Compare two treaties with a contingency table: treaty x word category.
# Here we use the two longest treaties as an example.

comparison_data <- dictionary_counts |>
  arrange(desc(n_words)) |>
  slice_head(n = 2) |>
  mutate(other_words = pmax(n_words - agriculture - services - protection - liberalisation, 0))

comparison_table <- comparison_data |>
  select(TODO("agriculture, services, protection, liberalisation, other_words")) |>
  as.matrix()

rownames(comparison_table) <- comparison_data$parties

print(comparison_table)
chisq.test(TODO("comparison_table"))

# Interpretation question:
# Are the two agreements distributed similarly across these content categories?

# -----------------------------------------------------------------------------
# Exercise 9. Document similarity with quanteda
# -----------------------------------------------------------------------------

# We now use quanteda to create a document-feature matrix and compute similarity.

corp <- corpus(tota_texts, text_field = "text", docid_field = "agreement_id")

# Multi-word expressions should be compounded before stop-word removal.
mwe <- phrase(c(
  "free trade", "tariff reduction", "market access",
  "most favoured nation", "most favored nation", "anti dumping",
  "countervailing duty"
))

toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_tolower() |>
  tokens_compound(pattern = TODO("mwe object"), concatenator = "_") |>
  tokens_remove(TODO("English stop words"))

dfmat       <- dfm(TODO("tokens object"))
dfmat_tfidf <- dfm_tfidf(TODO("dfm object"))

# 9a. Pick a target agreement (try different indices to find an interesting one).
target_id <- docnames(dfmat_tfidf)[1]

# 9b. Compute cosine similarity between the target and all agreements.
sim_to_target <- textstat_simil(TODO("target row of dfmat_tfidf"),
                                TODO("full dfmat_tfidf"),
                                method = "cosine")

# 9c. List the 10 most similar agreements.
similar_treaties <- tibble(
  agreement_id = docnames(dfmat_tfidf),
  similarity   = as.numeric(sim_to_target)
) |>
  filter(agreement_id != target_id) |>
  left_join(tota_texts |> select(agreement_id, year, parties), by = "agreement_id") |>
  arrange(desc(similarity)) |>
  slice_head(n = 10)

print(tota_texts |> filter(agreement_id == target_id) |> select(agreement_id, year, parties))
print(similar_treaties)

# Interpretation question:
# Are similar treaties similar because of countries, period, legal template, or content?

# -----------------------------------------------------------------------------
# Exercise 10. Optional challenge: LDA topic modelling
# -----------------------------------------------------------------------------

# This is optional. Set run_lda <- TRUE only if you finish early.
run_lda <- FALSE

if (run_lda) {
  p_load(topicmodels)

  dfmat_lda <- dfm_trim(dfmat, min_termfreq = 5, min_docfreq = 3)
  dfmat_lda <- dfmat_lda[ntoken(dfmat_lda) > 0, ]
  dtm <- convert(dfmat_lda, to = "topicmodels")

  set.seed(42)
  lda_model <- LDA(dtm, k = 5, control = list(seed = 42))
  print(terms(lda_model, 10))
  print(round(posterior(lda_model)$topics[1:5, ], 3))
}

# Final reflection questions:
# 1. What did your dictionary measure well? Where did it fail?
# 2. Which step involved the most researcher judgement?
# 3. Do similar treaties look similar because of legal content, geography, year,
#    or document length?
# 4. When would you prefer a dictionary over a supervised classifier?
# 5. What would you need to turn this into a publishable text measure?
