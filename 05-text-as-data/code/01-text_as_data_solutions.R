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
source(here("06-text-as-data", "code", "00-build_text_database.R"))

dir.create(here("06-text-as-data", "output"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("06-text-as-data", "output", "figures"), showWarnings = FALSE, recursive = TRUE)

tota_texts <- build_tota_text_database(force_rebuild = FALSE)

# -----------------------------------------------------------------------------
# 1b. Language detection — keep English-only treaties
# -----------------------------------------------------------------------------

p_load(cld2)

# Detect language from the first 2000 characters (fast; usually sufficient).
tota_texts <- tota_texts |>
  mutate(lang = cld2::detect_language(str_sub(text, 1, 2000)))

cat("Treaties before language filter:", nrow(tota_texts), "\n")

# Inspect non-English treaties before dropping them.
tota_texts |>
  filter(lang != "en" | is.na(lang)) |>
  count(lang, sort = TRUE) |>
  print()

tota_texts |>
  filter(lang != "en" | is.na(lang)) |>
  select(agreement_id, parties, year, lang) |>
  slice_head(n = 10) |>
  print()

tota_texts <- tota_texts |>
  filter(lang == "en")

cat("Treaties after keeping English only:", nrow(tota_texts), "\n")

# -----------------------------------------------------------------------------
# 2. Inspect the corpus
# -----------------------------------------------------------------------------

glimpse(tota_texts)

corpus_summary <- tota_texts |>
  summarise(
    n_agreements = n(),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    mean_words = mean(n_words, na.rm = TRUE),
    median_words = median(n_words, na.rm = TRUE),
    longest_agreement = parties[which.max(n_words)],
    max_words = max(n_words, na.rm = TRUE)
  )

print(corpus_summary)

# Treaty length as a simple proxy for agreement depth.
p_depth <- tota_texts |>
  filter(!is.na(year)) |>
  group_by(year) |>
  summarise(mean_words = mean(n_words, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = mean_words)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(
    title = "Trade agreements became longer over time",
    subtitle = "Word count is a simple proxy for agreement depth",
    x = "Year signed", y = "Mean number of words"
  ) +
  theme_minimal()

print(p_depth)
ggsave("output/figures/treaty_length_over_time.png", p_depth, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 3. Regular expressions: warm-up
# -----------------------------------------------------------------------------

example_text <- c(
  "Tariffs shall be reduced by 10 percent.",
  "The parties agree on sanitary and phytosanitary measures.",
  "Most-favoured-nation treatment applies.",
  "Article 12: Safeguards and countervailing duties."
)

str_detect(example_text, "tariff")
str_detect(str_to_lower(example_text), "tariff")
str_detect(str_to_lower(example_text), "tariff|duty|duties")
str_extract(example_text, "[0-9]+")
str_replace_all(example_text, "Most-favoured-nation", "MFN")

# Words starting with p/P.
words <- c("Policy", "trade", "production", "data", "protection")
words[str_detect(words, "^[Pp]")]

# -----------------------------------------------------------------------------
# 4. Tokenise, remove stop words, lemmatise
# -----------------------------------------------------------------------------

# Step 1: raw tokenisation — one token per row, no cleaning yet.
tota_raw <- tota_texts |>
  select(agreement_id, parties, year, text) |>
  unnest_tokens(output = word, input = text)

cat("Raw tokens:", nrow(tota_raw), "| Unique types:", n_distinct(tota_raw$word), "\n")

# The most common tokens are stopwords — linguistically uninformative.
tota_raw |> count(word, sort = TRUE) |> slice_head(n = 10)

# Step 2: remove stopwords.
tota_nostop <- tota_raw |>
  anti_join(stop_words, by = "word")

cat("After stopword removal:", nrow(tota_nostop), "tokens\n")

# Numbers are now prominent — also not useful for content analysis.
tota_nostop |> count(word, sort = TRUE) |> slice_head(n = 10)

# Step 3: remove pure numbers.
tota_nonums <- tota_nostop |>
  filter(!str_detect(word, "^[0-9]+$"))

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

top_words <- tota_nonums |>
  count(word, sort = TRUE) |>
  slice_head(n = 30)

print(top_words)

p_load(wordcloud2)
wordcloud2(top_words, color = 'random-light', backgroundColor = "#152238")

# -----------------------------------------------------------------------------
# 4b. Collocations: capturing meaningful multi-word expressions
# -----------------------------------------------------------------------------

# Build a quanteda token object from the cleaned texts for collocation detection.
# We apply the same preprocessing steps as the tidy pipeline above.
tota_toks <- corpus(tota_texts, text_field = "text") |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en")) |>
  tokens_select(min_nchar = 2)

tota_toks <- as.tokens(lapply(as.list(tota_toks), lemmatize_words))

# Detect statistically significant bigrams and trigrams (lambda + z-score).
# min_count = 50 avoids noise from rare pairs.
collocations <- textstat_collocations(tota_toks, size = 2:3, min_count = 50)
print(head(collocations, 20))

# Compound significant collocations (z > 3) back into the token stream so
# "most_favoured_nation" or "intellectual_property" travel as single tokens.
tota_toks_comp <- tokens_compound(tota_toks,
                                  pattern = collocations[collocations$z > 3, ])

# Most frequent tokens after compounding — multi-word expressions now visible.
dfm(tota_toks_comp) |>
  topfeatures(20) |>
  print()

# Convert compounded tokens back to tidy one-row-per-token format, replacing
# tota_words so all downstream steps (TF-IDF, dictionary, similarity) use
# the compounded vocabulary.
tota_words <- dfm(tota_toks_comp) |>
  tidy() |>
  rename(agreement_id = document, word = term) |>
  left_join(tota_texts |> select(agreement_id, parties, year),
            by = "agreement_id") |>
  uncount(count)

# Most frequent words after the full pipeline.
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
ggsave("output/figures/top_words.png", p_top_words, width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 5. TF-IDF: distinctive words by treaty
# -----------------------------------------------------------------------------

treaty_tfidf <- tota_words |>
  count(agreement_id, parties, year, word, sort = TRUE) |>
  bind_tf_idf(term = word, document = agreement_id, n = n) |>
  arrange(desc(tf_idf))

# Top distinctive words for a few treaties.
tfidf_top_by_treaty <- treaty_tfidf |>
  group_by(agreement_id, parties, year) |>
  slice_max(tf_idf, n = 8, with_ties = FALSE) |>
  ungroup()

print(tfidf_top_by_treaty |> select(agreement_id, year, parties, word, tf_idf) |> head(40))

# Pick one treaty and inspect its most distinctive terms.
one_treaty_id <- treaty_tfidf$agreement_id[1]
treaty_tfidf |>
  filter(agreement_id == one_treaty_id) |>
  arrange(desc(tf_idf)) |>
  select(parties, year, word, n, tf, idf, tf_idf) |>
  slice_head(n = 15)


# -----------------------------------------------------------------------------
# 6. Build a custom dictionary
# -----------------------------------------------------------------------------

# These are deliberately simple categories. The purpose is to show that the
# dictionary is a research choice and must be validated.
trade_dictionary <- list(
  agriculture = c("agric", "farm", "farmer", "crop", "livestock", "fish", "fisher", "sanitary", "phytosanitary"),
  services = c("service", "financial", "telecommunication", "transport", "professional", "bank", "insurance"),
  protection = c("safeguard", "anti-dumping", "antidumping", "countervailing", "quota", "restriction", "prohibit"),
  liberalisation = c("liberal", "free trade", "market access", "tariff reduction", "most favoured nation", "most favored nation", "mfn")
)

count_dictionary_category <- function(text, patterns) {
  pattern <- paste(patterns, collapse = "|")
  str_count(str_to_lower(text), regex(pattern, ignore_case = TRUE))
}

dictionary_counts <- tota_texts |>
  mutate(
    agriculture = map_int(text, count_dictionary_category, patterns = trade_dictionary$agriculture),
    services = map_int(text, count_dictionary_category, patterns = trade_dictionary$services),
    protection = map_int(text, count_dictionary_category, patterns = trade_dictionary$protection),
    liberalisation = map_int(text, count_dictionary_category, patterns = trade_dictionary$liberalisation),
    agriculture_per_1000 = 1000 * agriculture / n_words,
    services_per_1000 = 1000 * services / n_words,
    protection_per_1000 = 1000 * protection / n_words,
    liberalisation_per_1000 = 1000 * liberalisation / n_words
  )

# Which agreements score highest on each category?
dictionary_counts |>
  arrange(desc(agriculture_per_1000)) |>
  select(agreement_id, year, parties, agriculture, agriculture_per_1000) |>
  head(10)

dictionary_counts |>
  arrange(desc(services_per_1000)) |>
  select(agreement_id, year, parties, services, services_per_1000) |>
  head(10)

# -----------------------------------------------------------------------------
# 7. Validate the dictionary measure
# -----------------------------------------------------------------------------

# Create a small validation sample: high agriculture scores and zero scores.
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
    excerpt = str_squish(str_sub(text, 1, 900)),
    human_agriculture = NA,
    correct = NA
  ) |>
  select(agreement_id, year, parties, sample_type, machine_agriculture,
         agriculture, agriculture_per_1000, excerpt, human_agriculture, correct)

write_csv(validation_sample, "output/agriculture_validation_template.csv")

# Example code for when students return a coded file.
if (file.exists("output/agriculture_validation_coded.csv")) {
  validation_coded <- read_csv("output/agriculture_validation_coded.csv", show_col_types = FALSE) |>
    mutate(
      human_agriculture = as.logical(human_agriculture),
      machine_agriculture = as.logical(machine_agriculture)
    )

  validation_summary <- validation_coded |>
    summarise(
      TP = sum(machine_agriculture & human_agriculture, na.rm = TRUE),
      FP = sum(machine_agriculture & !human_agriculture, na.rm = TRUE),
      FN = sum(!machine_agriculture & human_agriculture, na.rm = TRUE),
      TN = sum(!machine_agriculture & !human_agriculture, na.rm = TRUE),
      precision = TP / (TP + FP),
      recall = TP / (TP + FN),
      accuracy = (TP + TN) / (TP + FP + FN + TN)
    )

  print(validation_summary)
}

# -----------------------------------------------------------------------------
# 8. Chi-square comparison of treaty content
# -----------------------------------------------------------------------------

# Compare two treaties with a contingency table: treaty x word category.
comparison_table <- dictionary_counts |>
  arrange(desc(n_words)) |>
  slice_head(n = 2) |>
  mutate(other_words = pmax(n_words - agriculture - services - protection - liberalisation, 0)) |>
  select(agriculture, services, protection, liberalisation, other_words) |>
  as.matrix()

rownames(comparison_table) <- dictionary_counts |>
  arrange(desc(n_words)) |>
  slice_head(n = 2) |>
  pull(parties)

print(comparison_table)
print(chisq.test(comparison_table))

# -----------------------------------------------------------------------------
# 9. Document similarity with quanteda
# -----------------------------------------------------------------------------

corp <- corpus(tota_texts, text_field = "text", docid_field = "agreement_id")

mwe <- phrase(c(
  "free trade", "tariff reduction", "market access",
  "most favoured nation", "most favored nation", "anti dumping",
  "countervailing duty"
))

toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_tolower() |>
  tokens_compound(pattern = mwe, concatenator = "_") |>
  tokens_remove(stopwords("en"))

dfmat <- dfm(toks)
dfmat_tfidf <- dfm_tfidf(dfmat)

# Similar treaties to a selected agreement.
target_id <- docnames(dfmat_tfidf)[2]
sim_to_target <- textstat_simil(dfmat_tfidf[target_id, ], dfmat_tfidf, method = "cosine")

similar_treaties <- tibble(
  agreement_id = docnames(dfmat_tfidf),
  similarity = as.numeric(sim_to_target)
) |>
  filter(agreement_id != target_id) |>
  left_join(tota_texts |> select(agreement_id, year, parties), by = "agreement_id") |>
  arrange(desc(similarity)) |>
  slice_head(n = 10)

print(tota_texts |> filter(agreement_id == target_id) |> select(agreement_id, year, parties))
print(similar_treaties)

# -----------------------------------------------------------------------------
# 10. Optional: LDA topic modelling
# -----------------------------------------------------------------------------

run_lda <- TRUE
if (run_lda) {
  if (!requireNamespace("topicmodels", quietly = TRUE)) {
    install.packages("topicmodels")
  }
  library(topicmodels)

  dfmat_lda <- dfm_trim(dfmat, min_termfreq = 5, min_docfreq = 3)
  dfmat_lda <- dfmat_lda[ntoken(dfmat_lda) > 0, ]

  dtm <- convert(dfmat_lda, to = "topicmodels")

  set.seed(42)
  lda_model <- topicmodels::LDA(dtm, k = 5, control = list(seed = 42))

  print(terms(lda_model, 10))
  print(round(posterior(lda_model)$topics[1:min(5, nrow(dtm)), ], 3))
}
# -----------------------------------------------------------------------------
# 12. Discussion prompts
# -----------------------------------------------------------------------------

# 1. Which dictionary category was easiest to measure? Which was hardest?
# 2. Which false positives appeared in the validation exercise?
# 3. Do similar treaties look similar because of legal content, geography, year,
#    parties, or document length?
# 4. When would you prefer a dictionary over a supervised classifier?
# 5. What would you need to turn this into a publishable text measure?
