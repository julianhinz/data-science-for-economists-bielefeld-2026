###
# 04 - Process Results
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(jsonlite)

# load the prompts
ecb_speeches <- fread("temp/ecb_speeches_prompts.csv")

# load the responses
response_files <- list.files("temp/LLM_responses", full.names = TRUE)

responses <- lapply(response_files, function(x) {
  tryCatch({
    parsed <- fromJSON(readLines(x, warn = FALSE))
    data.table(
      classification = parsed$classification,
      confidence = as.numeric(parsed$confidence),
      keywords = list(parsed$keywords)
    )
  }, error = function(e) {
    message(paste("Error parsing JSON from", x, ":", e$message))
    return(NULL)
  })
})

# extract IDs from filenames
ids <- as.integer(gsub("\\D", "", basename(response_files)))

# combine responses
responses <- rbindlist(responses, idcol = "file_idx")
responses[, id := ids[file_idx]]
responses[, file_idx := NULL]

# expand keywords into separate columns
responses[, paste0("keyword_", seq_len(max(lengths(keywords)))) :=
  transpose(keywords)]

responses[, keywords := NULL]

# merge responses with prompts
ecb_speeches <- merge(ecb_speeches, responses, by = "id")

# Voila!
print(ecb_speeches[, .(id, classification, confidence, keyword_1, keyword_2, keyword_3)])
