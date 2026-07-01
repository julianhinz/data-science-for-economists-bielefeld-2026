###
# 03 - Call LLM API with Prepared Speech Prompts
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(ellmer)

# settings -- set OPENROUTER_API_KEY in .Renviron
api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (api_key == "") stop("Set OPENROUTER_API_KEY in .Renviron")

# load prompts
ecb_speeches <- fread("temp/ecb_speeches_prompts.csv")

# create the chat object using ellmer
chat <- chat_openai(
  base_url = "https://openrouter.ai/api/v1",
  api_key = api_key,
  model = "deepseek/deepseek-chat-v3-0324:free",
  system_prompt = "You are a monetary policy expert.",
  api_args = list(temperature = 0)
)

# define expected output structure
type_classification <- type_object(
  classification = type_enum(values = c("hawkish", "dovish", "neutral")),
  confidence = type_number(),
  keywords = type_array(items = type_string())
)

# test with a single speech
result <- chat$extract_data(ecb_speeches$prompt[1], type = type_classification)
print(result)

# create output directory
dir.create("temp/LLM_responses", showWarnings = FALSE, recursive = TRUE)

# loop through first 10 speeches (for testing)
for (i in 1:10) {
  print(paste("Processing speech ID:", ecb_speeches$id[i]))

  # fresh chat for each speech to avoid context accumulation
  chat_i <- chat$clone()

  tryCatch({
    result <- chat_i$extract_data(ecb_speeches$prompt[i], type = type_classification)

    # save the response as JSON
    writeLines(
      jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE),
      paste0("temp/LLM_responses/response_", ecb_speeches$id[i], ".json")
    )
  }, error = function(e) {
    message(paste("Error processing speech", ecb_speeches$id[i], ":", e$message))
  })

  Sys.sleep(1)  # rate limiting
}
