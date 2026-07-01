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
chat <- chat_openrouter(
  model = "deepseek/deepseek-v4-flash",
  api_args = list(temperature = 0)
)

example = chat$chat(ecb_speeches$prompt[1])

# create output directory
dir.create("temp/LLM_responses", showWarnings = FALSE, recursive = TRUE)

# loop through first 10 speeches (for testing)
for (i in 1:10) {
  print(paste("Processing speech ID:", ecb_speeches$id[i]))

  # fresh chat for each speech to avoid context accumulation
  chat_i <- chat$clone()

  tryCatch({
    result <- chat_i$chat(ecb_speeches$prompt[i])

    # save the response as JSON
    writeLines(result,
      paste0("temp/LLM_responses/response_", ecb_speeches$id[i], ".json")
    )
  }, error = function(e) {
    message(paste("Error processing speech", ecb_speeches$id[i], ":", e$message))
  })

  Sys.sleep(1)  # rate limiting
}
