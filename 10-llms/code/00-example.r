###
#
#
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ellmer)
p_load(data.table)
#?ellmer

# use openrouter.ai
Sys.setenv(OPENROUTER_API_KEY = API_KEY)

# chat object
chat <- chat_openrouter(
  model = "openai/gpt-4.1-mini",
  system_prompt = "You are a helpful economics research assistant, but you always end with a pun."
)

# chat with a prompt
response = chat$chat("Summarize the main argument of Acemoglu et al. 2001 in two sentences.")

str(response)
response

# build a earnings call mood anayzer prompt
chat_earnings <- chat_openrouter(
  model = "cohere/north-mini-code:free",
  system_prompt = "You are a monetary policy expert. You detect the mood from earnings call and return either of the words: \"positive\", \"neutral\", or \"negative\"."
)

# test the earnings call classifier
response_earnings = chat_earnings$chat("The company reported a 10% increase in revenue and a 5% decrease in expenses.")


# function to classify earnings call mood
classify_earnings_call_mood <- function(text) {
  response = chat_earnings$chat(text)
  return(response)
}

example_texts <- c(
  "The company reported a 10% increase in revenue and a 5% decrease in expenses.",
  "The company faced significant challenges this quarter, with declining sales and increased costs.",
  "The company's performance was stable, with no major changes in revenue or expenses."
)

# loop over calls and classify mood
mood_results <- sapply(example_texts, classify_earnings_call_mood)
mood_results

# refine the prompt to add confidence score
chat_earnings_refined <- chat_openrouter(
  model = "cohere/north-mini-code:free",
    system_prompt = "You are a monetary policy expert. You detect the mood from
                    earnings call and return either of the words: \"positive\",
                    \"neutral\", or \"negative\". Also provide a confidence score between 0 and 1.
                    
                   Return the result in the csv format with two columns: mood and confidence_score.

                   Example output:
                   mood,confidence_score
                   positive,0.95
                   "
)

# test the refined earnings call classifier
response_earnings_refined = chat_earnings_refined$chat("The company reported a 10% increase in revenue and a 5% decrease in expenses.")
response_earnings_refined = fread(response_earnings_refined)

response_earnings_refined[, confidence_score]
