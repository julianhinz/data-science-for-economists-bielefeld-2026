###
#
#
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ellmer)
#?ellmer

# use openrouter.ai
# store your key outside of version control, e.g. in ~/.Renviron:
#   OPENROUTER_API_KEY=sk-or-v1-...
# ellmer's chat_openrouter() reads OPENROUTER_API_KEY from the environment automatically
API_KEY = Sys.getenv("OPENROUTER_API_KEY")

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
