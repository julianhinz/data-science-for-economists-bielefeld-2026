###
# 02 - Prepare Prompts for the LLM
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(stringr)
p_load(glue)

# read the cleaned dataset
ecb_speeches <- fread("temp/ecb_speeches_cleaned.csv")

# create prompt template
prompt_template <- 'You are an expert on central banking communication. You have a specific task: Classify the following ECB speech as either "hawkish", "dovish", or "neutral". Also return the confidence level of your classification on a scale from 0 to 1, where 0 means no confidence and 1 means full confidence. Please provide up to three keywords of the main message of the speech. The speech is as follows:

```speech
{text}
```

Return the classification, confidence level, and keywords in the following valid JSON format:

```json
{{"classification": "hawkish|dovish|neutral", "confidence": 0.0-1.0, "keywords": ["keyword1", "keyword2", "keyword3"]}}
```
'

# create prompts
ecb_speeches[, prompt := glue(prompt_template, text = contents)]
# ecb_speeches[1]$prompt

# add id column
ecb_speeches[, id := 1:.N]

# save prompts
fwrite(ecb_speeches, "temp/ecb_speeches_prompts.csv")
