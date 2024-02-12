# Load the required libraries
library(tidyverse)
library(glue)

# Load the data from CSV
data <- read.csv("data/mturkiter2.csv")

prompt_template <- readLines("prompt_template.txt") %>% paste(collapse = "\n")


# Exclude specific workerIds
data_filtered <- data %>%
  filter(!workerId %in% c("A27VK38SRSSHV3", "A2Y7L7R5UTQ5R2", "A29P011QEF23G8"))

# Pivot the data to a longer format, filter for causal scenarios, and create the `place` column
long_data <- data_filtered %>%
  pivot_longer(cols = c(starts_with("scenario_1_causal_"), starts_with("scenario_2_causal_"), 
                        starts_with("scenario_3_causal_"), starts_with("Q429"), starts_with("Q43"), starts_with("Q44")), 
               names_to = "scenario", values_to = "value") %>%
  mutate(place = case_when(
    scenario %in% c("scenario_3_causal_1", "scenario_3_causal_2", "scenario_3_causal_3", "Q429_3", "Q43_2", "Q44_3") ~ place_3,
    scenario %in% c("scenario_2_causal_1", "scenario_2_causal_2", "scenario_2_causal_3", "Q429_2", "Q43_7", "Q44_2") ~ place_2,
    scenario %in% c("scenario_1_causal_1", "scenario_1_causal_2", "scenario_1_causal_3", "Q429_1", "Q43_1", "Q44_1") ~ place_1,
  )) %>%
  select(place, scenario, value)  # Select only the place, scenario, and value columns

long_data <- long_data %>%
  mutate(place = str_remove_all(place, "(?i)\\b(?:the|a)\\b\\s*"))

long_data <- long_data %>%
  mutate(embedded_prompt = glue(prompt_template))

long_data <- long_data %>%
  slice(-1:-54)

# View the updated dataframe
print(long_data, n=100)

write.csv(long_data, "processed_mturkiter2.csv", row.names = FALSE)

