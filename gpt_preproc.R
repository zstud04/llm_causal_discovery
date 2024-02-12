# Load the required libraries
library(tidyverse)
library(glue)

# Load the data from CSV
data <- read.csv("data/mturkiter2.csv")

prompt_template <- readLines("prompt_template.txt") %>% paste(collapse = "\n")

# Rename columns as specified
data_renamed <- data %>%
  rename(
    scenario_3_counter_1 = Q429_3,
    scenario_2_counter_1 = Q429_2,
    scenario_1_counter_1 = Q429_1,
    scenario_3_counter_2 = Q43_2,
    scenario_2_counter_2 = Q43_7,
    scenario_1_counter_2 = Q43_1,
    scenario_3_counter_3 = Q44_3,
    scenario_2_counter_3 = Q44_2, # Corrected as per the instructions
    scenario_1_counter_3 = Q44_1  # Adjusted name to match pattern
  )

# Exclude specific workerIds
data_filtered <- data_renamed %>%
  filter(!workerId %in% c("A27VK38SRSSHV3", "A2Y7L7R5UTQ5R2", "A29P011QEF23G8"))

# Separate causal and counterfactual scenarios for easier manipulation
causal_data <- data_filtered %>%
  pivot_longer(cols = contains("_causal_"), names_to = "scenario", values_to = "causal_value") %>%
  mutate(scenario = str_replace(scenario, "_causal_", "_"))

counterfactual_data <- data_filtered %>%
  pivot_longer(cols = contains("_counter_"), names_to = "scenario", values_to = "counterfactual_value") %>%
  mutate(scenario = str_replace(scenario, "_counter_", "_"))

# Combine causal and counterfactual scenarios based on scenario and place
long_data <- left_join(causal_data, counterfactual_data, by = c("workerId", "scenario", "place_1", "place_2", "place_3"))

# Adjust place column based on scenario
long_data <- long_data %>%
  mutate(place = case_when(
    str_detect(scenario, "3_") ~ place_3,
    str_detect(scenario, "2_") ~ place_2,
    str_detect(scenario, "1_") ~ place_1
  )) %>%
  select(workerId, place, scenario, causal_value, counterfactual_value) %>%
  distinct()

# Remove articles from place
long_data <- long_data %>%
  mutate(place = str_remove_all(place, "(?i)\\b(?:the|a)\\b\\s*"))

# Generate embedded_prompt using causal_value
long_data <- long_data %>%
  mutate(embedded_prompt = glue(prompt_template, place = place, causal_value = causal_value, counterfactual_value = counterfactual_value))

# Optionally, adjust the slice to remove specific rows if needed
long_data <- long_data %>%
  slice(-1:-54)

# View the updated dataframe
print(long_data, n = 100)

# Export the dataframe to a new CSV file
write.csv(long_data, "processed_mturkiter2.csv", row.names = FALSE)
