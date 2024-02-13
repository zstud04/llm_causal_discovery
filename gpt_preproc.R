# Load the required libraries
library(tidyverse)
library(glue)

# Load the data from CSV
data <- read.csv("data/mturkiter2_1.csv") %>%
  slice(-1:-6)
  
  print(data)

prompt_template <- readLines("prompt_template.txt") %>% paste(collapse = "\n")

# Rename columns as specified
data_renamed <- data %>%
  rename(
    scenario_1_counter_3 = Q429_3,
    scenario_1_counter_2 = Q429_2,
    scenario_1_counter_1 = Q429_1,
    scenario_2_counter_3 = Q43_2,
    scenario_2_counter_2 = Q43_7,
    scenario_2_counter_1 = Q43_1,
    scenario_3_counter_3 = Q44_3,
    scenario_3_counter_2 = Q44_2,
    scenario_3_counter_1 = Q44_1
  )

# Exclude specific workerIds
data_filtered <- data_renamed %>%
  filter(!workerId %in% c("A27VK38SRSSHV3", "A2Y7L7R5UTQ5R2", "A29P011QEF23G8"))

# Pivot the data to a longer format and create causal and counterfactual columns
long_data <- data_filtered %>%
  pivot_longer(cols = c(starts_with("scenario_")), names_to = "scenario", values_to = "value") %>%
  mutate(
    scenario_base = str_extract(scenario, "scenario_\\d_"),
    scenario_type = ifelse(str_detect(scenario, "_causal_"), "causal_value", "counterfactual_value"),
    scenario_num = str_extract(scenario, "\\d$"),
    place = case_when(
      str_detect(scenario, "^scenario_1_") ~ place_1,
      str_detect(scenario, "^scenario_2_") ~ place_2,
      str_detect(scenario, "^scenario_3_") ~ place_3
    )
  ) %>%
  select(-scenario) %>%
  pivot_wider(names_from = scenario_type, values_from = value) %>%
  group_by(workerId, place, scenario_base, scenario_num) %>%
  summarise(
    causal_value = first(causal_value),
    counterfactual_value = first(counterfactual_value),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(place = str_remove_all(place, "(?i)\\b(?:the|a)\\b\\s*"))

# Generate embedded_prompt using causal_value
long_data <- long_data %>%
  mutate(embedded_prompt = glue(prompt_template, place = place, value = causal_value))

# View the updated dataframe
print(long_data, n = 100)

# Export the dataframe to a new CSV file
write.csv(long_data, "processed_mturkiter2.csv", row.names = FALSE)
