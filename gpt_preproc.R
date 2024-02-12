# Load the required libraries
library(tidyverse)
library(glue)

# Load the data from CSV
data <- read.csv("data/mturkiter2.csv")

# Exclude specific workerIds
data_filtered <- data %>%
  filter(!workerId %in% c("A27VK38SRSSHV3", "A2Y7L7R5UTQ5R2", "A29P011QEF23G8"))

# Drop the first 54 observations
data_filtered <- data_filtered %>%
  slice(-1:-54)

# Pivot the data to a longer format, filter for causal scenarios, and create the `place` column
long_data <- data_filtered %>%
  pivot_longer(cols = c(starts_with("scenario_1_causal_"), starts_with("scenario_2_causal_"), 
                        starts_with("scenario_3_causal_")), 
               names_to = "scenario", values_to = "value") %>%
  mutate(place = case_when(
    scenario %in% c("scenario_3_causal_1", "scenario_3_causal_2", "scenario_3_causal_3") ~ place_3,
    scenario %in% c("scenario_2_causal_1", "scenario_2_causal_2", "scenario_2_causal_3") ~ place_2,
    scenario %in% c("scenario_1_causal_1", "scenario_1_causal_2", "scenario_1_causal_3") ~ place_1
  )) %>%
  select(place, scenario, value)  # Select only the place, scenario, and value columns

long_data <- long_data %>%
  mutate(embedded_prompt = glue("Imagine a typical {place}. Now consider this statement about that place:\n\n{value}.\n\nRespond with a \"1\" if you believe this is true, and a \"0\" if you believe this is false."))

# View the updated dataframe
print(long_data, n=100)

write.csv(long_data, "processed_mturkiter2.csv", row.names = FALSE)

