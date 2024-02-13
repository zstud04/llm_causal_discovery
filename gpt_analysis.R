library(tidyverse)


data = read.csv("processed_mturkiter_gpt_responses.csv")

data_clean <- data %>%
  drop_na()
# Parse GPT responses into separate columns for easier comparison
data_clean <- data_clean %>%
  mutate(
    gpt4_response_1 = as.integer(str_split_fixed(gpt4_response, ",", 2)[,1]),
    gpt4_response_2 = as.integer(str_split_fixed(gpt4_response, ",", 2)[,2]),
    gpt3_5_response_1 = as.integer(str_split_fixed(gpt3.5_response, ",", 2)[,1]),
    gpt3_5_response_2 = as.integer(str_split_fixed(gpt3.5_response, ",", 2)[,2])
  ) %>% drop_na()

  print(data_clean)

summary_stats <- data_clean %>%
  summarise(
    percent_1s_gpt4_response_1 = mean(gpt4_response_1 == 1) * 100,
    mean_value_gpt4_response_1 = mean(gpt4_response_1),
    percent_1s_gpt4_response_2 = mean(gpt4_response_2 == 1) * 100,
    mean_value_gpt4_response_2 = mean(gpt4_response_2),
    percent_1s_gpt3_5_response_1 = mean(gpt3_5_response_1 == 1) * 100,
    mean_value_gpt3_5_response_1 = mean(gpt3_5_response_1),
    percent_1s_gpt3_5_response_2 = mean(gpt3_5_response_2 == 1) * 100,
    mean_value_gpt3_5_response_2 = mean(gpt3_5_response_2)
  )

# # Print the summary statistics
print(summary_stats)