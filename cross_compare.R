# Load necessary libraries
library(dplyr)

# Step 1: Read the CSV file
data <- read.csv("data/causal_response.csv")



# Step 2: Identify self-rating questions
self_rating_columns <- grep("self_rating$", names(data), value = TRUE)

# Assuming column names are proxies for question prompts
question_prompts <- gsub("_self_rating$", "", self_rating_columns)

# Step 3: Map strings to numbers for all self-rating questions using case_when
data_mapped <- data %>%
  mutate(across(all_of(self_rating_columns), ~case_when(
    . == "Definitely false" ~ 1,
    . == "Probably false" ~ 2,
    . == "Neither true nor false" ~ 3,
    . == "Probably true" ~ 4,
    . == "Definitely true" ~ 5,
    TRUE ~ NA_real_ # This line handles cases that don't match any of the above
  )))

  



# Step 4: Calculate average scores for each self-rating question
average_scores <- sapply(data_mapped[self_rating_columns], function(column) mean(column, na.rm = TRUE))

# Step 5: Create a new DataFrame with question prompts and average scores
result_df <- data.frame(Question = question_prompts, AverageScore = average_scores)

gpt_data <- read.csv("filtered_gpt_data.csv")





print(result_df)

differences <- abs(result_df$AverageScore - result_df$GPTScore)

# Calculate the mean of these absolute differences
mean_difference <- mean(differences, na.rm = TRUE)

# Print the mean difference
print(mean_difference)


##########
# Step 2: Identify self-rating questions
agree_columns <- grep("others_agreee_w_self$", names(data), value = TRUE)

print("HERE:")
print(agree_columns)

# Assuming column names are proxies for question prompts
question_prompts <- gsub("_others_agreee_w_self$", "", agree_columns)

# Step 3: Map strings to numbers for all self-rating questions using case_when
data_mapped <- data %>%
  mutate(across(all_of(agree_columns), ~case_when(
    . == "0-20%" ~ 1,
    . == "21-40%" ~ 2,
    . == "41-60%" ~ 3,
    . == "61-80%" ~ 4,
    . == "81-100%" ~ 5,
    TRUE ~ NA_real_ # This line handles cases that don't match any of the above
  )))
  



# Step 4: Calculate average scores for each self-rating question
average_agree_scores <- sapply(data_mapped[agree_columns], function(column) mean(column, na.rm = TRUE))

result_df = result_df %>% mutate(AverageAgreeScore =average_agree_scores)

result_df <- result_df[1:(nrow(result_df) - 2), ]


print(agree_columns)

result_df = result_df %>% mutate(GPTScore = gpt_data$V2) %>% mutate(text = gpt_data$V1)


# Assuming result_df has columns AverageScore and GPTScore
# Generate a dot plot
# Load necessary library
library(ggplot2)

# Create an improved plot without individual point labels
plot <- ggplot(result_df, aes(x = AverageScore, y = GPTScore)) +
  geom_point(alpha = 1) + # Makes points solid by setting alpha = 1
  labs(x = "Average Score", y = "GPT Score", title = "Dot Plot of AverageScore vs. GPTScore")

# Display the plot
print(plot)

# Save the plot with improved settings
ggsave("improved_dotplot_AverageScore_vs_GPTScore.png", plot, width = 10, height = 6, dpi = 300)

agree_coeff <- cor(result_df$AverageAgreeScore, result_df$AverageScore, use = "complete.obs")

# Print the correlation coefficient
print(agree_coeff)

result_df$ScoreDifference <- abs(result_df$AverageScore - result_df$GPTScore)

# Order the statements by score difference in descending order
result_df_ordered <- result_df %>%
  arrange(desc(ScoreDifference))

# View the ordered DataFrame

print(result_df_ordered)

# Specify the path and filename for the output CSV file
output_csv_path <- "statements_with_greatest_disagreement.csv"

# Export the ordered DataFrame to a CSV file
write.csv(result_df_ordered, output_csv_path, row.names = FALSE)

# Print a message indicating completion
cat("Exported the ordered statements to:", output_csv_path, "\n")

result_df_reordered <- result_df %>%
  arrange(desc(GPTScore), desc(ScoreDifference))

# View the reordered DataFrame
print(result_df_reordered)

# Specify the path and filename for the output CSV file
output_csv_path_reordered <- "statements_ordered_by_GPTScore_then_AverageScore.csv"

# Export the reordered DataFrame to a CSV file
write.csv(result_df_reordered, output_csv_path_reordered, row.names = FALSE)

# Print a message indicating completion
cat("Exported the reordered statements to:", output_csv_path_reordered, "\n")

print(agree_coeff)


plot <- ggplot(result_df, aes(x = AverageScore, y = AverageAgreeScore)) +
  geom_point(alpha = 1) + # Makes points solid by setting alpha = 1
  labs(x = "Average Score", y = "Average Agree Score", title = "Dot Plot of AverageScore vs. Average Agree")

# Display the plot
print(plot)

# Save the plot with improved settings
ggsave("improved_dotplot_AverageScore_vs_AverageAgreeScore.png", plot, width = 10, height = 6, dpi = 300)