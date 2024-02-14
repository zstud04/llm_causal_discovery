# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Read the CSV file
data <- read.csv("data/causal_response.csv")

# Step 2: Identify self-rating and others agree columns
self_rating_columns <- grep("self_rating$", names(data), value = TRUE)
agree_columns <- grep("others_agreee_w_self$", names(data), value = TRUE)

# Step 3: Map strings to numbers for all self-rating and agree questions using case_when
data_mapped <- data %>%
  mutate(across(all_of(self_rating_columns), ~case_when(
    . == "Definitely false" ~ 1,
    . == "Probably false" ~ 2,
    . == "Neither true nor false" ~ 3,
    . == "Probably true" ~ 4,
    . == "Definitely true" ~ 5,
    TRUE ~ NA_real_ # Handles cases that don't match any of the above
  ))) %>%
  mutate(across(all_of(agree_columns), ~case_when(
    . == "0-20%" ~ 1,
    . == "21-40%" ~ 2,
    . == "41-60%" ~ 3,
    . == "61-80%" ~ 4,
    . == "81-100%" ~ 5,
    TRUE ~ NA_real_ # Handles cases that don't match any of the above
  )))

# Step 4: Calculate average scores for each self-rating and agree question
average_scores <- sapply(data_mapped[self_rating_columns], mean, na.rm = TRUE)
average_agree_scores <- sapply(data_mapped[agree_columns], mean, na.rm = TRUE)

# Step 4b: calculate the variance
variance_score <- sapply(data_mapped[self_rating_columns], var, na.rm = TRUE)
variance_agree_score <- sapply(data_mapped[agree_columns], var, na.rm = TRUE)

# Step 5: Create a new DataFrame with question prompts, average scores, and average agree scores
question_prompts <- gsub("_self_rating$", "", self_rating_columns)
result_df <- data.frame(Question = question_prompts, 
                        AverageScore = average_scores, 
                        AverageAgreeScore = average_agree_scores,
                        VarianceScore = variance_score,
                        VarianceAgreeScore = variance_agree_score)

# Before proceeding, ensure result_df has 51 rows by removing the last two rows
result_df <- head(result_df, n = 51)

# Load additional data for comparison
gpt_data <- read.csv("filtered_gpt_data.csv")

# Assuming gpt_data contains columns for GPT scores (V2) and human agreement scores (e.g., HumanAgreeScore)
result_df <- result_df %>%
  mutate(GPTScore = gpt_data$V2, Text = gpt_data$V1, OriginalGPTScore = gpt_data$OriginalGPTScore, HumanAgreementScore = gpt_data$HumanAgreeScore)

# Calculate differences and correlation
result_df$ScoreDifference <- abs(result_df$AverageScore - result_df$GPTScore)
mean_difference <- mean(result_df$ScoreDifference, na.rm = TRUE)
agree_coeff <- cor(result_df$AverageAgreeScore, result_df$AverageScore, use = "complete.obs")

# Print results
print(mean_difference)
print(agree_coeff)

# Visualize data with ggplot2
plot1 <- ggplot(result_df, aes(x = AverageScore, y = GPTScore)) +
  geom_point(alpha = 1) +
  labs(x = "Average Score", y = "GPT Score", title = "Dot Plot of AverageScore vs. GPTScore")
ggsave("improved_dotplot_AverageScore_vs_GPTScore.png", plot1, width = 10, height = 6, dpi = 300)

plot2 <- ggplot(result_df, aes(x = AverageScore, y = AverageAgreeScore)) +
  geom_point(alpha = 1) +
  labs(x = "Average Score", y = "Average Agree Score", title = "Dot Plot of AverageScore vs. Average Agree")
ggsave("improved_dotplot_AverageScore_vs_AverageAgreeScore.png", plot2, width = 10, height = 6, dpi = 300)

# Export dataframes

result_df_by_diff = result_df %>% arrange(desc(ScoreDifference))
write.csv(result_df_by_diff, "statements_with_greatest_disagreement.csv", row.names = FALSE)
cat("Exported the ordered statements to: statements_with_greatest_disagreement.csv\n")

result_df_reordered <- result_df %>% arrange(desc(GPTScore), desc(ScoreDifference))
write.csv(result_df_reordered, "statements_ordered_by_GPTScore_then_AverageScore.csv", row.names = FALSE)
cat("Exported the reordered statements to: statements_ordered_by_GPTScore_then_AverageScore.csv\n")

