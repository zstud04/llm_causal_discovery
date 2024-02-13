library(tidyverse)

# Read the data from the CSV file
data <- read.csv("data/mturkiter2_1.csv", stringsAsFactors = FALSE)

# Filter out only the columns starting with 'Q122_'
q122_data <- data %>% select(starts_with("Q122_"))

# Reshape the data to long format
long_data <- pivot_longer(q122_data, cols = everything(), names_to = "question", values_to = "response")

# Now we map the abbreviated column names to the full labels
# Please ensure the labels are in the same order as the columns in your CSV
question_labels <- c(
  "Q122_1" = "If an LLM provides a correct response, it has understood what you are asking it",
  "Q122_2" = "LLMs understand something about how the physical world works",
  "Q122_3" = "LLMs can produce informative text about human feelings/emotions",
  "Q122_4" = "LLMs understand human feelings/emotions",
  "Q122_5" = "LLMs can produce correct responses even though they might not really understand what is being asked",
  "Q122_6" = "There are some things that all adults easily understand that LLMs will never be able to understand"
)

# Replace the question codes with the full labels
long_data$question <- factor(long_data$question, levels = names(question_labels), labels = question_labels)

# Make sure the responses are ordered if necessary (adjust the levels accordingly)
long_data$response <- factor(long_data$response, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

# Plot
likert_plot <- ggplot(long_data, aes(x = question, fill = response)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Blues") + # Optional: to use a color palette
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Question", y = "Percentage", fill = "Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(likert_plot)

# Save the plot if needed
ggsave("likert_plot.png", plot = likert_plot, width = 12, height = 8, dpi = 300)
