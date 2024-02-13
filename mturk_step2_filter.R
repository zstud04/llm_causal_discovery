library(dplyr)

# TODO: define filtered questions in config- this is terrible code but no time right now
rows_to_keep <- c(6,
10,
11,
13,
17,
18,
20,
21,
22,
26,
27,
45,
56,
57,
58,
64,
65,
66,
83,
84,
85,
86,
87,
88,
89,
90,
91,
111,
110,
112,
113,
114,
115,
116,
117,
118,
128,
129,
130,
132,
134,
135,
136,
143,
47,
53,
60,
62,
63,
97)

# Read the CSV file
data <- read.csv("processed_mturkiter2.csv")

# Filter the data to keep only the specified rows
filtered_data <- data[rows_to_keep, ]

# Write the filtered data to a new CSV file
write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)

print("Filtered data has been saved to filtered_data.csv")

print(paste("The filtered data contains", nrow(filtered_data), "rows."))
