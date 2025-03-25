# Install necessary libraries if not already installed
install.packages(c("ggplot2", "dplyr", "tidyr","readxl", "ggplot2", "gridExtra","grid","readr"))
library(gridExtra)
library(grid)
library(readxl)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(readr)

getwd()
setwd("/Users/cvtmert/Desktop")

# Read the Excel file
data <- read_excel("fitness.xlsx", sheet = 1)  # Adjust the sheet parameter if needed
head(data)
str(data)  # Check structure and data types


# For numeric columns, replace NA with a placeholder (e.g., 0)
numeric_columns <- sapply(data, is.numeric)  # Identify numeric columns
data[, numeric_columns] <- lapply(data[, numeric_columns], function(x) {
  ifelse(is.na(x), 0, x)  # Replace NA with 0
})

# For character columns, replace NA with an empty string
character_columns <- sapply(data, is.character)  # Identify character columns
data[, character_columns] <- lapply(data[, character_columns], function(x) {
  ifelse(is.na(x), "", x)  # Replace NA with an empty string
})

# Verify the changes
summary(data)

# Summarize gender and age data
gender_age_dist <- data %>%
  group_by(gender, age) %>%
  summarise(count = n(), .groups = "drop")  # Use .groups to suppress grouping messages

print(head(gender_age_dist))

plot <- ggplot(gender_age_dist, aes(x = age, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Gender and Age Distribution",
       x = "Age Group",
       y = "Count") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Explicitly print the plot
print(plot)

# Summarize weekly exercise frequency
exercise_freq_dist <- data %>%
  group_by('frequency_of_exercise_(weekly)') %>%
  summarise(count = n())

ggplot(data, aes(x = `frequency_of_exercise_(weekly)`)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Weekly Exercise Frequency Distribution",
       x = "Frequency (times per week)",
       y = "Count")

# Flatten and summarize barriers
barriers_flat <- unlist(data$barriers)
barriers_dist <- as.data.frame(table(barriers_flat))

# Plot: Barriers to Exercise
ggplot(barriers_dist, aes(x = reorder(barriers_flat, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  labs(title = "Barriers to Exercise",
       x = "Barrier Code",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Assuming 'data' is the dataset loaded with the barriers column
# Step 1: Remove unwanted characters (e.g., brackets and quotes)
data$barriers_cleaned <- gsub("\\[|\\]|'", "", data$barriers)  # Remove square brackets and quotes

# Step 2: Split the column into individual barrier codes
barriers_list <- strsplit(data$barriers_cleaned, ",\\s*")  # Split by commas with optional whitespace

# Step 3: Flatten the list into a single vector
barriers_flat <- unlist(barriers_list)

# Step 4: Create a frequency table of barriers
barriers_dist <- as.data.frame(table(barriers_flat))

# Step 5: Rename columns for clarity
colnames(barriers_dist) <- c("Barrier_Code", "Frequency")

# Step 6: Sort by frequency (optional)
barriers_dist <- barriers_dist[order(-barriers_dist$Frequency), ]


# Plot the frequency of barriers
ggplot(barriers_dist, aes(x = reorder(Barrier_Code, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  labs(title = "Barriers to Exercise",
       x = "Barrier Code",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 1: Clean the motivation column (remove unwanted characters)
data$motivation_cleaned <- gsub("\\[|\\]|'", "", data$motivation)  # Remove square brackets and quotes

# Step 2: Split the cleaned column into individual motivation codes
motivation_list <- strsplit(data$motivation_cleaned, ",\\s*")  # Split by commas with optional whitespace

# Step 3: Flatten the list into a single vector
motivation_flat <- unlist(motivation_list)

# Step 4: Create a frequency table of motivations
motivation_dist <- as.data.frame(table(motivation_flat))

# Step 5: Rename columns for clarity
colnames(motivation_dist) <- c("Motivation_Code", "Frequency")

# Step 6: Sort by frequency (optional)
motivation_dist <- motivation_dist[order(-motivation_dist$Frequency), ]

# Plot the frequency of motivations
ggplot(motivation_dist, aes(x = reorder(Motivation_Code, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Motivations for Exercise",
       x = "Motivation Code",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


numeric_columns <- c("importance_of_exercise", "daily_exercise_allocation", "health_score")
# Convert columns to numeric to handle any formatting issues
data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)

# Function to calculate descriptive statistics
descriptive_stats <- function(column_data) {
  if (length(column_data[!is.na(column_data)]) == 0) {
    return(data.frame(n = NA, mean = NA, sd = NA, median = NA,
                      min = NA, max = NA, range = NA,
                      skewness = NA, kurtosis = NA, se = NA))
  }
  stats <- data.frame(
    n = length(column_data[!is.na(column_data)]),
    mean = mean(column_data, na.rm = TRUE),
    sd = sd(column_data, na.rm = TRUE),
    median = median(column_data, na.rm = TRUE),
    min = min(column_data, na.rm = TRUE),
    max = max(column_data, na.rm = TRUE),
    range = diff(range(column_data, na.rm = TRUE)),
    skewness = sum((column_data - mean(column_data, na.rm = TRUE))^3, na.rm = TRUE) /
      (length(column_data[!is.na(column_data)]) * sd(column_data, na.rm = TRUE)^3),
    kurtosis = sum((column_data - mean(column_data, na.rm = TRUE))^4, na.rm = TRUE) /
      (length(column_data[!is.na(column_data)]) * sd(column_data, na.rm = TRUE)^4) - 3,
    se = sd(column_data, na.rm = TRUE) / sqrt(length(column_data[!is.na(column_data)]))
  )
  return(stats)
}

# Calculate descriptive statistics for numeric columns
stats_table <- data.frame()
for (col in numeric_columns) {
  column_data <- data[[col]]
  stats <- descriptive_stats(column_data)
  stats <- cbind(Variable = col, stats)  # Add column name
  stats_table <- rbind(stats_table, stats)
}

# Print the stats_table for debugging
print(stats_table)

# Convert stats_table to a tableGrob
stats_table_grob <- tableGrob(
  stats_table,
  rows = NULL,  # No row names
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.8)), # Adjust text size
    colhead = list(fg_params = list(cex = 0.9, fontface = "bold")) # Adjust column header
  )
)

# Display the table in the RStudio Plot Panel
grid.newpage()  # Open a new graphics page
grid.draw(stats_table_grob)  # Render the table




