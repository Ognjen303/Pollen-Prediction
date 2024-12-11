# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the CSV file into a dataframe
data <- read.csv("final_data/becej.csv")

# Convert the date column to Date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Add new columns
data$day <- day(data$date)                  # Extract day of the month
#data$month <- month(data$date)              # Extract month as a number
data$week <- week(data$date)                # Calculate week of the year
data$year <- year(data$date)                # Extract year

# Combine week, month, and year into a single column
data$week_year <- paste(data$week, data$year, sep = "_")

# Select necessary columns and rename
becej_data <- data %>%
  select(day, week_year, week, year, y)

# Create a new dataset averaging `y` over weeks and selecting desired columns
weekly_data <- becej_data %>%
  filter(!is.na(y)) %>%
  group_by(week_year) %>%
  summarize(week = unique(week),
            year = unique(year),
            y = mean(y)) %>%
  arrange(year, week) %>%
  select(week, year, week_year, y)

# Write the weekly data to a CSV file
write.csv(weekly_data, "final_data/becej_weekly.csv", row.names = FALSE)
