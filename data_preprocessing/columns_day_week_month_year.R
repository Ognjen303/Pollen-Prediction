# Load the necessary library
library(dplyr)
library(lubridate)

# Read the CSV file into a dataframe
data <- read.csv("2016_2024.csv")

# Convert the date column to Date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Add new columns
data$day <- day(data$date)                  # Extract day of the month
data$month <- month(data$date)              # Extract month as a number
data$day_month <- paste(data$day, data$month, sep = "_")  # Create day_month combination
data$week <- week(data$date)          # Calculate week of the month
data$year <- year(data$date)                # Extract year


becej <- filter(data, location=="BECEJ", year == 2016)
becej_data <- becej[c('day', 'month', 'day_month', 'week', 'ALNUS')]

colnames(becej_data) <- c('day', 'month', 'day_month', 'week', 'y')

write.csv(becej_data, "becej_alnus_2016.csv", row.names = FALSE)

# Find the minimum date
# min_date <- min(data$date)

# Create a new column "Day" by calculating the difference in days
# data <- data %>%
#  mutate(day = as.integer(date - min_date) + 1)

# Reorder columns to place "Day" immediately after "date"
# data <- data %>%
#  select(location, day, date, everything())

# Save the modified dataframe to a new CSV file
# write.csv(data, "2016_2024_day.csv", row.names = FALSE)
