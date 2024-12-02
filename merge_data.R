# Create a list of years
years <- 2016:2024

# Create an empty list to store sets of cities for each year
city_lists <- list()

# Read each file and extract unique cities
for (year in years) {
  filename <- paste0(year, ".csv")
  data <- read.csv(filename, encoding = "UTF-8")
  city_lists[[as.character(year)]] <- unique(data$location)
}

# Find common cities across all datasets using Reduce and intersect
common_cities <- Reduce(intersect, city_lists)

print(common_cities)

# Create an empty list to store filtered datasets
filtered_datasets <- list()

# Read and filter each dataset to include only common cities
for (year in years) {
  filename <- paste0(year, ".csv")
  data <- read.csv(filename, encoding = "UTF-8")
  # Filter for common cities only
  filtered_data <- data[data$location %in% common_cities, ]
  filtered_datasets[[as.character(year)]] <- filtered_data
}

# Combine all filtered datasets
merged_data <- do.call(rbind, filtered_datasets)

# Sort by location and date
merged_data <- merged_data[order(merged_data$location, merged_data$date), ]

# Print summary of merged dataset
cat("\nMerged dataset summary:\n")
cat("Number of rows:", nrow(merged_data), "\n")
cat("Number of columns:", ncol(merged_data), "\n")
cat("Date range:", min(merged_data$date), "to", max(merged_data$date), "\n")

# Save merged dataset
write.csv(merged_data, "2016_2024.csv", row.names = FALSE, fileEncoding = "UTF-8")