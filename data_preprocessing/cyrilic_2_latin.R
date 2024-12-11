# Create location name mapping
location_mapping <- c(
  "БЕОГРАД - НОВИ БЕОГРАД" = "BELGRADE - NEW BELGRADE",
  "БЕОГРАД - ЗЕЛЕНО БРДО" = "BELGRADE - ZELENO BRDO",
  "БЕОГРАД - ЧУКАРИЦА" = "BELGRADE - CUKARICA",
  "БЕЧЕЈ" = "BECEJ",
  "ВАЉЕВО" = "VALJEVO",
  "ВРАЊЕ" = "VRANJE",
  "ВРБАС" = "VRBAS",
  "ВРШАЦ" = "VRSAC",
  "ЗАЈЕЧАР" = "ZAJECAR",
  "ЗЛАТИБОР" = "ZLATIBOR",
  "ЗРЕЊАНИН" = "ZRENJANIN",
  "КАЊИЖА" = "KANJIZA",
  "КИКИНДА" = "KIKINDA",
  "КРАГУЈЕВАЦ" = "KRAGUJEVAC",
  "КРАЉЕВО" = "KRALJEVO",
  "КРУШЕВАЦ" = "KRUSEVAC",
  "КУЛА" = "KULA",
  "ЛОЗНИЦА" = "LOZNICA",
  "НИШ" = "NIS",
  "НОВИ ПАЗАР" = "NOVI PAZAR",
  "ОБРЕНОВАЦ" = "OBRENOVAC",
  "ПАНЧЕВО" = "PANCEVO",
  "ПОЖАРЕВАЦ" = "POZAREVAC",
  "СОКОБАЊА" = "SOKOBANJA",
  "СОМБОР" = "SOMBOR",
  "СРЕМСКА МИТРОВИЦА" = "SREMSKA MITROVICA",
  "СУБОТИЦА" = "SUBOTICA",
  "ХАЈДУКОВО" = "HAJDUKOVO",
  "ЧАЧАК" = "CACAK"
)

# Create a function to replace location names in a dataset
replace_locations <- function(data) {
  data$location <- as.character(data$location)  # Ensure location is character type
  data$location <- location_mapping[data$location]
  return(data)
}

# Process all files
years <- 2016:2024

for (year in years) {
  # Read file
  filename <- paste0(year, "_cyr.csv")
  data <- read.csv(filename, encoding = "UTF-8")
  
  # Replace location names
  data <- replace_locations(data)
  
  # Save modified file
  output_filename <- paste0(year, ".csv")
  write.csv(data, output_filename, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Print confirmation
  cat(sprintf("Processed %s -> %s\n", filename, output_filename))
}