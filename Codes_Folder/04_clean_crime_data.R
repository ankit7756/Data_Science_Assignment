# Load necessary library for data manipulation
library(tidyverse)

# Define base folder for raw crime data and output file path
crime_base <- "Obtained_Data/Crime_Data"
output_file <- "Cleaned_Data/cleaned_crime_data.csv"

# List all monthly subfolders containing crime data CSV files
month_folders <- list.dirs(crime_base, full.names = TRUE, recursive = FALSE)

cat("Found", length(month_folders), "monthly folders for crime data\n")

# Function to clean individual crime data files
clean_crime_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Assign County based on filename pattern
  county <- ifelse(str_detect(file_path, "south-yorkshire"), 
                   "SOUTH YORKSHIRE", 
                   "WEST YORKSHIRE")
  
  df_clean <- df %>%
    mutate(
      County = county,
      across(where(is.character), ~ str_trim(.))  # Trim whitespace in all character columns
    ) %>%
    filter(!is.na(Month), !is.na(`Crime type`)) %>% # Remove rows missing key info
    mutate(`Crime ID` = na_if(`Crime ID`, "")) %>%  # Replace empty Crime ID with NA
    filter(!(is.na(Longitude) & is.na(Latitude)))   # Remove rows missing all location info
  
  return(df_clean)
}

# Get all CSV file paths from all monthly folders
all_files <- map(month_folders, ~ list.files(.x, pattern = "\\.csv$", full.names = TRUE)) %>% 
  unlist()

cat("Found", length(all_files), "CSV files in total\n")

# Clean and combine all crime data files into a single dataframe
crime_data_cleaned <- map_df(all_files, clean_crime_file)

# Remove exact duplicate rows if any
crime_data_cleaned <- distinct(crime_data_cleaned)

cat("Cleaned combined dataset with", nrow(crime_data_cleaned), "rows and", ncol(crime_data_cleaned), "columns\n")

# Create output folder and save the cleaned crime dataset
dir.create("Cleaned_Data", showWarnings = FALSE)
write_csv(crime_data_cleaned, output_file)

cat("Cleaned crime dataset saved to", output_file, "\n")
