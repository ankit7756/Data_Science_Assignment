# Load necessary library for data manipulation
library(tidyverse)

# Define the raw broadband speed dataset file path
broadband_file <- "Obtained_Data/BoardBand_Speed_Data/201809_fixed_pc_r03/201805_fixed_pc_performance_r03.csv"

# Read the raw broadband speed CSV file
broadband_raw <- read_csv(broadband_file, show_col_types = FALSE)

cat("Raw broadband speed dataset loaded with", nrow(broadband_raw), "rows and", ncol(broadband_raw), "columns\n")

# Select relevant columns and rename for clarity
broadband_clean <- broadband_raw %>%
  select(
    Postcode = postcode,
    Postcode_Space = postcode_space,
    Avg_Download_Speed_Mbps = `Average download speed (Mbit/s)`,
    Median_Download_Speed_Mbps = `Median download speed (Mbit/s)`,
    Min_Download_Speed_Mbps = `Minimum download speed (Mbit/s)`,
    Max_Download_Speed_Mbps = `Maximum download speed (Mbit/s)`,
    Avg_Upload_Speed_Mbps = `Average upload speed (Mbit/s)`,
    Median_Upload_Speed_Mbps = `Median upload speed (Mbit/s)`,
    Min_Upload_Speed_Mbps = `Minimum upload speed (Mbit/s)`,
    Max_Upload_Speed_Mbps = `Maximum upload speed (Mbit/s)`
  ) %>%
  mutate(
    Postcode = str_to_upper(str_trim(Postcode)),          
    Postcode_Space = str_to_upper(str_trim(Postcode_Space))
  ) %>%
  filter(!is.na(Postcode)) %>%    
  distinct()                      

# Save the cleaned broadband speed dataset
dir.create("Cleaned_Data", showWarnings = FALSE)
write_csv(broadband_clean, "Cleaned_Data/cleaned_broadband_speed.csv")

cat("Cleaned broadband speed dataset saved with", nrow(broadband_clean), "rows and", ncol(broadband_clean), "columns\n")
