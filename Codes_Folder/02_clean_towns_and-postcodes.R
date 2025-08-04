# Load necessary library for data manipulation
library(tidyverse)

# Define the raw postcode to LSOA dataset file path
raw_postcode_file <- "Obtained_Data/PostCode_To_LSOA/PostCode_To_LSOA.csv"

# Read the raw CSV file
postcode_raw <- read_csv(raw_postcode_file, show_col_types = FALSE)

cat("Raw postcode dataset loaded with", nrow(postcode_raw), "rows and", ncol(postcode_raw), "columns\n")

# Define target districts in South and West Yorkshire for filtering
target_districts <- c(
  "BARNSLEY", "DONCASTER", "ROTHERHAM", "SHEFFIELD",
  "LEEDS", "WAKEFIELD", "KIRKLEES", "CALDERDALE", "BRADFORD"
)

# Clean and filter the postcode dataset to include only relevant districts
postcode_clean <- postcode_raw %>%
  select(
    Postcode = pcds,
    LSOA_Code = lsoa11cd,
    LSOA_Name = lsoa11nm,
    MSOA_Name = msoa11nm,
    District = ladnm
  ) %>%
  distinct() %>%
  filter(!is.na(Postcode)) %>%
  mutate(
    Postcode = str_trim(Postcode),
    District = str_to_upper(str_trim(District)),
    LSOA_Name = str_trim(LSOA_Name),
    MSOA_Name = str_trim(MSOA_Name),
    shortPostcode = str_extract(Postcode, "^[A-Z]{1,2}[0-9][0-9A-Z]?")
  ) %>%
  filter(District %in% target_districts)

# Save the cleaned postcode dataset for Yorkshire districts
dir.create("Cleaned_Data", showWarnings = FALSE)
write_csv(postcode_clean, "Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv")

cat("Cleaned Yorkshire-only postcode dataset saved with", nrow(postcode_clean), 
    "rows and", ncol(postcode_clean), "columns\n")
