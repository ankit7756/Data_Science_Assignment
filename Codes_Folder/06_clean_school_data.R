library(tidyverse)

# Define base path and year-folder mapping
base_path <- "Obtained_Data/School_Dataset"
year_folders <- c(
  "Performance_Tables_2021-2022" = 2022,
  "Performance_Tables_2022-2023" = 2023,
  "Performance_Tables_2023-2024" = 2024
)

# Functions to load KS4 performance and school info data, adding Year column
load_ks4 <- function(folder, year) {
  read_csv(file.path(base_path, folder, "filtered_england_ks4final.csv"), show_col_types = FALSE) %>%
    mutate(Year = year)
}

load_info <- function(folder, year) {
  read_csv(file.path(base_path, folder, "filtered_england_school_information.csv"), show_col_types = FALSE) %>%
    mutate(Year = year)
}

# Load and combine KS4 and School Info data across years
ks4_all <- map2_dfr(names(year_folders), year_folders, load_ks4)
school_info_all <- map2_dfr(names(year_folders), year_folders, load_info)

# Detect the average Attainment 8 score column
att8_candidates <- names(ks4_all)[str_detect(tolower(names(ks4_all)), "att8")]
cat("Detected Attainment 8 columns:\n")
print(att8_candidates)

att8_col <- names(ks4_all)[str_detect(tolower(names(ks4_all)), "att8scr|att8avg")][1]

if (is.na(att8_col)) {
  stop("Could not find average Attainment 8 Score column. Please check KS4 file column names.")
}

cat("Using Attainment 8 Score column:", att8_col, "\n")

# Merge KS4 and school info by URN and Year
merged_school_data <- ks4_all %>%
  left_join(school_info_all, by = c("URN", "Year"))

# Filter for South & West Yorkshire districts
yorkshire_districts <- c(
  "Barnsley", "Doncaster", "Rotherham", "Sheffield",
  "Leeds", "Bradford", "Wakefield", "Calderdale", "Kirklees"
)

merged_school_data <- merged_school_data %>%
  filter(LANAME %in% yorkshire_districts)

# Clean and select essential columns
school_cleaned <- merged_school_data %>%
  rename(SchoolName = SCHNAME.x) %>%
  select(Year, District = LANAME, URN, SchoolName, Attainment8Score = all_of(att8_col)) %>%
  filter(!is.na(Attainment8Score), !Attainment8Score %in% c("SUPP", "LOWCOV")) %>%
  mutate(Attainment8Score = as.numeric(Attainment8Score)) %>%
  filter(!is.na(Attainment8Score), Attainment8Score > 0)

# Save full cleaned dataset
write_csv(school_cleaned, "Cleaned_Data/cleaned_school_data_full.csv")

# Create filtered dataset removing outliers with scores below 15
school_filtered <- school_cleaned %>%
  filter(Attainment8Score >= 15)

# Save filtered dataset
write_csv(school_filtered, "Cleaned_Data/cleaned_school_data_filtered.csv")

# Output summaries and previews
cat("Full cleaned data saved to: Cleaned_Data/cleaned_school_data_full.csv\n")
cat("Rows in full cleaned data:", nrow(school_cleaned), "\n\n")

cat("Filtered cleaned data (Attainment8Score >= 15) saved to: Cleaned_Data/cleaned_school_data_filtered.csv\n")
cat("Rows in filtered cleaned data:", nrow(school_filtered), "\n\n")

cat("Sample preview of filtered data:\n")
print(head(school_filtered, 10))
