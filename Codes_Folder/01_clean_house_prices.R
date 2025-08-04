# Load necessary libraries for data manipulation and date handling
library(tidyverse)
library(lubridate)

# List all raw house price CSV files from 2021 to 2023
house_files <- list.files(
  "Obtained_Data/House_Prices_Data",
  pattern = "pp-202[1-3].csv",
  full.names = TRUE
)

# Define official Property Price Dataset column names for consistent reading
ppd_cols <- c(
  "TransactionID", "Price", "Date", "Postcode", "PropertyType", "OldNew",
  "Duration", "PAON", "SAON", "Street", "Locality", "Town", 
  "District", "County", "PPDCategory", "RecordStatus"
)

# Read and combine all yearly CSV files into one dataframe
HousePrices <- house_files %>%
  map_df(~ read_csv(.x, col_names = ppd_cols, na = c("", "NA"), show_col_types = FALSE))

# Clean and filter the data for relevant counties and valid entries
cleanHousePrices <- HousePrices %>%
  mutate(
    County = str_trim(toupper(County)),       
    Year = year(Date),                         
    shortPostcode = str_trim(str_sub(Postcode, 1, 4)),  
    Price = as.numeric(Price)                  
  ) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%  
  select(Postcode, shortPostcode, Price, Year, PropertyType, Town, District, County) %>% 
  filter(!is.na(Price), !is.na(Postcode), !is.na(Year)) %>%  
  arrange(County, District, Town, Year)  

dir.create("Cleaned_Data", showWarnings = FALSE)
write.csv(cleanHousePrices, "Cleaned_Data/cleaned_house_prices.csv", row.names = FALSE)

cat("Cleaned house price dataset ready with", nrow(cleanHousePrices), "rows\n")
