# Linear Modeling 1: House Price vs Download Speed for both counties

library(tidyverse)
library(scales)

# Load cleaned datasets
house_prices <- read_csv("Cleaned_Data/cleaned_house_prices.csv")
broadband <- read_csv("Cleaned_Data/cleaned_broadband_speed.csv")

# Extract outward postcode (short postcode) from broadband
# e.g., "S73 8TU" -> "S73"
broadband <- broadband %>%
  mutate(shortPostcode = str_extract(Postcode_Space, "^[A-Z0-9]+"))

# Aggregate broadband speed by short postcode
broadband_summary <- broadband %>%
  group_by(shortPostcode) %>%
  summarise(AvgDownloadSpeed = mean(Avg_Download_Speed_Mbps, na.rm = TRUE))

# Merge datasets and filter to South & West Yorkshire
merged_data <- house_prices %>%
  left_join(broadband_summary, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  filter(!is.na(Price), !is.na(AvgDownloadSpeed))

cat("Rows after merging:", nrow(merged_data), "\n")
print(head(merged_data, 5))

# Compute correlation
correlation <- cor(merged_data$Price, merged_data$AvgDownloadSpeed)
cat("\n🔹 Correlation between House Price and Download Speed:", round(correlation, 3), "\n")

# Fit linear model
lm_model <- lm(Price ~ AvgDownloadSpeed, data = merged_data)
cat("\n🔹 Linear Model Summary:\n")
print(summary(lm_model))

# Visualization with clean y-axis and county-wise colors
plot_data <- merged_data %>%
  group_by(County, District) %>%
  summarise(
    AvgPrice = mean(Price, na.rm = TRUE),
    AvgSpeed = mean(AvgDownloadSpeed, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_data, aes(x = AvgSpeed, y = AvgPrice, color = County)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average House Price vs Download Speed by District",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (£)"
  ) +
  theme_minimal()



# Linear Modeling 2: House Price vs Drug Rates (2023) per 10000 people for both counties

library(tidyverse)
library(scales)

# Load datasets
crime_data <- read_csv("Cleaned_Data/cleaned_crime_data.csv")
house_prices <- read_csv("Cleaned_Data/cleaned_house_prices.csv")
population <- read_csv("Cleaned_Data/mutated_population_2011_2025.csv")
postcode_to_lsoa <- read_csv("Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv")

# Filter for 2023 Drug Crimes
drug_crime_2023 <- crime_data %>%
  filter(grepl("^2023", Month), `Crime type` == "Drugs") %>%
  group_by(`LSOA code`) %>%
  summarise(DrugOffenses = n(), .groups = "drop") %>%
  rename(LSOA = `LSOA code`)

# Prepare postcode to LSOA mapping with shortPostcode
postcode_to_lsoa <- postcode_to_lsoa %>%
  mutate(shortPostcode = str_extract(Postcode, "^[A-Z0-9]+")) %>%
  select(LSOA_Code, shortPostcode) %>%
  rename(LSOA = LSOA_Code)

# Map LSOA → shortPostcode and aggregate drug crimes
drug_crime_by_postcode <- drug_crime_2023 %>%
  left_join(postcode_to_lsoa, by = "LSOA") %>%
  filter(!is.na(shortPostcode)) %>%
  group_by(shortPostcode) %>%
  summarise(DrugOffenses = sum(DrugOffenses, na.rm = TRUE), .groups = "drop")

# Join population (2023) to compute per 10,000 rate
drug_crime_pop <- drug_crime_by_postcode %>%
  left_join(population %>% filter(Year == 2023), by = "shortPostcode") %>%
  mutate(DrugRate_10000 = (DrugOffenses / Population) * 10000) %>%
  filter(!is.na(DrugRate_10000))

# Aggregate house prices by shortPostcode (use 2023)
house_price_2023 <- house_prices %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Merge house prices with drug crime rates
merged_data2 <- house_price_2023 %>%
  left_join(drug_crime_pop, by = "shortPostcode") %>%
  filter(!is.na(DrugRate_10000))

cat("Rows in merged dataset:", nrow(merged_data2), "\n")
print(head(merged_data2))

# Compute correlation
correlation2 <- cor(merged_data2$AvgPrice, merged_data2$DrugRate_10000, use = "complete.obs")
cat("\n🔹 Correlation between House Price and Drug Rate per 10k (2023):", round(correlation2, 3), "\n")

# Fit linear model
lm_model2 <- lm(AvgPrice ~ DrugRate_10000, data = merged_data2)
cat("\n🔹 Linear Model Summary:\n")
print(summary(lm_model2))

ggplot(merged_data2, aes(x = DrugRate_10000, y = AvgPrice, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "House Price vs Drug Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average House Price (£)"
  ) +
  scale_y_continuous(labels = comma) +  # Show normal numbers on Y-axis
  scale_x_continuous(labels = comma) +  # Show normal numbers on X-axis
  theme_minimal()


# Linear Modeling 3: Attainment 8 score vs House Price for both counties 

library(tidyverse)
library(scales)

# Load datasets
house_prices <- read_csv("Cleaned_Data/cleaned_house_prices.csv")
school_data <- read_csv("Cleaned_Data/cleaned_school_data_filtered.csv")

# Compute average house price per district per year
avg_house_price <- house_prices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(District, County, Year) %>%
  summarise(AvgHousePrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Standardize District names to uppercase
house_prices <- house_prices %>%
  mutate(District = toupper(District))

school_data <- school_data %>%
  mutate(District = toupper(District))

# Merge school attainment data with average house prices
school_price <- school_data %>%
  filter(Year %in% c(2021, 2022, 2023, 2024)) %>%  # Use available years
  inner_join(avg_house_price, by = c("District", "Year"))

# Check merged data
cat("Rows after merging:", nrow(school_price), "\n")
print(head(school_price))

# Remove NA for correlation & model
school_price <- school_price %>% filter(!is.na(Attainment8Score), !is.na(AvgHousePrice))

# Compute correlation
correlation <- cor(school_price$Attainment8Score, school_price$AvgHousePrice)
cat("\n🔹 Correlation between Attainment 8 Score and House Price:", round(correlation, 3), "\n")

# Linear model
lm_model <- lm(AvgHousePrice ~ Attainment8Score, data = school_price)
cat("\n🔹 Linear Model Summary:\n")
print(summary(lm_model))

# Scatter plot with regression line
ggplot(school_price, aes(x = Attainment8Score, y = AvgHousePrice, color = County)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_y_continuous(labels = scales::comma) +   # Avoid scientific notation
  labs(
    title = "Attainment 8 Score vs Average House Price",
    x = "Attainment 8 Score",
    y = "Average House Price (£)"
  ) +
  theme_minimal()


# Linear Modeling 4: Attainment 8 scores vs Drug Offense rates per 10000 people in 2023

library(tidyverse)
library(scales)

# Load cleaned datasets
house_prices <- read_csv("Cleaned_Data/cleaned_house_prices.csv")
school_data <- read_csv("Cleaned_Data/cleaned_school_data_filtered.csv")
crime_data <- read_csv("Cleaned_Data/cleaned_crime_data.csv")
population <- read_csv("Cleaned_Data/mutated_population_2011_2025.csv")
postcode_lsoa <- read_csv("Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv")

# Ensure uniform case for joins
postcode_lsoa <- postcode_lsoa %>% mutate(District = toupper(District))
school_data <- school_data %>% mutate(District = toupper(District))

# Prepare 2023 Drug Crime Data
drug_crime_2023 <- crime_data %>%
  filter(str_detect(Month, "2023"), `Crime type` == "Drugs") %>%
  group_by(`LSOA code`, County) %>%
  summarise(DrugOffenses = n(), .groups = "drop")

# Compute drug offense rate per 10,000 population
# First, link LSOA to short postcode for population mapping
population_2023 <- population %>% filter(Year == 2023)

drug_crime_2023 <- drug_crime_2023 %>%
  left_join(postcode_lsoa %>% select(Postcode, LSOA_Code = `LSOA_Code`), 
            by = c("LSOA code" = "LSOA_Code")) %>%
  mutate(shortPostcode = str_extract(Postcode, "^[A-Z0-9]+")) %>%
  left_join(population_2023, by = c("shortPostcode")) %>%
  mutate(
    DrugRate_10000 = (DrugOffenses / Population) * 10000
  ) %>%
  filter(!is.na(DrugRate_10000))

# Aggregate Drug Rates by District for school merging
district_drug_rate <- drug_crime_2023 %>%
  left_join(postcode_lsoa %>% select(Postcode, District) %>% 
              mutate(District = toupper(District)),
            by = "Postcode") %>%
  group_by(District, County) %>%
  summarise(AvgDrugRate_10000 = mean(DrugRate_10000, na.rm = TRUE), .groups = "drop")

# Merge with school Attainment 8 scores for 2023
attainment_drug <- school_data %>%
  filter(Year == 2023) %>%
  inner_join(district_drug_rate, by = "District") %>%
  filter(!is.na(Attainment8Score), !is.na(AvgDrugRate_10000))

# Correlation and Linear Model
correlation <- cor(attainment_drug$Attainment8Score, attainment_drug$AvgDrugRate_10000)
cat("\n🔹 Correlation between Attainment 8 Score and Drug Offense Rate:",
    round(correlation, 3), "\n")

lm_model <- lm(Attainment8Score ~ AvgDrugRate_10000, data = attainment_drug)
cat("\n🔹 Linear Model Summary:\n")
print(summary(lm_model))

# Scatter plot with regression line
ggplot(attainment_drug, aes(x = AvgDrugRate_10000, y = Attainment8Score, color = County)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate (2023)",
    x = "Drug Offense Rate per 10,000 People (2023)",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()


# Linear Modeling 5: Average Download speed vs Drug Offense Rate per 10000 people for both counties 

library(tidyverse)
library(scales)

# Load datasets
broadband <- read_csv("Cleaned_Data/cleaned_broadband_speed.csv")
crime_data <- read_csv("Cleaned_Data/cleaned_crime_data.csv")
population <- read_csv("Cleaned_Data/mutated_population_2011_2025.csv")
postcode_lsoa <- read_csv("Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv")

# Prepare broadband: Extract short postcode and aggregate average download speed by short postcode
broadband <- broadband %>%
  mutate(shortPostcode = str_extract(Postcode_Space, "^[A-Z0-9]+"))

broadband_summary <- broadband %>%
  group_by(shortPostcode) %>%
  summarise(AvgDownloadSpeed = mean(Avg_Download_Speed_Mbps, na.rm = TRUE), .groups = "drop")

# Prepare drug crime data for 2023
drug_crime_2023 <- crime_data %>%
  filter(str_detect(Month, "2023"), `Crime type` == "Drugs") %>%
  group_by(`LSOA code`, County) %>%
  summarise(DrugOffenses = n(), .groups = "drop")

# Link LSOA codes to Postcodes & extract short postcode for joining broadband and population
# Select distinct to reduce duplicates
postcode_lsoa_distinct <- postcode_lsoa %>%
  distinct(Postcode, .keep_all = TRUE) %>%
  mutate(shortPostcode = str_extract(Postcode, "^[A-Z0-9]+"),
         District = toupper(District))

# Join drug crime with postcode to get shortPostcode
drug_crime_2023 <- drug_crime_2023 %>%
  left_join(postcode_lsoa_distinct %>% select(LSOA_Code = `LSOA_Code`, shortPostcode), 
            by = c("LSOA code" = "LSOA_Code"))

# Join population for 2023 by short postcode
population_2023 <- population %>%
  filter(Year == 2023)

drug_crime_2023 <- drug_crime_2023 %>%
  left_join(population_2023, by = "shortPostcode") %>%
  filter(!is.na(Population), Population > 0) %>%
  mutate(DrugRate_10000 = (DrugOffenses / Population) * 10000) %>%
  filter(!is.na(DrugRate_10000))

# Aggregate drug rate and average broadband speed by short postcode and County
drug_broadband <- drug_crime_2023 %>%
  left_join(broadband_summary, by = "shortPostcode") %>%
  filter(!is.na(AvgDownloadSpeed), County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(
    AvgDrugRate_10000 = mean(DrugRate_10000, na.rm = TRUE),
    AvgDownloadSpeed = mean(AvgDownloadSpeed, na.rm = TRUE),
    .groups = "drop"
  )

# Compute correlation
correlation <- cor(drug_broadband$AvgDownloadSpeed, drug_broadband$AvgDrugRate_10000)
cat("\n🔹 Correlation between Average Download Speed and Drug Offense Rate:", round(correlation, 3), "\n")

# Linear model
lm_model <- lm(AvgDownloadSpeed ~ AvgDrugRate_10000, data = drug_broadband)
cat("\n🔹 Linear Model Summary:\n")
print(summary(lm_model))

# Scatter plot with regression line
ggplot(drug_broadband, aes(x = AvgDrugRate_10000, y = AvgDownloadSpeed, color = County)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7, size = 2) +  # jitter on x to reduce overlap
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Average Download Speed vs Drug Offense Rate (2023)",
    x = "Drug Offense Rate per 10,000 People (2023)",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()


# Linear Modeling 6: Average download speed vs Attainment 8 score for both counties 

library(tidyverse)
library(scales)

# Load cleaned datasets
house_prices <- read_csv("Cleaned_Data/cleaned_house_prices.csv")
school_data <- read_csv("Cleaned_Data/cleaned_school_data_filtered.csv")
broadband <- read_csv("Cleaned_Data/cleaned_broadband_speed.csv")

# Prepare broadband data: extract short postcode and summarize average download speed by short postcode
broadband_summary <- broadband %>%
  mutate(shortPostcode = str_extract(Postcode_Space, "^[A-Z0-9]+")) %>%
  group_by(shortPostcode) %>%
  summarise(AvgDownloadSpeed = mean(Avg_Download_Speed_Mbps, na.rm = TRUE), .groups = "drop")

# Compute average Attainment 8 score per district and year (already filtered if needed)
avg_attainment <- school_data %>%
  filter(Year %in% c(2021, 2022, 2023, 2024)) %>%
  mutate(District = toupper(District)) %>%
  group_by(District, Year) %>%
  summarise(AvgAttainment8Score = mean(Attainment8Score, na.rm = TRUE), .groups = "drop")

# Compute average download speed by district and year
# First, join house prices to get District and Year with shortPostcode
house_prices <- house_prices %>%
  mutate(District = toupper(District))

# Join broadband summary with house prices by shortPostcode
broadband_district <- house_prices %>%
  select(shortPostcode, District, Year, County) %>%
  distinct() %>%
  left_join(broadband_summary, by = "shortPostcode") %>%
  filter(!is.na(AvgDownloadSpeed), County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(District, Year, County) %>%
  summarise(AvgDownloadSpeed = mean(AvgDownloadSpeed, na.rm = TRUE), .groups = "drop")

# Merge average attainment and broadband by District and Year
attainment_broadband <- avg_attainment %>%
  inner_join(broadband_district, by = c("District", "Year")) %>%
  filter(!is.na(AvgAttainment8Score), !is.na(AvgDownloadSpeed))

# Calculate correlation
correlation <- cor(attainment_broadband$AvgDownloadSpeed, attainment_broadband$AvgAttainment8Score)
cat("\n🔹 Correlation between Average Download Speed and Attainment 8 Score:", round(correlation, 3), "\n")

# Fit linear model
lm_model <- lm(AvgDownloadSpeed ~ AvgAttainment8Score, data = attainment_broadband)
cat("\n🔹 Linear Model Summary:\n")
print(summary(lm_model))

# Scatter plot with regression line
ggplot(attainment_broadband, aes(x = AvgAttainment8Score, y = AvgDownloadSpeed, color = County)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average Download Speed vs Attainment 8 Score (2021-2024)",
    x = "Average Attainment 8 Score",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

