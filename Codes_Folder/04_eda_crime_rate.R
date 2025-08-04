# Load the required libraries

library(tidyverse)
library(stringr)
library(fmsb)
library(scales)

# --- Load Data ---
crime_data <- read_csv("Cleaned_Data/cleaned_crime_data.csv", show_col_types = FALSE)
population_data <- read_csv("Cleaned_Data/mutated_population_2011_2025.csv", show_col_types = FALSE)
postcode_lsoa <- read_csv("Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv", show_col_types = FALSE)

# --- Filter for Drug Crimes ---
crime_data <- crime_data %>%
  mutate(
    Year = as.numeric(substr(Month, 1, 4)),
    LSOA_Code = `LSOA code`,
    County = toupper(County)
  ) %>%
  filter(`Crime type` == "Drugs")

# --- Prepare Postcode-to-LSOA Mapping ---
postcode_lsoa <- postcode_lsoa %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[A-Z0-9]+"),
    LSOA_Code = LSOA_Code, 
    District = toupper(District)
  ) %>%
  select(LSOA_Code, shortPostcode, District)

# --- Aggregate Drug Crime Counts by Year, LSOA, County ---
crime_counts <- crime_data %>%
  group_by(County, LSOA_Code, Year) %>%
  summarise(CrimeCount = n(), .groups = "drop") %>%
  left_join(postcode_lsoa, by = "LSOA_Code")

# --- Join with Population Data & Calculate Rate ---
crime_with_pop <- crime_counts %>%
  left_join(population_data, by = c("shortPostcode", "Year")) %>%
  filter(!is.na(Population), Population > 0) %>%
  mutate(CrimeRate_per10k = (CrimeCount / Population) * 10000)

# --- Aggregate by District for Boxplots ---
crime_rates_agg <- crime_with_pop %>%
  group_by(County, District, Year) %>%
  summarise(AvgCrimeRate = mean(CrimeRate_per10k, na.rm = TRUE), .groups = "drop")

# --- Boxplot Visualization: South Yorkshire ---
ggplot(filter(crime_rates_agg, County == "SOUTH YORKSHIRE"),
       aes(x = District, y = AvgCrimeRate)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Distribution of Drug Offense Rate by District - South Yorkshire",
       x = "District", y = "Drug Offense Rate per 10,000") +
  theme_minimal()

# --- Boxplot Visualization: West Yorkshire ---
ggplot(filter(crime_rates_agg, County == "WEST YORKSHIRE"),
       aes(x = District, y = AvgCrimeRate)) +
  geom_boxplot(fill = "lightgreen", outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Distribution of Drug Offense Rate by District - West Yorkshire",
       x = "District", y = "Drug Offense Rate per 10,000") +
  theme_minimal()


# Vehicle Crime - Radar Chart

selected_county <- "SOUTH YORKSHIRE"
selected_year <- 2022
selected_month <- "10"  # October

crime_data_raw <- read_csv("Cleaned_Data/cleaned_crime_data.csv", show_col_types = FALSE) %>%
  mutate(
    Year = as.numeric(substr(Month, 1, 4)),
    County = toupper(County)
  )

postcode_lsoa <- read_csv("Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv", show_col_types = FALSE) %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[A-Z0-9]+"),
    District = toupper(District)
  ) %>%
  select(LSOA_Code = LSOA_Code, shortPostcode, District)

population_data <- read_csv("Cleaned_Data/mutated_population_2011_2025.csv", show_col_types = FALSE)

vehicle_data <- crime_data_raw %>%
  filter(
    County == selected_county,
    `Crime type` == "Vehicle crime",
    Year == selected_year,
    substr(Month, 6, 7) == selected_month
  ) %>%
  left_join(postcode_lsoa, by = c("LSOA code" = "LSOA_Code")) %>%
  left_join(population_data, by = c("shortPostcode", "Year")) %>%
  filter(!is.na(Population), Population > 0) %>%
  group_by(District) %>%
  summarise(VehicleRate = (n() / mean(Population)) * 10000, .groups = "drop")

max_val <- max(vehicle_data$VehicleRate, na.rm = TRUE) * 1.2
min_val <- 0

radar_data <- rbind(
  rep(max_val, nrow(vehicle_data)),
  rep(min_val, nrow(vehicle_data)),
  vehicle_data$VehicleRate
)

colnames(radar_data) <- vehicle_data$District
radar_data <- as.data.frame(radar_data)

fmsb::radarchart(
  radar_data,
  axistype = 1,
  pcol = "red",
  pfcol = alpha("red", 0.4),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = round(seq(min_val, max_val, length.out = 5), 1),
  title = paste("Vehicle Crime Rate per 10k -", selected_county, selected_month, selected_year)
)


# Robbery Rate - Pie Chart (West Yorkshire)

selected_county <- "WEST YORKSHIRE"
selected_year <- 2022
selected_month <- "09"

robbery_data <- crime_data_raw %>%
  filter(
    County == selected_county,
    `Crime type` == "Robbery",
    Year == selected_year,
    substr(Month, 6, 7) == selected_month
  ) %>%
  left_join(postcode_lsoa, by = c("LSOA code" = "LSOA_Code")) %>%
  left_join(population_data, by = c("shortPostcode", "Year")) %>%
  filter(!is.na(Population), Population > 0) %>%
  group_by(District) %>%
  summarise(RobberyRate = (n() / mean(Population)) * 10000, .groups = "drop") %>%
  mutate(
    Percent = RobberyRate / sum(RobberyRate) * 100,
    Label = sprintf("%.1f%%", Percent)
  )

ggplot(robbery_data, aes(x = "", y = RobberyRate, fill = District)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            fontface = "bold", color = "black", size = 4) +
  labs(title = paste("Robbery Rate Distribution -", selected_county, selected_month, selected_year),
       x = NULL, y = NULL, fill = "District") +
  theme_void() +
  theme(legend.position = "right")


# Drug Offense Rate - Line Chart (Both Counties, All Years)

drug_counts <- crime_data_raw %>%
  filter(`Crime type` == "Drugs") %>%
  mutate(Year = as.numeric(substr(Month, 1, 4))) %>%
  left_join(postcode_lsoa, by = c("LSOA code" = "LSOA_Code")) %>%
  group_by(County, shortPostcode, Year) %>%
  summarise(TotalCrimes = n(), .groups = "drop")

drug_data <- drug_counts %>%
  left_join(population_data, by = c("shortPostcode", "Year")) %>%
  filter(!is.na(Population), Population > 0) %>%
  group_by(County, Year) %>%
  summarise(
    TotalCrimes = sum(TotalCrimes),
    TotalPopulation = sum(Population),
    DrugRate = (TotalCrimes / TotalPopulation) * 10000,
    .groups = "drop"
  )

ggplot(drug_data, aes(x = Year, y = DrugRate, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Drug Offense Rate per 10,000 People - Both Counties",
    x = "Year",
    y = "Drug Offense Rate per 10,000",
    color = "County"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "blue", "WEST YORKSHIRE" = "red")) +
  scale_y_continuous(labels = comma) +   # ✅ Proper number formatting
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )
