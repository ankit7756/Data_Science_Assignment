library(tidyverse)

# Load raw 2011 population dataset
population_raw <- read_csv(
  "Obtained_Data/Population_Dataset_2011/Population_Dataset_2011.csv",
  show_col_types = FALSE
)

cat("Loaded population dataset with", nrow(population_raw), "rows\n")

# Clean and aggregate population by short postcode
population_clean <- population_raw %>%
  mutate(Postcode = str_trim(Postcode),
         shortPostcode = str_trim(substr(Postcode, 1, 4))) %>%
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population, na.rm = TRUE), .groups = "drop")

cat("Aggregated into", nrow(population_clean), "unique short postcodes\n")

# Apply annual growth rates to mutate population estimates from 2012 to 2025
population_mutated <- population_clean %>%
  mutate(
    Population2012 = Population2011 * 1.00695353132322269,
    Population2013 = Population2012 * 1.00669740535540783,
    Population2014 = Population2013 * 1.00736463978721671,
    Population2015 = Population2014 * 1.00792367505802859,
    Population2016 = Population2015 * 1.00757874492811929,
    Population2017 = Population2016 * 1.00679374473924223,
    Population2018 = Population2017 * 1.00605929132212552,
    Population2019 = Population2018 * 1.00561255390388033,
    Population2020 = Population2019 * 1.00561255390388033,
    Population2021 = Population2020 * 1.005425,
    Population2022 = Population2021 * 1.004920,
    Population2023 = Population2022 * 1.004510,
    Population2024 = Population2023 * 1.004220,
    Population2025 = Population2024 * 1.004000
  )

# Reshape data to long format for easier analysis and joining
population_long <- population_mutated %>%
  pivot_longer(
    cols = starts_with("Population20"),
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "Population")))

# Save the cleaned and mutated population dataset
dir.create("Cleaned_Data", showWarnings = FALSE)
write_csv(population_long, "Cleaned_Data/mutated_population_2011_2025.csv")

cat("Population dataset mutated and saved to Cleaned_Data/mutated_population_2011_2025.csv\n")
