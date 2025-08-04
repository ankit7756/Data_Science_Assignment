library(tidyverse)

house_data <- read_csv("Cleaned_Data/cleaned_house_prices.csv", show_col_types = FALSE)
broadband_data <- read_csv("Cleaned_Data/cleaned_broadband_speed.csv", show_col_types = FALSE)
crime_data <- read_csv("Cleaned_Data/cleaned_crime_data.csv", show_col_types = FALSE)
population_data <- read_csv("Cleaned_Data/mutated_population_2011_2025.csv", show_col_types = FALSE)
postcode_lsoa <- read_csv("Cleaned_Data/cleaned_postcode_to_lsoa_yorkshire.csv", show_col_types = FALSE)
school_data <- read_csv("Cleaned_Data/cleaned_school_data_filtered.csv", show_col_types = FALSE)

house_agg <- house_data %>%
  group_by(District, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

broadband_agg <- broadband_data %>%
  mutate(Postcode_Space = str_to_upper(str_trim(Postcode_Space))) %>%
  inner_join(postcode_lsoa %>% select(Postcode, District),
             by = c("Postcode_Space" = "Postcode")) %>%
  group_by(District) %>%
  summarise(avg_bandwidth = mean(Avg_Download_Speed_Mbps, na.rm = TRUE), .groups = "drop")

crime_agg <- crime_data %>%
  mutate(Year = as.numeric(substr(Month, 1, 4))) %>%
  filter(Year == 2024) %>%                           # 🔹 Filter early
  group_by(`LSOA code`, County) %>%
  summarise(total_crimes = n(), .groups = "drop") %>%
  left_join(postcode_lsoa %>% select(LSOA_Code, District),
            by = c("LSOA code" = "LSOA_Code")) %>%
  group_by(District, County) %>%
  summarise(avg_crime = mean(total_crimes, na.rm = TRUE), .groups = "drop")

pop_agg <- postcode_lsoa %>%
  select(shortPostcode, District) %>%
  distinct() %>%
  left_join(population_data %>% filter(Year == 2024), by = "shortPostcode") %>%
  group_by(District) %>%
  summarise(avg_2024 = mean(Population, na.rm = TRUE), .groups = "drop")

school_agg <- school_data %>%
  mutate(District = str_to_upper(str_trim(District))) %>%   # Normalize case
  group_by(District) %>%
  summarise(avg_attainment8 = mean(Attainment8Score, na.rm = TRUE), .groups = "drop")

final_df <- house_agg %>%
  left_join(broadband_agg, by = "District") %>%
  left_join(crime_agg, by = c("District", "County")) %>%
  left_join(pop_agg, by = "District") %>%
  left_join(school_agg, by = "District")

bw_mean <- mean(final_df$avg_bandwidth, na.rm = TRUE)
school_mean <- mean(final_df$avg_attainment8, na.rm = TRUE)

final_df <- final_df %>%
  mutate(
    avg_bandwidth = if_else(is.na(avg_bandwidth), bw_mean, avg_bandwidth),
    avg_attainment8 = if_else(is.na(avg_attainment8), school_mean, avg_attainment8)
  )

normalize <- function(x) { 
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))  # avoid divide by 0
  (x - min(x, na.rm = TRUE)) / rng
}

final_df <- final_df %>%
  mutate(
    norm_bandwidth = normalize(avg_bandwidth),        # higher = better
    norm_price = 1 - normalize(avg_price),            # lower = better
    norm_rate = 1 - normalize(avg_crime),             # lower = better
    norm_pop = normalize(avg_2024),                   # higher = better
    norm_school = normalize(avg_attainment8),         # higher = better
    score = 0.25*norm_bandwidth + 
      0.25*norm_rate + 
      0.25*norm_price + 
      0.15*norm_pop + 
      0.10*norm_school,
    rating_score_scaled = 1 + 9*normalize(score),
    rating_1_to_10 = round(rating_score_scaled, 0),
    recommendation = case_when(
      rating_1_to_10 >= 9 ~ "Highly Recommended",
      rating_1_to_10 >= 6 ~ "Recommended",
      TRUE ~ "Not Recommended"
    )
  ) %>%
  arrange(desc(rating_1_to_10))

write_csv(final_df, "Cleaned_Data/final_recommendation.csv")
print(final_df)
