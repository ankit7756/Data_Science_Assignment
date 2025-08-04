# Load necessary library
library(tidyverse)

# Load cleaned datasets
broadband_clean <- read_csv("Cleaned_Data/cleaned_broadband_speed.csv", show_col_types = FALSE)
house_prices_clean <- read_csv("Cleaned_Data/cleaned_house_prices.csv", show_col_types = FALSE)

cat("Broadband dataset:", nrow(broadband_clean), "rows,", ncol(broadband_clean), "columns\n")
cat("House prices dataset:", nrow(house_prices_clean), "rows,", ncol(house_prices_clean), "columns\n")

# Prepare postcodes for joining
broadband_clean <- broadband_clean %>%
  mutate(Postcode_nospace = gsub(" ", "", Postcode))

house_prices_clean <- house_prices_clean %>%
  mutate(Postcode_nospace = gsub(" ", "", Postcode))

# Join broadband with location info from house prices
broadband_with_loc <- broadband_clean %>%
  inner_join(
    house_prices_clean %>% select(Postcode_nospace, Town, District, County),
    by = "Postcode_nospace"
  )

cat("Joined dataset created with", nrow(broadband_with_loc), "rows\n")

# Basic EDA
glimpse(broadband_with_loc)
summary(broadband_with_loc$Avg_Download_Speed_Mbps)
colSums(is.na(broadband_with_loc))

# Top 10 fastest towns
broadband_with_loc %>%
  group_by(Town) %>%
  summarise(mean_speed = mean(Avg_Download_Speed_Mbps, na.rm = TRUE)) %>%
  arrange(desc(mean_speed)) %>%
  head(10)

# Top 10 slowest towns
broadband_with_loc %>%
  group_by(Town) %>%
  summarise(mean_speed = mean(Avg_Download_Speed_Mbps, na.rm = TRUE)) %>%
  arrange(mean_speed) %>%
  head(10)

# Boxplot of Download Speeds by District (South Yorkshire)
ggplot(filter(broadband_with_loc, County == "SOUTH YORKSHIRE"), 
       aes(x = District, y = Avg_Download_Speed_Mbps)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.2) +
  coord_flip() +
  labs(title = "Distribution of Broadband Download Speeds by District - South Yorkshire",
       x = "District", y = "Average Download Speed (Mbps)") +
  theme_minimal()

# Boxplot of Download Speeds by District (West Yorkshire)
ggplot(filter(broadband_with_loc, County == "WEST YORKSHIRE"), 
       aes(x = District, y = Avg_Download_Speed_Mbps)) +
  geom_boxplot(fill = "lightgreen", outlier.alpha = 0.2) +
  coord_flip() +
  labs(title = "Distribution of Broadband Download Speeds by District - West Yorkshire",
       x = "District", y = "Average Download Speed (Mbps)") +
  theme_minimal()

# Bar Chart: Average Download Speed by Town per County
town_speed <- broadband_with_loc %>%
  group_by(County, Town) %>%
  summarise(mean_speed = mean(Avg_Download_Speed_Mbps, na.rm = TRUE), .groups = "drop")

# Top 10 and Bottom 10 towns per county
town_speed_filtered <- town_speed %>%
  group_by(County) %>%
  arrange(desc(mean_speed)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    town_speed %>%
      group_by(County) %>%
      arrange(mean_speed) %>%
      slice_head(n = 10)
  ) %>%
  ungroup()

# South Yorkshire Town Speeds
ggplot(filter(town_speed_filtered, County == "SOUTH YORKSHIRE"), 
       aes(x = reorder(Town, mean_speed), y = mean_speed, fill = County)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Download Speeds by Town - South Yorkshire",
       x = "Town", y = "Mean Download Speed (Mbps)") +
  theme_minimal()

# West Yorkshire Town Speeds
ggplot(filter(town_speed, County == "WEST YORKSHIRE"), 
       aes(x = reorder(Town, mean_speed), y = mean_speed, fill = County)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Download Speeds by Town - West Yorkshire",
       x = "Town", y = "Mean Download Speed (Mbps)") +
  theme_minimal()
