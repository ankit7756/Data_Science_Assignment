# Load necessary libraries

library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

# Load cleaned dataset
house_data <- read_csv("Cleaned_Data/cleaned_house_prices.csv", show_col_types = FALSE)

cat("Rows:", nrow(house_data), "\n")
cat("Columns:", ncol(house_data), "\n")
glimpse(house_data)

# Check for missing values
missing_summary <- colSums(is.na(house_data))
print(missing_summary)

# Summary statistics
summary(select(house_data, Price, Year))

# Unique values in key categorical variables
cat("Counties:\n"); print(unique(house_data$County))
cat("Districts sample:\n"); print(unique(house_data$District) %>% head(10))

# Histogram: Overall house price distribution
ggplot(house_data, aes(x = Price)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(title = "Distribution of House Prices", x = "Price (£)", y = "Count") +
  theme_minimal()

# Boxplot: Price distribution by County
ggplot(house_data, aes(x = County, y = Price, fill = County)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "House Price Distribution by County", x = "County", y = "Price (£)") +
  theme_minimal() +
  theme(legend.position = "none")

# Average price by Year and County
avg_price_year_county <- house_data %>%
  group_by(Year, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# District-wise average price for 2023
avg_price_2023 <- house_data %>%
  filter(Year == 2023) %>%
  group_by(District, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  arrange(County, desc(avg_price))

# Average price per District for all years
avg_price_district_county <- house_data %>%
  group_by(District, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# Line plot: Average house prices 2021-2023 by District and County
avg_price_year_county <- house_data %>%
  filter(Year %in% 2021:2023) %>%
  group_by(Year, County, District) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

ggplot(avg_price_year_county, aes(x = Year, y = avg_price, color = District, group = District)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ County) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(breaks = unique(avg_price_year_county$Year)) +
  labs(
    title = "Average House Prices (2021-2023) by District and County",
    subtitle = "Line plot showing average prices per district across years for South & West Yorkshire",
    x = "Year", y = "Average Price (£)", color = "District"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Bar chart: Average house prices in 2023 by District (South Yorkshire)
south_yorkshire_2023 <- avg_price_2023 %>% filter(County == "SOUTH YORKSHIRE")

ggplot(south_yorkshire_2023, aes(x = District, y = avg_price, fill = District)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Average House Prices in 2023 - South Yorkshire", x = "District", y = "Average Price (£)") +
  theme_minimal()

# Bar chart: Average house prices in 2023 by District (West Yorkshire)
west_yorkshire_2023 <- avg_price_2023 %>% filter(County == "WEST YORKSHIRE")

ggplot(west_yorkshire_2023, aes(x = District, y = avg_price, fill = District)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Average House Prices in 2023 - West Yorkshire", x = "District", y = "Average Price (£)") +
  theme_minimal()

# Boxplot: Price distribution by District (South Yorkshire)
south_yorkshire <- house_data %>% filter(County == "SOUTH YORKSHIRE")

ggplot(south_yorkshire, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
  scale_y_log10(labels = scales::comma_format()) +
  coord_flip() +
  labs(title = "House Price Distribution by District - South Yorkshire", x = "District", y = "Price (£, log scale)") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot: Price distribution by District (West Yorkshire)
west_yorkshire <- house_data %>% filter(County == "WEST YORKSHIRE")

ggplot(west_yorkshire, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
  scale_y_log10(labels = scales::comma_format()) +
  coord_flip() +
  labs(title = "House Price Distribution by District - West Yorkshire", x = "District", y = "Price (£, log scale)") +
  theme_minimal() +
  theme(legend.position = "none")
