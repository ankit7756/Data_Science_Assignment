# Load required libraries

library(tidyverse)
library(ggplot2)

# Load cleaned school dataset (filtered version)
school_data <- read_csv("Cleaned_Data/cleaned_school_data_filtered.csv")

# Basic EDA Checks 

cat("Sample Data Preview:\n")
print(head(school_data, 10))

# Structure of data
cat("\nStructure of data:\n")
glimpse(school_data)

# Summary statistics for Attainment8Score
cat("\nSummary statistics for Attainment 8 Score:\n")
print(summary(school_data$Attainment8Score))

# Missing value check
cat("\nNumber of missing values per column:\n")
print(colSums(is.na(school_data)))

# Count of schools per district per year
cat("\nSchools per District per Year:\n")
print(table(school_data$Year, school_data$District))

# Add a column for Yorkshire Region Classification -------
south_yorkshire <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield")
west_yorkshire <- c("Leeds", "Bradford", "Wakefield", "Calderdale", "Kirklees")

school_data <- school_data %>%
  mutate(
    Region = case_when(
      District %in% south_yorkshire ~ "South Yorkshire",
      District %in% west_yorkshire ~ "West Yorkshire",
      TRUE ~ "Other"
    )
  )

# Verify classification
cat("\nNumber of schools by Region:\n")
print(table(school_data$Region))


# Boxplot for Average Attainment 8 Score 2022 – South Yorkshire
south_2022 <- school_data %>%
  filter(Year == 2022, Region == "South Yorkshire")

ggplot(south_2022, aes(x = District, y = Attainment8Score, fill = District)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Attainment 8 Score (2022) - South Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Boxplot for Average Attainment 8 Score 2022 – West Yorkshire
west_2022 <- school_data %>%
  filter(Year == 2022, Region == "West Yorkshire")

ggplot(west_2022, aes(x = District, y = Attainment8Score, fill = District)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Attainment 8 Score (2022) - West Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Line Graph for Yearly Trend of Attainment 8 Score per District
school_data %>%
  group_by(Year, District) %>%
  summarise(Average_Att8 = mean(Attainment8Score), .groups = "drop") %>%
  ggplot(aes(x = Year, y = Average_Att8, color = District, group = District)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Yearly Trend of Average Attainment 8 Score by District - South and West Yorkshire",
    x = "Year",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()