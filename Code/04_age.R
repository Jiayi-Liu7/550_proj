# Age and Experience Analysis
# Coder 4 - Lihui Duan

# Load required packages
library(readr)    # For reading CSV files
library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization

# ------------------------------
# 1. Read the cleaned NBA data from CSV file.
# ------------------------------

nba <- read_csv("Output/nba_clean.csv")

# Print first few rows for verification
print("Preview of cleaned NBA data:")
print(head(nba))

# ------------------------------
# 2. Create visualizations for age distribution.
# Includes density plot.
# ------------------------------

# 2 Density Plot of Age
age_density <- ggplot(nba, aes(x = Age)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Age",
       x = "Age", y = "Density") +
  theme_minimal()

ggsave("Output/age_density.png", age_density, width = 8, height = 6)

# ------------------------------
# 3. Investigate relationship between age and performance.
# We use points per 36 minutes (PTS) as primary performance metric.
# ------------------------------

age_vs_pts <- ggplot(nba, aes(x = Age, y = PTS)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Age vs. Points per 36 Minutes",
       x = "Age", y = "Points") +
  theme_minimal()

ggsave("Output/age_vs_points.png", age_vs_pts, width = 8, height = 6)

# ------------------------------
# 4. Compare performance across age groups.
# Create boxplots of points by age intervals.
# ------------------------------

# 4.1 Create age group variable
nba <- nba %>%
  mutate(age_group = cut(Age, breaks = c(18, 22, 26, 30, 34, 40),
                         labels = c("18-22", "23-26", "27-30", "31-34", "35+"),
                         right = FALSE))

# 4.2 Boxplot of Points by Age Group
boxplot_age <- ggplot(nba, aes(x = age_group, y = PTS)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Points per 36 Minutes by Age Group",
       x = "Age Group", y = "Points") +
  theme_minimal()

ggsave("Output/age_group_boxplot.png", boxplot_age, width = 8, height = 6)

# ------------------------------
# 5. Identify peak performance age.
# Based on age group with highest average points.
# ------------------------------

peak_age <- nba %>%
  group_by(Age) %>%
  summarise(mean_points = mean(PTS, na.rm = TRUE)) %>%
  arrange(desc(mean_points)) %>%
  slice(1)

print("Peak performance age:")
print(peak_age)

write_csv(peak_age, "Output/peak_age.csv")
