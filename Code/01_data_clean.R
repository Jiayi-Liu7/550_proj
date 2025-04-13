# Load necessary packages
library(readr)    # For reading/writing CSV files
library(dplyr)    # For data manipulation
library(ggplot2)  # For creating plots



# 1. Read the cleaned NBA data from CSV file.
nba_clean <- read_csv("Output/nba_clean.csv")
# Print first few rows for debugging
print("First few rows of nba_clean:")
print(head(nba_clean))

# 2. Generate basic descriptive statistics for key performance metrics.
# Here we use key variables: PTS, TRB, AST, eFG% and FT%, as well as minutes played (MP).
desc_stats <- nba_clean %>%
  summarise(
    MeanPoints     = mean(PTS, na.rm = TRUE),
    MedianPoints   = median(PTS, na.rm = TRUE),
    MaxPoints      = max(PTS, na.rm = TRUE),
    MeanRebounds   = mean(TRB, na.rm = TRUE),
    MedianRebounds = median(TRB, na.rm = TRUE),
    MaxRebounds    = max(TRB, na.rm = TRUE),
    MeanAssists    = mean(AST, na.rm = TRUE),
    MedianAssists  = median(AST, na.rm = TRUE),
    MaxAssists     = max(AST, na.rm = TRUE),
    MeaneFGPct     = mean(`eFG%`, na.rm = TRUE),
    MeanFTPercent  = mean(`FT%`, na.rm = TRUE),
    MeanMinutes    = mean(MP, na.rm = TRUE)
  )
print("Descriptive Statistics:")
print(desc_stats)
write_csv(desc_stats, "Output/desc_stats.csv")

# 3. Create summary tables by team and by position.
# In addition to count, we include average points, rebounds, assists, eFG%, and minutes played.

## 3.1 Summary table by team
team_summary <- nba_clean %>%
  group_by(Team) %>%
  summarise(
    Count       = n(),
    AvgPoints   = mean(PTS, na.rm = TRUE),
    AvgRebounds = mean(TRB, na.rm = TRUE),
    AvgAssists  = mean(AST, na.rm = TRUE),
    AvgeFGPct   = mean(`eFG%`, na.rm = TRUE),
    AvgMP       = mean(MP, na.rm = TRUE)
  )
print("Team Summary:")
print(team_summary)
write_csv(team_summary, "Output/team_summary.csv")

## 3.2 Summary table by position
position_summary <- nba_clean %>%
  group_by(Pos) %>%
  summarise(
    Count       = n(),
    AvgPoints   = mean(PTS, na.rm = TRUE),
    AvgRebounds = mean(TRB, na.rm = TRUE),
    AvgAssists  = mean(AST, na.rm = TRUE),
    AvgeFGPct   = mean(`eFG%`, na.rm = TRUE),
    AvgMP       = mean(MP, na.rm = TRUE)
  )
print("Position Summary:")
print(position_summary)
write_csv(position_summary, "Output/position_summary.csv")

# 4. Output player ranking tables.

## 4.1 Ranking by Points in descending order
points_ranking <- nba_clean %>%
  arrange(desc(PTS)) %>%
  select(Player, Team, Pos, PTS)
print("Ranking by Points:")
print(points_ranking)
write_csv(points_ranking, "Output/points_ranking.csv")

## 4.2 Ranking by Rebounds (Total Rebounds) in descending order
rebounds_ranking <- nba_clean %>%
  arrange(desc(TRB)) %>%
  select(Player, Team, Pos, TRB)
print("Ranking by Rebounds:")
print(rebounds_ranking)
write_csv(rebounds_ranking, "Output/rebounds_ranking.csv")

## 4.3 Ranking by Assists in descending order
assists_ranking <- nba_clean %>%
  arrange(desc(AST)) %>%
  select(Player, Team, Pos, AST)
print("Ranking by Assists:")
print(assists_ranking)
write_csv(assists_ranking, "Output/assists_ranking.csv")

## 4.4 Overall ranking table (sorted by Points, then Rebounds, then Assists)
overall_ranking <- nba_clean %>%
  arrange(desc(PTS), desc(TRB), desc(AST)) %>%
  select(Player, Team, Pos, PTS, TRB, AST)
print("Overall Ranking (PTS, then TRB, then AST):")
print(overall_ranking)
write_csv(overall_ranking, "Output/overall_ranking.csv")

# 5. Create charts for visualization and save the plots to the Output folder.

# -----------------------------
# 5.1 TEAM DISTRIBUTION CHARTS
# -----------------------------
# Calculate team distribution and percentages
team_distribution <- nba_clean %>% 
  count(Team) %>% 
  mutate(perc = n/sum(n) * 100)

# Team Bar Chart: displays count above bars
team_bar <- ggplot(team_distribution, aes(x = reorder(Team, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Team Distribution - Bar Chart", x = "Team", y = "Count") +
  theme_minimal()

# Print and save team charts
print(team_bar)

ggsave("Output/team_bar_chart.png", team_bar, width = 8, height = 6)


# -----------------------------
# 5.2 POSITION DISTRIBUTION CHARTS
# -----------------------------
# Calculate position distribution and percentages
position_distribution <- nba_clean %>% 
  count(Pos) %>% 
  mutate(perc = n/sum(n) * 100)

# Position Bar Chart: displays count above bars
position_bar <- ggplot(position_distribution, aes(x = reorder(Pos, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Position Distribution - Bar Chart", x = "Position", y = "Count") +
  theme_minimal()

# Position Pie Chart: displays percentage on each slice (2 decimal places)
position_pie <- ggplot(position_distribution, aes(x = "", y = n, fill = Pos)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = sprintf("%.2f%%", perc)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Position Distribution - Pie Chart") +
  theme_void()

print(position_bar)
print(position_pie)

ggsave("Output/position_bar_chart.png", position_bar, width = 8, height = 6)
ggsave("Output/position_pie_chart.png", position_pie, width = 8, height = 6)



#-----------------------------
#5.3 Bar Plot: Age distribution divided into 10 intervals.
# -----------------------------
# Create age bins by dividing Age into 10 intervals
nba_clean <- nba_clean %>% mutate(age_bin = cut(Age, breaks = 10))
plot_age_distribution <- ggplot(nba_clean, aes(x = age_bin)) +
  geom_bar(fill = "forestgreen") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Age Distribution", x = "Age Intervals", y = "Count") +
  theme_minimal()

print("Age Distribution Plot:")
print(plot_age_distribution)

ggsave("Output/age_distribution.png", plot = plot_age_distribution, width = 8, height = 6)

