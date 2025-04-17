# Load required packages
library(tidyverse)
library(dplyr)
library(ggradar)
library(gridExtra)
library(here)

here::i_am("Code/05_team_potential.R")

# Read the dataset
nba <- read.csv("Output/nba_clean.csv")

# Data cleaning: select relevant columns and drop missing rows
nba_clean <- nba %>%
  select(Team, Age, FG., X3P, FT., STL, BLK, DRB, PTS) %>%
  drop_na()

# Aggregate team-level statistics
team_avg <- nba_clean %>%
  group_by(Team) %>%
  summarise(
    Age = mean(Age),
    FG_percent = mean(FG.),
    X3P_percent = mean(X3P),
    FT_percent = mean(FT.),
    STL = mean(STL),
    BLK = mean(BLK),
    DRB = mean(DRB),
    PTS = mean(PTS),
    Youth = 1 / mean(Age)  # inverse age to represent youth
  )

# Normalize all features
normalize <- function(x) rescale(x, to = c(0, 1))
team_norm <- team_avg %>%
  mutate(
    Youth = normalize(Youth),
    FG_percent = normalize(FG_percent),
    X3P_percent = normalize(X3P_percent),
    FT_percent = normalize(FT_percent),
    PTS = normalize(PTS),
    STL = normalize(STL),
    BLK = normalize(BLK),
    DRB = normalize(DRB)
  )

# Calculate weighted potential score:
# Youth (40%), Scoring (30% = FG, 3P, FT, PTS), Defense (30% = STL, BLK, DRB)
team_norm <- team_norm %>%
  mutate(
    Potential_Score = 0.4 * Youth +
      0.075 * FG_percent +
      0.075 * X3P_percent +
      0.075 * FT_percent +
      0.075 * PTS +
      0.10 * STL +
      0.10 * BLK +
      0.10 * DRB
  )

# Get top 6 teams based on potential score
top6 <- team_norm %>%
  arrange(desc(Potential_Score)) %>%
  slice(1:6)

# Prepare data for ggradar (must include group column and be in long format)
radar_df <- top6 %>%
  select(Team, Youth, FG_percent, X3P_percent, FT_percent, PTS, STL, BLK, DRB) %>%
  rename(group = Team)

# ggradar requires tibble and some scaling safeguards
radar_df <- radar_df %>%
  mutate(across(where(is.numeric), ~ scales::rescale(.x, to = c(0, 1)))) %>%
  as_tibble()

# Create individual radar plots
radar_list <- lapply(1:nrow(radar_df), function(i) {
  ggradar(radar_df[i, ], 
          grid.min = 0, grid.mid = 0.5, grid.max = 1,
          font.radar = "Arial", values.radar = c("0", "0.5", "1"),
          group.line.width = 1.2,
          group.point.size = 2.5,
          legend.position = "none") +
    ggtitle(radar_df$group[i])
})

# Arrange plots into a grid and save
g <- arrangeGrob(grobs = radar_list, ncol = 3)
ggsave(
  here::here("Output/top6_team_radar_grid.png"),
  plot = g,
  width = 20, height = 6
)

