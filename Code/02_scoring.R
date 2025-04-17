# 02_scoring.R 
# Scoring analysis by player positions (Coder 2 - Yilin Zhang)

library(tidyverse)
library(here)
library(yaml)
library(ggplot2)



#read data
nba <- read_csv(here("Output", "nba_clean.csv"))

# read config.yml
config  <- read_yaml(here("Config", "config.yml"))
cut_min <- as.numeric(config$cutpoint %||% 0)  

nba <- nba %>% filter(G >= cut_min)

#Position consolidation
nba <- nba %>%
  mutate(Pos = case_when(
    str_detect(Pos, "C")          ~ "Center",
    str_detect(Pos, "F")          ~ "Forward",
    str_detect(Pos, "G")          ~ "Guard",
    TRUE                          ~ "Other"
  )) %>%
  filter(Pos != "Other")  #Only analyze the three major positions

# Count different sources of scores 
scoring_by_pos <- nba %>%
  group_by(Pos) %>%
  summarise(
    avg_2pt = mean(`2P`, na.rm = TRUE),
    avg_3pt = mean(`3P`, na.rm = TRUE),
    avg_ft  = mean(FT ,  na.rm = TRUE)
  )

# Group bar chart: Source of scores
scoring_long <- scoring_by_pos %>%
  pivot_longer(starts_with("avg"), names_to = "Type", values_to = "Points") %>%
  mutate(Type = recode(Type,
                       "avg_2pt" = "2PT",
                       "avg_3pt" = "3PT",
                       "avg_ft"  = "Free Throw"))

plot_bar <- ggplot(scoring_long,
                   aes(x = Pos, y = Points, fill = Pos)) +
  geom_col(position = "dodge") +
  facet_wrap(~Type) +
  scale_fill_manual(values = c(Guard = "blue",
                               Forward = "orange",
                               Center = "green")) +
  labs(title = "Scoring Sources by Position",
       y = "Average Points per 36 min") +
  theme_minimal(base_size = 14)

ggsave(here("Output", "plots", "scoring_sources_by_position.png"),
       plot_bar, width = 8, height = 6, dpi = 400)

# Box plot: Distribution of shooting percentage 
plot_box <- ggplot(nba, aes(Pos, `FG%`, fill = Pos)) +
  geom_boxplot() +
  scale_fill_manual(values = c(Guard = "blue",
                               Forward = "orange",
                               Center = "green")) +
  labs(title = "Field‑Goal % by Position",
       y = "FG%") +
  theme_minimal(base_size = 14)

ggsave(here("Output", "plots", "boxplot_fg_by_pos.png"),
       plot_box, width = 8, height = 6, dpi = 400)

# Scatter plot: Efficiency vs. Score
plot_scatter <- ggplot(nba,
                       aes(`FG%`, PTS, color = Pos, size = PTS)) +
  geom_point(alpha = 0.65) +
  scale_color_manual(values = c(Guard = "blue",
                                Forward = "orange",
                                Center = "green")) +
  labs(title = "Shooting Efficiency vs Points",
       x = "Field‑Goal %",
       y = "Points per 36 min",
       size = "Pts") +
  theme_minimal(base_size = 14)

ggsave(here("Output", "plots", "scatter_efficiency_vs_pts.png"),
       plot_scatter, width = 8, height = 6, dpi = 400)
