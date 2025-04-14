# Defense and rebounding analysis by positions
# Coder 3: Tianjie Zhang
# Analysis 1: Generate stacked bar plots for rebound types by position
# Analysis 2: Create grouped bar plots of defensive metrics


suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

# Define output path
output_path <- "Output/"

# Create output directories
if(!dir.exists(paste0(output_path, "tables"))) {
  dir.create(paste0(output_path, "tables"), recursive = TRUE)
}
if(!dir.exists(paste0(output_path, "plots"))) {
  dir.create(paste0(output_path, "plots"), recursive = TRUE)
}

# Load the cleaned dataset
nba_data <- read.csv("Output/nba_clean.csv")

# Screen players with at least 20 games
nba_data <- nba_data %>% filter(G >= 20)

# Analysis 1: Generate stacked bar plots for rebound types by position
# Calculate average rebounding statistics by position
rebound_by_pos <- nba_data %>%
  group_by(Pos) %>%
  summarize(
    Avg_ORB = mean(ORB, na.rm = TRUE),
    Avg_DRB = mean(DRB, na.rm = TRUE),
    Avg_TRB = mean(TRB, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(Avg_TRB))

# Save the summary table
write.csv(rebound_by_pos, paste0(output_path, "tables/rebounding_by_position.csv"), row.names = FALSE)

# Prepare data for plotting
rebound_long <- rebound_by_pos %>%
  select(Pos, Avg_ORB, Avg_DRB) %>%
  pivot_longer(cols = c(Avg_ORB, Avg_DRB), 
               names_to = "Rebound_Type", 
               values_to = "Average")

# Create a factor to order positions by total rebounds
rebound_long$Pos <- factor(rebound_long$Pos, 
                           levels = rebound_by_pos$Pos[order(rebound_by_pos$Avg_TRB, decreasing = TRUE)])

# Create stacked bar plot for rebounds
p1 <- ggplot(rebound_long, aes(x = Pos, y = Average, fill = Rebound_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Avg_ORB" = "orange", "Avg_DRB" = "blue"),
                    labels = c("Avg_ORB" = "Offensive Rebounds", "Avg_DRB" = "Defensive Rebounds")) +
  labs(title = "Average Rebounds by Position (Per 36 Minutes)",
       x = "Position",
       y = "Average Rebounds",
       fill = "Rebound Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(paste0(output_path, "plots/rebounds_by_position_stacked.png"), p1, width = 8, height = 6, dpi = 400)

# Analysis 2: Create grouped bar plots of defensive metrics
# Calculate average defensive statistics by position
defense_by_pos <- nba_data %>%
  group_by(Pos) %>%
  summarize(
    Avg_STL = mean(STL, na.rm = TRUE),
    Avg_BLK = mean(BLK, na.rm = TRUE),
    n = n()
  )

# Save the summary table
write.csv(defense_by_pos, paste0(output_path, "tables/defense_by_position.csv"), row.names = FALSE)

# Prepare data for plotting
defense_long <- defense_by_pos %>%
  select(Pos, Avg_STL, Avg_BLK) %>%
  pivot_longer(cols = c(Avg_STL, Avg_BLK), 
               names_to = "Defensive_Metric", 
               values_to = "Average")

# Create grouped bar plot for defensive metrics
p2 <- ggplot(defense_long, aes(x = Pos, y = Average, fill = Defensive_Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Avg_STL" = "blue", "Avg_BLK" = "orange"),
                    labels = c("Avg_STL" = "Steals", "Avg_BLK" = "Blocks")) +
  labs(title = "Defensive Metrics by Position (Per 36 Minutes)",
       x = "Position",
       y = "Average",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(paste0(output_path, "plots/defensive_metrics_by_position.png"), p2, width = 8, height = 6, dpi = 400)

# Print summary information
cat("\nSummary of Rebounding by Position:\n")
print(rebound_by_pos)

cat("\nSummary of Defensive Statistics by Position:\n")
print(defense_by_pos)

cat("\nDefense and Rebounding Analysis Complete!\n")