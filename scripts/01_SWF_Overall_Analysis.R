# --- Phase 0: Setup and Data Preparation ---

# 1. Install/load libraries
library(readxl)
library(tidyverse)
library(fmsb)        
library(stringr)     

# 2. Load all three data files
scores_df <- read_excel("Data_Chapter5.xlsx", sheet = "Sheet1")
code_book_df <- read_excel("code_book.xlsx")

print("--- All data files successfully loaded ---")

# 3. Create the 'analysis_df' (long-format data)
var_map <- code_book_df %>%
  mutate(
    Indicator = str_trim(Indicator), 
    Scope = str_trim(Scope)          
  ) %>%
  select(`Column Name`, Axis, Scope, Indicator) %>%
  rename(Column.Name = `Column Name`) %>%
  filter(!is.na(Axis) & Axis != "" & Axis != "N/A")

score_columns_to_pivot <- intersect(var_map$Column.Name, names(scores_df))

analysis_df <- scores_df %>%
  pivot_longer(
    cols = all_of(score_columns_to_pivot),
    names_to = "Column.Name",
    values_to = "Score"
  ) %>%
  left_join(var_map, by = "Column.Name") %>%
  select(Respondent_ID, Garden_ID, Axis, Scope, Indicator, Score)

# 4. Create main data tables

# 4a. For Phase 1a & 2 (Axis-level scores)
respondent_axis_scores <- analysis_df %>%
  filter(Axis %in% c("Soil", "Water", "Food")) %>%
  group_by(Respondent_ID, Garden_ID, Axis) %>%
  summarise(Total_Axis_Score = sum(Score, na.rm = TRUE), .groups = 'drop')

# 4b. For Phase 1b (Raw Indicator scores)
indicator_respondent_scores <- analysis_df %>%
  filter(Axis %in% c("Soil", "Water", "Food")) %>%
  group_by(Respondent_ID, Garden_ID, Axis, Scope, Indicator) %>%
  summarise(Total_Indicator_Score = sum(Score, na.rm = TRUE), .groups = 'drop')

print("--- Data preparation complete (using raw scores). ---")

# --- End of Phase 0 ---


# --- Phase 1a: Overall Sustainability (fmsb Triangle) ---

print("--- Generating Phase 1a: Overall Radar Plot ---")

# 1. Calculate the overall average score and pivot wide
overall_axis_summary <- respondent_axis_scores %>%
  group_by(Axis) %>%
  summarise(Mean_Score = mean(Total_Axis_Score, na.rm = TRUE)) %>%
  pivot_wider(names_from = Axis, values_from = Mean_Score) %>%
  select(Soil, Water, Food) 

# 2. Prepare data for 'radarchart' function
data_phase1a <- rbind(
  rep(100, 3), # Max row (100)
  rep(0, 3),   # Min row (0)
  overall_axis_summary
)

# 3. Save the plot to a PNG file first
png("phase1a_radar_plot_600dpi.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(1, 1, 1, 1)) 
radarchart(
  data_phase1a,
  axistype = 1, 
  pcol = "forestgreen",                   
  pfcol = scales::alpha("forestgreen", 0.3), 
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25), 
  cglwd = 0.8,
  vlcex = 1.2, 
  title = "Phase 1a: Overall Sustainability Performance (Average)"
)
dev.off() # Close the PNG device

# 4. Also draw the plot in the RStudio 'Plots' pane
par(mar = c(1, 1, 1, 1)) 
radarchart(
  data_phase1a,
  axistype = 1, 
  pcol = "forestgreen",                   
  pfcol = scales::alpha("forestgreen", 0.3), 
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25), 
  cglwd = 0.8,
  vlcex = 1.2, 
  title = "Overall Sustainability Performance (Average)"
)

# --- End of Phase 1a ---


# --- Phase 1b: Indicator Distribution (Violin/Box Plots) ---

print("--- Generating Phase 1b: Indicator Distribution Plots ---")

plot_data_1b <- indicator_respondent_scores

# 1. Create the 'Soil' plot
soil_plot <- ggplot(filter(plot_data_1b, Axis == "Soil"), 
                    aes(x = Indicator, y = Total_Indicator_Score, fill = Scope)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  facet_grid(. ~ Scope, scales = "free_x", space = "free_x") +
  labs(
    title = "Soil Indicator Distribution (All Gardens)",
    y = "Total Indicator Score", 
    x = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none" 
  )

# Save the 'Soil' plot at 600 dpi
ggsave("soil_distribution_plot_600dpi.png", soil_plot, width = 10, height = 7, dpi = 600)


# 2. Create the 'Water' plot
water_plot <- ggplot(filter(plot_data_1b, Axis == "Water"), 
                     aes(x = Indicator, y = Total_Indicator_Score, fill = Scope)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  facet_grid(. ~ Scope, scales = "free_x", space = "free_x") +
  labs(
    title = "Water Indicator Distribution (All Gardens)",
    y = "Total Indicator Score", 
    x = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Save the 'Water' plot at 600 dpi
ggsave("water_distribution_plot_600dpi.png", water_plot, width = 10, height = 7, dpi = 600)


# 3. Create the 'Food' plot
food_plot <- ggplot(filter(plot_data_1b, Axis == "Food"), 
                    aes(x = Indicator, y = Total_Indicator_Score, fill = Scope)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  facet_grid(. ~ Scope, scales = "free_x", space = "free_x") +
  labs(
    title = "Food Indicator Distribution (All Gardens)",
    y = "Total Indicator Score", 
    x = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Save the 'Food' plot at 600 dpi
ggsave("food_distribution_plot_600dpi.png", food_plot, width = 10, height = 7, dpi = 600)

# 4. Print all three plots to the RStudio window
print(soil_plot)
print(water_plot)
print(food_plot)

# --- End of Phase 1b ---
