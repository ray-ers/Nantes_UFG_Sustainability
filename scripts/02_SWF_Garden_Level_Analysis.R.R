# --- Phase 0: Setup and Data Preparation ---

# 1. Install/load libraries
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("RColorBrewer") 

library(readxl)
library(tidyverse)
library(stringr)     
library(RColorBrewer) 

# 2. Load all three data files
# Make sure these files are in your R working directory
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

# 4. Create main data table
# This is the base data for all plots
indicator_respondent_scores <- analysis_df %>%
  filter(Axis %in% c("Soil", "Water", "Food")) %>%
  group_by(Respondent_ID, Garden_ID, Axis, Scope, Indicator) %>%
  summarise(Total_Indicator_Score = sum(Score, na.rm = TRUE), .groups = 'drop')

print("--- Data preparation complete. ---")

# --- End of Phase 0 ---


# --- Phase 2b: Detailed Indicator Analysis (Data Export) ---

print("--- Generating Phase 2b: Summary Statistics Table ---")

# 1. Create the new summary statistics table
garden_indicator_summary_stats <- indicator_respondent_scores %>%
  group_by(Garden_ID, Axis, Scope, Indicator) %>%
  summarise(
    Mean_Score = mean(Total_Indicator_Score, na.rm = TRUE),
    Median_Score = median(Total_Indicator_Score, na.rm = TRUE),
    Std_Dev = sd(Total_Indicator_Score, na.rm = TRUE),
    Min_Score = min(Total_Indicator_Score, na.rm = TRUE),
    Q1_Score = quantile(Total_Indicator_Score, 0.25, na.rm = TRUE),
    Q3_Score = quantile(Total_Indicator_Score, 0.75, na.rm = TRUE),
    Max_Score = max(Total_Indicator_Score, na.rm = TRUE),
    N_Samples = n(), 
    .groups = 'drop' 
  ) %>%
  mutate(Std_Dev = ifelse(is.na(Std_Dev), 0, Std_Dev)) %>%
  arrange(Garden_ID, Axis, Scope, Indicator)

# 2. Save the new table to a CSV file
write.csv(garden_indicator_summary_stats, "garden_indicator_summary_stats.csv", row.names = FALSE)
print("--- Summary table saved as 'garden_indicator_summary_stats.csv' ---")

# --- End of Phase 2b (Data) ---


# --- Phase 2b: Detailed Indicator Analysis (15 Plots) ---

print("--- Generating Phase 2b: 15 Separate Violin Plots ---")

# 1. Get unique lists of gardens and axes
all_gardens <- unique(indicator_respondent_scores$Garden_ID)
all_axes <- c("Soil", "Water", "Food")

# 2. Create a consistent color palette for *Scopes*
all_scopes <- unique(indicator_respondent_scores$Scope)
scope_colors <- brewer.pal(n = length(all_scopes), name = "Paired")
names(scope_colors) <- all_scopes

# 3. Loop through each garden and each axis
for (current_garden in all_gardens) {
  for (current_axis in all_axes) {
    
    # 4. Filter data for the current plot
    plot_data <- indicator_respondent_scores %>%
      filter(Garden_ID == current_garden, Axis == current_axis)
    
    # 5. Check if data exists before trying to plot
    if (nrow(plot_data) > 0) {
      
      # 6. **UPDATED**: Create the simplified plot title
      plot_title <- paste0(current_garden, " - ", current_axis, " Indicators")
      
      p <- ggplot(plot_data, aes(x = Indicator, y = Total_Indicator_Score, fill = Scope)) +
        geom_violin(trim = FALSE, alpha = 0.7) +
        geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
        facet_wrap(~ Scope, scales = "free_x") +
        scale_fill_manual(values = scope_colors) +
        labs(
          title = plot_title, # Use new title
          y = "Total Indicator Score",
          x = ""
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none" 
        )
      
      # 7. Create a simpler filename
      filename <- paste0("violins_", current_garden, "_", current_axis, ".png")
      ggsave(filename, p, width = 10, height = 7, dpi = 600)
      
      # 8. Print the plot to the RStudio window
      print(p)
      
    } # end if
  } # end axis loop
} # end garden loop

print("--- Phase 2b: 15 plots generated and saved. ---")
print("--- Please upload 'garden_indicator_summary_stats.csv' for analysis. ---")

# --- End of Phase 2b (Plots) ---