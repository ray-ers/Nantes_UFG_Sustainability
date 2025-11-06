# --- Phase 0: Setup and Data Preparation ---
# This section is included so the file is standalone.

# 1. Install/load libraries
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("rstatix")      # For Friedman post-hoc
# install.packages("dunn.test")    # For Kruskal-Wallis post-hoc

library(readxl)
library(tidyverse)
library(stringr)     
library(rstatix)
library(dunn.test)

# 2. Load all data files
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
respondent_axis_scores <- analysis_df %>%
  filter(Axis %in% c("Soil", "Water", "Food")) %>%
  group_by(Respondent_ID, Garden_ID, Axis) %>%
  summarise(Total_Axis_Score = sum(Score, na.rm = TRUE), .groups = 'drop')

print("--- Data preparation complete. ---")

# --- End of Phase 0 ---


# --- Phase 3a: Between-Garden Statistical Comparison ---

print("---")
print("--- Running Phase 3a: Between-Garden Comparison (Kruskal-Wallis) ---")
print("---")

# 1. Create an empty list to store results
between_garden_results_list <- list()

all_axes <- c("Soil", "Water", "Food")

for (current_axis in all_axes) {
  
  print(paste("### Analysis for:", current_axis, "###"))
  
  axis_data <- respondent_axis_scores %>% filter(Axis == current_axis)
  
  # 2. Run the Kruskal-Wallis Test
  kruskal_result <- kruskal.test(Total_Axis_Score ~ Garden_ID, data = axis_data)
  print(kruskal_result)
  
  # 3. Check for significance and run post-hoc
  if (kruskal_result$p.value < 0.05) {
    print("--- Dunn's Post-Hoc Test (which gardens are different?) ---")
    dunn_result <- dunn.test(
      axis_data$Total_Axis_Score, 
      axis_data$Garden_ID, 
      method = "bonferroni"
    )
    print(dunn_result)
    
    # Store the tidy results
    results_df <- data.frame(
      axis = current_axis,
      kw_statistic = kruskal_result$statistic,
      kw_p.value = kruskal_result$p.value,
      comparison = dunn_result$comparisons,
      p.adjusted = dunn_result$P.adjusted
    )
    
  } else {
    print("No significant difference found between gardens for this axis.")
    # Store the non-significant result
    results_df <- data.frame(
      axis = current_axis,
      kw_statistic = kruskal_result$statistic,
      kw_p.value = kruskal_result$p.value,
      comparison = "N/A (Omnibus test not significant)",
      p.adjusted = NA
    )
  }
  
  between_garden_results_list[[current_axis]] <- results_df
  print("---")
}

# 4. Combine all results and save to CSV
between_garden_results_df <- dplyr::bind_rows(between_garden_results_list)
write.csv(between_garden_results_df, "phase3a_between_garden_stats.csv", row.names = FALSE)
print("--- Between-garden stats saved to 'phase3a_between_garden_stats.csv' ---")

# --- End of Phase 3a ---


# --- Phase 3b: Within-Garden Statistical Comparison ---

print("---")
print("--- Running Phase 3b: Within-Garden Comparison (Friedman Test) ---")
print("---")

# 1. Create an empty list to store results
within_garden_results_list <- list()

all_gardens <- unique(respondent_axis_scores$Garden_ID)

for (current_garden in all_gardens) {
  
  print(paste("### Analysis for:", current_garden, "###"))
  
  garden_data <- respondent_axis_scores %>% filter(Garden_ID == current_garden)
  
  # 2. Run the Friedman Test
  friedman_result <- friedman.test(
    Total_Axis_Score ~ Axis | Respondent_ID, 
    data = garden_data
  )
  print(friedman_result)
  
  # 3. Check for significance and run post-hoc
  if (friedman_result$p.value < 0.05) {
    print("--- Paired Wilcoxon Test (which axes are different?) ---")
    
    # **MODIFIED**: Use rstatix for tidy output
    wilcox_result_df <- garden_data %>%
      rstatix::pairwise_wilcox_test(
        Total_Axis_Score ~ Axis,
        paired = TRUE,
        p.adjust.method = "bonferroni"
      )
    
    print(wilcox_result_df)
    
    # Store the tidy results
    results_df <- wilcox_result_df %>%
      mutate(
        garden_id = current_garden,
        friedman_statistic = friedman_result$statistic,
        friedman_p.value = friedman_result$p.value
      ) %>%
      # Select relevant columns for a clean table
      select(garden_id, group1, group2, p.adj, p.adj.signif, friedman_p.value)
    
  } else {
    print("No significant difference found between axes for this garden.")
    
    # Store the non-significant result
    results_df <- data.frame(
      garden_id = current_garden,
      group1 = "N/A",
      group2 = "N/A (Omnibus test not significant)",
      p.adj = NA,
      p.adj.signif = "ns",
      friedman_p.value = friedman_result$p.value
    )
  }
  
  within_garden_results_list[[current_garden]] <- results_df
  print("---")
}

# 4. Combine all results and save to CSV
within_garden_results_df <- dplyr::bind_rows(within_garden_results_list)
write.csv(within_garden_results_df, "phase3b_within_garden_stats.csv", row.names = FALSE)
print("--- Within-garden stats saved to 'phase3b_within_garden_stats.csv' ---")

# --- End of Phase 3b ---