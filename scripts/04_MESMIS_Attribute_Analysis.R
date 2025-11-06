# --- Phase 4 & 5 Merged: Calculate and Visualize Attributes ---
# This is a standalone script

# 1. Install/load libraries
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("fmsb")
# install.packages("RColorBrewer")

library(readxl)
library(tidyverse)
library(stringr)     
library(fmsb)
library(RColorBrewer)

# ---
# --- Phase 4: Calculate New Sustainability Attributes ---
# ---

# --- Step 1: Define New Attribute Maps ---
production_indicators <- c(
  "Self-consumption", "Close network sharing", "Social sharing", 
  "Labour availability", "Garden management tool", "Irrigation Water", 
  "Water-use efficiency"
)
stability_indicators <- c(
  "Physical properties", "Chemical properties", "Biological properties", 
  "Evidence of the erosive process", "Extreme weather conditions", 
  "Vegetation cover", "Susceptibility to drought"
)
adaptability_indicators <- c(
  "Disturbance minimization", "Cover crop/munching", "Crop rotation", 
  "Plant management", "Water management", "Soil management", 
  "Water reuse/recycle", "Biodiversity enhancement", "Logistics and infrastructure"
)
equity_indicators <- c(
  "Trace elements", "Public participation", "Tradition and culture", 
  "Social and associative participation", "Close network sharing", "Social sharing"
)
self_management_indicators <- c(
  "Synthetic input", "Soil pollution intervention", "Dependence on economic input", 
  "Land tenure policy", "Support organizations", "Public policy"
)

# --- Step 2: Load All Data Files ---
scores_df <- read_excel("Data_Chapter5.xlsx", sheet = "Sheet1")
code_book_df <- read_excel("code_book.xlsx")
weighting_df <- read_excel("weighting_FWS.xlsx")

print("--- All data files successfully loaded ---")

# --- Step 3: Create Master Lookup Tables ---

f_clean_names <- . %>% 
  mutate(
    Indicator_Clean = str_to_lower(str_trim(Indicator)),
    Indicator_Clean = str_replace_all(Indicator_Clean, "[ /%()]", "")
  )
var_map <- code_book_df %>%
  select(`Column Name`, Indicator) %>%
  rename(Column.Name = `Column Name`) %>%
  filter(!is.na(Indicator) & Indicator != "N/A") %>%
  mutate(Indicator_Clean = str_to_lower(str_trim(Indicator)),
         Indicator_Clean = str_replace_all(Indicator_Clean, "[ /%()]", ""))
subweight_map <- weighting_df %>%
  select(Indicator, Subweight) %>%
  filter(!is.na(Indicator) & !is.na(Subweight)) %>%
  mutate(Indicator_Clean = str_to_lower(str_trim(Indicator)),
         Indicator_Clean = str_replace_all(Indicator_Clean, "[ /%()]", "")) %>%
  mutate(Indicator_Clean = str_replace(Indicator_Clean, "landtenurepolicyownership", "landtenurepolicy")) %>%
  distinct(Indicator_Clean, .keep_all = TRUE) %>%
  select(Indicator_Clean, Subweight)

# --- Step 4: Create Long-Format Score Data ---
analysis_df <- scores_df %>%
  select(Respondent_ID, Garden_ID, all_of(var_map$Column.Name)) %>%
  pivot_longer(
    cols = -c(Respondent_ID, Garden_ID),
    names_to = "Column.Name",
    values_to = "Score"
  ) %>%
  left_join(var_map, by = "Column.Name") %>%
  select(Respondent_ID, Garden_ID, Indicator, Score)

# --- Step 5: Calculate ScoreObserved for each plot ---
attribute_list_df <- bind_rows(
  data.frame(Attribute = "Production", Indicator = production_indicators),
  data.frame(Attribute = "Stability", Indicator = stability_indicators),
  data.frame(Attribute = "Adaptability", Indicator = adaptability_indicators),
  data.frame(Attribute = "Equity", Indicator = equity_indicators),
  data.frame(Attribute = "Self_Management", Indicator = self_management_indicators)
)
plot_scores_by_attribute <- analysis_df %>%
  left_join(attribute_list_df, by = "Indicator") %>%
  filter(!is.na(Attribute)) 
observed_scores <- plot_scores_by_attribute %>%
  group_by(Respondent_ID, Garden_ID, Attribute) %>%
  summarise(ScoreObserved = sum(Score, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Attribute,
    values_from = ScoreObserved,
    names_prefix = "Observed_"
  )

# --- Step 6: Calculate ScoreMaximum for each attribute ---
indicator_subweight_map <- var_map %>%
  left_join(subweight_map, by = "Indicator_Clean") %>%
  select(Indicator, Subweight) %>%
  distinct(Indicator, .keep_all = TRUE)
max_scores <- attribute_list_df %>%
  left_join(indicator_subweight_map, by = "Indicator") %>%
  filter(is.na(Subweight))
if (nrow(max_scores) > 0) {
  print("ERROR: Could not find Subweight for the following indicators:")
  print(unique(max_scores$Indicator))
}
score_maximum_df <- attribute_list_df %>%
  left_join(indicator_subweight_map, by = "Indicator") %>%
  group_by(Attribute) %>%
  summarise(ScoreMaximum = sum(Subweight, na.rm = TRUE))

print("--- ScoreMaximum calculated for each attribute: ---")
print(as.data.frame(score_maximum_df))

# --- Step 7: Calculate ScoreNormalized and Save ---
max_values <- as.list(setNames(score_maximum_df$ScoreMaximum, score_maximum_df$Attribute))
final_normalized_data <- observed_scores %>%
  mutate(
    Normalized_Production = (Observed_Production / max_values$Production) * 100,
    Normalized_Stability = (Observed_Stability / max_values$Stability) * 100,
    Normalized_Adaptability = (Observed_Adaptability / max_values$Adaptability) * 100,
    Normalized_Equity = (Observed_Equity / max_values$Equity) * 100,
    Normalized_Self_Management = (Observed_Self_Management / max_values$Self_Management) * 100
  )
write.csv(final_normalized_data, "phase5_normalized_attributes.csv", row.names = FALSE)
print("--- Phase 4 complete. Normalized scores saved. ---")


# ---
# --- Phase 5: Visualize Average Performance ---
# ---

print("--- Generating Plot 5a: Overall Attribute Performance ---")

# 1. Calculate the overall mean for each normalized attribute
overall_summary <- final_normalized_data %>%
  summarise(
    Production = mean(Normalized_Production, na.rm = TRUE),
    Stability = mean(Normalized_Stability, na.rm = TRUE),
    Adaptability = mean(Normalized_Adaptability, na.rm = TRUE),
    Equity = mean(Normalized_Equity, na.rm = TRUE),
    Self_Management = mean(Normalized_Self_Management, na.rm = TRUE)
  )

# 2. Print the scores to the console
print("--- Average Scores for 'Sustainability performance of UFGs in Nantes Nord' ---")
print(as.data.frame(overall_summary))

# 3. Prepare data for 'radarchart' (5-axis pentagon)
data_5a <- rbind(
  rep(100, 5), # Max row
  rep(0, 5),   # Min row
  overall_summary
)

# 4. **UPDATED**: Create the new title with scores
plot_title_main <- "Sustainability performance of UFGs in Nantes Nord"
plot_title_scores <- paste0(
  "Production: ", round(overall_summary$Production, 1), ", ",
  "Stability: ", round(overall_summary$Stability, 1), "\n",
  "Adaptability: ", round(overall_summary$Adaptability, 1), ", ",
  "Equity: ", round(overall_summary$Equity, 1), ", ",
  "Self-Management: ", round(overall_summary$Self_Management, 1)
)
# Combine main title and score subtitle
plot_title_full <- paste(plot_title_main, plot_title_scores, sep = "\n")
plot_filename <- "nantes_nord_avg_performance_600dpi.png"

# 5. Save the plot to a PNG file
png(plot_filename, width = 8, height = 8, units = "in", res = 600)
# Adjust margins to make space for the multi-line title
par(mar = c(1, 1, 2, 1)) 
radarchart(
  data_5a,
  axistype = 1, 
  pcol = "darkgreen",                   
  pfcol = scales::alpha("darkgreen", 0.3), 
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25), 
  cglwd = 0.8,
  vlcex = 1.0, 
  title = plot_title_full # Use new title
)
dev.off() 

# 6. Also draw the plot in the RStudio 'Plots' pane
par(mar = c(1, 1, 2, 1)) # Adjust margins
radarchart(
  data_5a, axistype = 1, pcol = "darkgreen", pfcol = scales::alpha("darkgreen", 0.3), 
  plwd = 2, cglcol = "grey", cglty = 1, axislabcol = "grey",
  caxislabels = seq(0, 100, 25), cglwd = 0.8, vlcex = 1.0, 
  title = plot_title_full # Use new title
)
par(mfrow = c(1, 1)) # Reset plot window

print(paste("--- Phase 5 complete. Plot saved as:", plot_filename, "---"))

# --- End of Script ---