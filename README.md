
Sustainability Assessment of Urban Food Gardens (UFGs) in Nantes

This repository contains the R scripts and data for the sustainability assessment of 54 allotment garden plots across 5 sites in Nantes Nord. The analysis uses two frameworks:\
1.  A Soil-Water-Food (SWF) nexus model.\
2.  A 5-attribute (MESMIS-style) model (Production, Stability, Adaptability, Equity, Self-Management).\

## Project Structure

* **/data**: Contains the raw data files (`Data_Chapter5.xlsx`, `code_book.xlsx`, `weighting_FWS.xlsx`).\
* **/scripts**: Contains all R scripts for the analysis, numbered in order of execution.\
* **/output**: The default location for all generated plots (600dpi .png) and data tables (.csv). This folder is ignored by Git.\

## Analytical Workflow

This analysis is broken into four main scripts:

1.  **`01_SWF_Overall_Analysis.R`**: (Phase 1) Network-wide overview of all 54 plots, including radar plots and indicator violin plots.
2.  **`02_SWF_Garden_Level_Analysis.R`**: (Phase 2) Garden-specific comparison, including 5 separate radar plots and 15 detailed violin plots.
3.  **`03_SWF_Statistical_Tests.R`**: (Phase 3) Statistical validation using Kruskal-Wallis and Friedman tests.
4.  **`04_MESMIS_Attribute_Analysis.R`**: (Phase 4/5) Calculation and visualization of the new 5-attribute framework.

## Required R Packages

To run these scripts, you will need the following R packages:
- `readxl`
- `tidyverse`
- `stringr`
- `RColorBrewer`
- `fmsb`
- `rstatix`
- `dunn.test`
