{\rtf1\ansi\ansicpg1252\cocoartf2822
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Sustainability Assessment of Urban Food Gardens (UFGs) in Nantes\
\
This repository contains the R scripts and data for the sustainability assessment of 54 allotment garden plots across 5 sites in Nantes Nord. The analysis uses two frameworks:\
1.  A Soil-Water-Food (SWF) nexus model.\
2.  A 5-attribute (MESMIS-style) model (Production, Stability, Adaptability, Equity, Self-Management).\
\
## Project Structure\
\
* **/data**: Contains the raw data files (`Data_Chapter5.xlsx`, `code_book.xlsx`, `weighting_FWS.xlsx`).\
* **/scripts**: Contains all R scripts for the analysis, numbered in order of execution.\
* **/output**: The default location for all generated plots (600dpi .png) and data tables (.csv). This folder is ignored by Git.\
\
## Analytical Workflow\
\
This analysis is broken into four main scripts:\
\
1.  **`01_SWF_Overall_Analysis.R`**: (Phase 1) Network-wide overview of all 54 plots, including radar plots and indicator violin plots.\
2.  **`02_SWF_Garden_Level_Analysis.R`**: (Phase 2) Garden-specific comparison, including 5 separate radar plots and 15 detailed violin plots.\
3.  **`03_SWF_Statistical_Tests.R`**: (Phase 3) Statistical validation using Kruskal-Wallis and Friedman tests.\
4.  **`04_MESMIS_Attribute_Analysis.R`**: (Phase 4/5) Calculation and visualization of the new 5-attribute framework.\
\
## Required R Packages\
\
To run these scripts, you will need the following R packages:\
- `readxl`\
- `tidyverse`\
- `stringr`\
- `RColorBrewer`\
- `fmsb`\
- `rstatix`\
- `dunn.test`}