# Punished or Forgiven? Experimental Evidence on the Effects of Political Scandals

This GitHub repository contains replication materials for my Master's thesis in Political Science at the University of Copenhagen. The repository consists of two main folders.

## Analysis

Contains all materials necessary for replicating the analysis, including R code, raw data, clean data.

**00. Code** contains five R scripts, numbered in the order they should be run: `01_Power_Analysis.R`, `02_Data_Cleaning.R`, `03_Descriptives.R`, `04_Conjoint_Analysis.R` and `05_Vignette_Analysis.R`. Only the cleaning script needs to be run before the others.

**01. Raw data** contains the raw survey export (`data_raw.csv`, one row per respondent), the randomised conjoint design file generated before fielding (`choice_questions.csv`), and Danish population data from Statistics Denmark used for the sample comparison (`Population_data.xlsx`).

**02. Clean data** contains the three analysis datasets produced by the cleaning script: `conjoint_data.rds` with one row per candidate profile shown, `vignette_data.rds` with one row per vignette rated, and `descriptive_data.rds` with one row per respondent.

## Survey

This folder contains all materials necessary for replicating the survey I employed, built using surveydown and deployed with Shiny. It contains the script generating the randomised candidate profiles, the survey itself as a Quarto document, the Shiny app, and the deployment script.

---

Note that the scripts set the working directory to the folder in which the analysis was originally run. To reproduce the analysis, change the working directory.
