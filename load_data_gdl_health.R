# ====================================================================
# SCRIPT FOR LOADING GDL HEALTH DATA
# Description: Loads health data from Excel and makes it available
# File: MA_Data/GDL-Health-index-data.xlsx
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Load necessary libraries
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}

# ====================================================================
# LOAD AND PROCESS DATA
# ====================================================================

# Define file path
health_file <- "MA_Data/GDL-Health-index-data.xlsx"

# Verify that the file exists
if (!file.exists(health_file)) {
  stop("Error: File not found ", health_file)
}

# Read data from Excel
cat("Loading health data from:", health_file, "\n")
health_data_raw <- read_excel(health_file)

# Identify year columns (4 digits between 1900-2100)
year_cols <- names(health_data_raw)[grepl("^(19|20|21)\\d{2}$", names(health_data_raw))]
metadata_cols <- setdiff(names(health_data_raw), year_cols)

# Convert to long format (each row = country-year)
health_data <- health_data_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Value = suppressWarnings(as.numeric(gsub(",", ".", as.character(Value))))
  ) %>%
  filter(!is.na(Value))

# Display basic information
cat("✓ Data loaded successfully\n")
cat("  - Dimensions:", nrow(health_data), "rows x", ncol(health_data), "columns\n")
cat("  - Available years:", min(health_data$Year), "-", max(health_data$Year), "\n")
if ("Country" %in% names(health_data)) {
  cat("  - Unique countries:", length(unique(health_data$Country)), "\n")
}
cat("  - Available DataFrame: 'health_data'\n")

# Clean temporary variables
rm(health_data_raw, year_cols, metadata_cols, health_file)
