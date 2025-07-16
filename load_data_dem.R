# ====================================================================
# SCRIPT FOR LOADING DEMOCRATISATION DATA
# Description: Functions to read and process V-Dem democratisation data
# File: MA_Data/V-Dem-CY-Core-v15.csv
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Function to load and process V-Dem data
read_dem_data <- function() {
  # Load necessary libraries
  library(readr)
  library(dplyr)

  # Define columns we want to keep
  relevant_columns <- c(
    "country_name", "country_text_id", "country_id", "year",
    "historical_date", "project", "historical", "histname",
    "codingstart", "codingend", "codingstart_contemp", "codingend_contemp",
    "codingstart_hist", "codingend_hist", "gapstart1", "gapstart2",
    "gapstart3", "gapend1", "gapend2", "gapend3", "gap_index",
    "COWcode", "v2x_polyarchy"
  )

  # Read CSV file
  cat("Loading V-Dem data...\n")
  dpc <- read_csv("MA_Data/V-Dem-CY-Core-v15.csv",
                  show_col_types = FALSE)

  # Select only relevant columns
  dpc <- dpc %>%
    select(all_of(relevant_columns))

  # Display basic dataset information
  cat("Data loaded successfully!\n")
  cat("Dimensions:", nrow(dpc), "rows x", ncol(dpc), "columns\n")
  cat("Year range:", min(dpc$year, na.rm = TRUE), "-", max(dpc$year, na.rm = TRUE), "\n")
  cat("Number of unique countries:", length(unique(dpc$country_name)), "\n")

  return(dpc)
}
