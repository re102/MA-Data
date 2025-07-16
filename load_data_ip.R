# ====================================================================
# SCRIPT FOR LOADING INTERNET PENETRATION DATA
# Description: Functions to read and process Internet Penetration data from Excel file
# File: MA_Data/P_Internet Penetration over time.xlsx
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Load necessary libraries
library(readxl)
library(dplyr)

# Function to read and process Internet Penetration data
read_internet_penetration <- function() {
  # Read Excel file from 'Data' sheet
  ip_raw <- read_excel("MA_Data/P_Internet Penetration over time.xlsx",
                       sheet = "Data",
                       col_names = TRUE)

  # Convert '..' to NA
  ip <- ip_raw %>%
    mutate(across(everything(), ~ifelse(.x == "..", NA, .x)))

  # Remove completely empty rows (until finding blanks)
  ip <- ip %>%
    filter(!if_all(everything(), is.na))

  # Convert numeric columns that may have been read as text
  # (this is useful if there's a mix of numbers and '..' in columns)
  numeric_cols <- names(ip)[sapply(ip, function(x) {
    # Identify columns that should be numeric
    test_numeric <- suppressWarnings(as.numeric(as.character(x)))
    sum(!is.na(test_numeric)) > sum(is.na(test_numeric)) * 0.5
  })]

  # Convert identified columns to numeric (except the first one if it's countries/regions)
  if(length(numeric_cols) > 0) {
    first_col_name <- names(ip)[1]
    numeric_cols <- numeric_cols[numeric_cols != first_col_name]
    ip <- ip %>%
      mutate(across(all_of(numeric_cols), ~as.numeric(as.character(.x))))
  }

  # Verify that data has been loaded correctly
  cat("Internet Penetration data loaded successfully!\n")
  cat("DataFrame dimensions:", dim(ip)[1], "rows x", dim(ip)[2], "columns\n")
  cat("First columns:", paste(names(ip)[1:min(5, ncol(ip))], collapse = ", "), "\n")

  return(ip)
}
