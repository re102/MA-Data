# ====================================================================
# SCRIPT FOR LOADING COUNTRY - LANGUAGE - REGION MAPPING DATA
# Description: Functions to read unemployment data from Excel file
# File: MA_Data/Country_Lang_Reg_Coding.xlsx
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Function to read unemployment data from Excel file
read_lang_region_data <- function() {
  # Load necessary libraries
  library(readxl)
  library(dplyr)

  # Read Excel file specifying 'Data' sheet
  lang_region_data <- read_excel("MA_Data/Country_Lang_Reg_Coding.xlsx",
                                 sheet = "Data",
                                 col_names = TRUE)

  # Verify that data has been loaded correctly
  cat("Data loaded successfully!\n")
  cat("DataFrame dimensions:", dim(lang_region_data)[1], "rows x", dim(lang_region_data)[2], "columns\n")
  cat("First columns:", paste(names(lang_region_data)[1:min(5, ncol(lang_region_data))], collapse = ", "), "\n")

  return(lang_region_data)
}

# Alternative function if you need more control over data cleaning
read_lang_region_data_clean <- function() {
  library(readxl)
  library(dplyr)

  file_path <- "../MA_Data/Country_Lang_Reg_Coding.xlsx"

  # Read the data
  lang_region_data <- read_excel(
    path = file_path,
    sheet = "Data",
    col_names = TRUE
  )

  # Basic data cleaning (optional)
  lang_region_data_clean <- lang_region_data %>%
    # Remove completely empty rows
    filter(!if_all(everything(), is.na)) %>%
    # Remove completely empty columns
    select(where(~!all(is.na(.))))

  cat("Data loaded and cleaned!\n")
  cat("Final dimensions:", dim(lang_region_data_clean)[1], "rows x", dim(lang_region_data_clean)[2], "columns\n")

  return(lang_region_data_clean)
}
