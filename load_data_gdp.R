# ====================================================================
# SCRIPT FOR LOADING GDP DATA
# Description: Functions to read GDP data from Excel file
# File: MA_Data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_131991.xls
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Function to read GDP data from Excel file
read_gdp_data <- function() {
  # Load necessary libraries
  library(readxl)
  library(dplyr)

  # Define file path
  file_path <- "MA_Data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_131991.xls"

  # Read Excel file specifying 'Data' sheet
  # skip = 3 so that row 4 becomes the headers
  gdp_data <- read_excel(
    path = file_path,
    sheet = "Data",
    skip = 3,
    col_names = TRUE
  )

  # Verify that data has been loaded correctly
  cat("Data loaded successfully!\n")
  cat("DataFrame dimensions:", dim(gdp_data)[1], "rows x", dim(gdp_data)[2], "columns\n")
  cat("First columns:", paste(names(gdp_data)[1:min(5, ncol(gdp_data))], collapse = ", "), "\n")

  return(gdp_data)
}

# Alternative function if you need more control over data cleaning
read_gdp_data_clean <- function() {
  library(readxl)
  library(dplyr)

  file_path <- "MA_Data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_131991.xls"

  # Read the data
  gdp_data <- read_excel(
    path = file_path,
    sheet = "Data",
    skip = 3,
    col_names = TRUE
  )

  # Basic data cleaning (optional)
  gdp_data_clean <- gdp_data %>%
    # Remove completely empty rows
    filter(!if_all(everything(), is.na)) %>%
    # Remove completely empty columns
    select(where(~!all(is.na(.))))

  cat("Data loaded and cleaned!\n")
  cat("Final dimensions:", dim(gdp_data_clean)[1], "rows x", dim(gdp_data_clean)[2], "columns\n")

  return(gdp_data_clean)
}
