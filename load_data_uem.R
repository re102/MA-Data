# ====================================================================
# SCRIPT FOR LOADING UNEMPLOYMENT DATA
# Description: Functions to read unemployment data from Excel file
# File: MA_Data/API_SL.UEM.TOTL.ZS_DS2_en_excel_v2_85265.xls
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Function to read unemployment data from Excel file
read_uem_data <- function() {
  # Load necessary libraries
  library(readxl)
  library(dplyr)

  # Define file path
  file_path <- "MA_Data/API_SL.UEM.TOTL.ZS_DS2_en_excel_v2_85265.xls"

  # Read Excel file specifying 'Data' sheet
  # skip = 3 so that row 4 becomes the headers
  uem_data <- read_excel(
    path = file_path,
    sheet = "Data",
    skip = 3,
    col_names = TRUE
  )

  # Verify that data has been loaded correctly
  cat("Data loaded successfully!\n")
  cat("DataFrame dimensions:", dim(uem_data)[1], "rows x", dim(uem_data)[2], "columns\n")
  cat("First columns:", paste(names(uem_data)[1:min(5, ncol(uem_data))], collapse = ", "), "\n")

  return(uem_data)
}

# Alternative function if you need more control over data cleaning
read_uem_data_clean <- function() {
  library(readxl)
  library(dplyr)

  file_path <- "MA_Data/API_SL.UEM.TOTL.ZS_DS2_en_excel_v2_85265.xls"

  # Read the data
  uem_data <- read_excel(
    path = file_path,
    sheet = "Data",
    skip = 3,
    col_names = TRUE
  )

  # Basic data cleaning (optional)
  uem_data_clean <- uem_data %>%
    # Remove completely empty rows
    filter(!if_all(everything(), is.na)) %>%
    # Remove completely empty columns
    select(where(~!all(is.na(.))))

  cat("Data loaded and cleaned!\n")
  cat("Final dimensions:", dim(uem_data_clean)[1], "rows x", dim(uem_data_clean)[2], "columns\n")

  return(uem_data_clean)
}
