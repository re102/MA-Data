# ====================================================================
# SCRIPT FOR PROCESSING GDL EDUCATION FILES
# Description: Processes and loads multiple data files from MA_Data folder
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

# Load necessary libraries
library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(purrr)

# Define folder path (relative to current working directory)
data_folder <- "MA_Data"

# Check current working directory
cat("Current working directory:", getwd(), "\n")

# Show content of current directory
cat("Current directory contents:\n")
print(list.files(".", include.dirs = TRUE))

# Check if MA_Data folder exists
if (dir.exists("../MA_Data")) {
  cat("✓ MA_Data folder found\n")
} else {
  cat("✗ MA_Data folder not found\n")
  cat("Available folders:\n")
  dirs <- list.files(".", include.dirs = TRUE, full.names = FALSE)
  dirs <- dirs[file.info(dirs)$isdir & !is.na(file.info(dirs)$isdir)]
  print(dirs)
}

# Function to extract information from filename
extract_file_info <- function(file_name) {
  # Corrected pattern for: something-(YYYY)-data.extension
  pattern <- "^(.+)-\\((\\d{4})\\)-data\\.(xlsx|csv)$"

  if (str_detect(file_name, pattern)) {
    matches <- str_match(file_name, pattern)
    return(list(
      filename = file_name,
      location_indicator = matches[,2],
      year = as.numeric(matches[,3]),
      extension = matches[,4]
    ))
  } else {
    cat("File does not match pattern:", file_name, "\n")
    return(NULL)
  }
}

# Function to read files according to their extension
read_data_file <- function(filepath, extension) {
  tryCatch({
    if (extension == "xlsx") {
      return(read_excel(filepath))
    } else if (extension == "csv") {
      return(read_csv(filepath, show_col_types = FALSE))
    }
  }, error = function(e) {
    warning(paste("Error reading file:", filepath, "-", e$message))
    return(NULL)
  })
}

# Check that the folder exists
if (!dir.exists(data_folder)) {
  stop(paste("The folder", data_folder, "does not exist in directory:", getwd(),
             "\nVerify that you are in the correct project directory."))
}

# Get list of files in the folder
files <- list.files(data_folder, pattern = "\\.(xlsx|csv)$", full.names = FALSE)

if (length(files) == 0) {
  stop("No .xlsx or .csv files found in MA_Data folder")
}

cat("Files found:", length(files), "\n")
print(files)

# Process file information
file_info_list <- map(files, extract_file_info)
file_info_list <- file_info_list[!sapply(file_info_list, is.null)]  # Remove NULLs

if (length(file_info_list) == 0) {
  stop("No files match the expected pattern: *-(YYYY)-data.(xlsx|csv)")
}

# Create DataFrame with file information
files_df <- do.call(rbind, lapply(file_info_list, function(x) {
  data.frame(
    filename = x$filename,
    location_indicator = x$location_indicator,
    year = x$year,
    extension = x$extension,
    stringsAsFactors = FALSE
  )
}))

cat("Files matching the pattern:", nrow(files_df), "\n")
print(files_df)

# Read all files
cat("\nReading files...\n")

# Add full path and read data
files_df$filepath <- file.path(data_folder, files_df$filename)
files_df$data <- vector("list", nrow(files_df))

for (i in 1:nrow(files_df)) {
  cat("Reading:", files_df$filename[i], "...")
  files_df$data[[i]] <- read_data_file(files_df$filepath[i], files_df$extension[i])
  if (!is.null(files_df$data[[i]])) {
    cat(" ✓ (", nrow(files_df$data[[i]]), " rows)\n")
  } else {
    cat(" ✗ Error\n")
  }
}

# Filter files that were read correctly
successful_files <- files_df[!sapply(files_df$data, is.null), ]
cat("\nFiles read successfully:", nrow(successful_files), "\n")

if (nrow(successful_files) == 0) {
  stop("Could not read any file correctly")
}

# OPTION 1: Combined DataFrame with metadata
combined_data <- data.frame()

for (i in 1:nrow(successful_files)) {
  temp_data <- successful_files$data[[i]]
  if (!is.null(temp_data) && nrow(temp_data) > 0) {
    # Add metadata columns
    temp_data$location_indicator <- successful_files$location_indicator[i]
    temp_data$year <- successful_files$year[i]
    temp_data$source_file <- successful_files$filename[i]

    # Combine with main dataset
    if (nrow(combined_data) == 0) {
      combined_data <- temp_data
    } else {
      # Use bind_rows to handle different column structures
      combined_data <- bind_rows(combined_data, temp_data)
    }
  }
}

# OPTION 2: List by indicator - CORRECTED: use reframe() instead of summarise()
data_by_indicator <- successful_files %>%
  group_by(location_indicator) %>%
  reframe(
    years = list(year),
    filenames = list(filename),
    datasets = data
  )

# OPTION 3: List by year - CORRECTED: use reframe() instead of summarise()
data_by_year <- successful_files %>%
  group_by(year) %>%
  reframe(
    indicators = list(location_indicator),
    filenames = list(filename),
    datasets = data
  )

# Function to access specific data - CORRECTED
get_data_by_indicator_year <- function(indicator, target_year) {
  # Check that successful_files exists
  if (!exists("successful_files")) {
    stop("Error: Data is not available. Run the complete loading script first.")
  }

  matching_rows <- successful_files[
    successful_files$location_indicator == indicator &
      successful_files$year == target_year,
  ]

  if (nrow(matching_rows) > 0) {
    return(matching_rows$data[[1]])
  } else {
    cat("No data found for:", indicator, "in year", target_year, "\n")
    cat("Available indicators:", paste(unique(successful_files$location_indicator), collapse = ", "), "\n")
    cat("Available years:", paste(sort(unique(successful_files$year)), collapse = ", "), "\n")
    return(NULL)
  }
}

# Show data summary
cat("\n=== DATA SUMMARY ===\n")
cat("1. Combined dataset (combined_data):\n")
cat("   - Rows:", nrow(combined_data), "\n")
cat("   - Columns:", ncol(combined_data), "\n")
if (nrow(combined_data) > 0) {
  cat("   - Unique years:", paste(sort(unique(combined_data$year)), collapse = ", "), "\n")
  cat("   - Unique indicators:", length(unique(combined_data$location_indicator)), "\n")
  cat("   - Column names:", paste(names(combined_data), collapse = ", "), "\n")
}

cat("\n2. Data by indicator (data_by_indicator):\n")
if (nrow(data_by_indicator) > 0) {
  for (i in 1:nrow(data_by_indicator)) {
    cat("   -", data_by_indicator$location_indicator[i],
        "- Years:", paste(data_by_indicator$years[[i]], collapse = ", "), "\n")
  }
}

cat("\n3. Data by year (data_by_year):\n")
if (nrow(data_by_year) > 0) {
  for (i in 1:nrow(data_by_year)) {
    cat("   - Year", data_by_year$year[i],
        "- Indicators:", paste(data_by_year$indicators[[i]], collapse = ", "), "\n")
  }
}

cat("\n=== CREATED VARIABLES ===\n")
cat("- combined_data: DataFrame with all data\n")
cat("- data_by_indicator: List grouped by indicator\n")
cat("- data_by_year: List grouped by year\n")
cat("- successful_files: DataFrame with file information (REQUIRED for functions)\n")
cat("- get_data_by_indicator_year(): Function to obtain specific data\n")

cat("\n=== USAGE EXAMPLE ===\n")
cat("# View combined dataset structure:\n")
cat("str(combined_data)\n")
cat("# Get specific data:\n")
cat("data_2000 <- get_data_by_indicator_year('GDL-Years-of-education', 2000)\n")

# IMPORTANT: Do NOT clean successful_files - it is necessary for functions
# Clean only unnecessary temporary variables
rm(files, file_info_list, files_df, temp_data, i)

cat("\n=== SCRIPT COMPLETED WITHOUT ERRORS ===\n")
