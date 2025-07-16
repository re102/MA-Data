# ====================================================================
# MAIN ANALYSIS SCRIPT
# Description: Integrated analysis of cleavages' data
# Author: Robyn Elrick
# Date: May 2025
# Version: 1.0
# ====================================================================

cat("=== STARTING GDL DATA ANALYSIS ===\n")
cat("Date:", format(Sys.Date()), "\n")
cat("Author: Robyn Elrick\n")
cat("Version: 1.0 (May 2025)\n\n")

# ====================================================================
# 1. DATA LOADING
# ====================================================================

cat("1. LOADING DATA SOURCES...\n")

# Load education data
cat("   - Loading education data...\n")
source("load_data_scripts/load_data_gdl_years.R")

if (exists("combined_data")) {
  education_data <- combined_data
  cat("   ✓ Education data loaded:", nrow(education_data), "rows\n")

  # DIAGNOSIS: Show education data structure
  cat("   - Available columns in education:", paste(names(education_data), collapse = ", "), "\n")

} else {
  stop("   ✗ Error: education data not loaded")
}

# Load health data
cat("   - Loading health data...\n")
if (file.exists("MA_Data/GDL-Health-index-data.xlsx")) {
  source("load_data_scripts/load_data_gdl_health.R")
  if (exists("health_data")) {
    cat("   ✓ Health data loaded:", nrow(health_data), "rows\n")
    cat("   - Available columns in health:", paste(names(health_data), collapse = ", "), "\n")
  } else {
    cat("   ⚠ health_data variable not created\n")
    health_data <- NULL
  }
} else {
  cat("   ⚠ Health file not found\n")
  health_data <- NULL
}

cat("\n")

# Load GDP data
cat("   - Loading GDP data...\n")
# Load functions
source("load_data_scripts/load_data_gdp.R")

# Load data
gdp_data <- read_gdp_data()

cat("\n")

# Load Unemployment data
cat("   - Loading Unemployment data...\n")
# Load functions
source("load_data_scripts/load_data_uem.R")

# Load data
uem_data <- read_uem_data()

cat("\n")

# Load Internet Penetration data
cat("   - Loading Internet Penetration data...\n")
# Load functions
source("load_data_scripts/load_data_ip.R")

# Load data
ip_data <- read_internet_penetration()

cat("\n")

# Load Democratization per Country data
cat("   - Loading Democratization per Country data...\n")
# Load functions
source("load_data_scripts/load_data_dem.R")

# Load data
dem_data <- read_dem_data()

cat("\n")

# Load COUNTRY - LANGUAGE - REGION MAPPING DATA
cat("   - Loading Country - Language - Region Mapping data...\n")
# Load functions
source("load_data_scripts/load_data_country.R")

# Load data
lang_region_data <- read_lang_region_data()

cat("\n")

# ====================================================================
# PREPROCESSING: EXPAND COUNTRIES WITH MULTIPLE LANGUAGES
# ====================================================================

cat("   - Expanding countries with multiple languages...\n")

# Make sure that the column names are standardized
colnames(lang_region_data) <- c("Country", "InstLang", "Region", "ISO")

# Separate countries with multiple languages (e.g. "French + others")
lang_region_expanded <- lang_region_data %>%
  tidyr::separate_rows(InstLang, sep = "\\s*\\+\\s*") %>%
  mutate(InstLang = trimws(InstLang))  # Delete blank spaces

cat("   ✓ Expanded countries:", nrow(lang_region_expanded), "rows\n")
cat("   ✓ Unique languages:", paste(unique(lang_region_expanded$InstLang), collapse = ", "), "\n")


# ====================================================================
# 2. DATA EXPLORATION
# ====================================================================

cat("2. DATA EXPLORATION...\n")

# Education data
cat("=== EDUCATION ===\n")

# Check main columns
required_cols <- c("year", "location_indicator", "country")
missing_cols <- setdiff(required_cols, names(education_data))

if (length(missing_cols) > 0) {
  cat("   ⚠ WARNING: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  cat("   Available columns:", paste(names(education_data), collapse = ", "), "\n")

  # Try to identify similar columns
  if (!"year" %in% names(education_data)) {
    year_cols <- grep("year|Year|YEAR|ano", names(education_data), value = TRUE, ignore.case = TRUE)
    if (length(year_cols) > 0) {
      cat("   Possible year columns:", paste(year_cols, collapse = ", "), "\n")
    }
  }

  if (!"country" %in% names(education_data)) {
    country_cols <- grep("country|Country|pais|location|Location", names(education_data), value = TRUE, ignore.case = TRUE)
    if (length(country_cols) > 0) {
      cat("   Possible country columns:", paste(country_cols, collapse = ", "), "\n")
    }
  }

} else {
  cat("Available years:", paste(sort(unique(education_data$year)), collapse = ", "), "\n")
  cat("Indicators:", paste(unique(education_data$location_indicator), collapse = ", "), "\n")
  cat("Unique countries:", length(unique(education_data$country)), "\n")
}

# Health data
if (!is.null(health_data)) {
  cat("\n=== HEALTH ===\n")

  # Check health data structure
  if ("Year" %in% names(health_data)) {
    cat("Available years:", min(health_data$Year, na.rm = TRUE), "-", max(health_data$Year, na.rm = TRUE), "\n")
  }

  if ("Country" %in% names(health_data)) {
    cat("Unique countries:", length(unique(health_data$Country)), "\n")
  }

  if ("Continent" %in% names(health_data)) {
    cat("Continents:", paste(unique(health_data$Continent), collapse = ", "), "\n")
  }
}

cat("\n")

# ====================================================================
# 3. ANALYSIS FUNCTIONS
# ====================================================================

# Function to get education data
get_education_data <- function(indicator, year = NULL, countries = NULL) {

  # Check that necessary columns exist
  if (!"location_indicator" %in% names(education_data)) {
    cat("Error: 'location_indicator' column not found\n")
    return(data.frame())
  }

  filtered <- education_data[education_data$location_indicator == indicator, ]

  if (!is.null(year) && "year" %in% names(education_data)) {
    filtered <- filtered[filtered$year == year, ]
  }

  if (!is.null(countries) && "country" %in% names(education_data)) {
    filtered <- filtered[filtered$country %in% countries, ]
  }

  return(filtered)
}

# Function to get health data
get_health_data <- function(countries = NULL, years = NULL) {
  if (is.null(health_data)) {
    cat("Health data not available\n")
    return(NULL)
  }

  filtered <- health_data

  if (!is.null(countries) && "Country" %in% names(health_data)) {
    filtered <- filtered[filtered$Country %in% countries, ]
  }

  if (!is.null(years) && "Year" %in% names(health_data)) {
    filtered <- filtered[filtered$Year %in% years, ]
  }

  return(filtered)
}

# Function to combine education and health data
combine_education_health <- function(education_indicator, target_year) {
  if (is.null(health_data)) {
    cat("Health data not available\n")
    return(NULL)
  }

  # Get education data
  edu_data <- get_education_data(education_indicator, target_year)
  if (nrow(edu_data) == 0) {
    cat("No education data for", education_indicator, "in", target_year, "\n")
    return(NULL)
  }

  # Get health data
  health_data_year <- get_health_data(years = target_year)
  if (is.null(health_data_year) || nrow(health_data_year) == 0) {
    cat("No health data for", target_year, "\n")
    return(NULL)
  }

  # Check columns before merge
  edu_country_col <- if ("Country" %in% names(edu_data)) "Country" else NULL
  health_country_col <- if ("Country" %in% names(health_data_year)) "Country" else NULL

  if (is.null(edu_country_col) || is.null(health_country_col)) {
    cat("Error: cannot identify country columns for merge\n")
    cat("Education columns:", paste(names(edu_data), collapse = ", "), "\n")
    cat("Health columns:", paste(names(health_data_year), collapse = ", "), "\n")
    return(NULL)
  }

  # Combine data
  combined <- merge(
    edu_data,
    health_data_year[, c(health_country_col, "Value")],
    by.x = edu_country_col,
    by.y = health_country_col,
    all = FALSE
  )

  # Rename columns if they exist
  if ("Value" %in% names(combined)) {
    names(combined)[names(combined) == "Value"] <- "health_value"
  }

  # Look for education value column
  value_cols <- grep("value|Value|val", names(combined), ignore.case = TRUE, value = TRUE)
  if (length(value_cols) > 0 && !"education_value" %in% names(combined)) {
    # Take the first value column that is not health_value
    edu_value_col <- setdiff(value_cols, "health_value")[1]
    if (!is.na(edu_value_col)) {
      names(combined)[names(combined) == edu_value_col] <- "education_value"
    }
  }

  if (nrow(combined) > 0) {
    cat("Combined data:", nrow(combined), "countries for", target_year, "\n")
  } else {
    cat("No common countries found\n")
  }

  return(combined)
}

cat("3. ANALYSIS FUNCTIONS DEFINED\n\n")

# ====================================================================
# 4. USAGE EXAMPLES
# ====================================================================

cat("4. USAGE EXAMPLES...\n")

# Example 1: Recent education data
if ("year" %in% names(education_data)) {
  recent_year <- max(education_data$year, na.rm = TRUE)
  cat("--- Education for", recent_year, "---\n")
  edu_example <- get_education_data("GDL-Years-of-education", recent_year)

  if (nrow(edu_example) > 0) {
    cat("Countries with data:", nrow(edu_example), "\n")

    # Look for value columns
    value_cols <- grep("value|Value|val", names(edu_example), ignore.case = TRUE, value = TRUE)
    if (length(value_cols) > 0) {
      value_col <- value_cols[1]
      values <- edu_example[[value_col]]
      values <- values[!is.na(values) & is.finite(values)]

      if (length(values) > 0) {
        cat("Value range:", round(min(values), 2), "-", round(max(values), 2), "\n")
        cat("Value column used:", value_col, "\n")
      } else {
        cat("No valid numeric values\n")
      }
    } else {
      cat("No value column found\n")
      cat("Available columns:", paste(names(edu_example), collapse = ", "), "\n")
    }
  }
} else {
  cat("--- Cannot determine most recent year ---\n")
}

# Example 2: Combined data (if health data exists)
if (!is.null(health_data)) {
  cat("\n--- Attempting combined data for 2020 ---\n")
  combined_example <- combine_education_health("GDL-Years-of-education", 2020)

  if (!is.null(combined_example) && nrow(combined_example) > 0) {
    # Show correlation if sufficient data
    if (nrow(combined_example) >= 5 &&
        "education_value" %in% names(combined_example) &&
        "health_value" %in% names(combined_example)) {

      edu_vals <- combined_example$education_value
      health_vals <- combined_example$health_value

      # Filter valid values
      valid_indices <- !is.na(edu_vals) & !is.na(health_vals) &
        is.finite(edu_vals) & is.finite(health_vals)

      if (sum(valid_indices) >= 5) {
        correlation <- cor(edu_vals[valid_indices], health_vals[valid_indices])
        cat("Education-health correlation:", round(correlation, 3), "\n")
      } else {
        cat("Insufficient valid data for correlation\n")
      }
    }

    # Show some examples
    cat("First 5 countries:\n")
    display_cols <- intersect(c("country", "education_value", "health_value"), names(combined_example))
    if (length(display_cols) > 0) {
      print(head(combined_example[, display_cols, drop = FALSE], 5))
    } else {
      print(head(combined_example, 5))
    }
  }
} else {
  cat("\n--- Health data not available ---\n")
}

cat("\n")

# ====================================================================
# 5. FINAL SUMMARY
# ====================================================================

cat("=== SUMMARY ===\n")
cat("✓ Education:", nrow(education_data), "observations\n")

if (!is.null(health_data)) {
  cat("✓ Health:", nrow(health_data), "observations\n")

  # Common years (with column verification)
  if ("year" %in% names(education_data) && "Year" %in% names(health_data)) {
    edu_years <- unique(education_data$year)
    health_years <- unique(health_data$Year)
    common_years <- intersect(edu_years, health_years)

    if (length(common_years) > 0) {
      cat("✓ Common years:", length(common_years),
          "(", min(common_years), "-", max(common_years), ")\n")
    } else {
      cat("⚠ No common years between education and health data\n")
    }
  }
} else {
  cat("⚠ Health: Not available\n")
}

cat("\n=== AVAILABLE FUNCTIONS ===\n")
cat("- get_education_data(indicator, year, countries)\n")
cat("- get_health_data(countries, years)\n")
cat("- combine_education_health(education_indicator, target_year)\n")

cat("\n=== ANALYSIS READY ===\n")


# Demographic Gap Analysis in Africa
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)

# Define African countries of interest
african_countries <- c(
  "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
  "Central African Republic", "Chad", "Comoros", "Democratic Republic of the Congo",
  "Cote d'Ivoire", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana",
  "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar",
  "Malawi", "Mali", "Mauritania", "Mozambique", "Namibia", "Nigeria",
  "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone", "South Africa",
  "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe"
)

# Country name mapping for different datasets
country_mapping <- list(
  "Central African Republic" = c("CAR", "Central African Rep."),
  "Democratic Republic of the Congo" = c("DRC", "Congo, Dem. Rep."),
  "Cote d'Ivoire" = c("Ivory Coast", "Côte d'Ivoire"),
  "Sao Tome and Principe" = c("Sao Tome", "São Tomé and Príncipe")
)

# Function to standardize country names
standardize_country_names <- function(data, country_col) {
  data_clean <- data
  for (standard_name in names(country_mapping)) {
    alternatives <- country_mapping[[standard_name]]
    for (alt in alternatives) {
      data_clean[[country_col]] <- ifelse(data_clean[[country_col]] == alt,
                                          standard_name,
                                          data_clean[[country_col]])
    }
  }
  return(data_clean)
}

# =============================================================================
# 1. GENDER GAP ANALYSIS IN INTERNET USAGE
# =============================================================================

analyze_gender_internet_gap <- function(ip_data) {
  cat("Analyzing gender gap in internet usage...\n")

  # Clean and prepare internet data
  ip_clean <- ip_data %>%
    standardize_country_names("Country Name") %>%
    filter(`Country Name` %in% african_countries) %>%
    select(-`Series Name`, -`Series Code`, -`Country Code`) %>%
    # Convert all year columns to character before pivot
    mutate_at(vars(matches("^20\\d{2}")), as.character) %>%
    pivot_longer(cols = matches("^20\\d{2}"),  # Only columns starting with 20 followed by 2 digits
                 names_to = "year_raw",
                 values_to = "internet_penetration") %>%
    # Extract year from column (e.g., "2000 [YR2000]" -> 2000)
    mutate(
      year = as.numeric(str_extract(year_raw, "^20\\d{2}")),
      # Convert internet_penetration to numeric, handling special values
      internet_penetration = as.numeric(internet_penetration)
    ) %>%
    # Filter only years 2000-2019 and valid values
    filter(
      !is.na(internet_penetration),
      year >= 2000,
      year <= 2019,
      internet_penetration > 0  # Remove 0 or negative values
    ) %>%
    select(country = `Country Name`, year, internet_penetration)

  # Check if we have data after cleaning
  if(nrow(ip_clean) == 0) {
    cat("⚠ No valid internet data found for the period 2000-2019\n")
    return(list(data = data.frame(), plot = NULL))
  }

  cat("Internet data processed:", nrow(ip_clean), "observations\n")
  cat("Countries with data:", length(unique(ip_clean$country)), "\n")
  cat("Period:", min(ip_clean$year), "-", max(ip_clean$year), "\n")

  # Create internet penetration plot by country
  p1 <- ggplot(ip_clean, aes(x = year, y = internet_penetration, color = country)) +
    geom_line(size = 0.8) +
    geom_point(size = 1, alpha = 0.7) +
    facet_wrap(~country, scales = "free_y") +
    labs(title = "Internet Penetration in African Countries (2000-2019)",
         x = "Year",
         y = "% Population using Internet",
         caption = "Note: Gender-specific data is not available in this dataset") +
    theme_minimal() +
    theme(legend.position = "none",
          strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(2000, 2019, 4))  # Show years every 4 years

  return(list(data = ip_clean, plot = p1))
}

# More robust alternative function if the previous one still has issues
analyze_gender_internet_gap_robust <- function(ip_data) {
  cat("Analyzing gender gap in internet usage (robust version)...\n")

  # Identify year columns
  year_cols <- names(ip_data)[grepl("^20\\d{2}", names(ip_data))]

  # Filter only year columns between 2000 and 2019
  year_cols_filtered <- year_cols[as.numeric(str_extract(year_cols, "^20\\d{2}")) <= 2019]

  cat("Year columns found:", length(year_cols_filtered), "\n")

  if(length(year_cols_filtered) == 0) {
    cat("⚠ No valid year columns found\n")
    return(list(data = data.frame(), plot = NULL))
  }

  # Select necessary columns
  cols_to_select <- c("Country Name", year_cols_filtered)

  ip_clean <- ip_data %>%
    standardize_country_names("Country Name") %>%
    filter(`Country Name` %in% african_countries) %>%
    select(all_of(cols_to_select)) %>%
    # Convert all year columns to character
    mutate_at(vars(all_of(year_cols_filtered)), as.character) %>%
    pivot_longer(cols = all_of(year_cols_filtered),
                 names_to = "year_raw",
                 values_to = "internet_penetration") %>%
    mutate(
      year = as.numeric(str_extract(year_raw, "^20\\d{2}")),
      internet_penetration = suppressWarnings(as.numeric(internet_penetration))
    ) %>%
    filter(
      !is.na(internet_penetration),
      !is.na(year),
      year >= 2000,
      year <= 2019,
      internet_penetration >= 0
    ) %>%
    select(country = `Country Name`, year, internet_penetration)

  if(nrow(ip_clean) == 0) {
    cat("⚠ No valid data found after processing\n")
    return(list(data = data.frame(), plot = NULL))
  }

  cat("Data processed successfully:", nrow(ip_clean), "observations\n")

  # Create plot
  p1 <- ggplot(ip_clean, aes(x = year, y = internet_penetration, color = country)) +
    geom_line(size = 0.8) +
    geom_point(size = 1, alpha = 0.7) +
    facet_wrap(~country, scales = "free_y") +
    labs(title = "Internet Penetration in African Countries (2000-2019)",
         x = "Year",
         y = "% Population using Internet") +
    theme_minimal() +
    theme(legend.position = "none",
          strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(2000, 2019, 4))

  return(list(data = ip_clean, plot = p1))
}

# =============================================================================
# 2. RURAL/URBAN GAP ANALYSIS IN EDUCATION
# =============================================================================

analyze_rural_urban_education_gap <- function(education_data) {
  cat("Analyzing rural/urban gap in education...\n")

  # Prepare rural/urban education data
  edu_rural_urban <- education_data %>%
    standardize_country_names("Country") %>%
    filter(Country %in% african_countries,
           Level == "Urb/rur") %>%
    mutate(location_type = case_when(
      grepl("Urban", Region) ~ "Urban",
      grepl("Rural", Region) ~ "Rural",
      TRUE ~ Region
    )) %>%
    filter(location_type %in% c("Urban", "Rural")) %>%
    group_by(Country, year, location_type) %>%
    summarise(
      avg_education_female = mean(`Females 20-39`, na.rm = TRUE),
      avg_education_male = mean(`Males 20-39`, na.rm = TRUE),
      avg_education_total = mean((avg_education_female + avg_education_male)/2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = location_type,
                values_from = c(avg_education_female, avg_education_male, avg_education_total)) %>%
    mutate(
      rural_urban_gap_female = avg_education_female_Urban - avg_education_female_Rural,
      rural_urban_gap_male = avg_education_male_Urban - avg_education_male_Rural,
      rural_urban_gap_total = avg_education_total_Urban - avg_education_total_Rural
    ) %>%
    filter(!is.na(rural_urban_gap_total))

  # Rural/urban gap plot
  p2 <- edu_rural_urban %>%
    select(Country, year, rural_urban_gap_female, rural_urban_gap_male) %>%
    pivot_longer(cols = c(rural_urban_gap_female, rural_urban_gap_male),
                 names_to = "gender", values_to = "gap") %>%
    mutate(gender = ifelse(gender == "rural_urban_gap_female", "Women", "Men")) %>%
    ggplot(aes(x = year, y = gap, color = gender)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(~Country, scales = "free") +
    labs(title = "Rural-Urban Educational Gap by Gender",
         subtitle = "Difference in years of education (Urban - Rural)",
         x = "Year",
         y = "Difference in years of education",
         color = "Gender") +
    theme_minimal() +
    theme(strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(list(data = edu_rural_urban, plot = p2))
}

# =============================================================================
# 3. GENDER GAP ANALYSIS IN EDUCATION
# =============================================================================

analyze_gender_education_gap <- function(education_data) {
  cat("Analyzing gender gap in education...\n")

  # Calculate gender gap in education
  edu_gender_gap <- education_data %>%
    standardize_country_names("Country") %>%
    filter(Country %in% african_countries,
           Level == "National") %>%
    group_by(Country, year) %>%
    summarise(
      avg_education_female = mean(`Females 20-39`, na.rm = TRUE),
      avg_education_male = mean(`Males 20-39`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(gender_gap = avg_education_male - avg_education_female) %>%
    filter(!is.na(gender_gap))

  # Gender gap in education plot
  p3 <- ggplot(edu_gender_gap, aes(x = year, y = gender_gap)) +
    geom_line(color = "steelblue", size = 0.8) +
    geom_point(color = "steelblue", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    facet_wrap(~Country, scales = "free") +
    labs(title = "Gender Gap in Education",
         subtitle = "Difference in years of education (Men - Women)",
         x = "Year",
         y = "Difference in years of education",
         caption = "Positive values indicate male advantage, negative values indicate female advantage") +
    theme_minimal() +
    theme(strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(list(data = edu_gender_gap, plot = p3))
}

# =============================================================================
# 4. SOCIOECONOMIC GAP ANALYSIS IN EDUCATION
# =============================================================================

analyze_socioeconomic_education_gap <- function(education_data) {
  cat("Analyzing socioeconomic gap in education...\n")

  # Prepare education data by wealth quartiles
  edu_socio_gap <- education_data %>%
    standardize_country_names("Country") %>%
    filter(Country %in% african_countries,
           Level == "Wealth quartiles") %>%
    mutate(wealth_level = case_when(
      grepl("Lowest 25%", Region) ~ "Low",
      grepl("Highest 25%", Region) ~ "High",
      grepl("Second 25%", Region) ~ "Second",
      grepl("Third 25%", Region) ~ "Third",
      TRUE ~ Region
    )) %>%
    filter(wealth_level %in% c("Low", "High")) %>%
    group_by(Country, year, wealth_level) %>%
    summarise(
      avg_education_female = mean(`Females 20-39`, na.rm = TRUE),
      avg_education_male = mean(`Males 20-39`, na.rm = TRUE),
      avg_education_total = mean((avg_education_female + avg_education_male)/2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = wealth_level,
                values_from = c(avg_education_female, avg_education_male, avg_education_total)) %>%
    mutate(
      socio_gap_female = avg_education_female_High - avg_education_female_Low,
      socio_gap_male = avg_education_male_High - avg_education_male_Low,
      socio_gap_total = avg_education_total_High - avg_education_total_Low
    ) %>%
    filter(!is.na(socio_gap_total))

  # Check if we have data after processing
  if (nrow(edu_socio_gap) == 0) {
    cat("⚠ No data found for wealth quartiles (Lowest 25% and Highest 25%)\n")

    # Create empty dataframe and message plot
    empty_df <- data.frame(
      Country = character(0),
      year = numeric(0),
      socio_gap_female = numeric(0),
      socio_gap_male = numeric(0),
      socio_gap_total = numeric(0)
    )

    p4 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "Insufficient data for socioeconomic analysis\n(Wealth quartiles: Lowest 25% and Highest 25%)",
               size = 5, hjust = 0.5) +
      theme_void() +
      labs(title = "Socioeconomic Gap in Education by Gender",
           subtitle = "Insufficient data for analysis")

    return(list(data = empty_df, plot = p4))
  }

  cat("Socioeconomic gap data processed:", nrow(edu_socio_gap), "observations\n")
  cat("Countries with data:", length(unique(edu_socio_gap$Country)), "\n")

  # Socioeconomic gap plot
  p4 <- edu_socio_gap %>%
    select(Country, year, socio_gap_female, socio_gap_male) %>%
    pivot_longer(cols = c(socio_gap_female, socio_gap_male),
                 names_to = "gender", values_to = "gap") %>%
    mutate(gender = ifelse(gender == "socio_gap_female", "Women", "Men")) %>%
    ggplot(aes(x = year, y = gap, color = gender)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(~Country, scales = "free") +
    labs(title = "Socioeconomic Gap in Education by Gender",
         subtitle = "Difference in years of education (Highest quartile - Lowest quartile)",
         x = "Year",
         y = "Difference in years of education",
         color = "Gender") +
    theme_minimal() +
    theme(strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(list(data = edu_socio_gap, plot = p4))
}

# =============================================================================
# 5. MAIN FUNCTION TO EXECUTE ALL ANALYSES
# =============================================================================
run_gap_analysis <- function() {
  cat("=== STARTING DEMOGRAPHIC GAPS ANALYSIS ===\n\n")
  # Execute analyses
  internet_analysis <- analyze_gender_internet_gap(ip_data)
  rural_urban_analysis <- analyze_rural_urban_education_gap(education_data)
  gender_education_analysis <- analyze_gender_education_gap(education_data)
  socioeconomic_analysis <- analyze_socioeconomic_education_gap(education_data)
  # Display plots
  print(internet_analysis$plot)
  print(rural_urban_analysis$plot)
  print(gender_education_analysis$plot)
  print(socioeconomic_analysis$plot)
  # Return results
  results <- list(
    internet = internet_analysis,
    rural_urban = rural_urban_analysis,
    gender_education = gender_education_analysis,
    socioeconomic = socioeconomic_analysis
  )
  cat("\n=== ANALYSIS COMPLETED ===\n")
  cat("Results saved in 'gap_results'\n")
  return(results)
}
# Execute analysis
gap_results <- run_gap_analysis()
# Save plots to files
ggsave("plot_results/01_internet_penetration_africa.png", gap_results$internet$plot,
       width = 15, height = 12, dpi = 300)
ggsave("plot_results/02_rural_urban_education_gap.png", gap_results$rural_urban$plot,
       width = 15, height = 12, dpi = 300)
ggsave("plot_results/03_gender_education_gap.png", gap_results$gender_education$plot,
       width = 15, height = 12, dpi = 300)
ggsave("plot_results/04_socioeconomic_education_gap.png", gap_results$socioeconomic$plot,
       width = 15, height = 12, dpi = 300)

# ====================================================================
# EXTENDED GAP ANALYSIS WITH INTERNET PENETRATION
# Description: Analysis of educational gaps vs internet penetration with controls
# Author: Robyn Elrick
# Date: May 2025
# ====================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(corrplot)
library(gridExtra)
library(scales)

# ====================================================================
# 1. DATA PREPARATION FUNCTIONS
# ====================================================================

# Function to prepare comprehensive dataset
prepare_comprehensive_dataset <- function(education_data, ip_data, gdp_data, uem_data, dem_data, health_data) {
  cat("Preparing comprehensive dataset...\n")

  # Standardize country names across all datasets
  standardize_country_names <- function(data, country_col) {
    country_mapping <- list(
      "Central African Republic" = c("CAR", "Central African Rep."),
      "Democratic Republic of the Congo" = c("DRC", "Congo, Dem. Rep.", "Congo, Democratic Republic of the"),
      "Cote d'Ivoire" = c("Ivory Coast", "Côte d'Ivoire"),
      "Sao Tome and Principe" = c("Sao Tome", "São Tomé and Príncipe"),
      "Tanzania" = c("Tanzania, United Republic of"),
      "Congo" = c("Congo, Rep.")
    )

    data_clean <- data
    for (standard_name in names(country_mapping)) {
      alternatives <- country_mapping[[standard_name]]
      for (alt in alternatives) {
        data_clean[[country_col]] <- ifelse(data_clean[[country_col]] == alt,
                                            standard_name,
                                            data_clean[[country_col]])
      }
    }
    return(data_clean)
  }

  # African countries of interest
  african_countries <- c(
    "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
    "Central African Republic", "Chad", "Comoros", "Democratic Republic of the Congo",
    "Cote d'Ivoire", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana",
    "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar",
    "Malawi", "Mali", "Mauritania", "Mozambique", "Namibia", "Nigeria",
    "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone", "South Africa",
    "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe"
  )

  # 1. Prepare Internet Penetration Data
  cat("  Processing internet penetration data...\n")
  year_cols <- names(ip_data)[grepl("^20\\d{2}", names(ip_data))]
  year_cols_filtered <- year_cols[as.numeric(str_extract(year_cols, "^20\\d{2}")) %in% 2000:2019]

  ip_clean <- ip_data %>%
    standardize_country_names("Country Name") %>%
    filter(`Country Name` %in% african_countries) %>%
    select(all_of(c("Country Name", year_cols_filtered))) %>%
    mutate_at(vars(all_of(year_cols_filtered)), as.character) %>%
    pivot_longer(cols = all_of(year_cols_filtered),
                 names_to = "year_raw",
                 values_to = "internet_penetration") %>%
    mutate(
      year = as.numeric(str_extract(year_raw, "^20\\d{2}")),
      internet_penetration = suppressWarnings(as.numeric(internet_penetration))
    ) %>%
    filter(!is.na(internet_penetration), !is.na(year), year >= 2000, year <= 2019) %>%
    select(country = `Country Name`, year, internet_penetration)

  # 2. Prepare Education Data with Gaps
  cat("  Processing education data and calculating gaps...\n")

  # Gender Gap
  gender_gap <- education_data %>%
    standardize_country_names("Country") %>%
    filter(Country %in% african_countries, Level == "National") %>%
    group_by(Country, year) %>%
    summarise(
      avg_education_female = mean(`Females 20-39`, na.rm = TRUE),
      avg_education_male = mean(`Males 20-39`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(gender_gap = avg_education_male - avg_education_female) %>%
    select(country = Country, year, gender_gap)

  # Rural-Urban Gap
  rural_urban_gap <- education_data %>%
    standardize_country_names("Country") %>%
    filter(Country %in% african_countries, Level == "Urb/rur") %>%
    mutate(location_type = case_when(
      grepl("Urban", Region) ~ "Urban",
      grepl("Rural", Region) ~ "Rural",
      TRUE ~ Region
    )) %>%
    filter(location_type %in% c("Urban", "Rural")) %>%
    group_by(Country, year, location_type) %>%
    summarise(
      avg_education = mean((`Females 20-39` + `Males 20-39`)/2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = location_type, values_from = avg_education) %>%
    mutate(rural_urban_gap = Urban - Rural) %>%
    select(country = Country, year, rural_urban_gap)

  # Socioeconomic Gap (using wealth quartiles)
  socio_gap <- education_data %>%
    standardize_country_names("Country") %>%
    filter(Country %in% african_countries, Level == "Wealth quartiles") %>%
    mutate(wealth_level = case_when(
      grepl("Lowest 25%", Region) ~ "Low",
      grepl("Highest 25%", Region) ~ "High",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(wealth_level)) %>%
    group_by(Country, year, wealth_level) %>%
    summarise(
      avg_education = mean((`Females 20-39` + `Males 20-39`)/2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = wealth_level, values_from = avg_education) %>%
    mutate(socioeconomic_gap = High - Low) %>%
    select(country = Country, year, socioeconomic_gap)

  # 3. Prepare Control Variables
  cat("  Processing control variables...\n")

  # GDP Data
  gdp_years <- names(gdp_data)[grepl("^20\\d{2}", names(gdp_data))]
  gdp_years_filtered <- gdp_years[as.numeric(str_extract(gdp_years, "^20\\d{2}")) %in% 2000:2019]

  gdp_clean <- gdp_data %>%
    standardize_country_names("Country Name") %>%
    filter(`Country Name` %in% african_countries) %>%
    select(all_of(c("Country Name", gdp_years_filtered))) %>%
    mutate_at(vars(all_of(gdp_years_filtered)), as.character) %>%
    pivot_longer(cols = all_of(gdp_years_filtered),
                 names_to = "year_raw",
                 values_to = "gdp") %>%
    mutate(
      year = as.numeric(str_extract(year_raw, "^20\\d{2}")),
      gdp = suppressWarnings(as.numeric(gdp))
    ) %>%
    filter(!is.na(gdp), !is.na(year)) %>%
    select(country = `Country Name`, year, gdp)

  # Unemployment Data (for parental employment proxy - lagged 10 years)
  uem_years <- names(uem_data)[grepl("^19\\d{2}|^20\\d{2}", names(uem_data))]
  uem_years_filtered <- uem_years[as.numeric(str_extract(uem_years, "^(19|20)\\d{2}")) %in% 1990:2012]

  uem_clean <- uem_data %>%
    standardize_country_names("Country Name") %>%
    filter(`Country Name` %in% african_countries) %>%
    select(all_of(c("Country Name", uem_years_filtered))) %>%
    mutate_at(vars(all_of(uem_years_filtered)), as.character) %>%
    pivot_longer(cols = all_of(uem_years_filtered),
                 names_to = "year_raw",
                 values_to = "unemployment") %>%
    mutate(
      year_original = as.numeric(str_extract(year_raw, "^(19|20)\\d{2}")),
      year = year_original + 10,  # Lag 10 years for parental effect
      unemployment = suppressWarnings(as.numeric(unemployment))
    ) %>%
    filter(!is.na(unemployment), !is.na(year), year >= 2000, year <= 2019) %>%
    select(country = `Country Name`, year, parental_unemployment = unemployment)

  # Democracy Data
  dem_clean <- dem_data %>%
    standardize_country_names("country_name") %>%
    filter(country_name %in% african_countries, year >= 2000, year <= 2019) %>%
    select(country = country_name, year, democracy_index = v2x_polyarchy)

  # Health Data
  health_clean <- NULL
  if (!is.null(health_data)) {
    health_clean <- health_data %>%
      standardize_country_names("Country") %>%
      filter(Country %in% african_countries, Year >= 2000, Year <= 2019) %>%
      select(country = Country, year = Year, health_index = Value)
  }

  # 4. Merge all datasets
  cat("  Merging datasets...\n")

  # Start with internet penetration as base
  comprehensive_data <- ip_clean

  # Add education gaps
  comprehensive_data <- comprehensive_data %>%
    left_join(gender_gap, by = c("country", "year")) %>%
    left_join(rural_urban_gap, by = c("country", "year")) %>%
    left_join(socio_gap, by = c("country", "year"))

  # Add control variables
  comprehensive_data <- comprehensive_data %>%
    left_join(gdp_clean, by = c("country", "year")) %>%
    left_join(uem_clean, by = c("country", "year")) %>%
    left_join(dem_clean, by = c("country", "year"))

  if (!is.null(health_clean)) {
    comprehensive_data <- comprehensive_data %>%
      left_join(health_clean, by = c("country", "year"))
  }

  # Filter out rows where health data is missing (as per requirement)
  if ("health_index" %in% names(comprehensive_data)) {
    comprehensive_data <- comprehensive_data %>%
      filter(!is.na(health_index))
  }

  cat("  Final dataset:", nrow(comprehensive_data), "observations\n")
  cat("  Countries:", length(unique(comprehensive_data$country)), "\n")
  cat("  Years:", min(comprehensive_data$year, na.rm = TRUE), "-", max(comprehensive_data$year, na.rm = TRUE), "\n")

  return(comprehensive_data)
}

# ====================================================================
# 2. ANALYSIS FUNCTIONS
# ====================================================================

# Function to analyze internet penetration vs educational gaps over time
analyze_internet_gap_correlation <- function(data) {
  cat("Analyzing internet penetration vs educational gaps...\n")

  # Filter complete cases for each gap type
  gender_data <- data %>%
    filter(!is.na(internet_penetration), !is.na(gender_gap)) %>%
    select(country, year, internet_penetration, gap = gender_gap) %>%
    mutate(gap_type = "Gender Gap (M-F)")

  rural_urban_data <- data %>%
    filter(!is.na(internet_penetration), !is.na(rural_urban_gap)) %>%
    select(country, year, internet_penetration, gap = rural_urban_gap) %>%
    mutate(gap_type = "Rural-Urban Gap (U-R)")

  socio_data <- data %>%
    filter(!is.na(internet_penetration), !is.na(socioeconomic_gap)) %>%
    select(country, year, internet_penetration, gap = socioeconomic_gap) %>%
    mutate(gap_type = "Socioeconomic Gap (H-L)")

  # Combine all gap types
  combined_gaps <- bind_rows(gender_data, rural_urban_data, socio_data)

  return(combined_gaps)
}

# Function to create correlation plots
create_correlation_plots <- function(gap_data) {

  # 1. Scatter plot: Internet penetration vs gaps
  p1 <- ggplot(gap_data, aes(x = internet_penetration, y = gap, color = gap_type)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
    facet_wrap(~gap_type, scales = "free_y") +
    labs(title = "Educational Gaps vs Internet Penetration",
         subtitle = "Relationship between internet access and educational inequalities",
         x = "Internet Penetration (% of population)",
         y = "Educational Gap (years)",
         color = "Gap Type") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # 2. Time series: Evolution of gaps over time
  gap_by_year <- gap_data %>%
    group_by(year, gap_type) %>%
    summarise(
      avg_gap = mean(gap, na.rm = TRUE),
      avg_internet = mean(internet_penetration, na.rm = TRUE),
      .groups = "drop"
    )

  p2 <- ggplot(gap_by_year, aes(x = year)) +
    geom_line(aes(y = avg_gap, color = "Educational Gap"), size = 1.2) +
    geom_line(aes(y = avg_internet/10, color = "Internet Penetration"), size = 1.2, linetype = "dashed") +
    facet_wrap(~gap_type, scales = "free_y") +
    scale_y_continuous(
      name = "Average Educational Gap (years)",
      sec.axis = sec_axis(~.*10, name = "Average Internet Penetration (%)")
    ) +
    scale_color_manual(values = c("Educational Gap" = "steelblue", "Internet Penetration" = "red")) +
    labs(title = "Evolution of Educational Gaps and Internet Penetration Over Time",
         subtitle = "Trends in Africa (2000-2019)",
         x = "Year",
         color = "Measure") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))

  # 3. Country-specific analysis for top countries
  top_countries <- gap_data %>%
    group_by(country) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    arrange(desc(n_obs)) %>%
    head(12) %>%
    pull(country)

  p3 <- gap_data %>%
    filter(country %in% top_countries) %>%
    ggplot(aes(x = year, y = gap, color = gap_type)) +
    geom_line(size = 0.8, alpha = 0.8) +
    geom_point(size = 1, alpha = 0.7) +
    facet_wrap(~country, scales = "free") +
    labs(title = "Educational Gaps by Country Over Time",
         subtitle = "Countries with most complete data",
         x = "Year",
         y = "Educational Gap (years)",
         color = "Gap Type") +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(list(scatter = p1, time_series = p2, country_specific = p3))
}

# Function to calculate correlations with controls
analyze_correlations_with_controls <- function(data) {
  cat("Analyzing correlations with control variables...\n")

  # Prepare data for correlation analysis
  correlation_data <- data %>%
    select(internet_penetration, gender_gap, rural_urban_gap, socioeconomic_gap,
           gdp, parental_unemployment, democracy_index, health_index) %>%
    filter(complete.cases(.))

  if (nrow(correlation_data) < 10) {
    cat("⚠ Insufficient complete cases for correlation analysis\n")
    return(NULL)
  }

  # Calculate correlation matrix
  cor_matrix <- cor(correlation_data, use = "complete.obs")

  # Create correlation plot
  png("plot_results/05_correlation_matrix.png", width = 800, height = 600)
  corrplot(cor_matrix, method = "color", type = "upper",
           order = "hclust", tl.cex = 0.8, tl.col = "black")
  title("Correlation Matrix: Internet Penetration, Educational Gaps, and Controls")
  dev.off()

  return(cor_matrix)
}

# Function to create summary statistics
create_summary_statistics <- function(data) {
  cat("Creating summary statistics...\n")

  summary_stats <- data %>%
    select(internet_penetration, gender_gap, rural_urban_gap, socioeconomic_gap,
           gdp, parental_unemployment, democracy_index) %>%
    summarise_all(list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      n = ~sum(!is.na(.))
    ), .names = "{.col}_{.fn}")

  # Reshape for better display
  summary_long <- summary_stats %>%
    pivot_longer(everything(), names_to = "var_stat", values_to = "value") %>%
    separate(var_stat, into = c("variable", "statistic"), sep = "_(?=[^_]*$)") %>%
    pivot_wider(names_from = statistic, values_from = value)

  return(summary_long)
}

# ====================================================================
# 3. MAIN EXECUTION FUNCTION
# ====================================================================

run_comprehensive_analysis <- function() {
  cat("=== STARTING COMPREHENSIVE GAP ANALYSIS ===\n\n")

  # Check if required data exists
  required_data <- c("education_data", "ip_data", "gdp_data", "uem_data", "dem_data")
  missing_data <- setdiff(required_data, ls(envir = .GlobalEnv))

  if (length(missing_data) > 0) {
    cat("⚠ Missing required datasets:", paste(missing_data, collapse = ", "), "\n")
    return(NULL)
  }

  # Prepare comprehensive dataset
  comprehensive_data <- prepare_comprehensive_dataset(
    education_data, ip_data, gdp_data, uem_data, dem_data,
    if(exists("health_data")) health_data else NULL
  )

  # Analyze gaps vs internet penetration
  gap_analysis_data <- analyze_internet_gap_correlation(comprehensive_data)

  # Create plots
  plots <- create_correlation_plots(gap_analysis_data)

  # Print plots
  print(plots$scatter)
  print(plots$time_series)
  print(plots$country_specific)

  # Analyze correlations with controls
  correlation_matrix <- analyze_correlations_with_controls(comprehensive_data)

  # Create summary statistics
  summary_stats <- create_summary_statistics(comprehensive_data)

  # Save plots
  ggsave("plot_results/06_gaps_vs_internet_scatter.png", plots$scatter,
         width = 12, height = 8, dpi = 300)
  ggsave("plot_results/07_gaps_evolution_time_series.png", plots$time_series,
         width = 12, height = 8, dpi = 300)
  ggsave("plot_results/08_gaps_by_country.png", plots$country_specific,
         width = 15, height = 12, dpi = 300)

  # Print summary
  cat("\n=== ANALYSIS SUMMARY ===\n")
  print(summary_stats)

  if (!is.null(correlation_matrix)) {
    cat("\n=== KEY CORRELATIONS ===\n")
    cat("Internet Penetration vs Gender Gap:",
        round(correlation_matrix["internet_penetration", "gender_gap"], 3), "\n")
    cat("Internet Penetration vs Rural-Urban Gap:",
        round(correlation_matrix["internet_penetration", "rural_urban_gap"], 3), "\n")
    cat("Internet Penetration vs Socioeconomic Gap:",
        round(correlation_matrix["internet_penetration", "socioeconomic_gap"], 3), "\n")
  }

  # Return results
  results <- list(
    data = comprehensive_data,
    gap_analysis = gap_analysis_data,
    plots = plots,
    correlations = correlation_matrix,
    summary = summary_stats
  )

  cat("\n=== COMPREHENSIVE ANALYSIS COMPLETED ===\n")
  return(results)
}

# ====================================================================
# 4. EXECUTE ANALYSIS
# ====================================================================

# Run the comprehensive analysis
comprehensive_results <- run_comprehensive_analysis()

# Additional specialized analysis for specific cleavages
analyze_specific_cleavage_trends <- function(data, cleavage_type) {
  cat("Analyzing", cleavage_type, "trends...\n")

  if (cleavage_type == "gender") {
    plot_data <- data %>%
      filter(!is.na(gender_gap), !is.na(internet_penetration)) %>%
      group_by(year) %>%
      summarise(
        avg_gap = mean(gender_gap, na.rm = TRUE),
        avg_internet = mean(internet_penetration, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(plot_data, aes(x = avg_internet, y = avg_gap)) +
      geom_point(size = 3, color = "steelblue") +
      geom_text(aes(label = year), vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
      labs(title = "Gender Gap in Education vs Internet Penetration Over Time",
           subtitle = "Average across African countries by year",
           x = "Average Internet Penetration (%)",
           y = "Average Gender Gap in Education (years, M-F)") +
      theme_minimal()

  } else if (cleavage_type == "rural_urban") {
    plot_data <- data %>%
      filter(!is.na(rural_urban_gap), !is.na(internet_penetration)) %>%
      group_by(year) %>%
      summarise(
        avg_gap = mean(rural_urban_gap, na.rm = TRUE),
        avg_internet = mean(internet_penetration, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(plot_data, aes(x = avg_internet, y = avg_gap)) +
      geom_point(size = 3, color = "darkgreen") +
      geom_text(aes(label = year), vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
      labs(title = "Rural-Urban Gap in Education vs Internet Penetration Over Time",
           subtitle = "Average across African countries by year",
           x = "Average Internet Penetration (%)",
           y = "Average Rural-Urban Gap in Education (years, Urban-Rural)") +
      theme_minimal()

  } else if (cleavage_type == "socioeconomic") {
    plot_data <- data %>%
      filter(!is.na(socioeconomic_gap), !is.na(internet_penetration)) %>%
      group_by(year) %>%
      summarise(
        avg_gap = mean(socioeconomic_gap, na.rm = TRUE),
        avg_internet = mean(internet_penetration, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(plot_data, aes(x = avg_internet, y = avg_gap)) +
      geom_point(size = 3, color = "purple") +
      geom_text(aes(label = year), vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
      labs(title = "Socioeconomic Gap in Education vs Internet Penetration Over Time",
           subtitle = "Average across African countries by year",
           x = "Average Internet Penetration (%)",
           y = "Average Socioeconomic Gap in Education (years, High-Low)") +
      theme_minimal()
  }

  return(p)
}

# Create specific cleavage plots
if (!is.null(comprehensive_results)) {
  gender_trend_plot <- analyze_specific_cleavage_trends(comprehensive_results$data, "gender")
  rural_urban_trend_plot <- analyze_specific_cleavage_trends(comprehensive_results$data, "rural_urban")
  socioeconomic_trend_plot <- analyze_specific_cleavage_trends(comprehensive_results$data, "socioeconomic")

  print(gender_trend_plot)
  print(rural_urban_trend_plot)
  print(socioeconomic_trend_plot)

  # Save specialized plots
  ggsave("plot_results/09_gender_gap_vs_internet_trend.png", gender_trend_plot,
         width = 10, height = 7, dpi = 300)
  ggsave("plot_results/10_rural_urban_gap_vs_internet_trend.png", rural_urban_trend_plot,
         width = 10, height = 7, dpi = 300)
  ggsave("plot_results/11_socioeconomic_gap_vs_internet_trend.png", socioeconomic_trend_plot,
         width = 10, height = 7, dpi = 300)
}

cat("\n=== ALL ANALYSES COMPLETED ===\n")
cat("Check 'plot_results' folder for saved graphs\n")
cat("Results available in 'comprehensive_results' object\n")

# ====================================================================
# REGIONAL AND LANGUAGE ANALYSIS EXTENSION
# Description: Extend gap analysis to examine regional and language variations
# Author: Extension to Robyn Elrick's analysis
# Date: May 2025
# ====================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(corrplot)
library(gridExtra)
library(scales)
library(patchwork)  # For combining plots

# ====================================================================
# REGIONAL AND LANGUAGE MAPPING FUNCTIONS
# ====================================================================

# Function to add regional and language classifications
add_regional_language_data <- function(comprehensive_data, lang_region_expanded) {
  cat("Adding regional and language classifications...\n")

  # Clean and prepare lang_region_expanded data
  lang_region_clean <- lang_region_expanded %>%
    select(Country, InstLang, Region) %>%
    distinct() %>%
    # Standardize country names to match comprehensive_data
    mutate(
      Country = case_when(
        Country == "Bostwana" ~ "Botswana",  # Fix typo
        Country == "CAR" ~ "Central African Republic",
        TRUE ~ Country
      )
    )

  # Create language groupings
  lang_mapping <- lang_region_clean %>%
    mutate(
      language_group = case_when(
        InstLang == "Fr" ~ "French",
        InstLang == "Eng" ~ "English",
        InstLang == "Port" ~ "Portuguese",
        InstLang == "Other" ~ "Other",
        TRUE ~ "Other"
      )
    ) %>%
    select(country = Country, region = Region, language_group)

  # Handle countries with multiple language classifications
  # For analysis, we'll take the first (most common) classification per country
  lang_mapping_unique <- lang_mapping %>%
    group_by(country) %>%
    slice(1) %>%
    ungroup()

  # Merge with comprehensive data
  enhanced_data <- comprehensive_data %>%
    left_join(lang_mapping_unique, by = "country")

  # Check coverage
  countries_with_classification <- enhanced_data %>%
    filter(!is.na(region), !is.na(language_group)) %>%
    pull(country) %>%
    unique()

  countries_without_classification <- enhanced_data %>%
    filter(is.na(region) | is.na(language_group)) %>%
    pull(country) %>%
    unique()

  cat("Countries with regional/language classification:", length(countries_with_classification), "\n")
  cat("Countries without classification:", length(countries_without_classification), "\n")
  if(length(countries_without_classification) > 0) {
    cat("Missing classifications for:", paste(countries_without_classification, collapse = ", "), "\n")
  }

  return(enhanced_data)
}

# ====================================================================
# REGIONAL ANALYSIS FUNCTIONS
# ====================================================================

# Function to analyze correlations by region
analyze_correlations_by_region <- function(enhanced_data) {
  cat("Analyzing correlations by region...\n")

  # Filter data with regional classifications
  regional_data <- enhanced_data %>%
    filter(!is.na(region))

  regions <- unique(regional_data$region)

  correlation_results <- list()

  for(reg in regions) {
    cat("  Processing region:", reg, "\n")

    region_subset <- regional_data %>%
      filter(region == reg) %>%
      select(internet_penetration, gender_gap, rural_urban_gap, socioeconomic_gap) %>%
      filter(complete.cases(.))

    if(nrow(region_subset) >= 10) {  # Minimum observations for meaningful correlation
      cor_matrix <- cor(region_subset, use = "complete.obs")

      correlation_results[[reg]] <- list(
        n_obs = nrow(region_subset),
        correlations = cor_matrix,
        internet_gender = cor_matrix["internet_penetration", "gender_gap"],
        internet_rural_urban = cor_matrix["internet_penetration", "rural_urban_gap"],
        internet_socioeconomic = cor_matrix["internet_penetration", "socioeconomic_gap"]
      )
    } else {
      cat("    ⚠ Insufficient data for region", reg, "(n =", nrow(region_subset), ")\n")
    }
  }

  return(correlation_results)
}

# Function to analyze correlations by language
analyze_correlations_by_language <- function(enhanced_data) {
  cat("Analyzing correlations by language group...\n")

  # Filter data with language classifications
  language_data <- enhanced_data %>%
    filter(!is.na(language_group))

  languages <- unique(language_data$language_group)

  correlation_results <- list()

  for(lang in languages) {
    cat("  Processing language group:", lang, "\n")

    language_subset <- language_data %>%
      filter(language_group == lang) %>%
      select(internet_penetration, gender_gap, rural_urban_gap, socioeconomic_gap) %>%
      filter(complete.cases(.))

    if(nrow(language_subset) >= 10) {  # Minimum observations for meaningful correlation
      cor_matrix <- cor(language_subset, use = "complete.obs")

      correlation_results[[lang]] <- list(
        n_obs = nrow(language_subset),
        correlations = cor_matrix,
        internet_gender = cor_matrix["internet_penetration", "gender_gap"],
        internet_rural_urban = cor_matrix["internet_penetration", "rural_urban_gap"],
        internet_socioeconomic = cor_matrix["internet_penetration", "socioeconomic_gap"]
      )
    } else {
      cat("    ⚠ Insufficient data for language", lang, "(n =", nrow(language_subset), ")\n")
    }
  }

  return(correlation_results)
}

# ====================================================================
# VISUALIZATION FUNCTIONS
# ====================================================================

# Function to create regional comparison plots
create_regional_plots <- function(enhanced_data, regional_correlations) {

  # 1. Scatter plots by region
  regional_scatter_data <- enhanced_data %>%
    filter(!is.na(region)) %>%
    select(country, year, region, internet_penetration, gender_gap, rural_urban_gap, socioeconomic_gap) %>%
    pivot_longer(cols = c(gender_gap, rural_urban_gap, socioeconomic_gap),
                 names_to = "gap_type", values_to = "gap_value") %>%
    filter(!is.na(gap_value), !is.na(internet_penetration)) %>%
    mutate(gap_type = case_when(
      gap_type == "gender_gap" ~ "Gender Gap (M-F)",
      gap_type == "rural_urban_gap" ~ "Rural-Urban Gap (U-R)",
      gap_type == "socioeconomic_gap" ~ "Socioeconomic Gap (H-L)"
    ))

  p1 <- ggplot(regional_scatter_data, aes(x = internet_penetration, y = gap_value, color = region)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
    facet_wrap(~gap_type, scales = "free_y") +
    labs(title = "Educational Gaps vs Internet Penetration by Region",
         subtitle = "Regional differences in the relationship between internet access and educational inequalities",
         x = "Internet Penetration (% of population)",
         y = "Educational Gap (years)",
         color = "Region") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # 2. Regional correlation comparison
  if(length(regional_correlations) > 0) {
    correlation_summary <- map_dfr(names(regional_correlations), function(reg) {
      data.frame(
        region = reg,
        n_obs = regional_correlations[[reg]]$n_obs,
        gender_corr = regional_correlations[[reg]]$internet_gender,
        rural_urban_corr = regional_correlations[[reg]]$internet_rural_urban,
        socioeconomic_corr = regional_correlations[[reg]]$internet_socioeconomic
      )
    }) %>%
      pivot_longer(cols = c(gender_corr, rural_urban_corr, socioeconomic_corr),
                   names_to = "gap_type", values_to = "correlation") %>%
      mutate(gap_type = case_when(
        gap_type == "gender_corr" ~ "Gender Gap",
        gap_type == "rural_urban_corr" ~ "Rural-Urban Gap",
        gap_type == "socioeconomic_corr" ~ "Socioeconomic Gap"
      ))

    p2 <- ggplot(correlation_summary, aes(x = region, y = correlation, fill = gap_type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(title = "Correlation Coefficients: Internet Penetration vs Educational Gaps by Region",
           subtitle = "Negative values indicate gaps decrease as internet penetration increases",
           x = "Region",
           y = "Correlation Coefficient",
           fill = "Gap Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")

    # 3. Sample sizes by region
    sample_size_data <- map_dfr(names(regional_correlations), function(reg) {
      data.frame(
        region = reg,
        n_obs = regional_correlations[[reg]]$n_obs
      )
    })

    p3 <- ggplot(sample_size_data, aes(x = reorder(region, n_obs), y = n_obs)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = n_obs), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(title = "Sample Sizes by Region",
           subtitle = "Number of complete observations for correlation analysis",
           x = "Region",
           y = "Number of Observations") +
      theme_minimal()

    return(list(scatter = p1, correlations = p2, sample_sizes = p3))
  } else {
    return(list(scatter = p1))
  }
}

# Function to create language comparison plots
create_language_plots <- function(enhanced_data, language_correlations) {

  # 1. Scatter plots by language
  language_scatter_data <- enhanced_data %>%
    filter(!is.na(language_group)) %>%
    select(country, year, language_group, internet_penetration, gender_gap, rural_urban_gap, socioeconomic_gap) %>%
    pivot_longer(cols = c(gender_gap, rural_urban_gap, socioeconomic_gap),
                 names_to = "gap_type", values_to = "gap_value") %>%
    filter(!is.na(gap_value), !is.na(internet_penetration)) %>%
    mutate(gap_type = case_when(
      gap_type == "gender_gap" ~ "Gender Gap (M-F)",
      gap_type == "rural_urban_gap" ~ "Rural-Urban Gap (U-R)",
      gap_type == "socioeconomic_gap" ~ "Socioeconomic Gap (H-L)"
    ))

  p1 <- ggplot(language_scatter_data, aes(x = internet_penetration, y = gap_value, color = language_group)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
    facet_wrap(~gap_type, scales = "free_y") +
    labs(title = "Educational Gaps vs Internet Penetration by Language Group",
         subtitle = "Language-based differences in the relationship between internet access and educational inequalities",
         x = "Internet Penetration (% of population)",
         y = "Educational Gap (years)",
         color = "Language Group") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # 2. Language correlation comparison
  if(length(language_correlations) > 0) {
    correlation_summary <- map_dfr(names(language_correlations), function(lang) {
      data.frame(
        language_group = lang,
        n_obs = language_correlations[[lang]]$n_obs,
        gender_corr = language_correlations[[lang]]$internet_gender,
        rural_urban_corr = language_correlations[[lang]]$internet_rural_urban,
        socioeconomic_corr = language_correlations[[lang]]$internet_socioeconomic
      )
    }) %>%
      pivot_longer(cols = c(gender_corr, rural_urban_corr, socioeconomic_corr),
                   names_to = "gap_type", values_to = "correlation") %>%
      mutate(gap_type = case_when(
        gap_type == "gender_corr" ~ "Gender Gap",
        gap_type == "rural_urban_corr" ~ "Rural-Urban Gap",
        gap_type == "socioeconomic_corr" ~ "Socioeconomic Gap"
      ))

    p2 <- ggplot(correlation_summary, aes(x = language_group, y = correlation, fill = gap_type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(title = "Correlation Coefficients: Internet Penetration vs Educational Gaps by Language",
           subtitle = "Negative values indicate gaps decrease as internet penetration increases",
           x = "Language Group",
           y = "Correlation Coefficient",
           fill = "Gap Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")

    # 3. Sample sizes by language
    sample_size_data <- map_dfr(names(language_correlations), function(lang) {
      data.frame(
        language_group = lang,
        n_obs = language_correlations[[lang]]$n_obs
      )
    })

    p3 <- ggplot(sample_size_data, aes(x = reorder(language_group, n_obs), y = n_obs)) +
      geom_col(fill = "darkgreen", alpha = 0.8) +
      geom_text(aes(label = n_obs), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(title = "Sample Sizes by Language Group",
           subtitle = "Number of complete observations for correlation analysis",
           x = "Language Group",
           y = "Number of Observations") +
      theme_minimal()

    return(list(scatter = p1, correlations = p2, sample_sizes = p3))
  } else {
    return(list(scatter = p1))
  }
}

# Function to create combined comparison plots
create_combined_comparison <- function(enhanced_data) {

  # Average gaps by region and language over time
  regional_trends <- enhanced_data %>%
    filter(!is.na(region)) %>%
    group_by(region, year) %>%
    summarise(
      avg_internet = mean(internet_penetration, na.rm = TRUE),
      avg_gender_gap = mean(gender_gap, na.rm = TRUE),
      avg_rural_urban_gap = mean(rural_urban_gap, na.rm = TRUE),
      avg_socioeconomic_gap = mean(socioeconomic_gap, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(avg_gender_gap, avg_rural_urban_gap, avg_socioeconomic_gap),
                 names_to = "gap_type", values_to = "avg_gap") %>%
    filter(!is.na(avg_gap)) %>%
    mutate(gap_type = case_when(
      gap_type == "avg_gender_gap" ~ "Gender Gap",
      gap_type == "avg_rural_urban_gap" ~ "Rural-Urban Gap",
      gap_type == "avg_socioeconomic_gap" ~ "Socioeconomic Gap"
    ))

  p1 <- ggplot(regional_trends, aes(x = year, y = avg_gap, color = region)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~gap_type, scales = "free_y") +
    labs(title = "Evolution of Educational Gaps by Region",
         subtitle = "Regional trends in educational inequalities over time",
         x = "Year",
         y = "Average Educational Gap (years)",
         color = "Region") +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Language trends
  language_trends <- enhanced_data %>%
    filter(!is.na(language_group)) %>%
    group_by(language_group, year) %>%
    summarise(
      avg_internet = mean(internet_penetration, na.rm = TRUE),
      avg_gender_gap = mean(gender_gap, na.rm = TRUE),
      avg_rural_urban_gap = mean(rural_urban_gap, na.rm = TRUE),
      avg_socioeconomic_gap = mean(socioeconomic_gap, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(avg_gender_gap, avg_rural_urban_gap, avg_socioeconomic_gap),
                 names_to = "gap_type", values_to = "avg_gap") %>%
    filter(!is.na(avg_gap)) %>%
    mutate(gap_type = case_when(
      gap_type == "avg_gender_gap" ~ "Gender Gap",
      gap_type == "avg_rural_urban_gap" ~ "Rural-Urban Gap",
      gap_type == "avg_socioeconomic_gap" ~ "Socioeconomic Gap"
    ))

  p2 <- ggplot(language_trends, aes(x = year, y = avg_gap, color = language_group)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~gap_type, scales = "free_y") +
    labs(title = "Evolution of Educational Gaps by Language Group",
         subtitle = "Language-based trends in educational inequalities over time",
         x = "Year",
         y = "Average Educational Gap (years)",
         color = "Language Group") +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(list(regional_trends = p1, language_trends = p2))
}

# ====================================================================
# MAIN EXECUTION FUNCTION FOR REGIONAL/LANGUAGE ANALYSIS
# ====================================================================

run_regional_language_analysis <- function(comprehensive_results, lang_region_expanded) {
  cat("=== STARTING REGIONAL AND LANGUAGE ANALYSIS ===\n\n")

  if(is.null(comprehensive_results) || is.null(comprehensive_results$data)) {
    cat("⚠ No comprehensive results found. Please run main analysis first.\n")
    return(NULL)
  }

  # Add regional and language classifications
  enhanced_data <- add_regional_language_data(comprehensive_results$data, lang_region_expanded)

  # Analyze correlations by region
  regional_correlations <- analyze_correlations_by_region(enhanced_data)

  # Analyze correlations by language
  language_correlations <- analyze_correlations_by_language(enhanced_data)

  # Create visualization plots
  regional_plots <- create_regional_plots(enhanced_data, regional_correlations)
  language_plots <- create_language_plots(enhanced_data, language_correlations)
  combined_plots <- create_combined_comparison(enhanced_data)

  # Display plots
  print(regional_plots$scatter)
  if(!is.null(regional_plots$correlations)) print(regional_plots$correlations)
  if(!is.null(regional_plots$sample_sizes)) print(regional_plots$sample_sizes)

  print(language_plots$scatter)
  if(!is.null(language_plots$correlations)) print(language_plots$correlations)
  if(!is.null(language_plots$sample_sizes)) print(language_plots$sample_sizes)

  print(combined_plots$regional_trends)
  print(combined_plots$language_trends)

  # Save plots
  ggsave("plot_results/12_regional_gaps_vs_internet.png", regional_plots$scatter,
         width = 14, height = 10, dpi = 300)
  ggsave("plot_results/13_language_gaps_vs_internet.png", language_plots$scatter,
         width = 14, height = 10, dpi = 300)
  ggsave("plot_results/14_regional_trends.png", combined_plots$regional_trends,
         width = 14, height = 10, dpi = 300)
  ggsave("plot_results/15_language_trends.png", combined_plots$language_trends,
         width = 14, height = 10, dpi = 300)

  if(!is.null(regional_plots$correlations)) {
    ggsave("plot_results/16_regional_correlations.png", regional_plots$correlations,
           width = 12, height = 8, dpi = 300)
  }
  if(!is.null(language_plots$correlations)) {
    ggsave("plot_results/17_language_correlations.png", language_plots$correlations,
           width = 12, height = 8, dpi = 300)
  }

  # Print summary results
  cat("\n=== REGIONAL ANALYSIS SUMMARY ===\n")
  for(region in names(regional_correlations)) {
    cat("Region:", region, "\n")
    cat("  Sample size:", regional_correlations[[region]]$n_obs, "\n")
    cat("  Internet vs Gender Gap:", round(regional_correlations[[region]]$internet_gender, 3), "\n")
    cat("  Internet vs Rural-Urban Gap:", round(regional_correlations[[region]]$internet_rural_urban, 3), "\n")
    cat("  Internet vs Socioeconomic Gap:", round(regional_correlations[[region]]$internet_socioeconomic, 3), "\n\n")
  }

  cat("\n=== LANGUAGE ANALYSIS SUMMARY ===\n")
  for(language in names(language_correlations)) {
    cat("Language Group:", language, "\n")
    cat("  Sample size:", language_correlations[[language]]$n_obs, "\n")
    cat("  Internet vs Gender Gap:", round(language_correlations[[language]]$internet_gender, 3), "\n")
    cat("  Internet vs Rural-Urban Gap:", round(language_correlations[[language]]$internet_rural_urban, 3), "\n")
    cat("  Internet vs Socioeconomic Gap:", round(language_correlations[[language]]$internet_socioeconomic, 3), "\n\n")
  }

  # Return results
  results <- list(
    enhanced_data = enhanced_data,
    regional_correlations = regional_correlations,
    language_correlations = language_correlations,
    regional_plots = regional_plots,
    language_plots = language_plots,
    combined_plots = combined_plots
  )

  cat("\n=== REGIONAL AND LANGUAGE ANALYSIS COMPLETED ===\n")
  return(results)
}

# ====================================================================
# EXECUTE REGIONAL AND LANGUAGE ANALYSIS
# ====================================================================

# Run the regional and language analysis
# Make sure you have run the comprehensive analysis first and have lang_region_expanded available
regional_language_results <- run_regional_language_analysis(comprehensive_results, lang_region_expanded)

cat("\n=== ANALYSIS EXTENSION COMPLETED ===\n")
cat("Results available in 'regional_language_results' object\n")
cat("New plots saved to 'plot_results' folder (files 12-17)\n")

