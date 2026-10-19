# ============================================================
# PHASE 1: DATA CLEANING, PREPROCESSING AND PREPARATION
# Project: Renewable Energy vs. CO2 Emissions Across the World
# Input folder:  D:/ADVT_PROJECT/
# Output folder: D:/ADVT_PROJECT/
# ============================================================

# 1. Set working directory
setwd("D:/ADVT_PROJECT")

# 2. Load required package
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# 3. Import datasets
energy <- read.csv(
  "owid_energy.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "..")
)

co2 <- read.csv(
  "owid_co2.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "..")
)

# 4. Verify dataset structure
cat("ENERGY DATASET\n")
print(dim(energy))
print(names(energy))
str(energy)

cat("\nCO2 DATASET\n")
print(dim(co2))
print(names(co2))
str(co2)

# 5. Check required columns
energy_required <- c(
  "country",
  "year",
  "iso_code",
  "renewables_energy_share_pct"
)

co2_required <- c(
  "country",
  "year",
  "iso_code",
  "co2_per_capita"
)

if (!all(energy_required %in% names(energy))) {
  stop("Required columns are missing from owid_energy.csv")
}

if (!all(co2_required %in% names(co2))) {
  stop("Required columns are missing from owid_co2.csv")
}

# 6. Select relevant columns and clean energy data
energy_clean <- energy %>%
  select(all_of(energy_required)) %>%
  mutate(
    country = trimws(as.character(country)),
    iso_code = toupper(trimws(as.character(iso_code))),
    year = suppressWarnings(
      as.integer(as.character(year))
    ),
    renewables_energy_share_pct = suppressWarnings(
      as.numeric(as.character(renewables_energy_share_pct))
    ),
    country = na_if(country, ""),
    iso_code = na_if(iso_code, "")
  ) %>%
  filter(
    !is.na(country),
    !is.na(year),
    year > 0
  ) %>%
  distinct()

# 7. Select relevant columns and clean CO2 data
co2_clean <- co2 %>%
  select(all_of(co2_required)) %>%
  mutate(
    country = trimws(as.character(country)),
    iso_code = toupper(trimws(as.character(iso_code))),
    year = suppressWarnings(
      as.integer(as.character(year))
    ),
    co2_per_capita = suppressWarnings(
      as.numeric(as.character(co2_per_capita))
    ),
    country = na_if(country, ""),
    iso_code = na_if(iso_code, "")
  ) %>%
  filter(
    !is.na(country),
    !is.na(year),
    year > 0
  ) %>%
  distinct()

# 8. Check indicator ranges before removing invalid values
cat("\nRenewable energy share summary:\n")
print(summary(energy_clean$renewables_energy_share_pct))

cat("\nCO2 per capita summary:\n")
print(summary(co2_clean$co2_per_capita))

cat("\nRenewable share outside 0-100:\n")
print(
  energy_clean %>%
    filter(
      !is.na(renewables_energy_share_pct),
      renewables_energy_share_pct < 0 |
        renewables_energy_share_pct > 100
    )
)

cat("\nNegative CO2 per capita values:\n")
print(
  co2_clean %>%
    filter(
      !is.na(co2_per_capita),
      co2_per_capita < 0
    )
)

# 9. Remove invalid indicator values
# Assumes renewable share is expressed as a percentage
energy_clean <- energy_clean %>%
  filter(
    is.na(renewables_energy_share_pct) |
      (
        is.finite(renewables_energy_share_pct) &
          renewables_energy_share_pct >= 0 &
          renewables_energy_share_pct <= 100
      )
  )

co2_clean <- co2_clean %>%
  filter(
    is.na(co2_per_capita) |
      (
        is.finite(co2_per_capita) &
          co2_per_capita >= 0
      )
  )

# 10. Check and resolve duplicate join keys
# Exact duplicate rows have already been removed.
# Stop rather than arbitrarily deleting conflicting observations.

energy_duplicates <- energy_clean %>%
  count(country, year, iso_code) %>%
  filter(n > 1)

co2_duplicates <- co2_clean %>%
  count(country, year, iso_code) %>%
  filter(n > 1)

if (nrow(energy_duplicates) > 0) {
  print(energy_duplicates)
  stop(
    "Conflicting energy records found. Resolve duplicates before continuing."
  )
}

if (nrow(co2_duplicates) > 0) {
  print(co2_duplicates)
  stop(
    "Conflicting CO2 records found. Resolve duplicates before continuing."
  )
}

# 11. Merge using country, year and iso_code
cleaned_data <- inner_join(
  energy_clean,
  co2_clean,
  by = c("country", "year", "iso_code"),
  relationship = "one-to-one"
)

if (nrow(cleaned_data) == 0) {
  stop(
    "No matching records found. Check country names, ISO codes and years."
  )
}

# 12. Prepare complete observations for comparative analysis
# Keep missing indicator values in the main cleaned dataset.
# Create a separate complete-case dataset for correlation/plotting.

complete_data <- cleaned_data %>%
  filter(
    !is.na(renewables_energy_share_pct),
    !is.na(co2_per_capita)
  )

# 13. Report missing values and dataset dimensions
cat("\nCLEANED ENERGY DATASET\n")
print(dim(energy_clean))
print(colSums(is.na(energy_clean)))

cat("\nCLEANED CO2 DATASET\n")
print(dim(co2_clean))
print(colSums(is.na(co2_clean)))

cat("\nMERGED CLEANED DATASET\n")
print(dim(cleaned_data))
print(colSums(is.na(cleaned_data)))

cat("\nCOMPLETE-CASE DATASET\n")
print(dim(complete_data))

cat("\nYears available after merging:\n")
print(sort(unique(cleaned_data$year)))

# 14. Save cleaned datasets in the specified folder
write.csv(
  energy_clean,
  "D:/ADVT_PROJECT/owid_energy_cleaned.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  co2_clean,
  "D:/ADVT_PROJECT/owid_co2_cleaned.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  cleaned_data,
  "D:/ADVT_PROJECT/renewable_energy_co2_cleaned.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  complete_data,
  "D:/ADVT_PROJECT/renewable_energy_co2_complete.csv",
  row.names = FALSE,
  na = ""
)

# 15. Verify saved files
cat("\nSaved cleaned datasets:\n")
print(
  list.files(
    "D:/ADVT_PROJECT",
    pattern = "_cleaned\\.csv$|_complete\\.csv$",
    full.names = TRUE
  )
)

cat("\nDATA CLEANING AND PREPROCESSING COMPLETED SUCCESSFULLY\n")