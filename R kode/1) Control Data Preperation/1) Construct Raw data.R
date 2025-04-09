rm(list = ls())
cat("\014")

user <- Sys.info()[["user"]]

# Set working directory based on user
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
} else if (user == "Oscar_dream") {
  setwd("HER_INDSÆT_STI_FOR_OSCAR_DREAM")
} else if (user == "Kasper") {
  setwd("HER_INDSÆT_STI_FOR_KASPER")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# Use working directory-relative paths
source(file.path("R kode", "Functions", "Functions.R"))
source(file.path("R kode", "Functions", "Load_Packages.R"))

library(dplyr)
library(zoo)
library(eurostat)

# Load required packages with error handling
required_packages <- c("dplyr", "AER", "dynlm", "car", "zoo", "readxl", "stargazer", "lubridate", "purrr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# List of countries: EA20 plus additional countries (allows flexibility)
countries <- c("EA20", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE", 
               "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES", "DK")

# Create the full dataset by merging country-specific data
data <- map_dfr(countries, ~ get_country_dataset(.x))

# Calculate YoY log changes for key variables: HICP, rGDP, Consumption
d <- calc_log_yoy_change(
  df = data,
  vars = c("HICP", "rGDP", "Consumption")
) %>%
  # Filter the sample to be from 2005 until 2019 (pre-pandemic)
  filter(year > 2004 & year < 2020)

# Rename variables for clarity
d <- d %>% rename(
  d_HICP = HICP_yoy_log,
  d_rGDP = rGDP_yoy_log,
  d_Consumption = Consumption_yoy_log
)

# Select only relevant variables for control
d1 <- d %>% dplyr::select(country, year, quarter, d_HICP, d_rGDP, d_Consumption)

# Save the constructed control variables dataset
saveRDS(d1, file = file.path("Data", "Control Variables", "Eurozone_country_variables.rds"))

cat("Data construction complete.\n")

