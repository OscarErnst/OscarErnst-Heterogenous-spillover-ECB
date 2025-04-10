# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on system user
user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "Oscar_dream") {
  setwd("HER_INDSÆT_STI_FOR_OSCAR_DREAM")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}
# Use working directory-relative paths
source(file.path("R kode", "Functions","Functions.R"))
source(file.path("R kode", "Functions", "Load_Packages.R"))
library(dplyr)
library(zoo)
library(eurostat)


# Indstillinger:
# List of countries: EA20 plus additional countries
countries <- c("EA20", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE", 
               "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES", "DK")
start_year <- 2000
end_year <- 2020


# Load required packages with error handling
required_packages <- c("dplyr", "AER", "dynlm", "car", "zoo", "readxl", "stargazer", "lubridate", "purrr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}



# 1) Create the full dataset by merging country-specific data
raw_data <- map_dfr(countries, ~ get_country_dataset(.x))
#   This yields columns: HICP, rGDP, Consumption, plus year, quarter, etc.

# 2) Add log-level columns (100 * log(...)) for each
raw_data <- raw_data %>%
  mutate(
    HICP_log         = 100 * log(HICP),
    rGDP_log         = 100 * log(rGDP),
    Consumption_log  = 100 * log(Consumption)
  )

# 3) Calculate YoY log changes for HICP, rGDP, Consumption
d <- calc_log_yoy_change(
  df   = raw_data,
  vars = c("HICP", "rGDP", "Consumption")
) %>%
  # Filter the sample to be from 2005 until 2019 (pre-pandemic)
  filter(year >= start_year & year < end_year)

# 4) Rename those yoy log-change columns
d <- d %>% rename(
  d_HICP        = HICP_yoy_log,
  d_rGDP        = rGDP_yoy_log,
  d_Consumption = Consumption_yoy_log
)

# 5) Final dataset: keep yoy changes + newly created log-level columns
#    plus some original columns
d1 <- d %>%
  # Add a new Date column corresponding to the first day of the quarter at midnight.
  dplyr::mutate(Date = as.POSIXct(sprintf("%04d-%02d-01 00:00:00", year, (quarter - 1) * 3 + 1),
                           format = "%Y-%m-%d %H:%M:%S")) %>% 
  dplyr::select(
    Date, country,
               # yoy changes
               d_HICP, d_rGDP, d_Consumption,
               # log-level columns
               HICP_log, rGDP_log, Consumption_log
             )


# 6) Save the constructed control variables dataset
saveRDS(d1, file = file.path("Data", "Control Variables", "Eurozone_country_variables.rds"))

cat("Data construction complete.\n")

