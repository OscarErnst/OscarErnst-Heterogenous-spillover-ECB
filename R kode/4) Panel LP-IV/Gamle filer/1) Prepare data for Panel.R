# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on system user
user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# Load required libraries
library(dplyr)
library(lubridate)

# Load datasets
data <- readRDS("Data/LP-IV/PureMP, Path & QE/input_data_post_qe.rds")
shock <- readRDS("Data/LP-IV/PureMP, Path & QE/shocks_post_qe.rds")

# Adjust shock date to match the last day of the previous quarter
shock <- shock %>%
  mutate(Date = as.Date(Date) - days(1))

# Ensure data dates are also explicitly in date format
data <- data %>%
  mutate(Date = as.Date(Date))

# Merge datasets accurately
panel_data <- data %>%
  left_join(shock, by = "Date")

# Check if merge successful
head(panel_data)

# Save merged dataset
output_path <- "Data/Panel LP-IV/panel_input_data_post_qe.rds"
saveRDS(panel_data, file = output_path)

cat("Merged and correctly dated panel data saved successfully to:", output_path, "\n")

