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

# --- Load required packages --------------------------------------------
library(dplyr)
library(lubridate)
library(readxl)

size_of_bund <- "2Y"

#########################################################################
# 1. Load Endogenous Variables (Controls and Outcomes)
#########################################################################
control <- readRDS(file.path("Data", "Control Variables", "Eurozone_country_variables.rds"))%>%
  filter(year(Date) > 2005) %>% 
  dplyr::select(Date, country, d_HICP, d_rGDP, d_Consumption, HICP_log, rGDP_log, Consumption_log)

#########################################################################
# 2. Load and Aggregate Bund Yield Data to Quarterly Averages
#########################################################################
bund_yield <- read_excel(file.path("Data", "Generic Bundesbank yield.xlsx")) %>%
  dplyr::select(Date, size_of_bund) %>% 
  mutate(Date = as.Date(Date),
         Date = floor_date(Date, unit = "quarter")) %>%
  group_by(Date) %>%
  summarise(bund_yield = mean(.data[[size_of_bund]], na.rm = TRUE), .groups = "drop") %>%
  filter(Date >= as.Date("2006-01-01") & Date <= as.Date("2019-12-31"))

#########################################################################
# 3. Merge Bund Yield with Data
#########################################################################
data <- left_join(control, bund_yield, by = "Date")
rm(bund_yield, control)

#########################################################################
# 5. Load the Instrument (Pure Target Shock)
#########################################################################
shock <- readRDS(file.path("Data","LP-IV","PureMP & Path","1.stage_instrument.rds"))

# Restrict shock to the same window as the other data
shock <- window(shock, start = c(2006, 1), end = c(2019, 4))
dates <- seq(as.Date("2006-01-01"), by = "quarter", length.out = length(shock))

# Convert ts object values to a numeric vector
shock <- as.numeric(shock)

# Create a data frame that includes the date column (as POSIXct) and bund_yield.
shock <- data.frame(
  Date = as.POSIXct(dates, tz = "UTC"),  # Set timezone as needed.
  shock = shock
)

#########################################################################
# 6. Merge the Instrument into the Data (as an additional control variable)
#########################################################################
#data <- cbind(data, shock = shock)

# Ensure output directory exists
output_dir <- file.path("Data", "LP-IV", "PureMP, Path & QE")

# Save final dataset
saveRDS(data, file = file.path(output_dir, "input_data.rds"))
saveRDS(shock, file = file.path(output_dir, "shock.rds"))


