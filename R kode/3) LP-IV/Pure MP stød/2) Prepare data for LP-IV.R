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

# --- Load required packages --------------------------------------------
library(dplyr)
library(lubridate)
library(readxl)

size_of_bund <- "1Y"

#########################################################################
# 1. Load Endogenous Variables (Controls and Outcomes)
#########################################################################
control <- readRDS(file.path("Data", "Control Variables", "Eurozone_country_variables.rds")) %>%
  filter(country == "EA20") %>%
  dplyr::select(-country) %>%
  #filter(!(year == 2005 & quarter %in% c(1, 2)))
  filter(year > 2005) %>% 
  dplyr::select(d_HICP, d_rGDP, d_Consumption, HICP_log, rGDP_log, Consumption_log)

#########################################################################
# 2. Load and Aggregate Bund Yield Data to Quarterly Averages
#########################################################################
bund_yield <- read_excel(file.path("Data", "Generic Bundesbank yield.xlsx")) %>%
  dplyr::select(Date, size_of_bund) %>% 
  mutate(Date = as.Date(Date),
         Quarter = floor_date(Date, unit = "quarter")) %>%
  group_by(Quarter) %>%
  summarise(bund_yield = mean(.data[[size_of_bund]], na.rm = TRUE), .groups = "drop") %>%
  filter(Quarter >= as.Date("2006-01-01") & Quarter <= as.Date("2019-12-31"))

#########################################################################
# 3. Merge Bund Yield with Data
#########################################################################
data <- cbind(control, bund_yield = bund_yield$bund_yield[1:nrow(control)])
rm(bund_yield, control)

#########################################################################
# 5. Load the Instrument (Pure Target Shock)
#########################################################################
target_q <- readRDS(file.path("Data","LP-IV","Kun PureMP","Bund_instrument.rds"))

# Restrict target_q to the same window as the other data
target_q <- window(target_q, start = c(2006, 1), end = c(2019, 4))

# Ensure it matches the number of rows in 'data'
target_q <- as.numeric(target_q[1:nrow(data)])

#########################################################################
# 6. Merge the Instrument into the Data (as an additional control variable)
#########################################################################
data <- cbind(data, target_q = target_q)

# Ensure output directory exists
output_dir <- file.path("Data", "LP-IV", "Kun PureMP")

# Save final dataset
saveRDS(data, file = file.path(output_dir, "input_data.rds"))

