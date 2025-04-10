# Clear environment and console
rm(list = ls())
cat("\014")

# Set your working directory (adjust as needed)
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")

# source("R kode/Load_Packages.R") # If you have a script for packages
source("R kode/LP-IV/LP IV function.R")

library(dplyr)

#------------------------------------------------------------------------------
# 1. Load full data and instrument
full_data <- readRDS("Output data/Eurozone_country_variables.rds")
instrument_full <- readRDS("Output data/instrument.rds")

# 2. We only want d_rGDP as outcome
outcome_var <- "d_rGDP"

# 3. Define the set of countries
countries <- unique(full_data$country)
print(countries)
# [1] "AT"   "BE"   "CY"   "DE"   "DK"   "EA20" "EE"   "EL"   "ES"   "FI"   "FR"   "IE"   "IT"
# [14] "LT"   "LU"   "LV"   "MT"   "NL"   "PT"   "SI"   "SK"

# 4. Create a lookup for full country names
#    If a code is missing, it will fallback to using the code as the title.
country_names <- c(
  "AT"   = "Austria",
  "BE"   = "Belgium",
  "CY"   = "Cyprus",
  "DE"   = "Germany",
  "DK"   = "Denmark",
  "EA20" = "Euro Area (20)",
  "EE"   = "Estonia",
  "EL"   = "Greece",
  "ES"   = "Spain",
  "FI"   = "Finland",
  "FR"   = "France",
  "IE"   = "Ireland",
  "IT"   = "Italy",
  "LT"   = "Lithuania",
  "LU"   = "Luxembourg",
  "LV"   = "Latvia",
  "MT"   = "Malta",
  "NL"   = "Netherlands",
  "PT"   = "Portugal",
  "SI"   = "Slovenia",
  "SK"   = "Slovakia"
)

# Helper function to get the long name from the code
get_country_label <- function(code) {
  if (code %in% names(country_names)) {
    return(country_names[code])
  } else {
    return(code)  # Fallback if a code isn't in the lookup
  }
}

# 5. Lags and horizons
p <- 6
H <- 8
horizons <- 0:H

# 6. Prepare PNG (adjust size/res as needed)
plot_path <- "/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Graphs/IRP_per_country.png"
png(plot_path, width = 3200, height = 2400, res = 300)

# 3 rows × 7 columns = 21 subplots
par(mfrow = c(7, 3),
    mar = c(4, 4, 2, 1),
    cex.axis = 0.8,
    cex.lab  = 0.8,
    cex.main = 1.0)

# Define colors
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

# 7. Loop over countries
for (ctry in countries) {
  
  # Subset data for this country
  data_ctry <- full_data %>%
    filter(country == ctry) %>%
    dplyr::select(all_of(outcome_var))
  
  # Also subset the instrument (assuming alignment with rows)
  i_ctry <- instrument_full[1:nrow(data_ctry)]
  
  # Estimate LP-IV
  result <- estimateLPIV(
    data         = data_ctry,
    p            = p,
    H            = H,
    instrument   = i_ctry,
    outcome_vars = outcome_var,
    c_case       = 1,       # includes constant
    conf_level   = 0.95
  )
  
  # Extract IRF, Lower, Upper for the single outcome
  IRF   <- result$IRF[1, ]
  Lower <- result$Lower[1, ]
  Upper <- result$Upper[1, ]
  
  # Determine y-limits
  y_min <- min(Lower, na.rm = TRUE)
  y_max <- max(Upper, na.rm = TRUE)
  
  # Plot label (lookup full name)
  plot_label <- get_country_label(ctry)
  
  # Empty plot
  plot(horizons, IRF,
       type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon",
       ylab = "Impulse Response",
       main = plot_label)
  abline(h = 0, lty = 2, col = "black")
  
  # Confidence band polygon
  x_poly <- c(horizons, rev(horizons))
  y_poly <- c(Lower, rev(Upper))
  polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
  
  # IRF line
  lines(horizons, IRF, type = "b", col = burgundy, lwd = 2)
}

# Close PNG device
dev.off()
