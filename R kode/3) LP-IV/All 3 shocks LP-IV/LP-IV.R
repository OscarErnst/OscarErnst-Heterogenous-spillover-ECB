rm(list = ls())
cat("\014")
library(lpirfs)

# Set working directory (adjust as needed)
user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "Kasper") {
  setwd("HER_INDSÆT_STI_FOR_KASPER")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# Load data
data <- readRDS("Data/LP-IV/input_data_all_3.rds")
target_q <- readRDS("Data/LP-IV/tamas_instrument.rds")

# Define endogenous variables
outcome_vars <- c("d_rGDP", "d_HICP", "d_Consumption", "bund_yield")

# Create shock and response matrices
Y <- data[, outcome_vars]
shock <- data.frame(shock = target_q)

# Remove NAs
df <- cbind(Y, shock)
df <- na.omit(df)

# Extract after NA handling
Y <- df[, outcome_vars]
shock <- df[, "shock", drop = FALSE]

lpiv_res <- lp_lin_iv(
  endog_data = Y,
  shock = shock,
  use_twosls = FALSE,         # Only 2nd stage, reduced-form
  lags_endog_lin = 4,         # You can change this to what you want
  trend = 0,                  # No trend
  confint = 1.96,             # 95% confidence band (normal approx)
  use_nw = TRUE,              # Newey-West SEs
  hor = 12                    # 12-quarter horizon
)



impact <- lpiv_res[["irf_lin_low"]][4,1]  # First horizon = h = 0

# Get the impact response of bund_yield at horizon 0
impact <- lpiv_res[["irf_lin_low"]][4,1]  # row 4, column 1
scaling <- 1 / impact

# Apply normalization
lpiv_res$irf_lin_mean <- lpiv_res$irf_lin_mean * scaling
lpiv_res$irf_lin_low <- lpiv_res$irf_lin_low * scaling
lpiv_res$irf_lin_up <- lpiv_res$irf_lin_up * scaling



# Plot all variables
plot(lpiv_res)





