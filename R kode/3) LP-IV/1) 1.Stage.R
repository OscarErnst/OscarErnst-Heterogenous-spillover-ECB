# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on user
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

# Load required packages with error handling
required_packages <- c("dplyr", "AER", "dynlm", "car", "zoo", "readxl", "stargazer", "lubridate")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define color palette for plotting
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

# -------------------------------------------------------------------------
# 1. Set Date Range for Monthly Data
# -------------------------------------------------------------------------
start_month <- c(2005, 1)
end_month   <- c(2019, 12)

start_date <- as.Date(sprintf("%d-%02d-01", start_month[1], start_month[2]))
end_date   <- as.Date(sprintf("%d-%02d-01", end_month[1], end_month[2]))

# -------------------------------------------------------------------------
# 2. Load and Validate Data
# -------------------------------------------------------------------------
load_data <- function(file_path, error_msg) {
  if (!file.exists(file_path)) stop(error_msg)
  readRDS(file_path)
}

start_date_expected <- as.Date("2005-01-01")
end_date_expected <- as.Date("2019-12-01")
target_m <- load_data(file.path("Data", "Shocks", "Target Factor Shock.rds"),
                      "Target Factor Shock file not found!")
target_m <- ts(target_m, start = start_month, end = end_month, frequency = 12)

# Burde gå fra 2005 - 2020 og have [1:180] data punkter

control <- load_data(file.path("Data", "Interpolated data", "control_var_m.rds"),
                     "Control variables file not found!")

create_ts <- function(data, var_name, start_date, end_date) {
  ts(data[[var_name]], start = c(year(start_date), month(start_date)), frequency = 12)
}

d_rGDP_m <- create_ts(control, "d_rGDP_m", start_date, end_date)
d_HICP_m <- create_ts(control, "d_HICP_m", start_date, end_date)

target_m <- window(target_m, start = start_month, end = end_month)
d_rGDP_m <- window(d_rGDP_m, start = start_month, end = end_month)
d_HICP_m <- window(d_HICP_m, start = start_month, end = end_month)
# Burde gå fra 2005 - 2020 og have [1:180] data punkter

# Henter nu bundesbank data
Bundes_yield <- read_excel(file.path("Data", "Generic Bundesbank yield.xlsx"))
Bund_3M_m <- ts(Bundes_yield[[2]], start = start_month, frequency = 12)
d_Bund_3M_m <- diff(Bund_3M_m) * 100
d_Bund_3M_m <- window(ts(c(NA, d_Bund_3M_m), start = start_month, frequency = 12), start = start_month, end = end_month)
Bund_3M_m <- window(Bund_3M_m, start = start_month, end = end_month)
# Burde gå fra 2005 - 2020 og have [1:180] data punkter

# -------------------------------------------------------------------------
# 3. Combine Data and Create Time Series Object
# -------------------------------------------------------------------------
shocks_data <- cbind(
  target_m       = as.numeric(target_m),
  d_Bund_3M_m    = as.numeric(d_Bund_3M_m),
  Bund_3M_m      = as.numeric(Bund_3M_m),
  rGDPm_logchg   = as.numeric(d_rGDP_m),
  HICPm_logchg   = as.numeric(d_HICP_m),
  GFC_dummy = ifelse(as.yearmon(time(target_m)) >= as.yearmon("2007-01") &
                       as.yearmon(time(target_m)) <= as.yearmon("2007-09"), 1, 0)
)

shocks_ts <- ts(shocks_data, start = start_month, frequency = 12)

# -------------------------------------------------------------------------
# 4. First-Stage Regression
# -------------------------------------------------------------------------
FirstStage <- dynlm(
  Bund_3M_m ~ target_m + L(target_m, 1:12) +
    L(Bund_3M_m, 1:12) + L(rGDPm_logchg, 1:12) + L(HICPm_logchg, 1:12),
  data = shocks_ts
)

cat("\nFirst Stage Regression Results:\n")
print(summary(FirstStage))

# Joint significance test
joint_test <- linearHypothesis(FirstStage,
                               paste(names(coef(FirstStage))[grepl("target_m", names(coef(FirstStage)))], "= 0"))
cat("\nJoint F-test Results:\n")
print(joint_test)

# Newey-West standard errors
#cat("\nNewey-West Standard Errors:\n")
#print(coeftest(FirstStage, vcov = NeweyWest(FirstStage)))

# -------------------------------------------------------------------------
# 5. Create and Save Quarterly Instrument
# -------------------------------------------------------------------------
Bund_3M_m_hat <- fitted.values(FirstStage)

create_quarterly_ts <- function(monthly_data) {
  monthly_zoo <- zoo(coredata(monthly_data), as.yearmon(time(monthly_data)))
  quarterly_zoo <- aggregate(monthly_zoo, as.yearqtr, mean)
  as.ts(quarterly_zoo)
}
# OBS: Vi mister de første 2 kvartaler, da vi bruger 5 lags i 1.stage.
shock_var_q_ts <- create_quarterly_ts(Bund_3M_m_hat)

# Create directory if needed and save file
lpiv_dir <- file.path("Data", "LP-IV")
if (!dir.exists(lpiv_dir)) dir.create(lpiv_dir, recursive = TRUE)

saveRDS(shock_var_q_ts, file = file.path(lpiv_dir, "Target_instrument.rds"))
cat("\nQuarterly instrument saved to", file.path(lpiv_dir, "Target_instrument.rds"), "\n")

# -------------------------------------------------------------------------
# 6. Create Plots
# -------------------------------------------------------------------------
create_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

save_plot <- function(filename, plot_func, width = 2000, height = 1200, res = 150) {
  create_dir(dirname(filename))
  png(filename, width = width, height = height, res = res)
  par(mar = c(5, 5, 4, 2), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
  plot_func()
  dev.off()
}

# Plot 1: Actual vs Fitted Bund 3M
plot_actual_fitted <- function() {
  plot(shocks_ts[, "Bund_3M_m"], type = "l", col = burgundy_trans,
       main = "Actual vs. Fitted Bund 3M",
       xlab = "Time", ylab = "Bund 3M Yield (basis points)")
  lines(Bund_3M_m_hat, col = burgundy, lwd = 2)
  legend("topright", legend = c("Actual", "Fitted"),
         col = c(burgundy_trans, burgundy), lty = 1, lwd = 2, cex = 1.2)
}

# Plot 2: Target Shock and Fitted Instrument
plot_shock_fitted <- function() {
  par(mfrow = c(1, 2))
  plot(shocks_ts[, "target_m"], type = "l",
       main = "(a) Target Factor",
       xlab = "Time", ylab = "Basis Points",
       col = burgundy, lwd = 2)
  plot(Bund_3M_m_hat, type = "l",
       main = "(b) Fitted Bund 3M Instrument",
       xlab = "Time", ylab = "Fitted Value",
       col = burgundy, lwd = 2)
}

# Plot 3: Quarterly Instrument
plot_quarterly <- function() {
  plot(shock_var_q_ts,
       main = "Quarterly Fitted Bund 3M Instrument",
       xlab = "Time", ylab = "Instrument Value (Fitted Bund 3M)",
       col = burgundy, lwd = 2)
}

# Save plots
plot_dir <- file.path("Graphs", "Identify MP shock")
save_plot(file.path(plot_dir, "Actual_vs_Fitted_Bund_6M.png"), plot_actual_fitted)
save_plot(file.path(plot_dir, "Shock_and_Fitted_Panel.png"), plot_shock_fitted, height = 1600)
save_plot(file.path(plot_dir, "Quarterly_Instrument.png"), plot_quarterly)
