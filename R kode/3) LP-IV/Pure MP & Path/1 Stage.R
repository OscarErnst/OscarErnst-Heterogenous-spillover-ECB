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

create_ts <- function(data, var_name, start_date, end_date) {
  ts(data[[var_name]], start = c(year(start_date), month(start_date)), frequency = 12)
}

# -------------------------------------------------------------------------
# 2. Load and Validate Data
# -------------------------------------------------------------------------
load_data <- function(file_path, error_msg) {
  if (!file.exists(file_path)) stop(error_msg)
  readRDS(file_path)
}

# Henter shocks:
shock <- load_data(
  file.path("Instrumenter","Pure MP & Path", "PureMP&Path.rds"),
  "PureMP&Path.rds not found"
) %>%
  filter(year(Date) >= start_month[1], year(Date) <= end_month[1])

pureMP_m <- ts(shock$pureMP_m, start = start_month, end = end_month, frequency = 12)
Path_m   <- ts(shock$Path_m,   start = start_month, end = end_month, frequency = 12)

# Henter Kontrol Variable:
control <- load_data(
  file.path("Data", "Interpolated data", "control_var_m.rds"),
  "control_var_m.rds not found"
) %>%
  filter(year(Date) >= start_month[1], year(Date) <= end_month[1])

d_rGDP_m <- create_ts(control, "d_rGDP_m", start_date, end_date)
d_HICP_m <- create_ts(control, "d_HICP_m", start_date, end_date)

# Henter nu bundesbank data
Bund_length <- "1Y"
Bundes_yield <- read_excel(file.path("Data", "Generic Bundesbank yield.xlsx")) %>%
  filter(year(Date) >= start_month[1], year(Date) <= end_month[1])

Bund_1Y_m <- ts(Bundes_yield[[Bund_length]], start = start_month, frequency = 12)
d_Bund_1Y_m <- diff(Bund_1Y_m) * 100
d_Bund_1Y_m <- window(ts(c(NA, d_Bund_1Y_m), start = start_month, frequency = 12),
                      start = start_month, end = end_month)
Bund_1Y_m <- window(Bund_1Y_m, start = start_month, end = end_month)
# Should go from 2005 to 2020 and have [1:180] data points

# -------------------------------------------------------------------------
# 3. Combine Data and Create Time Series Object
# -------------------------------------------------------------------------
shocks_data <- cbind(
  pureMP_m    = as.numeric(pureMP_m),
  Path_m      = as.numeric(Path_m),
  Bund_1Y_m   = as.numeric(Bund_1Y_m),
  d_rGDP_m    = as.numeric(d_rGDP_m),
  d_HICP_m    = as.numeric(d_HICP_m),
  GFC_dummy   = ifelse(as.yearmon(time(pureMP_m)) >= as.yearmon("2007-01") &
                         as.yearmon(time(pureMP_m)) <= as.yearmon("2007-09"), 1, 0)
)

shocks_ts <- ts(shocks_data, start = start_month, frequency = 12)

# -------------------------------------------------------------------------
# 4. First-Stage Regression
# -------------------------------------------------------------------------
FirstStage <- dynlm(
  Bund_1Y_m ~ pureMP_m + Path_m +
    L(pureMP_m, 1:12) + L(Path_m, 1:12) +
    L(Bund_1Y_m, 1:1) + L(d_rGDP_m, 1:12) + L(d_HICP_m, 1:12),
  data = shocks_ts
)

cat("\nFirst Stage Regression Results:\n")
print(summary(FirstStage))

# 2) Joint significance test for BOTH pureMP_m & Path_m
#    Tester om koefficienterne for de to instrumenter = 0
joint_test <- linearHypothesis(
  FirstStage,
  c("pureMP_m = 0", "Path_m = 0")
)

cat("\nJoint F-test Results (pureMP_m & Path_m = 0):\n")
print(joint_test)

# Newey-West standard errors
cat("\nNewey-West Standard Errors:\n")
print(coeftest(FirstStage, vcov = NeweyWest(FirstStage)))

# -------------------------------------------------------------------------
# 5. Create and Save Quarterly Instrument
# -------------------------------------------------------------------------
Bund_1Y_m_hat <- fitted.values(FirstStage)

create_quarterly_ts <- function(monthly_data) {
  monthly_zoo <- zoo(coredata(monthly_data), as.yearmon(time(monthly_data)))
  quarterly_zoo <- aggregate(monthly_zoo, as.yearqtr, mean)
  as.ts(quarterly_zoo)
}

# OBS: We lose the first 2 quarters, as we use 12 lags in the 1.stage.
shock_var_q_ts <- create_quarterly_ts(Bund_1Y_m_hat)

saveRDS(shock_var_q_ts, file = file.path("Data","LP-IV","PureMP & Path","1.stage_instrument.rds"))
cat("\nQuarterly instrument saved to", file.path("Data","LP-IV","PureMP & Path","1.stage_instrument.rds"), "\n")

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

# === Plot 1: Actual vs Fitted Bund 1Y
plot_actual_fitted <- function() {
  plot(shocks_ts[, "Bund_1Y_m"], type = "l", col = burgundy_trans,
       main = "Actual vs. Fitted Bund 1Y",
       xlab = "Time", ylab = "Bund 1Y Yield (basis points)")
  lines(Bund_1Y_m_hat, col = burgundy, lwd = 2)
  legend("topright", legend = c("Actual", "Fitted"),
         col = c(burgundy_trans, burgundy), lty = 1, lwd = 2, cex = 1.2)
}

# === Plot 2: Pure MP Shock and Fitted Instrument
plot_shock_fitted <- function() {
  par(mfrow = c(1, 2))
  # (a) Plot the pure MP shock over time
  plot(shocks_ts[, "pureMP_m"], type = "l",
       main = "(a) Pure MP Shock",
       xlab = "Time", ylab = "Basis Points",
       col = burgundy, lwd = 2)
  
  # (b) Plot the fitted 1Y Bund instrument
  plot(Bund_1Y_m_hat, type = "l",
       main = "(b) Fitted Bund 1Y Instrument",
       xlab = "Time", ylab = "Fitted Value",
       col = burgundy, lwd = 2)
}

# === Plot 3: Quarterly Instrument
plot_quarterly <- function() {
  plot(shock_var_q_ts,
       main = "Quarterly Fitted Bund 1Y Instrument",
       xlab = "Time", ylab = "Instrument Value (Fitted Bund 1Y)",
       col = burgundy, lwd = 2)
}

# === NEW: 2x2 plot for 3 separate shocks and an aggregate
plot_4_shocks <- function() {
  # Calculate the sum of the three monthly shocks
  total_shock <- shocks_ts[, "pureMP_m"] + shocks_ts[, "Path_m"]
  
  # Set up a 2x2 grid
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
  
  # (1) Pure MP
  plot(shocks_ts[, "pureMP_m"], type = "l",
       main = "Pure MP Shock",
       xlab = "Time", ylab = "Basis Points",
       col = burgundy, lwd = 2)
  grid()
  
  # (2) Path
  plot(shocks_ts[, "Path_m"], type = "l",
       main = "Path Shock",
       xlab = "Time", ylab = "Basis Points",
       col = burgundy, lwd = 2)
  grid()
  
  # (4) Sum of All
  plot(shock_var_q_ts, type = "l",
       main = "Instrument",
       xlab = "Time", ylab = "Basis Points",
       col = burgundy, lwd = 2)
  grid()
}

# Save plots to the specified directory
plot_dir <- file.path(
  "Graphs",
  "LP-IV",
  "PureMP & Path",
  "1.Stage"
)


save_plot(file.path(plot_dir, "Actual_vs_Fitted_Bund_1Y.png"), plot_actual_fitted)
save_plot(file.path(plot_dir, "Shock_and_Fitted_Panel.png"), plot_shock_fitted, height = 1600)
save_plot(file.path(plot_dir, "Quarterly_Instrument.png"), plot_quarterly)

# NEW 2x2 plot for 3 individual shocks + aggregated shock
save_plot(file.path(plot_dir, "All_Shocks_2x2.png"), plot_4_shocks, width = 2000, height = 2000)

