# ----------------------------------------------------------------------
# Clear environment and load libraries
rm(list = ls())
cat("\014")

library(dplyr)
library(AER)
library(dynlm)
library(car)
library(zoo)      # for as.yearqtr()
library(sandwich) # for Newey-West SEs
library(lmtest)   # for coeftest()
library(stat)   # for coeftest()

setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")

# ----------------------------------------------------------------------
# LP-IV estimation function for local projections
estimateLPIV <- function(data, h, p, c_case = 1, alpha = 95, NWSE = TRUE) {
  # data: a ts or data.frame with at least two columns:
  #       "rGDP_change" (dependent variable) and "i_hat" (the instrument)
  # h: maximum horizon (number of quarters to forecast, e.g., 9 for 0:8)
  # p: number of lags of rGDP_change to include as controls
  # c_case: 0 = no constant, 1 = constant only, 2 = constant and trend (not used here)
  # alpha: confidence level (default 95)
  # NWSE: use Newey-West standard errors if TRUE
  
  # Container for results
  results <- data.frame(horizon = 0:(h - 1),
                        beta = NA, se = NA,
                        lower = NA, upper = NA)
  
  # Determine the critical z-value for two-sided confidence intervals
  zcrit <- qnorm(1 - (1 - alpha/100)/2)
  
  # Convert data to matrix if needed
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  
  # Assume data columns are named "rGDP_change" and "i_hat"
  y <- data[, "rGDP_change"]
  x_inst <- data[, "i_hat"]
  
  # Total number of observations
  Tn <- length(y)
  
  # Loop over horizons (0 means contemporaneous, h=1 means one-quarter ahead, etc.)
  for (h0 in 0:(h - 1)) {
    # Construct the dependent variable as the h0-lead of y.
    # dynlm uses L(y, -h0) for a lead.
    reg_formula <- as.formula(paste("L(rGDP_change, -", h0, ") ~ i_hat", sep = ""))
    
    # Build a data frame from the time series data
    regdata <- data.frame(rGDP_change = y, i_hat = x_inst)
    
    # Estimate the regression using dynlm (it automatically drops NAs)
    model_lp <- dynlm(reg_formula, data = regdata)
    
    # Obtain Newey-West standard errors
    if (NWSE) {
      nw_vcov <- NeweyWest(model_lp, lag = h0 + p, prewhite = FALSE, adjust = TRUE)
      se_i <- sqrt(diag(nw_vcov))["i_hat"]
    } else {
      se_i <- summary(model_lp)$coefficients["i_hat", "Std. Error"]
    }
    
    beta_i <- coef(model_lp)["i_hat"]
    
    # Store results with confidence intervals
    results$beta[results$horizon == h0] <- beta_i
    results$se[results$horizon == h0]   <- se_i
    results$lower[results$horizon == h0] <- beta_i - zcrit * se_i
    results$upper[results$horizon == h0] <- beta_i + zcrit * se_i
  }
  
  return(results)
}

# ----------------------------------------------------------------------
# 1) Load final_data and filter for EA20
final_data <- readRDS("Output data/Eurozone_country_variables.rds") %>%
  filter(country == "EA20")

# Create a quarterly time series for rGDP_change.
start_year    <- final_data$year[1]
start_quarter <- final_data$quarter[1]
end_year      <- final_data$year[nrow(final_data)]
end_quarter   <- final_data$quarter[nrow(final_data)]

rGDP_change_ts <- ts(final_data$rGDP_change,
                     start = c(start_year, start_quarter),
                     end   = c(end_year, end_quarter),
                     frequency = 4)

# ----------------------------------------------------------------------
# 2) Load the instrument (i_hat) from shocks_test.rds
# This file should contain a quarterly ts object representing the change in i_t.
i_hat_ts <- readRDS("Output data/shocks_test.rds")

# ----------------------------------------------------------------------
# 3) Combine into one ts object. They must span the same period.
lp_data <- cbind(rGDP_change = rGDP_change_ts, i_hat = i_hat_ts)
colnames(lp_data) <- c("rGDP_change", "i_hat")

# ----------------------------------------------------------------------
# 4) Run the LP-IV estimation for horizons 0 to 8 (i.e., 9 quarters) with 2 lags.
irf_results <- estimateLPIV(data = lp_data, h = 8, p = 4, c_case = 0, alpha = 95, NWSE = TRUE)
print(irf_results)

# ----------------------------------------------------------------------
# Plot the impulse response function (IRF) using base R plotting.
plot(irf_results$horizon, irf_results$beta, type = "b", pch = 16,
     ylim = range(c(irf_results$lower, irf_results$upper)),
     xlab = "Horizon (quarters)",
     ylab = "Impulse Response (β)",
     main = "Impulse Response of rGDP_change for MP shock")
lines(irf_results$horizon, irf_results$lower, lty = 2, col = "red")
lines(irf_results$horizon, irf_results$upper, lty = 2, col = "red")
abline(h = 0, lty = 3)

# Save the resulting data.frame as an RDS file.
#saveRDS(shock_df, file = "Output data/shock_df.rds")

