# Clear workspace and console
rm(list = ls())
cat("\014")

# Load required libraries
library(lpirfs)
library(dplyr)

# Set working directory
user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else {
  stop("User unknown")
}

# Load data
data <- readRDS("Data/Panel LP-IV/panel_input_data.rds") %>%
  arrange(country, Date)

# Define settings
outcome_vars <- c("d_rGDP", "d_HICP", "bund_yield")
shock_var <- "shock"
lags <- 4
horizon <- 8
confint_level <- 1.96

# Placeholder matrices
irf_mean <- matrix(NA, nrow = length(outcome_vars), ncol = 8)
irf_low <- matrix(NA, nrow = length(outcome_vars), ncol = 8)
irf_up <- matrix(NA, nrow = length(outcome_vars), ncol = 8)

# Run LPs for each outcome variable
for (i in seq_along(outcome_vars)) {
  outcome <- outcome_vars[i]
  
  lp <- lp_lin_panel(
    data_set        = data,
    endog_data      = outcome,
    cumul_mult      = FALSE,  # We want level IRFs
    shock           = shock_var,
    diff_shock      = FALSE,
    iv_reg          = FALSE,
    panel_model     = "within",
    panel_effect    = "individual",
    robust_cov      = "vcovSCC",
    robust_maxlag   = NULL,
    c_exog_data     = setdiff(outcome_vars, outcome),
    l_exog_data     = outcome_vars,
    lags_exog_data  = lags,
    hor             = horizon,
    confint         = confint_level
  )
  
  irf_mean[i, ] <- lp$irf_panel_mean
  irf_low[i, ]  <- lp$irf_panel_low
  irf_up[i, ]   <- lp$irf_panel_up
}

# Normalize by Bund Yield IRF at horizon 0
shock_index <- match("bund_yield", outcome_vars)
scaling <- 1 / irf_mean[shock_index, 1]
irf_mean <- irf_mean * scaling
irf_low  <- irf_low  * scaling
irf_up   <- irf_up   * scaling

# Plotting IRFs
pretty_names <- list(
  d_HICP     = "HICP",
  d_rGDP     = "Real GDP",
  bund_yield = "Bund Yield"
)

plot_panel_irfs <- function(irf_mean, irf_low, irf_up, outcome_vars, file_name) {
  horizons <- 0:(ncol(irf_mean) - 1)
  png(file_name, width = 2000, height = 2400, res = 300)
  par(mfrow = c(3, 1), mar = c(5, 5, 4, 2),
      cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
  
  for (i in seq_along(outcome_vars)) {
    var <- outcome_vars[i]
    nice_lbl <- ifelse(var %in% names(pretty_names), pretty_names[[var]], var)
    irf <- irf_mean[i, ]
    low <- irf_low[i, ]
    up  <- irf_up[i, ]
    
    range_irf <- range(c(irf, low, up), na.rm = TRUE)
    buffer <- 0.1 * max(abs(range_irf))
    y_min <- range_irf[1] - buffer
    y_max <- range_irf[2] + buffer
    
    plot(horizons, irf, type = "n", ylim = c(y_min, y_max),
         xlab = "Horizon (quarters)", ylab = "Impulse Response",
         main = paste("Shock on", nice_lbl))
    
    polygon(c(horizons, rev(horizons)),
            c(low, rev(up)),
            col = rgb(118/255, 0, 32/255, alpha = 0.3), border = NA)
    
    lines(horizons, irf, type = "b", col = "#760020", lwd = 2, pch = 16)
    abline(h = 0, lty = 2)
    grid()
  }
  dev.off()
}

# Save file
output_path <- "C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3/Graphs/Panel LP-IV/IRF_Panel.png"

plot_panel_irfs(
  irf_mean     = irf_mean,
  irf_low      = irf_low,
  irf_up       = irf_up,
  outcome_vars = outcome_vars,
  file_name    = output_path
)


