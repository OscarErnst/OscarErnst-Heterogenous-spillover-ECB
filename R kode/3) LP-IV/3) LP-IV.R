# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on system user
user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# -------------------------------------------------------------------------
# 1. Packages
# -------------------------------------------------------------------------
library(lpirfs)      # for lp_lin_iv()
library(dplyr)

# -------------------------------------------------------------------------
# 2. Load data
# -------------------------------------------------------------------------
data <- readRDS("Data/LP-IV/input_data_q.rds") %>%
  filter(country == "DE") %>%
  dplyr::select(-country)

shock_df <- readRDS("Data/LP-IV/shocks_q.rds")

# outcome vars & pretty names
outcome_vars <- c("d_HICP", "d_rGDP", "d_Consumption", "bund_yield")
pretty_names <- list(
  "d_HICP"        = "HICP",
  "d_rGDP"        = "Real GDP",
  "d_Consumption" = "Consumption",
  "bund_yield"    = "Bund Yield"
)

# merge shock into data
df <- na.omit(cbind(
  data[ , outcome_vars],
  shock = shock_df$shock
))
Y     <- df[, outcome_vars]
shock <- df["shock"]

# -------------------------------------------------------------------------
# 3. LP‑IV with automatic lag selection via AIC
# -------------------------------------------------------------------------
lpiv_res <- lp_lin_iv(
  endog_data     = Y,
  shock          = shock,
  use_twosls     = FALSE,
  cumul_mult     = FALSE,
  # let the function choose lags by AIC up to 12
  lags_endog_lin = NaN,
  lags_criterion = "AIC",
  max_lags       = 4,
  trend          = 0,
  confint        = 1.96,
  use_nw         = TRUE,
  hor            = 13
)

# -------------------------------------------------------------------------
# 4. Normalize so Bund‑yield response at h=0 == 1
# -------------------------------------------------------------------------
shockpos         <- match("bund_yield", outcome_vars)
impact_original  <- lpiv_res$irf_lin_mean[shockpos, 1]
scaling          <- 1 / impact_original

lpiv_res$irf_lin_mean <- lpiv_res$irf_lin_mean * scaling
lpiv_res$irf_lin_low  <- lpiv_res$irf_lin_low  * scaling
lpiv_res$irf_lin_up   <- lpiv_res$irf_lin_up   * scaling

# -------------------------------------------------------------------------
# 5. Plotting function (2×2) with wider y‑axis buffer (25%)
# -------------------------------------------------------------------------
plot_level_irfs <- function(lp, vars, file_name) {
  horizons <- 0:(ncol(lp$irf_lin_mean) - 1)
  
  png(file_name, width = 4000, height = 4000, res = 300)
  par(mfrow = c(2, 2),
      mar   = c(5, 5, 4, 2),
      cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
  
  for (i in seq_along(vars)) {
    v    <- vars[i]
    mean <- lp$irf_lin_mean[i, ]
    low  <- lp$irf_lin_low[i, ]
    up   <- lp$irf_lin_up[i, ]
    
    r    <- range(c(mean, low, up), na.rm = TRUE)
    buf  <- 0.25 * max(abs(r))     # 25% buffer
    ylim <- c(r[1] - buf, r[2] + buf)
    
    plot(horizons, mean, type = "n", ylim = ylim,
         xlab = "Horizon (quarters)",
         ylab = "Impulse Response",
         main = paste("Shock on", pretty_names[[v]]))
    
    polygon(c(horizons, rev(horizons)),
            c(low,       rev(up)),
            col   = rgb(118/255, 0, 32/255, alpha = 0.3),
            border= NA)
    
    lines(horizons, mean, type = "b",
          col = "#760020", lwd = 2, pch = 16)
    abline(h = 0, lty = 2)
    grid()
  }
  dev.off()
}

# -------------------------------------------------------------------------
# 6. Save the figure
# -------------------------------------------------------------------------
plot_level_irfs(
  lp       = lpiv_res,
  vars     = outcome_vars,
  file_name= "Graphs/LP-IV/Impulse Responses/IRF_DE.png"
)
