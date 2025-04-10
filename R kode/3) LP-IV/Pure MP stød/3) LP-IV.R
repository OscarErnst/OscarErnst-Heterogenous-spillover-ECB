# Clear workspace and console
rm(list = ls())
cat("\014")

library(lpirfs)

# ─────────────────────────────────────────────────────────────────────────
# 1. Working directory and data load (same as before)
# ─────────────────────────────────────────────────────────────────────────
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

data   <- readRDS("Data/LP-IV/Kun PureMP/input_data.rds") %>% filter(country == "EA20") %>% 
  dplyr::select(-country)
PureMP <- readRDS("Data/LP-IV/Kun PureMP/Bund_instrument.rds")

# Define outcome variables (unchanged)
outcome_vars <- c("d_HICP", "d_rGDP", "bund_yield")

# Nice display names for each variable:
pretty_names <- list(
  "d_HICP"        = "HICP",
  "d_rGDP"        = "Real GDP",
  "d_Consumption" = "Consumption",
  "bund_yield"    = "Bund Yield"
)

# ─────────────────────────────────────────────────────────────────────────
# 2. LP‐IV estimation (same as before)
# ─────────────────────────────────────────────────────────────────────────
Y     <- data[, outcome_vars]
shock <- data.frame(shock = PureMP)

df <- na.omit(cbind(Y, shock))

Y     <- df[, outcome_vars]
shock <- df[, "shock", drop = FALSE]

lpiv_res <- lp_lin_iv(
  endog_data     = Y,
  shock          = shock,
  use_twosls     = FALSE,
  lags_endog_lin = NaN,
  trend          = 0,
  confint        = 1.70,  # ~95% CI
  use_nw         = TRUE,
  lags_criterion = "AIC",
  max_lags       = 9,
  hor            = 7
)

# Identify which row is Bund Yield:
shockpos <- match("bund_yield", outcome_vars)
impact_original <- lpiv_res$irf_lin_mean[shockpos, 1]
scaling <- 1 / impact_original

# Normalize so that bund_yield(0) == +1
lpiv_res$irf_lin_mean <- lpiv_res$irf_lin_mean * scaling
lpiv_res$irf_lin_low  <- lpiv_res$irf_lin_low  * scaling
lpiv_res$irf_lin_up   <- lpiv_res$irf_lin_up   * scaling

# ─────────────────────────────────────────────────────────────────────────
# 3. Plotting function for level IRFs
# ─────────────────────────────────────────────────────────────────────────
plot_level_irfs <- function(lpiv_res, outcome_vars, file_name) {
  
  # Time horizon 0..(ncol-1)
  horizons <- 0:(ncol(lpiv_res$irf_lin_mean) - 1)
  
  # Set up PNG
  png(file_name, width = 2000, height = 2400, res = 300)
  par(mfrow = c(3, 1), mar = c(5, 5, 4, 2),
      cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
  
  for (i in seq_along(outcome_vars)) {
    var_name <- outcome_vars[i]
    nice_lbl <- ifelse(var_name %in% names(pretty_names),
                       pretty_names[[var_name]],
                       var_name)
    
    irf   <- lpiv_res$irf_lin_mean[i, ]
    lower <- lpiv_res$irf_lin_low[i, ]
    upper <- lpiv_res$irf_lin_up[i, ]
    
    irf_range <- range(c(irf, lower, upper), na.rm = TRUE)
    buffer    <- 0.1 * max(abs(irf_range))
    y_min     <- irf_range[1] - buffer
    y_max     <- irf_range[2] + buffer
    
    plot(horizons, irf, type = "n", ylim = c(y_min, y_max),
         xlab = "Horizon (quarters)", ylab = "Impulse Response",
         main = paste("Shock on", nice_lbl))
    
    polygon(c(horizons, rev(horizons)),
            c(lower, rev(upper)),
            col = rgb(118/255, 0, 32/255, alpha = 0.3), border = NA)
    
    lines(horizons, irf, type = "b", col = "#760020", lwd = 2, pch = 16)
    abline(h = 0, lty = 2, col = "black")
    grid()
  }
  dev.off()
}

# ─────────────────────────────────────────────────────────────────────────
# 4. Plot level IRFs
# ─────────────────────────────────────────────────────────────────────────
plot_level_irfs(
  lpiv_res        = lpiv_res,
  outcome_vars    = outcome_vars,
  file_name       = "Graphs/LP-IV/Kun PureMP/IRF_buildin_func.png"
)

# ─────────────────────────────────────────────────────────────────────────
# 5. Plot cumulative IRFs
# ─────────────────────────────────────────────────────────────────────────
cum_irf_mean <- lpiv_res$irf_lin_mean
cum_irf_low  <- lpiv_res$irf_lin_low
cum_irf_up   <- lpiv_res$irf_lin_up

# Cumulative sum across horizons
for (r in 1:nrow(cum_irf_mean)) {
  cum_irf_mean[r, ] <- cumsum(cum_irf_mean[r, ])
  cum_irf_low[r, ]  <- cumsum(cum_irf_low[r, ])
  cum_irf_up[r, ]   <- cumsum(cum_irf_up[r, ])
}

plot_cumulative_irfs <- function(cum_mean, cum_low, cum_up, outcome_vars, file_name) {
  
  horizons <- 0:(ncol(cum_mean) - 1)
  png(file_name, width=2000, height=2400, res=300)
  par(mfrow=c(3,1), mar=c(5,5,4,2), cex.axis=1.2, cex.lab=1.2, cex.main=1.5)
  
  for (i in seq_along(outcome_vars)) {
    var_name <- outcome_vars[i]
    nice_lbl <- ifelse(var_name %in% names(pretty_names),
                       pretty_names[[var_name]],
                       var_name)
    
    irf   <- cum_mean[i, ]
    lower <- cum_low[i, ]
    upper <- cum_up[i, ]
    
    irf_range <- range(c(irf, lower, upper), na.rm=TRUE)
    buffer    <- 0.1 * max(abs(irf_range))
    y_min     <- irf_range[1] - buffer
    y_max     <- irf_range[2] + buffer
    
    plot(horizons, irf, type="n", ylim=c(y_min, y_max),
         xlab="Horizon (quarters)", ylab="Cumulative IRF",
         main=paste("Cumulative Shock on", nice_lbl))
    
    polygon(c(horizons, rev(horizons)), c(lower, rev(upper)),
            col=rgb(118/255,0,32/255,0.3), border=NA)
    
    lines(horizons, irf, type="b", col="#760020", lwd=2, pch=16)
    abline(h=0, lty=2, col="black")
    grid()
  }
  dev.off()
}

plot_cumulative_irfs(
  cum_mean  = cum_irf_mean,
  cum_low   = cum_irf_low,
  cum_up    = cum_irf_up,
  outcome_vars = outcome_vars,
  file_name = "Graphs/LP-IV/Kun PureMP/IRF_cumulative.png"
)




