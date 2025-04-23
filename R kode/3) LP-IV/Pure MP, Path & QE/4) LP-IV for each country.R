# Clear workspace and console
rm(list = ls()); cat("\014")

# ─────────────────────────────────────────────────────────────────────────────
# 0. Working dir & packages
# ─────────────────────────────────────────────────────────────────────────────
user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else stop("Unknown user")

library(lpirfs)
library(dplyr)

# colors
burgundy       <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load data + shock
# ─────────────────────────────────────────────────────────────────────────────
full_data <- readRDS("Data/LP-IV/PureMP, Path & QE/input_data.rds")
shock_all <- readRDS("Data/LP-IV/PureMP, Path & QE/shock.rds")

# Rename Date columns to 'date' for consistent merging
full_data <- full_data %>% rename(date = Date)
shock_all <- shock_all %>% rename(date = Date)

countries <- c("DE","FR","NL","DK","AT","IT","ES","PT","EL")
country_names <- c(
  AT = "Austria", DE = "Germany", FR = "France",
  NL = "Netherlands", DK = "Denmark", IT = "Italy",
  ES = "Spain", PT = "Portugal", EL = "Greece"
)
get_label <- function(ctry) country_names[ctry]

# ─────────────────────────────────────────────────────────────────────────────
# 2. Function to plot IRFs for one variable across all countries
# ─────────────────────────────────────────────────────────────────────────────
plot_all_countries <- function(var, out_file) {
  png(out_file, width = 3600, height = 3600, res = 300)
  par(mfrow = c(3,3), mar = c(5,5,3,1), cex.lab = 0.8, cex.axis = 0.8, cex.main = 1)
  
  for (ctry in countries) {
    # Subset country data and merge shock by date
    d_ctry <- full_data %>%
      filter(country == ctry) %>%
      left_join(shock_all, by = "date")
    
    df <- na.omit(d_ctry %>% select(d_rGDP, d_HICP, bund_yield, shock))
    if (nrow(df) == 0) {
      plot(NA, axes = FALSE, xlab = "", ylab = "", main = get_label(ctry))
      next
    }
    
    # Prepare Y and shock matrices
    Y_ctry  <- df[, c("d_rGDP","d_HICP","bund_yield")]
    sh_ctry <- df["shock", drop = FALSE]
    
    # Run Local Projections IV
    lpiv_ctry <- lp_lin_iv(
      endog_data     = Y_ctry,
      shock          = sh_ctry,
      lags_endog_lin = 4,
      trend          = 0,
      confint        = 1.96,
      use_nw         = TRUE,
      hor            = 13
    )
    
    # Normalize so Bund-yield(0) = 1
    bpos    <- match("bund_yield", colnames(Y_ctry))
    scale_f <- 1 / lpiv_ctry$irf_lin_mean[bpos, 1]
    irf_mat <- lpiv_ctry$irf_lin_mean * scale_f
    low_mat <- lpiv_ctry$irf_lin_low   * scale_f
    up_mat  <- lpiv_ctry$irf_lin_up    * scale_f
    
    # Extract the desired variable
    pos   <- match(var, colnames(Y_ctry))
    irf   <- irf_mat[pos, ]
    low   <- low_mat[pos, ]
    up    <- up_mat[pos, ]
    horiz <- 0:(length(irf) - 1)
    
    # Determine y-axis limits with buffer
    r    <- range(c(irf, low, up), na.rm = TRUE)
    buf  <- 0.25 * max(abs(r))
    ylim <- c(r[1] - buf, r[2] + buf)
    
    # Plot
    plot(horiz, irf, type = "n", ylim = ylim,
         xlab = "Horizon (quarters)", ylab = "Impulse response",
         main = get_label(ctry))
    polygon(c(horiz, rev(horiz)), c(low, rev(up)),
            col = burgundy_trans, border = NA)
    lines(horiz, irf, type = "b", col = burgundy, lwd = 2, pch = 16)
    abline(h = 0, lty = 2)
    grid()
  }
  
  dev.off()
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. Produce two plots: one for d_rGDP, one for d_HICP
# ─────────────────────────────────────────────────────────────────────────────
plot_all_countries(
  var      = "d_rGDP",
  out_file = "Graphs/LP-IV/PureMP, Path & QE/Impulse Responses/All_Countries_d_rGDP.png"
)

plot_all_countries(
  var      = "d_HICP",
  out_file = "Graphs/LP-IV/PureMP, Path & QE/Impulse Responses/All_Countries_d_HICP.png"
)

