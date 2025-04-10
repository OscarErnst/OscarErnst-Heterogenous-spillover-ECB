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
data <- readRDS("Data/LP-IV/input_data.rds")
target_q <- readRDS("Data/LP-IV/tamas_instrument.rds")

# Define endogenous variables
outcome_vars <- c("d_HICP", "d_rGDP", "d_Consumption", "bund_yield")

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
  use_twosls = F,         # Only 2nd stage, reduced-form
  lags_endog_lin = NaN,         # You can change this to what you want
  trend = 0,                  # No trend
  confint = 1.70,             # 95% confidence band (normal approx)
  use_nw = TRUE,              # Newey-West SEs
  lags_criterion = "AIC",
  max_lags = 9,
  hor = 7                   # 12-quarter horizon
)

shockpos <- match("bund_yield", outcome_vars)  # returns 3 if bund_yield is the 3rd element

# Get the impact response of bund_yield at horizon 0
impact <- lpiv_res$irf_lin_mean[shockpos, 1]
scaling <- 1 / impact  # set impact to +1

lpiv_res$irf_lin_mean <- lpiv_res$irf_lin_mean * scaling
lpiv_res$irf_lin_low  <- lpiv_res$irf_lin_low  * scaling
lpiv_res$irf_lin_up   <- lpiv_res$irf_lin_up   * scaling

# Verify
cat("Post-normalization Bund Yield IRF at horizon 0:\n")
print(lpiv_res$irf_lin_mean[shockpos, 1])


cat("Pre-normalization Bund Yield IRF at horizon 0:\n")
print(lpiv_res$irf_lin_mean[shockpos, 1])
# === Define settings ===
output_vars <- c("HICP_log", "rGDP_log", "bund_yield")
horizons <- 0:(ncol(lpiv_res$irf_lin_mean) - 1)

# Colors
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

# Output path
plot_path <- "Graphs/LP-IV/IRF_buildin_func.png"

# === Save to PNG ===
png(file.path(getwd(), plot_path),
    width = 2000, height = 2400, res = 300)

# Layout for 3 variables
par(mfrow = c(3, 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# === Plot each variable ===
for (i in seq_along(output_vars)) {
  irf   <- lpiv_res$irf_lin_mean[i, ]
  lower <- lpiv_res$irf_lin_low[i, ]
  upper <- lpiv_res$irf_lin_up[i, ]
  
  # 1) Full range auto-scale
  irf_range <- range(c(irf, lower, upper), na.rm = TRUE)
  buffer <- 0.1 * max(abs(irf_range))
  y_min <- irf_range[1] - buffer
  y_max <- irf_range[2] + buffer
  
  # Base plot
  plot(horizons, irf, type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon (quarters)", ylab = "Impulse Response",
       main = paste("Shock on", output_vars[i]))
  
  polygon(c(horizons, rev(horizons)),
          c(lower, rev(upper)),
          col = burgundy_trans, border = NA)
  lines(horizons, irf, type = "b", col = burgundy, lwd = 2, pch = 16)
  abline(h = 0, lty = 2, col = "black")
  grid()
  axis(1, at = seq(0, max(horizons), by = 2))
}

# Close PNG device
dev.off()

# 1) Make copies of your IRF matrices
cum_irf_mean <- lpiv_res$irf_lin_mean
cum_irf_low  <- lpiv_res$irf_lin_low
cum_irf_up   <- lpiv_res$irf_lin_up

# 2) For each variable (row), cumsum across horizons (columns)
for (r in 1:nrow(cum_irf_mean)) {
  cum_irf_mean[r, ] <- cumsum(cum_irf_mean[r, ])
  cum_irf_low[r, ]  <- cumsum(cum_irf_low[r, ])
  cum_irf_up[r, ]   <- cumsum(cum_irf_up[r, ])
}

# 3) Plot them exactly as before
#    but replace 'lpiv_res$irf_lin_mean' with 'cum_irf_mean', etc.
horizons <- 0:(ncol(cum_irf_mean) - 1)

png("Graphs/LP-IV/IRF_cumulative.png", width=2000, height=2400, res=300)
par(mfrow=c(3,1), mar=c(5,5,4,2), cex.axis=1.2, cex.lab=1.2, cex.main=1.5)

for (i in seq_along(outcome_vars)) {
  irf   <- cum_irf_mean[i, ]
  lower <- cum_irf_low[i, ]
  upper <- cum_irf_up[i, ]
  
  irf_range <- range(c(irf, lower, upper), na.rm=TRUE)
  buffer    <- 0.1 * max(abs(irf_range))
  y_min     <- irf_range[1] - buffer
  y_max     <- irf_range[2] + buffer
  
  plot(horizons, irf, type="n", ylim=c(y_min, y_max),
       xlab="Horizon (quarters)", ylab="Cumulative IRF",
       main=paste("Cumulative Shock on", outcome_vars[i]))
  polygon(c(horizons, rev(horizons)), c(lower, rev(upper)),
          col=rgb(118/255,0,32/255,0.3), border=NA)
  lines(horizons, irf, type="b", col="#760020", lwd=2, pch=16)
  abline(h=0, lty=2, col="black"); grid()
}

dev.off()

