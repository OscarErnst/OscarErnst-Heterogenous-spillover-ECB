


# ─────────────────────────────────────────────────────────────────────────
# 1. Working directory and data load (same as before)
# ─────────────────────────────────────────────────────────────────────────
# Clear workspace and console
rm(list = ls())
cat("\014")
library(lpirfs)

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

# 3) Load your main data (quarterly, per country) and policy shock
#    Make sure 'full_data' has columns:
#      - Date       (quarterly, e.g. 2006-01-01, 2006-04-01, etc.)
#      - country    (e.g. "AT", "BE", etc.)
#      - d_rGDP     (your outcome of interest)
#      - bund_yield (a control or second outcome)
#      - Possibly others (HICP, consumption, etc.)
full_data <- readRDS("Data/LP-IV/Kun PureMP/input_data.rds")

#    Load shock data that has the same or overlapping Date range:
shock_all <- readRDS("Data/LP-IV/Kun PureMP/PureMP_shock.rds")
# 'shock_all' should have columns:
#   - Date     (matching the same quarterly dates)
#   - target_q (the shock we want to use in the IV)

# 3a) Merge the shock with the main dataset by Date
#     After merging, each row in 'merged_data' has the relevant shock value.
merged_data <- full_data %>%
  left_join(shock_all, by = "Date")

# 4) Define the set of countries
countries <- c("AT", "BE", "CY", "DE", "DK", "EA20", "EE", "EL", "ES", "FI",
               "FR", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
# If you only want to run for "EA20", uncomment below:
countries <- "EA20"

# 5) Create a lookup for nicer country names (optional)
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

get_country_label <- function(code) {
  if (code %in% names(country_names)) {
    return(country_names[code])
  } else {
    return(code)  # fallback
  }
}

# 6) Specify outcome variables for the model
#    We'll assume you want "d_rGDP" as your main outcome
#    and "bund_yield" as a second variable in the local projections.
outcome_vars <- c("d_rGDP", "bund_yield")

# 7) Open a PNG device for multi-panel plots (21 countries -> 7 rows x 3 columns)
png("Graphs/LP-IV/AllCountries_rGDP.png", width = 3200, height = 4200, res = 300)
par(mfrow = c(7, 3),
    mar = c(4, 4, 2, 1),
    cex.axis = 0.8,
    cex.lab  = 0.8,
    cex.main = 1.0)

# Define the burgundy colors used in your plots
burgundy       <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

# 8) Loop over each country
for (ctry in countries) {
  
  # --- (a) Subset merged_data to this country
  df_ctry <- merged_data %>%
    filter(country == ctry)
  
  # Check if we got any rows
  if (nrow(df_ctry) == 0) {
    message("No data found for country ", ctry, ". Skipping.")
    next
  }
  
  # Rename 'target_q' to 'shock' for convenience, and select relevant columns
  df_ctry <- df_ctry %>%
    dplyr::select(all_of(outcome_vars), shock = target_q)
  
  # Omit rows with NA in the columns of interest
  tmp <- na.omit(df_ctry)
  
  # If there's no data left after na.omit, skip
  if (nrow(tmp) == 0) {
    message("All data for country ", ctry, " is NA. Skipping.")
    next
  }
  
  # Create the endogenous data matrix (Y) and the shock series
  Y     <- tmp[, outcome_vars, drop = FALSE]
  shock <- tmp[, "shock", drop = FALSE]
  
  # --- (b) Run LP-IV within a tryCatch to handle any internal errors
  lpiv_res <- tryCatch(
    lp_lin_iv(
      endog_data     = Y,
      shock          = shock,
      lags_endog_lin = 4,
      trend          = 0,
      confint        = 1.96,    # ~95% CI
      use_nw         = TRUE,
      lags_criterion = "AIC",
      max_lags       = 9,
      hor            = 8
    ),
    error = function(e) e  # return the error object on failure
  )
  
  # If lp_lin_iv failed, skip
  if (inherits(lpiv_res, "error")) {
    message("Error for country ", ctry, ": ", lpiv_res$message)
    next
  }
  
  # --- (c) Identify the row corresponding to 'bund_yield' in the IRF
  shockpos <- match("bund_yield", outcome_vars)
  
  # If bund_yield isn't in outcome_vars or doesn't appear in the IRF matrix, skip
  if (is.na(shockpos) || shockpos > nrow(lpiv_res$irf_lin_mean)) {
    message("'bund_yield' not found in IRF for ", ctry, ". Skipping.")
    next
  }
  
  # We assume row shockpos, horizon=0 => column 1
  impact_original <- lpiv_res$irf_lin_mean[shockpos, 1]
  
  # If 'impact_original' is NA or zero, scaling will fail
  if (is.na(impact_original) || impact_original == 0) {
    message("Cannot scale IRF for ", ctry, ": bund_yield(0) is NA or 0. Skipping.")
    next
  }
  
  # Compute the scaling factor
  scaling <- 1 / impact_original
  
  # Scale the entire IRF
  lpiv_res$irf_lin_mean <- lpiv_res$irf_lin_mean * scaling
  lpiv_res$irf_lin_low  <- lpiv_res$irf_lin_low  * scaling
  lpiv_res$irf_lin_up   <- lpiv_res$irf_lin_up   * scaling
  
  # --- (d) Extract IRF for d_rGDP
  rgdp_pos <- match("d_rGDP", outcome_vars)
  if (is.na(rgdp_pos) || rgdp_pos > nrow(lpiv_res$irf_lin_mean)) {
    message("'d_rGDP' not found in IRF for ", ctry, ". Skipping.")
    next
  }
  
  rgdp_irf   <- lpiv_res$irf_lin_mean[rgdp_pos, ]
  rgdp_lower <- lpiv_res$irf_lin_low [rgdp_pos, ]
  rgdp_upper <- lpiv_res$irf_lin_up  [rgdp_pos, ]
  
  horizons <- 0:(length(rgdp_irf) - 1)
  
  # Determine y-limits
  y_min <- min(rgdp_lower, na.rm = TRUE)
  y_max <- max(rgdp_upper, na.rm = TRUE)
  
  # Country label
  ctry_label <- get_country_label(ctry)
  
  # --- (e) Plot
  plot(horizons, rgdp_irf,
       type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon (quarters)",
       ylab = "Impulse Response: d_rGDP",
       main = ctry_label)
  abline(h = 0, lty = 2, col = "black")
  
  # Confidence band polygon
  polygon(
    x = c(horizons, rev(horizons)),
    y = c(rgdp_lower, rev(rgdp_upper)),
    col = burgundy_trans, border = NA
  )
  
  # IRF line
  lines(horizons, rgdp_irf, type = "b", col = burgundy, lwd = 2, pch = 16)
}
# 9) Close the PNG device
dev.off()

