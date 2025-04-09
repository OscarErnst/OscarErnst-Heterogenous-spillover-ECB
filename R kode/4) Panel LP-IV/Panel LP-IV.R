rm(list = ls())
cat("\014")

# ----------------------------------------------------------------------------
# 0) Setup
# ----------------------------------------------------------------------------
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")

library(dplyr)
library(plm)
library(sandwich)
library(lmtest)

# ----------------------------------------------------------------------------
# 1) Load and filter your main dataset
# ----------------------------------------------------------------------------
# IMPORTANT: We do NOT drop EA20 so that it can serve as the baseline.
# We only filter to keep years < 2020.
full_data <- readRDS("Output data/Eurozone_country_variables.rds") %>%
  filter(year < 2020)

# Load the instrument
instrument_full <- readRDS("Output data/instrument.rds")

# ----------------------------------------------------------------------------
# 2) Create a single integer 'time' index in full_data
# ----------------------------------------------------------------------------
df_panel <- full_data %>%
  mutate(time = year * 4 + quarter)

# ----------------------------------------------------------------------------
# 3) Pair the 'instrument' vector with the distinct time values
# ----------------------------------------------------------------------------
# We'll assume one instrument value per unique time, in ascending order.
unique_times <- sort(unique(df_panel$time))
df_instrument <- data.frame(
  time       = unique_times,
  instrument = instrument_full[1:length(unique_times)]
)

df_panel <- left_join(df_panel, df_instrument, by = "time")

# ----------------------------------------------------------------------------
# 4) Ensure 'country' is a factor, with "EA20" as reference
# ----------------------------------------------------------------------------
df_panel$country <- factor(df_panel$country)
# Relevel so that EA20 is the baseline
# This means in the regression, the intercept is EA20's intercept, 
# and the coefficient on "instrument" is EA20's slope.
df_panel$country <- relevel(df_panel$country, ref = "EA20")

# ----------------------------------------------------------------------------
# 5) Create lags of your outcome
# ----------------------------------------------------------------------------
outcome_var <- "d_rGDP"
p_lags <- 4

for (lag_i in 1:p_lags) {
  lag_name <- paste0("L", lag_i, "_", outcome_var)
  df_panel <- df_panel %>%
    group_by(country) %>%
    mutate(!!lag_name := lag(.data[[outcome_var]], n = lag_i)) %>%
    ungroup()
}

# ----------------------------------------------------------------------------
# 6) Loop Over Horizons, Fit LSDV, Driscoll-Kraay SE
# ----------------------------------------------------------------------------
H_max <- 8

# We'll store output in a list, keyed by horizon
results_list <- list()

# Vector of needed lag columns
lag_cols <- paste0("L", 1:p_lags, "_", outcome_var)

for (h in 0:H_max) {
  message("-------- Horizon = ", h, " --------")
  
  # 1) Create y_lead
  df_panel <- df_panel %>%
    group_by(country) %>%
    mutate(y_lead = dplyr::lead(.data[[outcome_var]], n = h)) %>%
    ungroup()
  
  # 2) Filter out rows with missing y_lead or missing lags
  needed_vars <- c("y_lead", lag_cols)
  df_h <- df_panel %>%
    filter(!if_any(all_of(needed_vars), is.na))
  
  # Drop unused factor levels
  df_h <- droplevels(df_h)
  
  # If fewer than 2 countries remain, skip
  n_countries <- n_distinct(df_h$country)
  if (n_countries < 2) {
    message("Skipping horizon ", h, ": only ", n_countries, " country left.")
    results_list[[paste0("h", h)]] <- NULL
    next
  }
  
  # 3) Build the LSDV formula with EA20 as baseline
  # 
  #   y_lead ~ instrument + factor(country) + instrument:factor(country) + lags
  # 
  # Because country is releveled to EA20, the intercept is for EA20, 
  # and the coefficient on "instrument" is EA20's slope. 
  # The difference for other countries is in instrument:factor(country)XX.
  # 
  # Note we do NOT do "-1" because we want a baseline intercept for EA20.
  # That means factor(country) is "all countries except EA20" dummies.
  # 
  # For the lags, we assume common coefficients, so just "L1_d_rGDP + ...".
  
  formula_str <- paste(
    "y_lead ~ instrument + factor(country) + instrument:factor(country) +",
    paste(lag_cols, collapse = " + ")
  )
  
  form <- as.formula(formula_str)
  
  # 4) Fit LSDV with plm, model="pooling"
  mod <- plm(
    formula = form,
    data    = df_h,
    model   = "pooling",   # LSDV: we explicitly have the dummies in the formula
    index   = c("country", "time")
  )
  
  # 5) Driscoll-Kraay SE
  vcov_dk <- vcovDC(mod)
  
  ctest <- coeftest(mod, vcov = vcov_dk)
  
  # Convert to a tidy data frame
  ctab <- data.frame(
    term      = rownames(ctest),
    estimate  = ctest[, 1],
    std.error = ctest[, 2],
    t.value   = ctest[, 3],
    p.value   = ctest[, 4],
    stringsAsFactors = FALSE
  )
  
  # Store in results_list
  ctab$horizon <- h
  ctab$n.obs   <- nobs(mod)
  
  results_list[[paste0("h", h)]] <- ctab
}

# Combine into one big table
results_df <- dplyr::bind_rows(results_list, .id = "horizon_id")


