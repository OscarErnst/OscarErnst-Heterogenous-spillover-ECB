rm(list = ls())
cat("\014")

# Adjust if needed
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")

# Packages
library(dplyr)
library(plm)
library(sandwich)
library(lmtest)

# 1) Read data
full_data       <- readRDS("Output data/Eurozone_country_variables.rds")
instrument_full <- readRDS("Output data/instrument.rds")

# 2) Combine into df_panel
df_panel <- full_data %>%
  mutate(instrument = instrument_full[1:nrow(full_data)])

# 3) Drop EA20 altogether
df_panel <- df_panel %>% filter(country != "EA20")

# 4) Create a single integer 'time' index
df_panel <- df_panel %>%
  mutate(time = year * 4 + quarter)

# 5) Turn 'country' into a factor
df_panel$country <- factor(df_panel$country)

# Let’s confirm your outcome variable
outcome_var <- "d_rGDP"


p_lags <- 4
for (lag_i in 1:p_lags) {
  lag_name <- paste0("L", lag_i, "_", outcome_var)
  df_panel <- df_panel %>%
    group_by(country) %>%
    mutate(!!lag_name := lag(.data[[outcome_var]], n = lag_i)) %>%
    ungroup()
}

H_max  <- 8

# We'll store output in a list, keyed by horizon
results_list <- list()

# Vector of needed lag columns
lag_cols <- paste0("L", 1:p_lags, "_", outcome_var)

for (h in 0:H_max) {
  
  message("-------- Horizon = ", h, " --------")
  
  # 1) Create y_lead
  df_panel <- df_panel %>%
    group_by(country) %>%
    dplyr::mutate(y_lead = dplyr::lead(.data[[outcome_var]], n = h)) %>%
    ungroup()
  
  # 2) Filter out rows with missing y_lead or missing lags
  needed_vars <- c("y_lead", lag_cols)
  df_h <- df_panel %>% dplyr::filter(!if_any(all_of(needed_vars), is.na))
  
  # Also drop unused factor levels if any countries vanish
  df_h <- droplevels(df_h)
  
  n_countries <- n_distinct(df_h$country)
  if (n_countries < 2) {
    message("Skipping horizon ", h, ": only ", n_countries, " country left.")
    # We'll store an empty or a note
    results_list[[paste0("h", h)]] <- NULL
    next
  }
  
  # 3) Build the LSDV formula
  #
  # Intercepts for ALL countries:  -1 + factor(country)
  # Separate slope for each country: factor(country):instrument
  # Now, for the lags, you have a choice:
  #   a) "common" coefficients: + L1_d_rGDP + L2_d_rGDP + ...
  #   b) fully interacted: + factor(country):L1_d_rGDP + ...
  # For illustration, let's assume the lags are the *same* across countries 
  # (to avoid an explosion of parameters), i.e. we do not interact them with country.
  
  # If you DO want fully interacted lags, do: factor(country):L1_d_rGDP + ...
  # But let's do simpler:
  
  formula_str <- paste(
    "y_lead ~ -1 + factor(country) + factor(country):instrument +",
    paste(lag_cols, collapse = " + ")
  )
  # Example ends up like:
  # y_lead ~ -1 + factor(country) + factor(country):instrument + L1_d_rGDP + L2_d_rGDP + ...
  
  form <- as.formula(formula_str)
  # 4) Fit LSDV with plm, model="pooling"
  mod <- plm(
    formula = form,
    data = df_h,
    model = "pooling",      # we put the dummies in the formula ourselves
    index = c("country", "time")
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
  
} # end loop

# Combine into one big table
results_df <- dplyr::bind_rows(results_list, .id = "horizon_id")

