rm(list = ls())
cat("\014")

# Load required libraries
library(plm)       # for panel data estimation
library(lmtest)    # for coeftest function (to get coefficients with custom SEs)
library(dplyr)     # for data manipulation (lead/lag and binding results)

# Assume `data` is a data frame containing the panel:
# columns including "country", "time", outcome variable "d_rGDP", and the shock (instrument).
# Ensure the instrument is aligned by time for all countries:
# (If the shock is in a separate time-series data frame `shock_series` with columns time and shock, merge it)
# data <- merge(data, shock_series, by = "time")
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

data <- left_join(df_panel, df_instrument, by = "time")



# Create lead outcomes for horizons 0 to 8 and lag controls (lags 1 to 4 of outcome)
df <- data %>%
  arrange(country, time) %>%     # sort by panel and time
  group_by(country) %>%          # group by country for lead/lag
  mutate(
    lead0 = d_rGDP,                                 # h=0 (current outcome)
    lead1 = dplyr::lead(d_rGDP, 1),
    lead2 = dplyr::lead(d_rGDP, 2),
    lead3 = dplyr::lead(d_rGDP, 3),
    lead4 = dplyr::lead(d_rGDP, 4),
    lead5 = dplyr::lead(d_rGDP, 5),
    lead6 = dplyr::lead(d_rGDP, 6),
    lead7 = dplyr::lead(d_rGDP, 7),
    lead8 = dplyr::lead(d_rGDP, 8),
    lag1  = dplyr::lag(d_rGDP, 1),
    lag2  = dplyr::lag(d_rGDP, 2),
    lag3  = dplyr::lag(d_rGDP, 3),
    lag4  = dplyr::lag(d_rGDP, 4)
  ) %>%
  ungroup()

# Convert country to factor and set EA20 as the baseline level for dummies
df$country <- relevel(factor(df$country), ref = "EA20")

# Initialize a list to store results for each horizon
results_list <- list()

# Loop over horizons h = 0 to 8
for(h in 0:8) {
  # Define the dependent variable for this horizon (lead h of d_rGDP)
  y_var <- paste0("lead", h)
  
  # Filter to keep observations with complete data for this horizon:
  # (drop rows with NA in dependent, instrument, or any lag control)
  df_h <- df[!is.na(df[[y_var]]) & !is.na(df$instrument) & 
               !is.na(df$lag1) & !is.na(df$lag2) & !is.na(df$lag3) & !is.na(df$lag4), ]
  
  # Specify the model formula: outcome ~ shock * country dummies + 4 lags of outcome
  formula_h <- as.formula(paste(y_var, "~ instrument * country + lag1 + lag2 + lag3 + lag4"))
  
  # Estimate pooled OLS (with country dummies) for this horizon
  model_h <- plm(formula_h, data = df_h, index = c("country", "time"), model = "pooling")
  
  # Compute Driscoll-Kraay (cross-section and time robust) standard errors
  dk_se <- vcovSCC(model_h, type = "HC0", cluster = "time")
  model_coefs <- coeftest(model_h, vcov. = dk_se)
  
  # Tidy the coefficient results into a data frame
  coef_df <- data.frame(
    term      = rownames(model_coefs),
    estimate  = model_coefs[, "Estimate"],
    std.error = model_coefs[, "Std. Error"],
    t.value   = model_coefs[, "t value"],
    p.value   = model_coefs[, "Pr(>|t|)"],
    horizon   = h,
    nobs      = nobs(model_h)         # number of observations used in this regression
  )
  
  results_list[[h+1]] <- coef_df    # store results (h+1 index since h starts at 0)
}

# Combine results from all horizons into one data frame
results_df <- bind_rows(results_list)

# View the first few rows of the results
head(results_df)
