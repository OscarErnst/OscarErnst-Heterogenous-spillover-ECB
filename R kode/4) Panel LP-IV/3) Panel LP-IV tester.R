# Panel LP-IV with Country Interactions (starting with Denmark)

# ─────────────────────────────────────────────────────────────
# 1. Load and prepare data
# ─────────────────────────────────────────────────────────────
library(dplyr)
library(plm)
library(broom)
library(car)

# Load data
setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
data <- readRDS("Data/Panel LP-IV/panel_input_data.rds")

# Create Denmark dummy and interaction with the shock
data$dum_DK <- ifelse(data$country == "DK", 1, 0)
data$shock_interact_DK <- data$shock * data$dum_DK

# Drop rows with missing values (due to lags)
data_clean <- na.omit(data)

# Define the formula for the panel regression
formula <- d_rGDP ~ shock + shock_interact_DK +
  d_HICP + bund_yield +
  d_rGDP_lag_1 + d_rGDP_lag_2 + d_rGDP_lag_3 + d_rGDP_lag_4 +
  d_HICP_lag_1 + d_HICP_lag_2 + d_HICP_lag_3 + d_HICP_lag_4 +
  bund_yield_lag_1 + bund_yield_lag_2 + bund_yield_lag_3 + bund_yield_lag_4

# Convert to panel data structure
panel_data <- pdata.frame(data_clean, index = c("country", "Date"))

# Run fixed effects panel regression
model <- plm(formula, data = panel_data, model = "within", effect = "individual")

# Compute Driscoll-Kraay robust standard errors
library(sandwich)
library(lmtest)
coefs <- coeftest(model, vcov = vcovSCC(model))

# Extract and format result for the interaction term
results_df <- tidy(coefs) %>%
  filter(term == "shock_interact_DK") %>%
  mutate(
    Country = "Denmark",
    `t-stat` = estimate / std.error,
    `p-value` = 2 * (1 - pnorm(abs(`t-stat`))),
    Significance = case_when(
      `p-value` < 0.001 ~ "***",
      `p-value` < 0.01  ~ "**",
      `p-value` < 0.05  ~ "*",
      `p-value` < 0.1   ~ ".",
      TRUE              ~ ""
    )
  ) %>%
  select(Country, Estimate = estimate, `Std. Error` = std.error, `t-stat`, `p-value`, Significance)

# Print the results
print(results_df)

