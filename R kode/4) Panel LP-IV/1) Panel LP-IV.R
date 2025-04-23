################################################################################
##  0.  House-keeping
################################################################################
rm(list = ls()); cat("\014")

user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else stop("Unknown user")

source("R kode/4) Panel LP-IV/Panel LP-IV functions.R")
library(dplyr)
library(fixest)

################################################################################
##  1.  Settings
################################################################################
horizon   <- 13          # LP horizons (0 … 7)
lags      <- 6          # lags of each control
countries <- c("DE","FR","NL","DK","AT","IT","ES","PT","EL")
baseline  <- "DE"
others    <- setdiff(countries, baseline)

country_names <- c(
  AT = "Austria", DE = "Germany",   FR = "France",
  NL = "Netherlands", DK = "Denmark", IT = "Italy",
  ES = "Spain", PT = "Portugal", EL = "Greece"
)

################################################################################
##  2.  Load data  +  shock×country interactions
################################################################################
data <- readRDS("Data/Panel LP-IV/panel_input_data.rds") %>%
  arrange(country, Date)

for (c in others) {
  data[[paste0("shock_", c)]] <- ifelse(data$country == c, data$shock, 0)
}



Panel_LP_IV_results_GDP  <- estimate_panel_lpiv(data, "d_rGDP",
                                                horizon, lags,
                                                baseline, others)

Panel_LP_IV_results_HICP <- estimate_panel_lpiv(data, "d_HICP",
                                                horizon, lags,
                                                baseline, others)

################################################################################
##  5.  3×3 IRF plotting helper
################################################################################


plot_irfs_panel(Panel_LP_IV_results_GDP$IRF_summary,
                file_name = "IRF_d_rGDP.png",
                var_lab   = "d_rGDP IRF")

plot_irfs_panel(Panel_LP_IV_results_HICP$IRF_summary,
                file_name = "IRF_d_HICP.png",
                var_lab   = "d_HICP IRF")


sensitivity_HICP <- lag_sensitivity_table(
  data        = data,
  outcome_var = "d_HICP",
  horizon     = horizon,
  max_p       = 8,
  baseline    = baseline,
  others      = others,
  nice_names  = country_names
)

sensitivity_GDP <- lag_sensitivity_table(
  data        = data,
  outcome_var = "d_rGDP",
  horizon     = horizon,
  max_p       = 8,
  baseline    = baseline,
  others      = others,
  nice_names  = country_names
)

print(sensitivity_HICP, row.names = FALSE)
print(sensitivity_GDP, row.names = FALSE)

