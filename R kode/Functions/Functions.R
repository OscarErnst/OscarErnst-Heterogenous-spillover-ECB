# Clear environment and console
rm(list = ls())
cat("\014")

# Set working directory

# Load required functions and packages
source("R kode/Functions/Load_Packages.R")
library(dplyr)
library(lubridate)
library(purrr)
library(tempdisagg)
library(eurostat)

# ----- Functions for Data Construction -----

# 1) HICP (monthly data) -> group monthly to quarterly
get_HICP <- function(geo) {
  message("Loading HICP for country: ", geo)
  
  HICP_data <- suppressMessages(
    get_eurostat(
      "prc_hicp_midx",
      time_format = "date",
      filters = list(
        geo    = geo,
        coicop = "CP00",
        unit   = "I15"
      )
    )
  )
  
  HICP_data <- HICP_data %>%
    mutate(
      year    = year(time),
      quarter = quarter(time)
    ) %>%
    group_by(year, quarter) %>%
    summarise(
      HICP = mean(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    dplyr::select(year, quarter, year_quarter, HICP)
  
  return(HICP_data)
}

# 2) Real GDP (quarterly data)
get_real_GDP <- function(geo) {
  message("Loading Real GDP for country: ", geo)
  
  rGDP_data <- suppressMessages(
    get_eurostat(
      "naidq_10_gdp",
      time_format = "date",
      filters = list(
        geo    = geo,
        na_item= "B1GQ",
        s_adj  = "SCA",
        unit   = "CLV_I10"
      )
    )
  )
  
  rGDP_data <- rGDP_data %>%
    mutate(
      year    = year(time),
      quarter = quarter(time)
    ) %>%
    group_by(year, quarter) %>%
    summarise(
      rGDP = mean(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    dplyr::select(year, quarter, year_quarter, rGDP)
  
  return(rGDP_data)
}

# 3) Final Consumption by Households (quarterly data)
get_final_consumption <- function(geo) {
  message("Loading Final Consumption by Households for country: ", geo)
  
  consumption_data <- suppressMessages(
    get_eurostat(
      "namq_10_fcs",
      time_format = "num",  # numeric quarters like 2000.0, 2000.25, etc.
      filters = list(
        geo    = geo,
        na_item= "P31_S14",
        s_adj  = "SCA",
        unit   = "CLV10_MEUR"
      )
    )
  )
  
  consumption_data <- consumption_data %>%
    mutate(
      year = floor(time),
      quarter = as.integer(1 + (time - floor(time)) * 4)
    ) %>%
    group_by(year, quarter) %>%
    summarise(
      Consumption = mean(values, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    dplyr::select(year, quarter, year_quarter, Consumption)
  
  return(consumption_data)
}

# 4) Merge all three variables into one data frame
get_country_dataset <- function(geo) {
  # 1) Get HICP
  hicp <- get_HICP(geo)
  
  # 2) Get Real GDP
  rgdp <- get_real_GDP(geo)
  
  # 3) Get Household Consumption 
  consumption <- get_final_consumption(geo)
  
  message("Merging HICP, GDP, and Final Consumption for country: ", geo)
  
  df_merged <- list(hicp, rgdp, consumption) %>%
    reduce(left_join, by = c("year", "quarter", "year_quarter"))
  
  df_merged <- df_merged %>%
    mutate(country = geo)
  
  return(df_merged)
}

# Function to calculate YoY log changes (seasonal effects removed)
calc_log_yoy_change <- function(df, vars, suffix = "_yoy_log") {
  df <- df %>%
    group_by(country) %>%
    arrange(year, quarter, .by_group = TRUE)
  
  lag_n <- 4  # 4-quarter lag for YoY change
  
  for (v in vars) {
    new_col <- paste0(v, suffix)
    df <- df %>%
      mutate(!!new_col := 100 * (log(.data[[v]]) - log(lag(.data[[v]], lag_n))))
  }
  
  df <- ungroup(df)
  return(df)
}

# Interpolation: Convert a quarterly time series to monthly using Chow-Lin maximization
interpolate_to_monthly <- function(quarterly_ts, monthly_list, method = "chow-lin-maxlog") {
  rhs_vars <- paste(names(monthly_list), collapse = " + ")
  formula_str <- paste0("quarterly_ts ~ ", rhs_vars)
  interp_formula <- as.formula(formula_str)
  
  # Make monthly indicators available in the current environment
  list2env(monthly_list, envir = environment())
  
  model_td <- td(interp_formula, to = 12, method = method)
  monthly_level <- model_td$values
  
  # Calculate monthly log-changes using log differences for a true percent change
  monthly_logchg <- c(NA, diff(log(monthly_level)) * 100)
  
  return(list(
    monthly_level  = monthly_level,
    monthly_logchg = monthly_logchg
  ))
}

