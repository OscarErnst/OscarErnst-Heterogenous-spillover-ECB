########################################################################
##  PANEL LP‑IV   –   HETEROGENEITY TESTS (Germany baseline)
########################################################################
rm(list = ls())
cat("\014")

library(lpirfs)
library(dplyr)

## --------------------------------------------------------------- 
## 0. SETTINGS 
## --------------------------------------------------------------- 
horizon   <- 8
lags      <- 2

countries <- c("DE", "FR", "NL", "DK", "AT",    # core
               "IT", "ES", "PT", "EL")    # periphery  (EL = Greece)
baseline  <- "DE"
others    <- setdiff(countries, baseline)

## --------------------------------------------------------------- 
## 1. LOAD PANEL  +  INTERACTIONS
## --------------------------------------------------------------- 
user <- Sys.info()[["user"]]
if      (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else {
  stop("Unknown user – adjust path manually")
}

data <- readRDS("Data/Panel LP-IV/panel_input_data.rds") |>
  arrange(country, Date)

## interaction: shock × 1{country = c}   for every c ≠ baseline
for (c in others)
  data[[paste0("shock_", c)]] <- ifelse(data$country == c, data$shock, 0)

interaction_vars <- paste0("shock_", others)

## --------------------------------------------------------------- 
## 2. ESTIMATE LP‑IV  (dependent = d_rGDP)
## --------------------------------------------------------------- 
lp <- lp_lin_panel(
  data_set       = data,
  endog_data     = "d_rGDP",
  cumul_mult     = FALSE,
  shock          = "shock",                # baseline response (Germany)
  panel_model    = "pooling",
  panel_effect   = "time",
  robust_cov     = "vcovSCC",              # Driscoll–Kraay SEs
  c_exog_data    = interaction_vars,       # deviations from baseline
  l_exog_data    = "d_rGDP",
  lags_exog_data = lags,
  hor            = horizon,
  confint        = 1.96
)

coefmat <- function(obj) if (is.matrix(obj)) obj else obj$coefficients

## --------------------------------------------------------------- 
## 3.  COLLECT β and SE  →  t‑matrix   (rows = countries, cols = horizons)
## --------------------------------------------------------------- 
B <- S <- matrix(NA_real_, nrow = length(others), ncol = horizon,
                 dimnames = list(others, paste0("h", 0:(horizon-1))))

for (h in 1:horizon) {
  mat <- coefmat(lp$reg_summaries[[h]])
  for (c in others) {
    row <- paste0("shock_", c)
    if (row %in% rownames(mat)) {
      B[c, h] <- mat[row, 1]
      S[c, h] <- mat[row, 2]
    }
  }
}

tMat <- B / S                      # t‑statistics for H0: β_{c,h}=0  (vs DE)

## --------------------------------------------------------------- 
## 4‑A. OVERALL heterogeneity  (all horizons, all countries ≠ DE)
## --------------------------------------------------------------- 
t_all <- as.vector(tMat)
keep  <- is.finite(t_all)
chi2_overall <- sum(t_all[keep]^2)
df_overall   <- sum(keep)
p_overall    <- pchisq(chi2_overall, df_overall, lower.tail = FALSE)

## --------------------------------------------------------------- 
## 4‑B. COUNTRY‑level Wald tests  (all horizons for each c ≠ DE)
## --------------------------------------------------------------- 
country_tests <- data.frame(
  country   = others,
  chi2      = NA_real_,
  df        = NA_integer_,
  p         = NA_real_,
  crit_05   = NA_real_,
  reject_05 = FALSE
)

for (c in others) {
  t_vec <- tMat[c, ]
  keep  <- is.finite(t_vec)
  if (any(keep)) {
    chi2_val <- sum(t_vec[keep]^2)
    df_val   <- sum(keep)
    p_val    <- pchisq(chi2_val, df_val, lower.tail = FALSE)
    country_tests[country_tests$country == c,
                  c("chi2","df","p","crit_05","reject_05")] <-
      list(chi2_val, df_val, p_val, qchisq(0.95, df_val), p_val < 0.05)
  }
}

## --------------------------------------------------------------- 
## 4‑C.  Horizon‑specific t‑tests (df = 1)  for completeness
## --------------------------------------------------------------- 
horizon_tests <- expand.grid(country = others,
                             horizon = 0:(horizon-1),
                             t = NA_real_, p = NA_real_, reject_05 = FALSE)

for (c in others) for (h in 1:horizon) {
  t_val <- tMat[c, h]
  if (is.finite(t_val)) {
    p_val <- 2 * pnorm(-abs(t_val))
    horizon_tests[horizon_tests$country == c & horizon_tests$horizon == h-1,
                  c("t","p","reject_05")] <- list(t_val, p_val, p_val < 0.05)
  }
}

## --------------------------------------------------------------- 
## 5.  PRINT RESULTS
## --------------------------------------------------------------- 
fmt  <- function(x) sprintf("%.6f", x)

cat("\n================  OVERALL heterogeneity ====================\n")
cat("Chi² =", round(chi2_overall, 2),
    " df =", df_overall,
    " p =", fmt(p_overall),
    ifelse(p_overall < 0.05, " ==> REJECT\n", " ==> fail to reject\n"))

cat("\n==========  COUNTRY tests (vs Germany, all horizons)  =======\n")
country_tests$p <- fmt(country_tests$p)
print(country_tests[, c("country","chi2","df","crit_05","p","reject_05")],
      row.names = FALSE, right = FALSE)

cat("\nFirst few rows of horizon‑specific tests\n")
horizon_tests$p <- fmt(horizon_tests$p)
print(head(horizon_tests, 12), row.names = FALSE, right = FALSE)
