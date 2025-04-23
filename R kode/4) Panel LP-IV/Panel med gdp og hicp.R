library(lpirfs)
library(dplyr)

## Settings
horizon   <- 8
lags      <- 2
countries <- c("DE","FR","NL","DK","AT","IT","ES","PT","EL")
baseline  <- "DE"
others    <- setdiff(countries, baseline)

## Load data + build country×shock interactions
data <- readRDS("Data/Panel LP-IV/panel_input_data.rds") %>%
  arrange(country, Date)
for(c in others) {
  data[[paste0("shock_", c)]] <- ifelse(data$country==c, data$shock, 0)
}
interaction_vars <- paste0("shock_", others)

## Helper to run LP‑IV and return the t‑matrix
run_panel_tMat <- function(outcome) {
  lp <- lp_lin_panel(
    data_set       = data,
    endog_data     = outcome,
    cumul_mult     = FALSE,
    shock          = "shock",
    panel_model    = "pooling",
    panel_effect   = "time",
    robust_cov     = "vcovSCC",
    c_exog_data    = interaction_vars,
    l_exog_data    = outcome,
    lags_exog_data = lags,
    hor            = horizon,
    confint        = 1.96
  )
  coefmat <- function(obj) if(is.matrix(obj)) obj else obj$coefficients
  B <- S <- matrix(NA, nrow=length(others), ncol=horizon,
                   dimnames=list(others, paste0("h",0:(horizon-1))))
  for(h in 1:horizon) {
    mat <- coefmat(lp$reg_summaries[[h]])
    for(c in others) {
      rn <- paste0("shock_", c)
      if(rn %in% rownames(mat)) {
        B[c,h] <- mat[rn,1]
        S[c,h] <- mat[rn,2]
      }
    }
  }
  tMat <- B / S
  return(tMat)
}

## Loop over outcomes and print country‑level Wald tests
for(var in c("d_rGDP","d_HICP")) {
  tMat <- run_panel_tMat(var)
  
  # Overall test
  t_all       <- as.vector(tMat); keep      <- is.finite(t_all)
  chi2_overall<- sum(t_all[keep]^2)
  df_overall  <- sum(keep)
  p_overall   <- pchisq(chi2_overall, df_overall, lower.tail=FALSE)
  cat("\n=== Outcome:", var, "===\n")
  cat(sprintf("Overall Wald test: χ²(%d)=%.2f, p=%.6f\n",
              df_overall, chi2_overall, p_overall))
  
  # Country‑level Wald tests
  country_tests <- data.frame(
    country   = others,
    chi2      = NA_real_,
    df        = NA_integer_,
    p         = NA_real_,
    crit_05   = NA_real_,
    reject_05 = NA
  )
  for(c in others) {
    t_vec <- tMat[c,]; keep <- is.finite(t_vec)
    if(any(keep)) {
      chi2_val <- sum(t_vec[keep]^2)
      df_val   <- sum(keep)
      p_val    <- pchisq(chi2_val, df_val, lower.tail=FALSE)
      country_tests[country_tests$country==c, c("chi2","df","p","crit_05","reject_05")] <-
        list(chi2_val, df_val, p_val, qchisq(0.95, df_val), p_val<0.05)
    }
  }
  country_tests$p <- sprintf("%.6f", country_tests$p)
  cat("\nCountry‑level Wald tests (vs Germany, all horizons):\n")
  print(country_tests, row.names=FALSE)
}

