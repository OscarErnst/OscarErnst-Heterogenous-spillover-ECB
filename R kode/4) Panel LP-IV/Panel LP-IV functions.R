library(dplyr)
library(fixest)


estimate_panel_lpiv <- function(data, outcome_var,
                                horizon, lags,
                                baseline, others) {
  
  message("Estimating Panel LP-IV for: ", outcome_var)
  
  ## ----------- build all lags ONCE ----------------------------------------
  controls_to_lag <- c("d_rGDP", "d_HICP", "d_Consumption", "bund_yield")
  data_lagged <- data
  for (var in controls_to_lag) {
    for (L in 1:lags) {
      data_lagged <- data_lagged %>%
        group_by(country) %>%
        mutate(!!paste0("lag_", var, "_", L) := lag(get(var), L)) %>%
        ungroup()
    }
  }
  
  ## ----------- containers
  results_list     <- list()
  IRF_summary_list <- list()
  
  ## ----------- local projections over horizons
  for (h in 0:(horizon - 1)) {
    
    message("  Horizon: ", h)
    
    ## dep. var.  y_{i,t+h}
    data_h <- data_lagged %>%
      group_by(country) %>%
      arrange(Date) %>%
      mutate(y_dep = lead(get(outcome_var), h)) %>%
      ungroup()
    
    ## regression data
    reg_data <- data_h %>%
      select(country, Date, y_dep,
             shock, starts_with("shock_"),
             starts_with("lag_")) %>%
      na.omit()
    
    lag_controls <- names(reg_data)[grepl("^lag_", names(reg_data))]
    interactions <- paste0("shock_", others, collapse = " + ")
    
    fml <- paste0(
      "y_dep ~ shock + ", interactions, " + ",
      paste(lag_controls, collapse = " + "),
      " | country"
    )
    
    ## estimate with Driscoll–Kraay vcov
    reg <- feols(as.formula(fml),
                 data      = reg_data,
                 panel.id  = c("country", "Date"),
                 vcov      = "DK")
    
    results_list[[paste0("h", h)]] <- reg
    
    ## ----------- IRF summary (point, 95% CI)
    coefs    <- coef(reg)
    vcov_mat <- vcov(reg)        # already DK
    
    cntries   <- c(baseline, others)
    IRF_mean  <- IRF_lower <- IRF_upper <- setNames(numeric(length(cntries)), cntries)
    
    IRF_mean[baseline]  <- coefs["shock"]
    se_base             <- sqrt(vcov_mat["shock","shock"])
    IRF_lower[baseline] <- IRF_mean[baseline] - 1.96*se_base
    IRF_upper[baseline] <- IRF_mean[baseline] + 1.96*se_base
    
    for (c in others) {
      iv <- paste0("shock_", c)
      beta_c <- if (iv %in% names(coefs)) coefs[iv] else 0
      se_c   <- if (iv %in% rownames(vcov_mat)) sqrt(vcov_mat[iv, iv]) else 0
      IRF_mean[c]  <- IRF_mean[baseline] + beta_c
      tot_se       <- sqrt(se_base^2 + se_c^2)
      IRF_lower[c] <- IRF_mean[c] - 1.96*tot_se
      IRF_upper[c] <- IRF_mean[c] + 1.96*tot_se
    }
    
    IRF_summary_list[[paste0("h", h)]] <-
      list(IRF_mean = IRF_mean,
           IRF_lower = IRF_lower,
           IRF_upper = IRF_upper)
  }
  
  ## ----------- cumulative IRFs (rounded)
  IRF_means  <- do.call(rbind, lapply(IRF_summary_list, function(x) x$IRF_mean))
  IRF_lowers <- do.call(rbind, lapply(IRF_summary_list, function(x) x$IRF_lower))
  IRF_uppers <- do.call(rbind, lapply(IRF_summary_list, function(x) x$IRF_upper))
  
  cumulative_means  <- t(apply(IRF_means, 2, cumsum))
  cumulative_lowers <- t(apply(IRF_lowers, 2, cumsum))
  cumulative_uppers <- t(apply(IRF_uppers, 2, cumsum))
  
  cumulative_means  <- round(cumulative_means, 3)
  cumulative_lowers <- round(cumulative_lowers, 3)
  cumulative_uppers <- round(cumulative_uppers, 3)
  
  cumulative_mean_df <- as.data.frame(t(cumulative_means))
  cumulative_lower_df <- as.data.frame(t(cumulative_lowers))
  cumulative_upper_df <- as.data.frame(t(cumulative_uppers))
  
  cumulative_mean_df$horizon <- 0:(horizon-1)
  cumulative_lower_df$horizon <- 0:(horizon-1)
  cumulative_upper_df$horizon <- 0:(horizon-1)
  
  cumulative_mean_df <- cumulative_mean_df %>% relocate(horizon)
  cumulative_lower_df <- cumulative_lower_df %>% relocate(horizon)
  cumulative_upper_df <- cumulative_upper_df %>% relocate(horizon)
  
  ## ----------- per-horizon DK t-tests
  horizon_tests <- do.call(rbind, lapply(others, function(c) {
    do.call(rbind, lapply(0:(horizon-1), function(h) {
      reg  <- results_list[[paste0("h", h)]]
      iv   <- paste0("shock_", c)
      if (iv %in% names(coef(reg))) {
        se   <- sqrt(vcov(reg)[iv, iv])   # DK SE
        tval <- coef(reg)[iv] / se
        data.frame(country = c, horizon = h,
                   t_stat = tval,
                   p_value = 2*pnorm(-abs(tval)),
                   reject_05 = ifelse(2*pnorm(-abs(tval)) < 0.05, "Reject", "Accept"))
      }
    }))
  }))
  
  ## ----------- joint DK χ² tests (diag only)
  joint_tests <- do.call(rbind, lapply(others, function(c) {
    betas <- ses <- numeric(horizon)
    for (h in 0:(horizon-1)) {
      reg <- results_list[[paste0("h", h)]]
      iv  <- paste0("shock_", c)
      if (iv %in% names(coef(reg))) {
        betas[h+1] <- coef(reg)[iv]
        ses[h+1]   <- sqrt(vcov(reg)[iv, iv])
      }
    }
    keep <- ses > 0
    if (any(keep)) {
      chi2 <- sum((betas[keep]^2) / (ses[keep]^2))
      df   <- sum(keep)
      data.frame(country = c,
                 chi2_stat = chi2,
                 df = df,
                 p_value = pchisq(chi2, df, lower.tail = FALSE),
                 reject_05 = ifelse(pchisq(chi2, df, lower.tail = FALSE) < 0.05, "Reject", "Accept"))
    }
  }))
  
  list(IRF_summary = IRF_summary_list,
       cumulative_mean_irf = cumulative_mean_df,
       cumulative_lower_irf = cumulative_lower_df,
       cumulative_upper_irf = cumulative_upper_df,
       per_horizon = horizon_tests,
       joint_tests = joint_tests)
}



# ─────────────────────────────────────────────────────────────────────────────
# Helper: ISO → Pretty Country Name
# ─────────────────────────────────────────────────────────────────────────────
#'
#' Convert ISO Country Code to Human‑Readable Name
#'
#' @description
#' Takes a 2‑letter ISO country code and looks it up in a named vector
#' `country_names` that must exist in the calling environment. If the code is
#' found, the pretty name is returned; otherwise the original ISO code is
#' returned unchanged.
#'
#' @param x Character scalar, ISO‑2 country code.
#' @return Character scalar with pretty country name (or original code).
#'
#' @examples
#' country_names <- c(DE = "Germany", FR = "France")
#' nice_name("DE")  # → "Germany"
#' nice_name("IT")  # → "IT" (not found)
# -----------------------------------------------------------------------------
nice_name <- function(x) ifelse(x %in% names(country_names), country_names[[x]], x)

# ─────────────────────────────────────────────────────────────────────────────
# Plot IRFs (3×3 grid) and save to PNG
# ─────────────────────────────────────────────────────────────────────────────
#'
#' Plot Panel IRFs and Save as PNG
#'
#' @description
#' Creates a 3 × 3 grid of impulse‑response plots (one panel per country) using
#' the IRF summary list returned by `estimate_panel_lpiv()`. Confidence bands
#' are shaded; the file is written to *Graphs/Panel LP‑IV/* inside the current
#' working directory.
#'
#' @param irf_summary_list The `IRF_summary` component from `estimate_panel_lpiv`.
#' @param file_name File name of the PNG (default "IRF_plot.png").
#' @param var_lab  Y‑axis label (default "IRF").
#' @param width_px,width_px,res_dpi Graphic device size and resolution.
#'
#' @return Invisibly returns the output path; primarily called for its side‑effect
#'         of saving the figure.
#' @examples
#' plot_irfs_panel(results$IRF_summary, file_name = "GDP_IRF.png", var_lab = "d_rGDP")
# -----------------------------------------------------------------------------
plot_irfs_panel <- function(irf_summary_list,
                            file_name   = "IRF_plot.png",
                            var_lab     = "IRF",
                            width_px    = 2800,
                            height_px   = 2800,
                            res_dpi     = 300) {
  
  out_dir  <- file.path(getwd(), "Graphs", "Panel LP-IV")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, file_name)
  
  png(out_path, width = width_px, height = height_px, res = res_dpi)
  on.exit(dev.off(), add = TRUE)
  
  par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
  
  iso_vec <- names(irf_summary_list[["h0"]]$IRF_mean)
  horiz   <- seq_along(irf_summary_list) - 1
  
  for (iso in iso_vec) {
    means  <- sapply(irf_summary_list, function(x) x$IRF_mean [iso])
    lowers <- sapply(irf_summary_list, function(x) x$IRF_lower[iso])
    uppers <- sapply(irf_summary_list, function(x) x$IRF_upper[iso])
    
    ylim <- range(c(lowers, uppers), na.rm = TRUE)
    
    plot(horiz, means, type = "n", ylim = ylim,
         main = nice_name(iso), xlab = "Horizon", ylab = var_lab)
    polygon(c(horiz, rev(horiz)), c(lowers, rev(uppers)),
            col = rgb(0.7, 0, 0, 0.25), border = NA)
    lines(horiz, means, col = "#760020", lwd = 2, type = "b", pch = 16)
    abline(h = 0, lty = 2); grid()
  }
  
  message("Saved → ", out_path)
  invisible(out_path)
}

# ─────────────────────────────────────────────────────────────────────────────
# Lag‑Robustness Table helper
# ─────────────────────────────────────────────────────────────────────────────
#'
#' Produce Lag‑Sensitivity Table of Joint χ² Tests
#'
#' @description
#' Re‑estimates the panel LP‑IV model for p = 1,…,`max_p` lags, extracts the
#' country‑level joint χ² test statistic, and formats each cell as
#' "statistic (Accept/Reject)". Returns a wide data frame convenient for
#' inclusion in papers or LaTeX tables.
#'
#' @param data Panel data used for estimation.
#' @param outcome_var Dependent variable name (string).
#' @param horizon  Number of projection horizons.
#' @param max_p    Maximum lag length to iterate to.
#' @param baseline Baseline country ISO code.
#' @param others   Vector of other country codes.
#' @param nice_names Optional named vector for pretty country names.
#'
#' @return A wide data.frame: rows = countries, columns labelled "p = 1" … "p = max_p",
#'         each cell like "112.49 (Reject)" or "15.28 (Accept)".
#' @examples
#' lag_tbl <- lag_sensitivity_table(data, "d_HICP", 13, 8, "DE", others, country_names)
# -----------------------------------------------------------------------------
lag_sensitivity_table <- function(data,
                                  outcome_var = "d_HICP",
                                  horizon     = 13,       # df til χ²-testen
                                  max_p       = 6,
                                  baseline    = "DE",
                                  others      = setdiff(unique(data$country), baseline),
                                  nice_names  = NULL) {
  
  library(dplyr)
  library(tidyr)
  
  results <- vector("list", max_p)
  
  for (p in seq_len(max_p)) {
    
    res <- estimate_panel_lpiv(data, outcome_var, horizon, p, baseline, others)
    
    jt <- res$joint_tests %>%
      mutate(
        # p-værdi fra χ²(df = horizon)
        p_value = pchisq(chi2_stat, df = horizon, lower.tail = FALSE),
        
        # stjerne­kode
        stars = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.010 ~ "**",
          p_value < 0.050 ~ "*",
          TRUE            ~ ""
        ),
        
        lag_p   = p,
        display = sprintf("%.2f%s", chi2_stat, stars)
      )
    
    results[[p]] <- jt[, c("country", "lag_p", "display")]
  }
  
  out_tbl <- bind_rows(results) %>%
    pivot_wider(names_from  = lag_p,
                values_from = display,
                names_glue  = "p = {lag_p}") %>%
    arrange(match(country, c(baseline, others)))
  
  # pæne landenavne hvis ønsket
  if (!is.null(nice_names))
    out_tbl$country <- dplyr::recode(out_tbl$country, !!!nice_names)
  
  out_tbl
}

# ────────────────────────────────────────────────────────────────
#  Horizon-sensitivity helper
#  Produces a table like
#  Country   H = 4   H = 5   …   H = 12
#              8.13  12.77*  …   46.15***
# ────────────────────────────────────────────────────────────────
# ── horizon-sensitivity helper  ──────────────────────────────────────────────
horizon_sensitivity_table <- function(data,
                                      outcome_var = "d_HICP",
                                      min_H       = 4,
                                      max_H       = 12,
                                      p_lags      = 4,         # baseline lag length
                                      baseline    = "DE",
                                      others      = setdiff(unique(data$country), baseline),
                                      nice_names  = NULL) {
  
  require(dplyr)
  require(tidyr)
  
  store <- vector("list", max_H - min_H + 1)
  idx   <- 1
  
  for (H in min_H:max_H) {
    ## correctly name the arguments:
    res <- estimate_panel_lpiv(
      data        = data,
      outcome_var = outcome_var,
      horizon     = H,
      lags        = p_lags,
      baseline    = baseline,
      others      = others
    )
    
    jt <- res$joint_tests %>%
      mutate(
        H        = H,
        display  = sprintf(
          "%.2f%s",
          chi2_stat,
          ifelse(reject_05 == "Reject", "***", "")
        )
      ) %>%
      select(country, H, display)
    
    store[[idx]] <- jt
    idx <- idx + 1
  }
  
  out_tbl <- bind_rows(store) %>%
    pivot_wider(
      names_from  = H,
      values_from = display,
      names_prefix = "H = "
    ) %>%
    arrange(match(country, c(baseline, others)))
  
  if (!is.null(nice_names)) {
    out_tbl$country <- recode(out_tbl$country, !!!nice_names)
  }
  
  return(out_tbl)
}
