################################################################################
##  0.  House-keeping
################################################################################
rm(list = ls()); cat("\014")

user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

source("R kode/4) Panel LP-IV/Panel LP-IV functions.R")
library(dplyr)
library(fixest)

################################################################################
##  1.  Settings
################################################################################
horizon   <- 13          # LP horizons (0 … 7)
lags      <- 4          # lags of each control
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

# Example call
h_sens_HICP <- horizon_sensitivity_table(
  data        = data,
  outcome_var = "d_HICP",
  min_H       = 5,
  max_H       = 13,
  p_lags      = 6,          # same lag length as baseline
  baseline    = baseline,
  others      = others,
  nice_names  = country_names
)
print(h_sens_HICP, row.names = FALSE)


h_sens_GDP <- horizon_sensitivity_table(
  data        = data,
  outcome_var = "d_rGDP",
  min_H       = 5,
  max_H       = 13,
  p_lags      = 6,
  baseline    = baseline,
  others      = others,
  nice_names  = country_names
)


results <- estimate_panel_lpiv(data, outcome_var = "d_HICP", horizon = 12, lags = 4, baseline = "DE", others = c("FR", "NL", "IT"))

# Hent den kumulative tabel
results$cumulative_irf


plot_irfs_3x2 <- function(irf_list_gdp, irf_list_hicp,
                          countries = c("DE","IT","EL"),
                          country_names,
                          file_name = "IRF_GDP_HICP_3x2.png",
                          width_px   = 2200,   # slightly wider
                          height_px  = 2400,
                          res_dpi    = 300) {
  
  out_dir  <- file.path(getwd(), "Graphs", "Panel LP-IV")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, file_name)
  
  png(out_path, width = width_px, height = height_px, res = res_dpi)
  on.exit(dev.off(), add = TRUE)
  
  # 3 rows x 2 cols, larger bottom & left margins
  par(mfrow = c(3, 2),
      mar   = c(5, 6, 2, 1),    # bottom=5, left=6, top=2, right=1
      oma   = c(0, 0, 0, 0))    # no outer margins
  
  horiz <- seq_along(irf_list_gdp) - 1
  irf_lists <- list("GDP"  = irf_list_gdp,
                    "HICP" = irf_list_hicp)
  
  for (i in seq_along(countries)) {
    iso       <- countries[i]
    is_bottom <- (i == length(countries))
    
    for (var_name in names(irf_lists)) {
      irf_list <- irf_lists[[var_name]]
      
      means  <- sapply(irf_list, function(x) x$IRF_mean[iso])
      lowers <- sapply(irf_list, function(x) x$IRF_lower[iso])
      uppers <- sapply(irf_list, function(x) x$IRF_upper[iso])
      ylim   <- range(c(lowers, uppers), na.rm = TRUE)
      
      xaxt_arg <- if (is_bottom) "s" else "n"
      xlab_arg <- if (is_bottom) "Horizon" else ""
      
      if (var_name == "GDP") {
        ylab_arg <- "%-change"
        cex_lab  <- 1.5
      } else {
        ylab_arg <- ""
        cex_lab  <- 1.2
      }
      
      plot(horiz, means, type = "n", ylim = ylim,
           xaxt    = xaxt_arg,
           main    = paste(country_names[[iso]], "–", var_name),
           xlab    = xlab_arg,
           ylab    = ylab_arg,
           cex.lab = cex_lab,
           cex.axis= 1.2)
      
      polygon(c(horiz, rev(horiz)), c(lowers, rev(uppers)),
              col = rgb(118/255, 0, 32/255, 0.25), border = NA)
      lines(horiz, means, col = "#760020", lwd = 2, type = "b", pch = 16)
      
      if (is_bottom) axis(1, cex.axis = 1.3)
      abline(h = 0, lty = 2)
      grid()
    }
  }
  
  message("Saved 3×2 IRF plot → ", out_path)
  invisible(out_path)
}




# Call it:
plot_irfs_3x2(
  irf_list_gdp  = Panel_LP_IV_results_GDP$IRF_summary,
  irf_list_hicp = Panel_LP_IV_results_HICP$IRF_summary,
  countries     = c("DE","NL","EL"),
  country_names = country_names,
  file_name     = "IRF_3x2_DE_DK_EL.png"
)
