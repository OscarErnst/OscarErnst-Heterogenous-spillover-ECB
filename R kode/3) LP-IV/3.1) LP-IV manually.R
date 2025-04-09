#########################################################################
# Manual LP-IV Estimation with Lagged Controls and Robust SEs
#########################################################################
rm(list = ls())
cat("\014")

# Set working directory based on user
user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
} else if (user == "Oscar_dream") {
  setwd("HER_INDSÆT_STI_FOR_OSCAR_DREAM")
} else if (user == "Kasper") {
  setwd("HER_INDSÆT_STI_FOR_KASPER")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# --- Load required packages --------------------------------------------
library(dplyr)
library(sandwich)

# --- Load the Prepared Data --------------------------------------------
data <- readRDS(file.path("Data", "LP-IV", "input_data.rds"))

# --- Define Parameters --------------------------------------------------
H <- 6                      # Maximum horizon
p <- 8                      # Number of lags
controls <- c("d_HICP", "d_rGDP", "d_Consumption", "bund_yield")
instrument <- "target_q"
outcome_vars <- c("d_HICP", "d_rGDP", "d_Consumption", "bund_yield")

# --- Generate Lagged Control Variables ----------------------------------
for (ctrl in controls) {
  for (lag in 1:p) {
    lag_name <- paste0(ctrl, "_lag", lag)
    data[[lag_name]] <- dplyr::lag(data[[ctrl]], n = lag)
  }
}
lag_names <- unlist(lapply(controls, function(ctrl) paste0(ctrl, "_lag", 1:p)))

# --- LP-IV Estimation ---------------------------------------------------
LPIV_results <- list()

for (yvar in outcome_vars) {
  beta_h <- numeric(H)
  se_h   <- numeric(H)
  
  for (h in 0:(H - 1)) {
    data_temp <- data %>%
      mutate(y_lead = dplyr::lead(!!sym(yvar), n = h)) %>%
      filter(!is.na(y_lead))
    
    reg_formula <- as.formula(
      paste("y_lead ~", instrument, "+",
            paste(c(controls, lag_names), collapse = " + "))
    )
    
    reg_model <- lm(reg_formula, data = data_temp)
    nw_se <- sqrt(diag(NeweyWest(reg_model)))
    
    beta_h[h + 1] <- coef(reg_model)[instrument]
    se_h[h + 1]   <- nw_se[instrument]
  }
  
  LPIV_results[[yvar]] <- list(beta = beta_h, se = se_h)
}

#########################################################################
# Plotting Noncumulative LP-IV Impulse Responses (Robust SEs)
#########################################################################

# Opret mappe til grafer hvis den ikke eksisterer
output_dir <- file.path("Graphs", "LP-IV")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

png(filename = file.path(output_dir, "IRF.png"),
    width = 2000, height = 2400, res = 300)

# Plot layout
par(mfrow = c(4, 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

horizons <- 0:(H - 1)
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

for (i in 1:length(outcome_vars)) {
  beta_vec <- LPIV_results[[ outcome_vars[i] ]]$beta
  se_vec   <- LPIV_results[[ outcome_vars[i] ]]$se
  
  lower_vec <- beta_vec - 1.96 * se_vec
  upper_vec <- beta_vec + 1.96 * se_vec
  
  y_min <- min(lower_vec, beta_vec)
  y_max <- max(upper_vec, beta_vec)
  
  plot(horizons, beta_vec, type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon", ylab = "Impulse Response",
       main = paste("Impulse Response for", outcome_vars[i]))
  
  polygon(c(horizons, rev(horizons)),
          c(lower_vec, rev(upper_vec)),
          col = burgundy_trans, border = NA)
  
  lines(horizons, beta_vec, type = "b", col = burgundy, lwd = 2)
  abline(h = 0, lty = 2, col = "black")
}

dev.off()
