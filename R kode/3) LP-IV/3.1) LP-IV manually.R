#########################################################################
# Manual LP-IV Estimation with Lagged Controls and Robust SEs
#########################################################################

# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on user
user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "Kasper") {
  setwd("HER_INDSÆT_STI_FOR_KASPER")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# --- Libraries ---
library(dplyr)
library(sandwich)

# --- Load data and instrument ---
data <- readRDS(file.path("Data", "LP-IV", "input_data.rds"))
bund_hat <- readRDS(file.path("Data", "LP-IV", "instrument.rds"))

# --- Settings ---
outcome_vars <- c("d_rGDP", "d_HICP", "d_Consumption", "bund_yield")
controls <- c("d_rGDP", "d_HICP", "d_Consumption", "bund_yield")
shock_var <- "bund_yield"
p <- 7     # number of lags
H <- 12    # horizon
shocksize <- 1  # 1 pp increase

# --- Lag Function ---
# --- Lag Function (Keep as is) ---
lagmatrix <- function(X, lag){
  X <- as.matrix(X)
  # embed(...) creates (lag+1)*ncol(X) columns: t, t-1, ..., t-lag
  # [, -(1:ncol(X))] removes the first ncol(X) columns (time t)
  # Result has lag * ncol(X) columns (t-1, ..., t-lag)
  # Order is: V1_lag1, V2_lag1,... Vk_lag1, V1_lag2, ... Vk_lag2, ... V1_lagp, ... Vk_lagp
  embedded_matrix <- embed(rbind(matrix(NA, lag, ncol(X)), X), lag + 1)
  # Drop contemporaneous values (first ncol(X) columns)
  lagged_values <- embedded_matrix[, -(1:ncol(X)), drop = FALSE]
  # Ensure output has correct number of rows matching original X length minus lag
  # embed output has nrow(X) rows if you add NA rows first
  return(lagged_values[1:(nrow(X)), , drop = FALSE])
}

# --- Storage for IRFs ---
IRF <- matrix(NA, H + 1, length(outcome_vars))
IRF_low <- matrix(NA, H + 1, length(outcome_vars))
IRF_high <- matrix(NA, H + 1, length(outcome_vars))

# --- Estimation Loop ---
for (i in seq_along(outcome_vars)) {
  
  y <- data[[outcome_vars[i]]]
  X_ctrl <- data %>% select(all_of(controls))
  num_controls <- ncol(X_ctrl)
  n_obs_orig <- nrow(data) # Or length(y)
  
  # --- CORRECTED: Create Lagged Controls ---
  if (p > 0) {
    # Use the lagmatrix function once to get all lags from 1 to p
    X_lags_raw <- lagmatrix(X_ctrl, p) # Creates p * num_controls columns
    
    # Generate correct column names
    # Order matches lagmatrix output: Var1L1, Var2L1...VarKL1, Var1L2,...
    col_names <- paste0(rep(controls, times = p), "_lag", rep(1:p, each = num_controls))
    colnames(X_lags_raw) <- col_names
    
    # IMPORTANT: Align data. lagmatrix output implicitly drops the first 'p' observations
    # We need to use data starting from row p+1
    start_row <- p + 1
    end_row <- n_obs_orig
    
    # Select the aligned parts of y, shock, and the created lags
    y_aligned <- y[start_row:end_row]
    shock_aligned <- bund_hat[start_row:end_row] # Assumes bund_hat aligns with data
    X_lags_aligned <- X_lags_raw[start_row:end_row, , drop = FALSE]
    
  } else { # Handle case p = 0 (no lags)
    start_row <- 1
    end_row <- n_obs_orig
    y_aligned <- y
    shock_aligned <- bund_hat
    X_lags_aligned <- matrix(nrow = length(y_aligned), ncol = 0) # Empty matrix placeholder
  }
  
  # --- Horizon Loop (Now uses aligned data) ---
  n_obs_aligned <- length(y_aligned)
  
  for (h in 0:H) {
    # Calculate the final number of observations for this horizon
    # We need y_{t+h}, shock_t, lags_t. Requires data up to T-h.
    end_reg_row <- n_obs_aligned - h
    if (end_reg_row < 1) { # Check if horizon is too long
      warning(paste("Skipping h=", h, "for var=", outcome_vars[i], "- horizon exceeds available data after lagging."))
      IRF[h + 1, i] <- NA
      IRF_low[h + 1, i] <- NA
      IRF_high[h + 1, i] <- NA
      next
    }
    
    # Select y_{t+h} -> This corresponds to y_aligned from (h+1) to n_obs_aligned
    y_lead_h <- y_aligned[(h + 1):n_obs_aligned]
    
    # Select regressors at time t -> correspond to rows 1 to (n_obs_aligned - h)
    shock_reg <- shock_aligned[1:end_reg_row]
    X_lags_reg <- X_lags_aligned[1:end_reg_row, , drop = FALSE]
    
    # Combine for regression
    regression_data <- data.frame(
      y_lead = y_lead_h,
      shock = shock_reg
    )
    # Add lags only if p > 0
    if (ncol(X_lags_reg) > 0) {
      regression_data <- cbind(regression_data, X_lags_reg)
    }
    
    # Remove rows with *any* NAs (important if original data or bund_hat had NAs)
    regression_data_final <- na.omit(regression_data)
    
    # Check for sufficient observations AFTER na.omit
    # Need more obs than regressors (1 for intercept + 1 for shock + p*num_controls)
    min_obs_needed <- 1 + 1 + ncol(X_lags_reg)
    if (nrow(regression_data_final) < min_obs_needed) {
      warning(paste("Skipping h=", h, "for var=", outcome_vars[i], "- insufficient non-NA observations:", nrow(regression_data_final)))
      IRF[h + 1, i] <- NA
      IRF_low[h + 1, i] <- NA
      IRF_high[h + 1, i] <- NA
      next
    }
    
    # 2. trin regression (or Reduced Form)
    # The formula automatically includes all columns from regression_data_final
    model <- lm(y_lead ~ ., data = regression_data_final)
    
    # --- Extract results and SEs (check for existence) ---
    coeffs <- coef(model)
    if ("shock" %in% names(coeffs)) {
      IRF[h + 1, i] <- coeffs["shock"]
      
      # Robust standard errors
      tryCatch({
        vcov_matrix <- vcovHC(model, type = "HC1")
        # Check if 'shock' row/column exists in vcov matrix
        if ("shock" %in% rownames(vcov_matrix) && "shock" %in% colnames(vcov_matrix)) {
          se <- sqrt(diag(vcov_matrix)["shock"])
          if (!is.na(se) && se > 0) { # Check se is valid and positive
            IRF_low[h + 1, i] <- IRF[h + 1, i] - 1.96 * se
            IRF_high[h + 1, i] <- IRF[h + 1, i] + 1.96 * se
          } else {
            warning(paste("Invalid SE for shock at h=", h, "var=", outcome_vars[i]))
            IRF_low[h + 1, i] <- NA
            IRF_high[h + 1, i] <- NA
          }
        } else {
          warning(paste("SE for shock not found in vcovHC at h=", h, "var=", outcome_vars[i]))
          IRF_low[h + 1, i] <- NA
          IRF_high[h + 1, i] <- NA
        }
      }, error = function(e) {
        warning(paste("Error calculating vcovHC at h=", h, "var=", outcome_vars[i], ":", e$message))
        IRF_low[h + 1, i] <- NA
        IRF_high[h + 1, i] <- NA
      })
      
    } else {
      warning(paste("Coefficient 'shock' not found in model at h=", h, "var=", outcome_vars[i]))
      IRF[h + 1, i] <- NA
      IRF_low[h + 1, i] <- NA
      IRF_high[h + 1, i] <- NA
    }
  } # End horizon loop
} # End outcome variable loop

# --- Normalization so that Bund Yield = +1 pp at horizon 0 ---
scaling <- shocksize / IRF[1, outcome_vars == shock_var]
IRF <- IRF * scaling
IRF_low <- IRF_low * scaling
IRF_high <- IRF_high * scaling

# --- Plot IRFs ---
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (i in seq_along(outcome_vars)) {
  plot(0:H, IRF[, i], type = "l", col = "darkblue", lwd = 2,
       ylim = range(IRF_low[, i], IRF_high[, i]),
       xlab = "Horizon (quarters)", ylab = "Impulse Response",
       main = outcome_vars[i])
  
  polygon(c(0:H, rev(0:H)),
          c(IRF_low[, i], rev(IRF_high[, i])),
          col = rgb(0, 0, 1, 0.1), border = NA)
  
  abline(h = 0, lty = 2, col = "black")
  grid()
}

#########################################################################
# Gem og plot IRFs (2x2 layout, farvetema tilpasset)
#########################################################################
# --- Plot IRFs (Styled 2x2, Simplified Y-axis Scaling) ---
plot_dir <- "Graphs/LP-IV"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

png_filename <- file.path(plot_dir, "IRF_Normalized_Styled_SimpleY.png") # Changed filename slightly
png(png_filename, width = 2000, height = 2000, res = 300)

par(mfrow = c(2, 2),
    mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3) # Keep transparency
horizons <- 0:H

# Loop through outcome variables to create plots
for (i in seq_along(outcome_vars)) {
  
  # --- Calculate SIMPLE Y-axis Limits to show everything ---
  # Combine point estimate and CI bounds
  all_values <- c(IRF[, i], IRF_low[, i], IRF_high[, i])
  # Find the range of only the finite values (ignore NA, Inf, -Inf)
  finite_values <- all_values[is.finite(all_values)]
  
  if (length(finite_values) > 0) {
    plot_ylim <- range(finite_values)
    # Add a tiny buffer if the range is effectively zero to avoid plot errors
    if (diff(plot_ylim) < 1e-9) {
      plot_ylim <- plot_ylim + c(-0.1, 0.1) # Add small symmetric buffer
    }
  } else {
    # Fallback if absolutely no finite data exists for this variable
    plot_ylim <- c(-1, 1)
    warning(paste("No finite data points found for", outcome_vars[i], "- using default ylim c(-1, 1)."))
  }
  
  # --- Create Plot ---
  plot(horizons, IRF[, i], type = "n", # Type "n" avoids plotting data yet
       ylim = plot_ylim,             # Use the calculated simple limits
       xlab = "Horizon (Quarters)", ylab = "Impulse Response",
       main = paste("IRF:", outcome_vars[i]))
  
  # --- Plotting elements (Confidence band, line, zero line) ---
  
  # Add confidence band polygon (using only finite points for polygon)
  # Find indices where BOTH low and high CI are finite
  valid_ci_idx <- which(is.finite(IRF_low[, i]) & is.finite(IRF_high[, i]))
  
  if(length(valid_ci_idx) > 1) { # Need more than 1 point for a polygon area
    # Create polygon coordinates using only valid indices
    x_poly <- c(horizons[valid_ci_idx], rev(horizons[valid_ci_idx]))
    y_poly <- c(IRF_low[valid_ci_idx, i], rev(IRF_high[valid_ci_idx, i]))
    
    # Double-check polygon coordinates are finite before drawing
    if (all(is.finite(x_poly)) && all(is.finite(y_poly))) {
      polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
    } else {
      warning(paste("Skipping polygon for", outcome_vars[i], "due to non-finite coordinates after filtering."))
    }
  } else {
    # Handle cases with 0 or 1 valid CI points if necessary (optional)
    # warning(paste("Not enough valid CI points to draw polygon for", outcome_vars[i]))
  }
  
  
  # Add line and points for point estimates
  # lines() handles internal NAs by creating gaps automatically
  lines(horizons, IRF[, i], type = "b", col = burgundy, lwd = 2, pch = 16)
  
  # Add horizontal line at zero
  abline(h = 0, lty = 2, col = "black")
  
} # End loop through outcome variables

dev.off() # Close the PNG device
par(mfrow = c(1, 1)) # Reset plotting layout

print(paste("LP-IV estimation and normalization complete. Styled 2x2 plot with simple Y-axis saved to:", png_filename))

#########################################################################
# SLUT: Opdateret Plotting Sektion (Simpel Y-akse)
#########################################################################

#########################################################################
# Plot CUMULATIVE IRFs (2x2 layout, samme stil og placering)
#########################################################################

# --- Cumulative IRFs ---
cum_IRF <- apply(IRF, 2, cumsum)
cum_IRF_low <- apply(IRF_low, 2, cumsum)
cum_IRF_high <- apply(IRF_high, 2, cumsum)

# --- Gem plot som PNG ---
png(file.path(plot_dir, "Cumulative_IRF_Normalized_Styled.png"),
    width = 2000, height = 2000, res = 300)

par(mfrow = c(2, 2),
    mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# Brug samme farver
for (i in seq_along(outcome_vars)) {
  
  all_vals <- c(cum_IRF[, i], cum_IRF_low[, i], cum_IRF_high[, i])
  finite_vals <- all_vals[is.finite(all_vals)]
  
  if (length(finite_vals) > 0) {
    ylim_cum <- range(finite_vals)
    if (diff(ylim_cum) < 1e-9) {
      ylim_cum <- ylim_cum + c(-0.1, 0.1)
    }
  } else {
    ylim_cum <- c(-1, 1)
    warning(paste("No finite cumulative IRFs for", outcome_vars[i]))
  }
  
  # Tomt plot
  plot(horizons, cum_IRF[, i], type = "n",
       ylim = ylim_cum,
       xlab = "Horizon (Quarters)", ylab = "Cumulative IRF",
       main = paste("Cumulative IRF:", outcome_vars[i]))
  
  # Polygon CI
  valid_ci <- which(is.finite(cum_IRF_low[, i]) & is.finite(cum_IRF_high[, i]))
  
  if (length(valid_ci) > 1) {
    x_poly <- c(horizons[valid_ci], rev(horizons[valid_ci]))
    y_poly <- c(cum_IRF_low[valid_ci, i], rev(cum_IRF_high[valid_ci, i]))
    
    if (all(is.finite(x_poly)) && all(is.finite(y_poly))) {
      polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
    }
  }
  
  # Punktestimat
  lines(horizons, cum_IRF[, i], type = "b", col = burgundy, lwd = 2, pch = 16)
  
  # Nul-linje
  abline(h = 0, lty = 2, col = "black")
}

dev.off()