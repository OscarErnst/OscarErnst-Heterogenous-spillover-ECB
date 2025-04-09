rm(list=ls())
cat("\014")
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
#source("R kode/Load_Packages.R")
source("R kode/3) LP-IV/LP IV function.R")

library("dplyr")
#----------------------------------------------------------------------------
# 1. Load endogenous variables (controls and potential outcomes)
data <- readRDS("Data/Control Variables/Eurozone_country_variables.rds") %>% 
  filter(country == "EA20") %>% 
  dplyr::select(d_HICP, d_rGDP, d_Consumption)

# 2. Load the instrument (monetary policy innovation)
i <- readRDS("Data/Shocks/Information Shock_Quarterly.rds")

# Make sure the instrument is a numeric vector of the same length as the rows in 'data'
i <- i[1:nrow(data)]
data <- cbind(data, i)

# 3. Define the outcome variables you wish to analyze
outcome_vars <- c("d_HICP", "d_rGDP", "d_Consumption")

# 4. Call the function with different specifications:
#    Here we set p = 4 (number of lags) and H = 8 (maximum forecast horizon, so horizons 0 to 8)

# Base specification (instrument not included in controls)
result_base <- estimateLPIV(data = data, 
                          p = 4, 
                          H = 8, 
                          instrument = i, 
                          outcome_vars = outcome_vars, 
                          c_case = 1,       # 1 includes a constant
                          conf_level = 0.95, # 95% confidence intervals
                          se_type = "HAC",   # Newey-West standard errors
                          include_instrument_in_controls = FALSE
)

# Alternative specification (instrument included in controls)
result_alt <- estimateLPIV(data = data, 
                         p = 4, 
                         H = 8, 
                         instrument = i, 
                         outcome_vars = outcome_vars, 
                         c_case = 1,       
                         conf_level = 0.95,
                         se_type = "HAC",
                         include_instrument_in_controls = TRUE
)

# Print results for both specifications
cat("\nBase Specification Results:\n")
print(result_base)

cat("\nAlternative Specification Results (instrument in controls):\n")
print(result_alt)

# 5. Create plots for both specifications
# First for base specification
png("Graphs/LP-IV/IRP_base.png",
    width = 2000, height = 2400, res = 300)

# Set up a plotting area with 3 rows and 1 column
par(mfrow = c(3, 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# Determine forecast horizons
horizons <- 0:(ncol(result_base$IRF) - 1)

# Define burgundy color (RGB 118, 0, 32) and a transparent version for the confidence cloud
burgundy <- "#760020"  # solid burgundy for lines
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)  # transparent fill

# Loop over each outcome variable to create individual panels
for (i in 1:length(outcome_vars)) {
  
  # Set the y-limits based on the lower and upper confidence intervals
  y_min <- min(result_base$Lower[i, ], na.rm = TRUE)
  y_max <- max(result_base$Upper[i, ], na.rm = TRUE)
  
  # Set up an empty plot (type = "n")
  plot(horizons, result_base$IRF[i, ], type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon", ylab = "Impulse Response",
       main = paste("Impulse Response for", outcome_vars[i], "(Base Specification)"))
  
  # Create polygon coordinates for the confidence interval cloud
  x_poly <- c(horizons, rev(horizons))
  y_poly <- c(result_base$Lower[i, ], rev(result_base$Upper[i, ]))
  polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
  
  # Draw the impulse response line on top
  lines(horizons, result_base$IRF[i, ], type = "b", col = burgundy, lwd = 2)
  
  abline(h = 0, lty = 2, col = "black")
}

# Close the PNG device
dev.off()

# Now for alternative specification
png("Graphs/LP-IV/IRP_alt.png",
    width = 2000, height = 2400, res = 300)

# Set up a plotting area with 3 rows and 1 column
par(mfrow = c(3, 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# Loop over each outcome variable to create individual panels
for (i in 1:length(outcome_vars)) {
  
  # Set the y-limits based on the lower and upper confidence intervals
  y_min <- min(result_alt$Lower[i, ], na.rm = TRUE)
  y_max <- max(result_alt$Upper[i, ], na.rm = TRUE)
  
  # Set up an empty plot (type = "n")
  plot(horizons, result_alt$IRF[i, ], type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon", ylab = "Impulse Response",
       main = paste("Impulse Response for", outcome_vars[i], "(Instrument in Controls)"))
  
  # Create polygon coordinates for the confidence interval cloud
  x_poly <- c(horizons, rev(horizons))
  y_poly <- c(result_alt$Lower[i, ], rev(result_alt$Upper[i, ]))
  polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
  
  # Draw the impulse response line on top
  lines(horizons, result_alt$IRF[i, ], type = "b", col = burgundy, lwd = 2)
  
  abline(h = 0, lty = 2, col = "black")
}

# Close the PNG device
dev.off()

