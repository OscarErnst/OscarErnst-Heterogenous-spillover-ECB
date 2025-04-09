#########################################################################
# First-Stage LP-IV Analysis with Target Shock Included as a Control
#########################################################################

# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory (adjust path as needed)
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")

# Load additional functions (e.g., LP-IV function)
source("R kode/Functions/LP IV function.R")

# Load required packages
library(dplyr)
library(AER)
library(dynlm)
library(car)
library(zoo)    # for as.yearqtr()
library(readxl)
library(stargazer)

# Set the estimation window for the instrument
start_month <- c(2004, 9)
end_month   <- c(2019, 12)

#########################################################################
# 1. Load Endogenous Variables (Controls and Potential Outcomes)
#########################################################################
# Load Eurozone country variables and select the outcome/control variables.
# In our case, we use the log-change in HICP, rGDP, and Consumption.
data <- readRDS("Data/Control Variables/Eurozone_country_variables.rds") %>%
  filter(country == "EA20") %>%
  dplyr::select(d_HICP, d_rGDP, d_Consumption)

# Load German 2-year bund yield
bund_yield <- read_excel("Data/Generic Bundesbank yield.xlsx")
bund_yield <- bund_yield[[2]]  # Vælger kolonne 4
# Ensure it aligns with the estimation window
# Konverter bund_yield til et tidsserieobjekt
bund_yield_ts <- ts(bund_yield, start = c(2004, 9), frequency = 12)

# Anvend window() på tidsserieobjektet
bund_yield <- window(bund_yield_ts, start = start_month, end = end_month)

bund_yield <- as.numeric(bund_yield[1:nrow(data)])

# Merge bund_yield into the data
data <- cbind(data, bund_yield = bund_yield)

# Define outcome variables (husk at have 4 variabler, så plotting-layout skal tilpasses)
outcome_vars <- c("d_HICP", "d_rGDP", "d_Consumption", "bund_yield")

#########################################################################
# 2. Load the Instrument (Pure Target Shock)
#########################################################################
# Load the pre-saved Target Factor Shock (baseret på OIS_6M)
# Hvis du ønsker at bruge target instrument: 
 target_q <- readRDS("Data/LP-IV/Target_instrument.rds")
#target_q <- readRDS("Data/Shocks/Target Factor Shock.rds")

# Eller brug den faktiske identificerede shock (udkommenteret nedenfor):
# target_q <- readRDS("Data/Shocks/Target Factor Shock_Quarterly.rds")
target_q <- ts(cbind(target_q = target_q),
               start = c(1999, 1), end = c(2024, 12),
               frequency = 12)
target_q <- window(target_q, start = start_month, end = end_month)

# Ensure the instrument is a numeric vector and has the same length as 'data'
target_q <- as.numeric(target_q[1:nrow(data)])

#########################################################################
# 3. Merge the Target Shock into the Control Variables
#########################################################################
# Add target_q as an additional control variable.
# Dette inkluderer den monetære innovationsvariabel direkte i regressionen.
data <- cbind(data, target_q = target_q)

#########################################################################
# 5. Estimate the Local Projection IV Model
#########################################################################
# Her sættes p = 12 (antal lags) og H = 8 (fremtidige horisonter fra 0 til 8)
result <- estimateLPIV(data = data,
                       p = 12,
                       H = 8,
                       instrument = target_q,
                       outcome_vars = outcome_vars,
                       c_case = 1,       # med konstant (ændres til 0 eller 2, hvis nødvendigt)
                       conf_level = 0.95 # 95% konfidensintervaller
)

# Beregn kumulative IRF'er og deres intervaller
cum_IRF   <- t(apply(result$IRF, 1, cumsum))
cum_Lower <- t(apply(result$Lower, 1, cumsum))
cum_Upper <- t(apply(result$Upper, 1, cumsum))

#########################################################################
# 6. Plot the Cumulative IRFs
#########################################################################
# Gem plottet i en PNG-fil
png("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Graphs/LP-IV/IRF.png",
    width = 2000, height = 2400, res = 300)

# Tilpas plot-layout: 4 rækker da vi har 4 outcome-variabler
par(mfrow = c(4, 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# Bestem forecast horisonter (antager de starter ved 0)
horizons <- 0:(ncol(result$IRF) - 1)

# Definer farver til plottet
burgundy <- "#760020"                  # Solid burgundy til linjerne
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)  # Transparent burgundy til konfidensinterval

# Loop over hver outcome-variabel (hver række i IRF-matricen)
for (i in 1:length(outcome_vars)) {
  # Sæt y-aksens grænser baseret på de kumulative intervaller
  y_min <- min(cum_Lower[i, ], na.rm = TRUE)
  y_max <- max(cum_Upper[i, ], na.rm = TRUE)
  
  # Opret et tomt plot med de angivne grænser og labels
  plot(horizons, cum_IRF[i, ], type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon", ylab = "Kumulativ impulssvar",
       main = paste("Cumul. Impulse Response for", outcome_vars[i]))
  
  # Konstruer polygon-koordinater for at tegne skyggeområdet af konfidensintervallet
  x_poly <- c(horizons, rev(horizons))
  y_poly <- c(cum_Lower[i, ], rev(cum_Upper[i, ]))
  polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
  
  # Tegn impulssvarslinjen over skyggeområdet
  lines(horizons, cum_IRF[i, ], type = "b", col = burgundy, lwd = 2)
  
  # Tegn en vandret linje ved 0 som reference
  abline(h = 0, lty = 2, col = "black")
}

# Luk PNG-enheden og gem plottet
dev.off()

