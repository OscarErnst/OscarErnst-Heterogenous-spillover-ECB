
# Ryd workspace og konsol (valgfrit, men som i din kode)
rm(list = ls())
cat("\014")

# Indlæs nødvendige pakker (antager, at Load_Packages.R indlæser dem, ellers afkommenter nedenstående)
# required_packages <- c("dplyr", "AER", "dynlm", "car", "zoo", "readxl", "lubridate")
# for (pkg in required_packages) {
#   if (!require(pkg, character.only = TRUE)) {
#     install.packages(pkg)
#     library(pkg, character.only = TRUE)
#   }
# }
library(dplyr)
library(AER)
library(dynlm)
library(car)
library(zoo)
library(readxl) # Nødvendig for read_excel

# Sæt working directory
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
# source("R kode/Functions/Load_Packages.R") # Hvis du bruger denne

# Definer farver
burgundy <- "#760020"  # solid burgundy for lines
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3) # transparent fill

# ------------------------------------------------------------------------------
# 1) Sæt datoperiode for månedlige data
start_month <- c(2005, 1)
end_month   <- c(2019, 12)

# Konverter start/slut måned til Dato (antager første dag i måneden)
start_date <- as.Date(sprintf("%d-%02d-01", start_month[1], start_month[2]))
end_date   <- as.Date(sprintf("%d-%02d-01", end_month[1], end_month[2]))

# ------------------------------------------------------------------------------
# 2) Indlæs de tre chok
# Antag, at filen eksisterer og indeholder en liste eller data.frame med navngivne vektorer/kolonner
if (!file.exists("Data/Shocks/all_3_shocks.rds")) stop("Filen 'Data/Shocks/all_3_shocks.rds' blev ikke fundet!")
shocks <- readRDS("Data/Shocks/all_3_shocks.rds")

# Opret og window tidsserier for hvert chok
pureMP_m <- ts(cbind(pureMP_m = shocks$pureMP_m), start = c(1999, 1), frequency = 12) # Antager start 1999-01 ud fra data
pureMP_m <- window(pureMP_m, start = start_month, end = end_month)

Path_m <- ts(cbind(Path_m = shocks$Path_m), start = c(1999, 1), frequency = 12)
Path_m <- window(Path_m, start = start_month, end = end_month)

QE_m <- ts(cbind(QE_m = shocks$QE_m), start = c(1999, 1), frequency = 12)
QE_m <- window(QE_m, start = start_month, end = end_month)

# ------------------------------------------------------------------------------
# 3) Indlæs kontrolvariable (rGDP og HICP log-ændringer)
# Antag, at filen eksisterer og indeholder de nødvendige kolonner
if (!file.exists("Data/Interpolated data/control_var_m.rds")) stop("Filen 'Data/Interpolated data/control_var_m.rds' blev ikke fundet!")
control <- readRDS("Data/Interpolated data/control_var_m.rds")

# Opret tidsserie for rGDP log-ændring
# Antager, at 'control' har data fra før start_date for at matche ts-start
# Juster start i ts() om nødvendigt, hvis 'control' starter senere
rGDP_m <- ts(
  data      = control %>% dplyr::select(d_rGDP_m),
  start     = c(2005, 1), # Antager data starter her, juster om nødvendigt
  # start     = c(as.integer(format(start_date, "%Y")), as.integer(format(start_date, "%m"))), # Alternativ hvis control starter præcis ved start_date
  frequency = 12
)
d_rGDP_m <- window(rGDP_m, start = start_month, end = end_month)

# Opret tidsserie for HICP log-ændring
HICP_m <- ts(
  data      = control %>% dplyr::select(d_HICP_m),
  start     = c(2005, 1), # Antager data starter her, juster om nødvendigt
  # start     = c(as.integer(format(start_date, "%Y")), as.integer(format(start_date, "%m"))),
  frequency = 12
)
d_HICP_m <- window(HICP_m, start = start_month, end = end_month)

# ------------------------------------------------------------------------------
# 4) Indlæs Bundesbank 1-års rente
# Antag, at filen eksisterer
if (!file.exists("Data/Generic Bundesbank yield.xlsx")) stop("Filen 'Data/Generic Bundesbank yield.xlsx' blev ikke fundet!")
Bundes_yield <- read_excel("Data/Generic Bundesbank yield.xlsx")

# Udtræk 1-års renten (kolonne 3 - *overvej at bruge kolonnenavn hvis muligt*)
Bund_1y_m_level <- Bundes_yield[[4]]
# Beregn månedlige ændringer i basispoint (første værdi er NA)
d_Bund_1y_m_diff <- c(NA, diff(Bund_1y_m_level)) * 100

# Opret tidsserier for niveau og ændring
Bund_1y_m <- ts(Bund_1y_m_level, start = start_month, frequency = 12) # Antager excel starter i start_month
Bund_1y_m <- window(Bund_1y_m, start = start_month, end = end_month)

d_Bund_1y_m <- ts(d_Bund_1y_m_diff, start = start_month, frequency = 12) # Antager excel starter i start_month
d_Bund_1y_m <- window(d_Bund_1y_m, start = start_month, end = end_month)

# ------------------------------------------------------------------------------
# 5) Kombiner data til et tidsserieobjekt
# Tjek at GFC og QE dummy perioder er korrekte
shocks_data <- cbind(
  pureMP_m      = as.numeric(pureMP_m),
  Path_m        = as.numeric(Path_m),
  QE_m          = as.numeric(QE_m),
  d_Bund_1y_m   = as.numeric(d_Bund_1y_m),
  Bund_1y_m     = as.numeric(Bund_1y_m),
  rGDPm_logchg  = as.numeric(d_rGDP_m),
  HICPm_logchg  = as.numeric(d_HICP_m),
  GFC_dummy = ifelse(as.yearmon(time(pureMP_m)) >= as.yearmon("2007-01") &
                       as.yearmon(time(pureMP_m)) <= as.yearmon("2007-09"), 1, 0), # OBS: Sept 2007?
  QE_dummy = ifelse(as.yearmon(time(pureMP_m)) >= as.yearmon("2017-01") &
                      as.yearmon(time(pureMP_m)) <= as.yearmon("2017-12"), 1, 0)  # OBS: Kun 2017?
)

# Konverter til en `ts` objekt
shocks_ts <- ts(shocks_data, start = start_month, frequency = 12)

# ------------------------------------------------------------------------------
# 6) Kør 1. trins regression (First Stage)

# OBS: Overvej om GFC_dummy og QE_dummy skal inkluderes som kontrolvariable her
# Hvis ja, tilføj "+ GFC_dummy + QE_dummy" til formlen.
FirstStage <- dynlm(Bund_1y_m ~ pureMP_m + Path_m + QE_m +
                      L(pureMP_m, 1:5) + L(Path_m, 1:5) + L(QE_m, 1:5) +
                      L(Bund_1y_m, 1:5) +
                      L(rGDPm_logchg, 1:5) + L(HICPm_logchg, 1:5),
                    data = shocks_ts)

cat("--------------------------------------------------\n")
cat("Summary of First Stage Regression:\n")
cat("--------------------------------------------------\n")
print(summary(FirstStage))
cat("--------------------------------------------------\n")


# ------------------------------------------------------------------------------
# 7) Joint F-test for Instrument Relevans
#    Tester H0: Alle koefficienter for pureMP_m, Path_m, QE_m (og deres lags) er nul.
# ------------------------------------------------------------------------------

# 1. Hent alle koefficientnavne fra modellen
coef_names <- names(coef(FirstStage))

# 2. Identificer koefficienter relateret til instrumenterne
#    Vi søger efter navne, der indeholder "pureMP_m", "Path_m", eller "QE_m"
instrument_coef_names <- coef_names[grepl("pureMP_m|Path_m|QE_m", coef_names)]

# (Valgfrit: Tjek de fundne navne)
# print("Instrument koefficienter fundet til F-test:")
# print(instrument_coef_names)
# print(paste("Antal instrument koefficienter:", length(instrument_coef_names))) # Skulle gerne være 3 * (1+5) = 18

# 3. Opret hypotesestrengene (kun hvis instrumenter blev fundet)
if (length(instrument_coef_names) > 0) {
  hypotheses_to_test <- paste(instrument_coef_names, "= 0")
  
  # 4. Udfør F-testen med car::linearHypothesis
  joint_test <- linearHypothesis(FirstStage, hypotheses_to_test)
  
  # 5. Print resultaterne af F-testen pænt
  cat("\n--------------------------------------------------\n")
  cat("Joint F-test for Instrument Relevance Results:\n")
  cat("(H0: All instrument coefficients are jointly zero)\n")
  cat("--------------------------------------------------\n")
  print(joint_test)
  cat("\nSummary Statistics for Joint Instrument Test:\n")
  # Brug format.pval for pænere output af små p-værdier
  cat("F-statistic:", round(joint_test$F[2], 2),
      "\nP-value:", format.pval(joint_test$`Pr(>F)`[2], digits = 4, eps = 0.0001), "\n")
  cat("Degrees of Freedom (Hypothesis):", joint_test$Df[2], "\n")
  cat("Degrees of Freedom (Residual):", joint_test$Res.Df[2], "\n")
  cat("--------------------------------------------------\n")
  
  # Fortolkning ift. tommelfingerregel (F > 10)
  f_stat_value <- joint_test$F[2]
  if (!is.na(f_stat_value) && f_stat_value < 10) {
    cat("Note: First-stage F-statistic is", round(f_stat_value, 2), "which is BELOW the rule-of-thumb value of 10.\n")
    cat("      This may indicate weak instruments, proceed with caution.\n")
  } else if (!is.na(f_stat_value)) {
    cat("Note: First-stage F-statistic is", round(f_stat_value, 2), "which is ABOVE the rule-of-thumb value of 10.\n")
  } else {
    cat("Note: Could not evaluate F-statistic against the rule-of-thumb value of 10.\n")
  }
  cat("--------------------------------------------------\n")
  
} else {
  cat("\nFEJL: Ingen instrument-koefficienter fundet der matcher 'pureMP_m', 'Path_m', eller 'QE_m'.\n")
  cat("F-testen kan ikke udføres. Tjek regressionsformel og variabelnavne.\n")
  cat("--------------------------------------------------\n")
}

# calculate Newey-West standard errors for robust inference
newey_west_se <- coeftest(FirstStage, vcov = NeweyWest(FirstStage))
print(newey_west_se)

# Get the fitted values from the first-stage regression
Bund_1y_m_hat <- fitted.values(FirstStage)
plot(Bund_1y_m_hat, type = "l")
# ------------------------------------------------------------------------------

# Aggregatze the fitted values to a quarterly series.
shock_var     <- Bund_1y_m_hat
shock_var_zoo <- zoo(shock_var)
shock_var_q   <- aggregate(shock_var_zoo, as.yearqtr, mean)  # using mean

# Convert the aggregated zoo object back to a data frame.
shock_var_q_df <- data.frame(
  dates  = as.Date(index(shock_var_q)),
  values = coredata(shock_var_q)
)

# Convert the data frame into a quarterly ts object.
shock_var_q_ts <- ts(shock_var_q_df$values, start = c(2004, 1), frequency = 4)

# Save the final quarterly shocks as an RDS file.
saveRDS(shock_var_q_ts, file = "Output data/instrument.rds")
plot(shock_var_q_df)

plot()

# ---------------------------------------------------------------------
# 1) Plot: Actual vs. Fitted Bund_1y_m
# ---------------------------------------------------------------------
# Save Fitted vs. Actual Bund_1y_m Plot
png("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Graphs/Fitted_vs_Actual_Bund_1y_m_better_HU.png",
    width = 2000, height = 1200, res = 150)
par(mar = c(5, 5, 4, 2), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
plot(shocks_ts[, "Bund_1y_m"], type = "l", col = burgundy_trans,
     main = "Actual vs. Fitted Bund_1y_m", xlab = "Time", ylab = "Bund_1y_m")
lines(Bund_1y_m_hat, col = burgundy, lwd = 2)
legend("topright", legend = c("Actual", "Fitted"), col = c(burgundy_trans, burgundy), lty = 1, lwd = 2, cex = 1.2)
dev.off()

# ---------------------------------------------------------------------
# 2) Plot: 2x2 Panel with Shocks + Fitted Bund_1y_m
# ---------------------------------------------------------------------
png("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Graphs/Shock_Series_and_Fitted_better-HU.png",
    width = 2000, height = 1600, res = 150)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)
plot(shocks_ts[, "pureMP_m"], type = "l", main = "(a) Target Factor", xlab = "Time", ylab = "basis points", col = burgundy, lwd = 2)
plot(shocks_ts[, "Path_m"],   type = "l", main = "(b) Forward Guidance",   xlab = "Time", ylab = "basis points",   col = burgundy, lwd = 2)
plot(shocks_ts[, "QE_m"],     type = "l", main = "(c) Quantiative Easing",     xlab = "Time", ylab = "basis points",     col = burgundy, lwd = 2)
plot(Bund_1y_m_hat,           type = "l", main = "(d) Fitted Bund_1y_m instrument", xlab = "Time", ylab = "Bund_1y_m_hat", col = burgundy, lwd = 2)
dev.off()



load("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Input Data/IV_mpshock_release.RData")


plot(shock_var_q_ts, main = "Bund 1Y Instrument vs. Fitted", col = "blue", lwd = 2)
lines(shock_var_q, col = "red", lwd = 2)





