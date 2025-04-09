# --- 0. Opsætning ---
rm(list = ls())
cat("\014")
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
# Sørg for at denne fil indeholder den simple version af funktionen
source("R kode/Functions/LP IV function.R")

# Indlæs nødvendige pakker
library(dplyr)
library(zoo)      # Stadig nødvendig for as.yearqtr
library(lubridate)
library(sandwich)

# --- 1. Parametre ---
start_qtr_str <- "2005 Q1"
end_qtr_str   <- "2019 Q4"
p <- 4
H <- 8
conf <- 0.95
se <- "HAC"

# --- 2. Indlæs Outcomes/Kontroller ---
data_raw <- readRDS("Data/Control Variables/Eurozone_country_variables.rds")

# Klargør outcome data som data.frame
data_ea20 <- data_raw %>%
  filter(country == "EA20") %>%
  # Opret 'Time' kolonne (stadig som yearqtr objekt)
  mutate(Time = zoo::as.yearqtr(paste0(year, " Q", quarter))) %>%
  # Vælg og arranger
  dplyr::select(Time, d_HICP, d_rGDP, d_Consumption) %>%
  arrange(Time) %>%
  # Anvend tids-window direkte på data.frame
  filter(Time >= zoo::as.yearqtr(start_qtr_str) & Time <= zoo::as.yearqtr(end_qtr_str))

print("Dimensioner af data_ea20 efter windowing:")
print(dim(data_ea20))
print("Første rækker af data_ea20:")
print(head(data_ea20))

# --- 3. Indlæs Instrument ---
shock_raw <- readRDS("Data/LP-IV/Target_instrument.rds")
instrument_name <- "TargetShock" # Definer navnet vi vil bruge

# Klargør instrument som data.frame
# Kræver at vi kan udtrække tid og data fra shock_raw
if (is.ts(shock_raw)) {
  if (frequency(shock_raw) != 4) print("Advarsel: Instrument ts-frekvens er ikke 4.")
  instrument_df <- data.frame(
    Time = zoo::as.yearqtr(time(shock_raw)),
    Value = as.numeric(coredata(shock_raw))
  )
} else if (is.zoo(shock_raw)) {
  instrument_df <- data.frame(
    Time = index(shock_raw),
    Value = as.numeric(coredata(shock_raw))
  )
} else if(is.numeric(shock_raw)) {
  # Hvis det er en vektor, KAN vi IKKE lave en data.frame med tid sikkert
  # uden mere info. Stopper her, da tid er essentiel for join.
  stop("Instrument indlæst som numerisk vektor. Kan ikke klargøres uden tidsinformation.")
} else {
  stop("Ukendt format for indlæst instrument.")
}

# Omdøb værdi-kolonnen til det ønskede instrumentnavn
names(instrument_df)[names(instrument_df) == "Value"] <- instrument_name

# Anvend tids-window
instrument_df_windowed <- instrument_df %>%
  filter(Time >= zoo::as.yearqtr(start_qtr_str) & Time <= zoo::as.yearqtr(end_qtr_str))

print(paste("Dimensioner af", instrument_name, "efter windowing:"))
print(dim(instrument_df_windowed))
print(paste("Første rækker af", instrument_name, ":"))
print(head(instrument_df_windowed))

# --- 4. Merge Data (som DataFrames) ---
# Brug inner_join til at merge baseret på 'Time' kolonnen.
# Dette sikrer, at kun tidspunkter, der findes i BEGGE datasæt, beholdes.
final_data_df <- dplyr::inner_join(data_ea20, instrument_df_windowed, by = "Time")

# Tjek resultatet af join
print("Dimensioner af final_data_df efter inner_join:")
print(dim(final_data_df))
if(nrow(final_data_df) == 0){
  stop("Ingen matchende tidspunkter fundet mellem outcome data og instrument data efter windowing.")
}
print("Kolonnenavne i final_data_df:")
print(names(final_data_df))
print("Første rækker af final_data_df:")
print(head(final_data_df))

# Tjek for NAs (burde ikke være introduceret af inner_join, men kan stamme fra input)
if(anyNA(final_data_df)) {
  rows_before_na <- nrow(final_data_df)
  final_data_df <- na.omit(final_data_df) # Fjern rækker med NA
  print(paste("Fjernede", rows_before_na - nrow(final_data_df), "rækker med NA."))
  if(nrow(final_data_df) == 0){
    stop("Ingen observationer tilbage efter NA fjernelse.")
  }
}

# --- 5. Definer Udfaldsvariable ---
outcome_vars <- c("d_HICP", "d_rGDP", "d_Consumption")
# Tjek om de findes
if (!all(outcome_vars %in% names(final_data_df))) {
  print(paste("ADVARSEL: Nogle outcome_vars mangler:",
              paste(setdiff(outcome_vars, names(final_data_df)), collapse=", ")))
}

# --- 6. Estimer RÅ LP-IV resultater ---
cat("\nStarter estimering...\n")

# Tjek om instrumentnavnet findes FØR kaldet
if (!instrument_name %in% names(final_data_df)) {
  stop(paste("STOP: Instrumentkolonnen '", instrument_name,
             "' findes IKKE i final_data_df lige før kald. Faktiske navne er: '",
             paste(names(final_data_df), collapse="', '"), "'"))
} else {
  print(paste("INFO: Instrumentkolonnen '", instrument_name, "' fundet. Fortsætter."))
}

result_raw <- tryCatch({
  # Kald funktionen med den klargjorte data.frame
  estimateLPIV(
    data = final_data_df, # Nu en ren data.frame
    p = p,
    H = H,
    instrument = instrument_name, # Navnet på instrumentkolonnen
    outcome_vars = outcome_vars,
    c_case = 1,
    conf_level = conf,
    se_type = se,
    include_instrument_in_controls = TRUE
  )
}, error = function(e) {
  cat("FEJL under estimateLPIV funktionen:\n")
  print(e$message)
  # print(rlang::last_trace()) # Afkommenter for detaljeret trace
  return(NULL)
})

cat("Estimering færdig.\n")

# --- 7. Skalering af Impulsresponser ---
# Definer shocksize - dette skal være den størrelse af chokket, du vil skalere til
shocksize <- 1 # Ændr denne værdi til den ønskede chokstørrelse

# Find det umiddelbare respons af instrumentet på den første outcome-variabel
# Antager at du vil skalere baseret på effekten på d_HICP ved h=0
instrument_irf_h0 <- result_raw$IRF["d_HICP", "h0"]

if (!is.na(instrument_irf_h0) && instrument_irf_h0 != 0) {
  scaling_factor <- shocksize / instrument_irf_h0
  result <- list(
    IRF = result_raw$IRF * scaling_factor,
    Lower = result_raw$Lower * scaling_factor,
    Upper = result_raw$Upper * scaling_factor,
    outcome_vars = result_raw$outcome_vars,
    instrument_name = result_raw$instrument_name,
    p = result_raw$p,
    H = result_raw$H,
    c_case = result_raw$c_case,
    conf_level = result_raw$conf_level,
    se_type = result_raw$se_type,
    include_instrument_in_controls = result_raw$include_instrument_in_controls,
    data_rows_used = result_raw$data_rows_used
  )
  cat("\nImpulsresponser skaleret baseret på det umiddelbare respons af d_HICP.\n")
} else {
  cat("\nAdvarsel: Kunne ikke skalaere impulsresponser. Initial respons af d_HICP er NA eller nul.\n")
  result <- result_raw
}

png("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Graphs/LP-IV/IRF_skaleret.png",
    width = 2000, height = 2400, res = 300)

# Set up a plotting area with 3 rows and 1 column
par(mfrow = c(3, 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# Determine forecast horizons (assuming they start at 0)
horizons <- 0:(ncol(result$IRF) - 1)

# Define colors for the plot:
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)

# Loop over each outcome variable (each row of the IRF matrix)
for (i in 1:length(outcome_vars)) {
  
  # Set the y-axis limits based on the confidence interval bounds
  y_min <- min(result$Lower[i, ], na.rm = TRUE)
  y_max <- max(result$Upper[i, ], na.rm = TRUE)
  
  # Create an empty plot with proper limits and labels
  plot(horizons, result$IRF[i, ], type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horizon", ylab = "Impulse Response",
       main = paste("Impulse Response for", outcome_vars[i]))
  
  # Create polygon coordinates for the confidence interval (shaded area)
  x_poly <- c(horizons, rev(horizons))
  y_poly <- c(result$Lower[i, ], rev(result$Upper[i, ]))
  polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
  
  # Draw the impulse response line over the shaded area
  lines(horizons, result$IRF[i, ], type = "b", col = burgundy, lwd = 2)
  
  # Draw a horizontal line at zero for reference
  abline(h = 0, lty = 2, col = "black")
}

# Close the PNG device to save the plot
dev.off()

#########################################################################
# Beregn kumulative IRF'er og deres konfidensintervaller
#########################################################################
# Anvend cumsum() for at akkumulere effekten over horisonterne.
cum_IRF   <- t(apply(result$IRF, 1, cumsum))
cum_Lower <- t(apply(result$Lower, 1, cumsum))
cum_Upper <- t(apply(result$Upper, 1, cumsum))

#########################################################################
# 6. Plot de kumulative impulssvar
#########################################################################
# Gem plottet i en PNG-fil
png("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Graphs/LP-IV/IRF_skal_cum.png",
    width = 2000, height = 2400, res = 300)

# Juster plot-layout: antager at antallet af outcomer svarer til længden af outcome_vars
par(mfrow = c(length(outcome_vars), 1), mar = c(5, 5, 4, 2),
    cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.5)

# Bestem forecasthorisonterne, antager de starter ved 0
horizons <- 0:(ncol(result$IRF) - 1)

# Definer farver til plottet
burgundy <- "#760020"  # Solid burgundy til linjerne
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)  # Transparent burgundy til CI'erne

# Loop over hver outcome-variable for at lave individuelle plots
for (i in 1:length(outcome_vars)) {
  
  # Fastlæg y-aksens grænser baseret på konfidensintervallet
  y_min <- min(cum_Lower[i, ], na.rm = TRUE)
  y_max <- max(cum_Upper[i, ], na.rm = TRUE)
  
  # Opret et tomt plot med de angivne grænser og labels
  plot(horizons, cum_IRF[i, ], type = "n",
       ylim = c(y_min, y_max),
       xlab = "Horisont", ylab = "Kumulativ impulssvar",
       main = paste("Kumulativ impulssvar for", outcome_vars[i]))
  
  # Konstruer polygon-koordinater for at tegne det skyggede konfidensinterval
  x_poly <- c(horizons, rev(horizons))
  y_poly <- c(cum_Lower[i, ], rev(cum_Upper[i, ]))
  polygon(x_poly, y_poly, col = burgundy_trans, border = NA)
  
  # Tegn impulssvarslinjen over området
  lines(horizons, cum_IRF[i, ], type = "b", col = burgundy, lwd = 2)
  
  # Tilføj en vandret reference linje ved 0
  abline(h = 0, lty = 2, col = "black")
}

# Luk PNG-enheden for at gemme plottet
dev.off()

