# Sørg for at have 'sandwich' pakken installeret og indlæst
# install.packages("sandwich")
# library(sandwich)

estimateLPIV <- function(data,
                         p = 4,                      # Antal lags af kontroller/udfald
                         H = 10,                     # Max horisont (estimering fra h=0 til h=H)
                         instrument,                 # Instrument: Kolonnenavn (string) eller numerisk vektor
                         outcome_vars,               # Vektor af navne på udfaldsvariable
                         c_case = 1,                 # 0=ingen konstant, 1=konstant, 2=konstant+trend
                         conf_level = 0.95,          # Konfidensniveau for intervaller
                         se_type = "HAC",            # Standardfejl type: "HAC" (Newey-West), "HC" (Robust), "classical"
                         include_instrument_in_controls = FALSE # Skal lags af instrumentet inkluderes som kontroller?
) {
  
  # --- 1. Input validering ---
  if (missing(data) || !is.data.frame(data)) {
    stop("Argument 'data' skal være en data frame.")
  }
  T_full <- nrow(data)
  if (T_full == 0) stop("'data' er tom.")
  
  p <- as.integer(p)
  H <- as.integer(H)
  if (p < 0) stop("'p' (antal lags) må ikke være negativ.")
  if (H < 0) stop("'H' (max horisont) må ikke være negativ.")
  
  # Valider outcome_vars
  if (missing(outcome_vars) || is.null(outcome_vars)) {
    stop("'outcome_vars' skal specificeres (en vektor af kolonnenavne).")
  }
  if (!is.character(outcome_vars)) {
    if (is.numeric(outcome_vars)) { outcome_vars <- colnames(data)[outcome_vars] }
    else if (is.factor(outcome_vars)) { outcome_vars <- as.character(outcome_vars) }
    else { stop("'outcome_vars' skal være en karaktervektor med kolonnenavne.") }
  }
  missing_vars <- setdiff(outcome_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Udfaldsvariable ikke fundet i data:", paste(missing_vars, collapse=", ")))
  }
  outcome_vars <- unique(outcome_vars) # Fjern dubletter
  
  if (!c_case %in% 0:2) stop("c_case skal være 0, 1, eller 2.")
  if (conf_level <= 0 || conf_level >= 1) stop("conf_level skal være mellem 0 og 1.")
  if (!se_type %in% c("HAC", "HC", "classical")) stop("se_type skal være 'HAC', 'HC', eller 'classical'.")
  
  # --- 2. Forbered instrument ---
  inst_name <- NULL
  inst_series <- NULL
  # Bruges til at undgå at tilføje instrumentet til data hvis det allerede er en kolonne
  instrument_is_column <- FALSE
  
  if (is.character(instrument)) { # Instrument givet som kolonnenavn
    if (length(instrument) != 1) stop("Angiv ét enkelt kolonnenavn for 'instrument'.")
    inst_name <- instrument # Korrekt variabelnavn her
    if (!inst_name %in% colnames(data)) stop(paste("Instrumentvariabel '", inst_name, "' ikke fundet i data."))
    inst_series <- data[[inst_name]]
    instrument_is_column <- TRUE
  } else if (is.numeric(instrument)) { # Instrument givet som vektor
    if (length(instrument) != T_full) {
      stop(paste0("Instrument vektor længde (", length(instrument), ") skal matche antal rækker i data (", T_full, ")."))
    }
    inst_series <- instrument
    # Find et unikt navn til instrumentet hvis det ikke er et kolonnenavn
    base_inst_name <- "instrument_vec"
    inst_name <- base_inst_name
    suffix <- 1
    while (inst_name %in% colnames(data)) {
      inst_name <- paste0(base_inst_name, "_", suffix)
      suffix <- suffix + 1
    }
    # Midlertidigt tilføj instrumentvektoren til data for nem håndtering i loops
    # Vi fjerner den igen til sidst for ikke at modificere input data permanent
    data[[inst_name]] <- inst_series
    cat(paste0("INFO: Instrument vektor tilføjet til data med navn: '", inst_name, "'\n"))
  } else {
    stop("'instrument' skal være et kolonnenavn (streng) eller en numerisk vektor med længde nrow(data).")
  }
  
  # --- 3. Opsæt kontrolvariable (der skal lagges) ---
  # Standard: Alle variable i 'data' UNDTAGEN udfaldsvariablene ER kontroller.
  # Vi fjerner også instrumentet fra denne liste, medmindre include_instrument_in_controls=TRUE.
  control_vars_to_lag <- setdiff(colnames(data), outcome_vars)
  
  if (!include_instrument_in_controls) {
    # Fjern instrumentet fra listen af variable, hvis LAGS skal inkluderes som kontroller
    control_vars_to_lag <- setdiff(control_vars_to_lag, inst_name)
  }
  # Sikrer vi ikke har variable der ikke findes i data (bør ikke ske her)
  control_vars_to_lag <- intersect(control_vars_to_lag, colnames(data))
  
  # --- 4. Initialiser resultatmatricer ---
  outcome_count <- length(outcome_vars)
  horizons <- 0:H
  n_horizons <- length(horizons)
  # Brug navne i dimnames med det samme
  IRF   <- matrix(NA, nrow = outcome_count, ncol = n_horizons, dimnames = list(outcome_vars, paste0("h", horizons)))
  Lower <- matrix(NA, nrow = outcome_count, ncol = n_horizons, dimnames = list(outcome_vars, paste0("h", horizons)))
  Upper <- matrix(NA, nrow = outcome_count, ncol = n_horizons, dimnames = list(outcome_vars, paste0("h", horizons)))
  
  # Kritisk værdi for konfidensinterval
  alpha <- 1 - conf_level
  crit_val <- stats::qnorm(1 - alpha/2) # Brug stats:: for at være eksplicit
  
  # --- 5. Hovedløkker over udfaldsvariable og horisonter ---
  cat("Estimating Raw LP-IV:\n")
  for (i in seq_along(outcome_vars)) {
    outcome_name <- outcome_vars[i]
    cat(sprintf("  Outcome: %s (%d/%d). Horizons: ", outcome_name, i, outcome_count)) # Progress update
    y_full <- data[[outcome_name]]
    
    for (h in horizons) {
      cat(paste0(h, " ")) # Progress for horizons
      
      # Definer regressionssample for horisont h
      start_idx <- p + 1
      end_idx   <- T_full - h # Sidste tidspunkt t, hvor y(t+h) eksisterer
      if (end_idx < start_idx) {
        cat(" (Not enough obs) ")
        warning(paste("Ikke nok observationer for", outcome_name, "ved horisont", h, "og frem."), call. = FALSE)
        break # Stop for denne udfaldsvariabel, gå til næste
      }
      t_index <- start_idx:end_idx # Tidspunkter t for regressionen
      
      # Konstruer data frame for regressionen (kun for de nødvendige tidspunkter)
      # Afhængig variabel y_{t+h}
      reg_df <- data.frame(y_dep = y_full[t_index + h])
      # Instrument i_t
      reg_df[[inst_name]] <- inst_series[t_index]
      
      # Tilføj lags af kontrolvariable (hvis p > 0)
      if (p > 0 && length(control_vars_to_lag) > 0) {
        for (var in control_vars_to_lag) {
          var_series <- data[[var]]
          for (lag in 1:p) {
            reg_df[[paste0(var, "_L", lag)]] <- var_series[t_index - lag]
          }
        }
      }
      # Tilføj lags af selve udfaldsvariablen (y_{t-1}, ..., y_{t-p}) (hvis p > 0)
      if (p > 0) {
        for (lag in 1:p) {
          # Brug et navn der ikke konflikter hvis outcome også er en kontrol
          reg_df[[paste0("outcome_", outcome_name, "_L", lag)]] <- y_full[t_index - lag]
        }
      }
      
      # Tilføj deterministiske led (konstant tilføjes af lm som standard)
      if (c_case == 2) { reg_df$trend <- t_index } # Trend baseret på tidspunkt t
      
      # Fjern rækker med NA *før* regressionen
      valid_rows <- stats::complete.cases(reg_df)
      n_valid <- sum(valid_rows)
      n_regressors <- ncol(reg_df) # Antal kolonner inkl. y_dep
      
      # Tjek om der er nok observationer til at køre regressionen
      # Skal have flere observationer end regressorer (plus intercept hvis c_case!=0)
      min_obs_needed <- n_regressors + (c_case != 0)
      if(n_valid < min_obs_needed) {
        cat(" (Too few non-NA obs) ")
        warning(paste("Spring over h=", h, "for", outcome_name, ": utilstrækkelige non-NA obs (", n_valid, ") efter lag."), call. = FALSE)
        next # Gå til næste horisont
      }
      reg_df_complete <- reg_df[valid_rows, , drop = FALSE]
      
      # Definer formel baseret på c_case
      # Brug `y_dep` som navnet på den afhængige variabel i formlen
      if (c_case == 0) { formula_str <- "y_dep ~ . - 1" } else { formula_str <- "y_dep ~ ." }
      model_formula <- stats::as.formula(formula_str)
      
      # Kør OLS regression
      model <- try(stats::lm(model_formula, data = reg_df_complete), silent = TRUE)
      if (inherits(model, "try-error")) {
        warning(paste("LM regression fejlede for", outcome_name, "ved h=", h, ". Fejl:", model), call. = FALSE)
        next # Gå til næste horisont
      }
      
      # --- 6. Uddrag koefficient og standardfejl ---
      model_coefs <- stats::coef(model)
      
      # Tjek om instrumentkoefficienten eksisterer (vigtigt ift. kollinearitet)
      if (!inst_name %in% names(model_coefs)) {
        warning(paste("Instrument '", inst_name, "' koefficient ikke fundet (muligvis droppet) for", outcome_name, "ved h=", h), call. = FALSE)
        next # Gå til næste horisont
      }
      beta_hat <- model_coefs[[inst_name]]
      
      # Beregn specificerede standardfejl
      cov_matrix <- tryCatch({
        if (se_type == "HAC") {
          # Brug horisont h som max lag for Newey-West (konsistent med LP litteratur)
          sandwich::NeweyWest(model, lag = h, prewhite = FALSE, adjust = TRUE)
        } else if (se_type == "HC") {
          # Robust mod heteroskedasticitet (f.eks. HC3)
          sandwich::vcovHC(model, type = "HC3")
        } else { # "classical"
          # Klassiske OLS standardfejl (antager homoskedasticitet og ingen autokorrelation)
          stats::vcov(model) # Giver den klassiske (sigma^2 * (X'X)^-1) matrix
        }
      }, error = function(e) {
        warning(paste("Beregning af kovariansmatrix fejlede for", outcome_name, "ved h=", h, ". Type:", se_type, ". Fejl:", e$message), call. = FALSE)
        return(NULL) # Returner NULL ved fejl
      })
      
      # Tjek om SE beregning lykkedes og om instrumentet findes i matrixen
      se_beta <- NA # Default til NA
      if (!is.null(cov_matrix) && is.matrix(cov_matrix) && inst_name %in% rownames(cov_matrix)) {
        inst_var <- diag(cov_matrix)[inst_name]
        # Tjek om variansen er valid (ikke NA, ikke negativ)
        if (!is.na(inst_var) && inst_var >= 0) {
          se_beta <- sqrt(inst_var)
        } else {
          warning(paste("Ugyldig varians (", inst_var, ") for instrument for", outcome_name, "ved h=", h), call. = FALSE)
        }
      } else {
        warning(paste("Kunne ikke beregne/finde SE for instrument for", outcome_name, "ved h=", h), call. = FALSE)
      }
      
      # Gem resultater (selv hvis SE er NA)
      col_idx <- h + 1 # h=0 er col 1, h=1 er col 2 etc.
      IRF[i, col_idx]   <- beta_hat
      # Beregn kun konfidensinterval hvis SE er valid
      if (!is.na(se_beta)) {
        Lower[i, col_idx] <- beta_hat - crit_val * se_beta
        Upper[i, col_idx] <- beta_hat + crit_val * se_beta
      }
      # Hvis SE er NA, forbliver Lower/Upper NA
      
    } # Slut horisontløkke
    cat("\n") # Ny linje efter hver outcome variabel
  } # Slut udfaldsløkke
  cat("Estimation complete.\n")
  
  # --- 7. Ryd op: Fjern instrumentkolonne hvis den blev tilføjet ---
  if (!instrument_is_column) {
    data[[inst_name]] <- NULL
  }
  
  # --- 8. Returner resultater ---
  result <- list(
    IRF = IRF,
    Lower = Lower,
    Upper = Upper,
    # Inkluder metadata for reproducerbarhed/info
    outcome_vars = outcome_vars,
    instrument_name = inst_name, # Navnet brugt i regressionen
    p = p,
    H = H,
    c_case = c_case,
    conf_level = conf_level,
    se_type = se_type,
    include_instrument_in_controls = include_instrument_in_controls,
    data_rows_used = T_full # Antal rækker i input data
    # Note: Effektiv N varierer med horisont h og NA mønstre
  )
  return(result)
}
