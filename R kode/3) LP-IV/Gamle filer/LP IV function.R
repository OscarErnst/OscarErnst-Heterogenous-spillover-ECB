estimateLPIV <- function(data, p = 4, H = 10, instrument, outcome_vars, 
                       c_case = 1, conf_level = 0.95, se_type = "HAC",
                       include_instrument_in_controls = FALSE) {
  # Check data input
  if (missing(data) || !is.data.frame(data)) {
    stop("Argument 'data' must be a data frame containing the time-series variables.")
  }
  T <- nrow(data)  # number of observations
  
  # Validate lag and horizon parameters
  p <- as.integer(p)
  H <- as.integer(H)
  if (p < 0) stop("Number of lags 'p' must be a non-negative integer.")
  if (H < 0) stop("Max horizon 'H' must be a non-negative integer.")
  if (p >= T) stop("Lag length p is too large for the available data.")
  
  # Handle outcome_vars: default to all data columns if not provided
  if (missing(outcome_vars) || is.null(outcome_vars)) {
    outcome_vars <- colnames(data)
  }
  # If given as numeric indices or factors, convert to character names
  if (!is.character(outcome_vars)) {
    if (is.numeric(outcome_vars)) {
      outcome_vars <- colnames(data)[outcome_vars]
    } else if (is.factor(outcome_vars)) {
      outcome_vars <- as.character(outcome_vars)
    } else {
      stop("Argument 'outcome_vars' must be a character vector of column names (or numeric indices/factor).")
    }
  }
  # Check that all outcome_vars exist in data
  missing_vars <- setdiff(outcome_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Outcome variable(s) not found in data frame:", paste(missing_vars, collapse=", ")))
  }
  outcome_vars <- unique(outcome_vars)  # remove any duplicates
  
  # Validate c_case parameter
  if (!c_case %in% 0:2) {
    stop("c_case must be 0 (no constant), 1 (constant), or 2 (constant+trend).")
  }
  # Validate confidence level
  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1 (e.g., 0.95 for 95% confidence intervals).")
  }
  # Validate standard error type
  if (!se_type %in% c("HAC", "DK")) {
    stop("se_type must be either 'HAC' (Newey-West) or 'DK' (Driscoll-Kraay).")
  }
  
  # Prepare instrument series and name
  inst_name <- NULL
  inst_series <- NULL
  if (is.character(instrument)) {
    if (length(instrument) != 1) {
      stop("Please provide a single instrument column name.")
    }
    inst_name <- instrument
    if (!inst_name %in% colnames(data)) {
      stop(paste("Instrument variable", inst_name, "not found in data frame."))
    }
    inst_series <- data[[inst_name]]
  } else {
    # If instrument is provided as a vector (numeric)
    if (length(instrument) != T) {
      stop("Instrument vector length must equal number of rows in data.")
    }
    inst_series <- instrument
    inst_name <- "instrument"  # generic name if not a data column
  }
  
  # Set up control variables (all data columns as controls, excluding the instrument itself if it's among data columns)
  control_vars <- colnames(data)
  if (!include_instrument_in_controls && !is.null(inst_name) && inst_name %in% control_vars) {
    # Remove instrument column from lagged controls to avoid using the shock itself as a control
    control_vars <- setdiff(control_vars, inst_name)
  }
  
  # Initialize result matrices: rows = outcome variables, cols = horizons 0..H
  outcome_count <- length(outcome_vars)
  horizons <- 0:H
  n_horizons <- length(horizons)
  IRF   <- matrix(NA, nrow = outcome_count, ncol = n_horizons)
  Lower <- matrix(NA, nrow = outcome_count, ncol = n_horizons)
  Upper <- matrix(NA, nrow = outcome_count, ncol = n_horizons)
  SE    <- matrix(NA, nrow = outcome_count, ncol = n_horizons)
  Nobs  <- matrix(NA, nrow = outcome_count, ncol = n_horizons)
  rownames(IRF) <- outcome_vars
  rownames(Lower) <- outcome_vars
  rownames(Upper) <- outcome_vars
  rownames(SE) <- outcome_vars
  rownames(Nobs) <- outcome_vars
  colnames(IRF)   <- paste0("h", horizons)
  colnames(Lower) <- paste0("h", horizons)
  colnames(Upper) <- paste0("h", horizons)
  colnames(SE)    <- paste0("h", horizons)
  colnames(Nobs)  <- paste0("h", horizons)
  
  # Critical value for confidence intervals (two-tailed)
  alpha <- 1 - conf_level
  crit_val <- qnorm(1 - alpha/2)  # using normal approximation for large-sample HAC errors
  
  # Loop over each specified outcome variable
  for (i in seq_along(outcome_vars)) {
    outcome_name <- outcome_vars[i]
    y_full <- data[[outcome_name]]
    
    # Loop over horizons 0 to H
    for (h in horizons) {
      # Determine regression sample indices for given horizon h
      # t.index represents the time indices (t) for which we have data for lags and instrument at time t, and outcome at t+h
      start_idx <- p + 1
      end_idx   <- T - h
      if (end_idx < start_idx) {
        # Not enough data for this horizon (and beyond), break out of horizon loop
        break
      }
      t_index <- start_idx:end_idx
      
      # Construct regression data frame for this horizon and outcome
      # Dependent variable: y at t+h
      y_dep <- y_full[t_index + h]
      reg_df <- data.frame(y = y_dep)
      # Add instrument at time t
      reg_df[[inst_name]] <- inst_series[t_index]
      # Add lagged controls for each variable in control_vars
      for (var in control_vars) {
        var_series <- data[[var]]
        # Add lags 1 to p of this variable as separate columns
        for (lag in 1:p) {
          col_name <- paste0(var, "_L", lag)
          reg_df[[col_name]] <- var_series[t_index - lag]
        }
      }
      
      # Add deterministic terms if specified
      if (c_case == 2) {
        # constant (intercept) will be added by default in lm formula, so just add trend term
        # Use global time index as trend (to maintain consistency across horizons)
        reg_df$trend <- (t_index)  # a linear trend corresponding to the time index
      }
      
      # Set up regression formula
      formula_str <- "y ~ ."
      if (c_case == 0) {
        formula_str <- paste(formula_str, "- 1")  # exclude intercept
      }
      
      model <- lm(as.formula(formula_str), data = reg_df, na.action = na.omit)
      
      # Compute standard errors based on type
      if (se_type == "HAC") {
        # Newey-West robust standard errors
        cov_hac <- sandwich::NeweyWest(model, lag = h, prewhite = FALSE, adjust = TRUE)
      } else {
        # Driscoll-Kraay standard errors
        cov_hac <- sandwich::vcovHC(model, type = "HC1")
      }
      
      # Extract coefficient and SE for the instrument regressor
      beta_hat <- coef(model)[[inst_name]]
      se_beta  <- sqrt(diag(cov_hac))[inst_name]
      if (is.na(beta_hat) || is.na(se_beta)) {
        # If coefficient is NA (e.g., due to perfect collinearity or no obs), skip storing results
        next
      }
      
      # Store IRF, confidence interval, and additional statistics
      col_idx <- h + 1  # column index (h=0 -> col 1)
      IRF[i, col_idx]   <- beta_hat
      Lower[i, col_idx] <- beta_hat - crit_val * se_beta
      Upper[i, col_idx] <- beta_hat + crit_val * se_beta
      SE[i, col_idx]    <- se_beta
      Nobs[i, col_idx]  <- nobs(model)
    } # end horizon loop
  } # end outcome loop
  
  # Prepare output list
  result <- list(
    IRF          = IRF,
    Lower        = Lower,
    Upper        = Upper,
    SE           = SE,
    Nobs         = Nobs,
    outcome_vars = outcome_vars,
    instrument   = inst_name,
    p            = p,
    H            = H,
    c_case       = c_case,
    conf_level   = conf_level,
    se_type      = se_type,
    include_instrument_in_controls = include_instrument_in_controls
  )
  class(result) <- "LPIV"
  return(result)
}

# Add a print method for the LPIV class
print.LPIV <- function(x, ...) {
  cat("Local Projection IV Results\n")
  cat("-------------------------\n")
  cat("Instrument:", x$instrument, "\n")
  cat("Horizons: 0 to", x$H, "\n")
  cat("Lags:", x$p, "\n")
  cat("Confidence Level:", x$conf_level * 100, "%\n")
  cat("Standard Errors:", x$se_type, "\n")
  cat("Instrument included in controls:", x$include_instrument_in_controls, "\n")
  cat("\nImpulse Response Functions:\n")
  print(x$IRF)
  invisible(x)
}
