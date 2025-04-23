# plot_panel_lpiv_irfs.R

# 0. Clear & packages
rm(list=ls()); cat("\014")
library(lpirfs)
library(dplyr)

# 1. Settings
horizon   <- 8
lags      <- 2
countries <- c("DE","FR","NL","DK","AT","IT","ES","PT","EL")
baseline  <- "DE"
others    <- setdiff(countries, baseline)

# 2. Load data + build country×shock interactions
data <- readRDS("Data/Panel LP-IV/panel_input_data.rds") %>%
  arrange(country, Date)
for(c in others) {
  data[[paste0("shock_",c)]] <- ifelse(data$country==c, data$shock, 0)
}
interaction_vars <- paste0("shock_", others)

plot_panel_irfs <- function(outcome,
                            out_file  = paste0("Graphs/Panel LP-IV/IRFs_", outcome, ".png"),
                            baseline_row = "shock", 
                            col_line  = "#760020",
                            col_band  = rgb(118/255, 0, 32/255, alpha = 0.30),
                            png_w     = 3600,
                            png_h     = 3600,
                            png_res   = 300) {
  
  lp <- lp_lin_panel(
    data_set       = data,
    endog_data     = outcome,
    cumul_mult     = FALSE,
    shock          = "shock",
    panel_model    = "pooling",
    panel_effect   = "time",
    robust_cov     = "vcovSCC",
    c_exog_data    = interaction_vars,
    l_exog_data    = outcome,
    lags_exog_data = lags,
    hor            = horizon,
    confint        = 1.96
  )
  
  coefmat <- function(x) if (is.matrix(x)) x else x$coefficients
  
  if(!(baseline_row %in% rownames(coefmat(lp$reg_summaries[[1]])))) {
    stop("The baseline shock row '", baseline_row, "' was not found.\n",
         "Available rows: ", paste(rownames(coefmat(lp$reg_summaries[[1]])), collapse=", "))
  }
  
  base_coef <- sapply(lp$reg_summaries, function(z) coefmat(z)[baseline_row, 1])
  base_se   <- sapply(lp$reg_summaries, function(z) coefmat(z)[baseline_row, 2])
  
  B <- S <- matrix(0, length(others), horizon, 
                   dimnames=list(others, paste0("h",0:(horizon-1))))
  for(h in 1:horizon) {
    mat <- coefmat(lp$reg_summaries[[h]])
    for(c in others) {
      rowname <- paste0("shock_", c)
      if(rowname %in% rownames(mat)) {
        B[c,h] <- mat[rowname,1]
        S[c,h] <- mat[rowname,2]
      }
    }
  }
  
  png(out_file, width = png_w, height = png_h, res = png_res)
  par(mfrow=c(3,3), mar=c(5,5,3,1),
      cex.axis=0.8, cex.lab=0.9, cex.main=1)
  
  horizons <- 0:(horizon-1)
  
  for (ctry in countries) {
    if(ctry == baseline) {
      irf <- base_coef
      se  <- base_se
    } else {
      irf <- base_coef + B[ctry,]
      se  <- sqrt(base_se^2 + S[ctry,]^2)
    }
    
    low <- irf - 1.96*se
    up  <- irf + 1.96*se
    
    rng <- range(c(low, up))
    buf <- 0.25 * max(abs(rng))
    ylim <- c(rng[1]-buf, rng[2]+buf)
    
    plot(horizons, irf, type="n",
         ylim=ylim, xlab="Horizon (quarters)",
         ylab=paste("IRF of", outcome), main=ctry)
    
    polygon(c(horizons, rev(horizons)), c(low, rev(up)),
            col=col_band, border=NA)
    lines(horizons, irf, col=col_line, lwd=2, pch=16, type="b")
    abline(h=0, lty=2)
    grid()
  }
  dev.off()
  
  message("Saved → ", out_file)
}


dir.create("Graphs/Panel LP-IV", recursive = TRUE, showWarnings = FALSE)

plot_panel_irfs("d_rGDP")   # → Graphs/Panel LP-IV/IRFs_d_rGDP.png
plot_panel_irfs("d_HICP")   # → Graphs/Panel LP-IV/IRFs_d_HICP.png

