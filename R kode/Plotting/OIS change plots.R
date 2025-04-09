rm(list = ls())
cat("\014")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

#----------------------------------------------------------------------------
# 1) Load your data (assumes OIS_data_formatter is in "Seminar Functions.R")
#----------------------------------------------------------------------------
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis")
source("Seminar Functions.R")

d_1W <- OIS_data_formatter("1W", "Worksheet (6)")
d_1Y <- OIS_data_formatter("1Y", "Worksheet (6)")
d_5Y <- OIS_data_formatter("5Y", "Worksheet (6)")

# Directory to save PNG files
save_path <- file.path(
  "/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen",
  "Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis",
  "Graphs",
  "OIS rate changes"
)

#----------------------------------------------------------------------------
# 2) Helper to determine x-axis range (12:00-17:00) from each dataset's first row
#----------------------------------------------------------------------------
get_intraday_limits <- function(df) {
  my_date <- as.Date(df$start_time_ct[1])
  start_lim <- as.POSIXct(paste(my_date, "12:00:00"), tz = "UTC")
  end_lim   <- as.POSIXct(paste(my_date, "17:00:00"), tz = "UTC")
  c(start_lim, end_lim)
}

# Helper to add the vertical lines at 14:00 (black, dashed),
# 15:00 (black, dashed), 14:15 (darkred, solid), 14:45 (darkred, solid).
add_vertical_lines <- function(df) {
  my_date <- as.Date(df$start_time_ct[1])
  
  # 14:00 black dotted
  abline(
    v   = as.POSIXct(paste(my_date, "14:00:00"), tz="UTC"),
    col = "black",
    lty = 2,
    lwd = 2
  )
  
  # 15:00 black dotted
  abline(
    v   = as.POSIXct(paste(my_date, "15:00:00"), tz="UTC"),
    col = "black",
    lty = 2,
    lwd = 2
  )
  
  # 14:15 darkred solid
  abline(
    v   = as.POSIXct(paste(my_date, "14:15:00"), tz="UTC"),
    col = "darkred",
    lty = 1,
    lwd = 2
  )
  
  # 14:45 darkred solid
  abline(
    v   = as.POSIXct(paste(my_date, "14:45:00"), tz="UTC"),
    col = "darkred",
    lty = 1,
    lwd = 2
  )
}

#----------------------------------------------------------------------------
# 3) Individual Plots
#----------------------------------------------------------------------------

#### 3.1) 1W
png(filename = file.path(save_path, "plot_1W.png"), width = 800, height = 600)

xrange_1W <- get_intraday_limits(d_1W)

plot(
  d_1W$start_time_ct,
  d_1W$Open,
  type = "l",
  lwd  = 2,
  col  = "darkblue",
  main = "1W ESTR OIS Intraday",
  xlab = "Hour",
  ylab = "3M ESTR OIS (pct)",
  xlim = xrange_1W,
  xaxt = "n"
)

# X-axis in HH:MM:SS
axis.POSIXct(side = 1, x = d_1W$start_time_ct, format = "%H:%M:%S")

# Add vertical lines
add_vertical_lines(d_1W)

dev.off()


#### 3.2) 1Y
png(filename = file.path(save_path, "plot_1Y.png"), width = 800, height = 600)

xrange_1Y <- get_intraday_limits(d_1Y)

plot(
  d_1Y$start_time_ct,
  d_1Y$Open,
  type = "l",
  lwd  = 2,
  col  = "darkblue",
  main = "1Y ESTR OIS Intraday",
  xlab = "Hour",
  ylab = "3M ESTR OIS (pct)",
  xlim = xrange_1Y,
  xaxt = "n"
)

axis.POSIXct(side = 1, x = d_1Y$start_time_ct, format = "%H:%M:%S")
add_vertical_lines(d_1Y)

dev.off()


#### 3.3) 5Y
png(filename = file.path(save_path, "plot_5Y.png"), width = 800, height = 600)

xrange_5Y <- get_intraday_limits(d_5Y)

plot(
  d_5Y$start_time_ct,
  d_5Y$Open,
  type = "l",
  lwd  = 2,
  col  = "darkblue",
  main = "5Y ESTR OIS Intraday",
  xlab = "Hour",
  ylab = "3M ESTR OIS (pct)",
  xlim = xrange_5Y,
  xaxt = "n"
)

axis.POSIXct(side = 1, x = d_5Y$start_time_ct, format = "%H:%M:%S")
add_vertical_lines(d_5Y)

dev.off()

# Choose a higher-resolution figure size for Overleaf
png(
  filename = file.path(save_path, "plot_all_in_one.png"),
  width    = 6.5,     # inches wide
  height   = 8,       # inches tall
  units    = "in",    # specify inches
  res      = 300      # 300 dpi (high quality)
)

# Increase text size for title, axis labels, etc.
par(
  mfrow    = c(3, 1), # 3 rows, 1 column
  cex.main = 1.2,     # title (main) text size
  cex.lab  = 1.15,    # xlab, ylab text size
  cex.axis = 1.1      # axis tick label size
)

### 1W
xrange_1W <- get_intraday_limits(d_1W)
plot(
  d_1W$start_time_ct,
  d_1W$Open,
  type = "l", lwd = 2, col = "darkblue",
  main = "1W ESTR OIS Intraday",
  xlab = "Hour",
  ylab = "pct",
  xlim = xrange_1W,
  xaxt = "n"  # hide default axis
)
axis.POSIXct(side = 1, x = d_1W$start_time_ct, format = "%H:%M:%S")
add_vertical_lines(d_1W)

### 1Y
xrange_1Y <- get_intraday_limits(d_1Y)
plot(
  d_1Y$start_time_ct,
  d_1Y$Open,
  type = "l", lwd = 2, col = "darkblue",
  main = "1Y ESTR OIS Intraday",
  xlab = "Hour",
  ylab = "pct",
  xlim = xrange_1Y,
  xaxt = "n"
)
axis.POSIXct(side = 1, x = d_1Y$start_time_ct, format = "%H:%M:%S")
add_vertical_lines(d_1Y)

### 5Y
xrange_5Y <- get_intraday_limits(d_5Y)
plot(
  d_5Y$start_time_ct,
  d_5Y$Open,
  type = "l", lwd = 2, col = "darkblue",
  main = "5Y ESTR OIS Intraday",
  xlab = "Hour",
  ylab = "pct",
  xlim = xrange_5Y,
  xaxt = "n"
)
axis.POSIXct(side = 1, x = d_5Y$start_time_ct, format = "%H:%M:%S")
add_vertical_lines(d_5Y)

dev.off()

