###############################################################################
#  ESTR OIS intradag – 2×2‑plots  (base graphics + klokkeslæt på x‑aksen)    #
###############################################################################

# ---------------------------------------------------------------------------
# 0)  Clear workspace & console, indlæs lpirfs
# ---------------------------------------------------------------------------
rm(list = ls())
cat("\014")

library(lpirfs)
library(readxl);   library(dplyr);  library(purrr)
library(stringr);  library(lubridate); library(readr);  library(hms)

# ---------------------------------------------------------------------------
# 1)  Working directory pr. bruger
# ---------------------------------------------------------------------------
user <- Sys.info()[["user"]]

if      (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# ---------------------------------------------------------------------------
# 2)  Egne hjælpefunktioner  (read_OIS_intra) -------------------------------
# ---------------------------------------------------------------------------
source("Seminar Functions.R")

if (!exists("read_OIS_intra")) {
  # fallback‑version (kort)
  read_OIS_intra <- function(maturity = c("1M","6M","1Y","5Y"),
                             sheet_date,
                             base_dir = file.path("Data","Bloomberg data")) {
    
    file_map <- c(`1M`="ESTR 1M.xlsx", `6M`="ESTR 6M.xlsx",
                  `1Y`="ESTR 1Y.xlsx", `5Y`="ESTR 5Y.xlsx")
    file_path <- file.path(base_dir, file_map[[match.arg(maturity)]])
    df <- read_excel(file_path, sheet = sheet_date, .name_repair="minimal")[,1:3]
    names(df) <- c("Dates","Open","Close")
    
    times <- if(inherits(df$Dates,"POSIXct")) as_hms(df$Dates) else {
      stamp <- str_replace_all(df$Dates,"\\.",":")
      as_hms(as.POSIXct(paste(lubridate::dmy(sheet_date), stamp),
                        format="%Y-%m-%d %H:%M:%S",
                        tz="Europe/Copenhagen"))
    }
    tibble(time  = times,
           open  = readr::parse_number(df$Open,  locale = locale(decimal_mark=",")),
           close = readr::parse_number(df$Close, locale = locale(decimal_mark=",")))
  }
}

# ---------------------------------------------------------------------------
# 3)  Find alle dato‑faner  ---------------------------------------------------
# ---------------------------------------------------------------------------
base_dir  <- file.path("Data","Bloomberg data")
ois_files <- c("ESTR 1M.xlsx","ESTR 6M.xlsx","ESTR 1Y.xlsx","ESTR 5Y.xlsx")
windows   <- ois_files |>
  map(~ excel_sheets(file.path(base_dir, .x))) |>
  unlist() |> unique() |> sort()
print(windows)

# ---------------------------------------------------------------------------
# 4)  Plot‑hjælpere  ----------------------------------------------------------
# ---------------------------------------------------------------------------
# konverter klokkeslæt til positions‑index
vline_pos <- function(df, times_chr){
  which(format(df$time, "%H:%M:%S") %in% times_chr)
}

# grid‑breaks til x‑aksen (hver halve time)
tick_pos  <- function(df){
  idx <- which(minute(df$time) %% 30 == 0 & second(df$time)==0)
  if (length(idx) == 0) idx <- pretty(seq_along(df$time), n = 4)
  idx
}

# ---------------------------------------------------------------------------
# 5)  Loop over dato‑faner -> base‑PNG  --------------------------------------
# ---------------------------------------------------------------------------
out_dir <- "figures_base"
if(!dir.exists(out_dir)) dir.create(out_dir)

for (sheet_date in windows) {
  
  cat(">> Plotter", sheet_date, "\n")
  
  d_1M <- read_OIS_intra("1M", sheet_date)
  d_6M <- read_OIS_intra("6M", sheet_date)
  d_1Y <- read_OIS_intra("1Y", sheet_date)
  d_5Y <- read_OIS_intra("5Y", sheet_date)
  
  png(file.path(out_dir, paste0("OIS_intraday_", sheet_date, ".png")),
      width = 1800, height = 1800, res = 300)
  
  par(mfrow = c(2,2), mar = c(4,4,3,1),
      cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.1)
  
  plot_list <- list(`1M`=d_1M, `6M`=d_6M, `1Y`=d_1Y, `5Y`=d_5Y)
  
  for(mat in names(plot_list)){
    df <- plot_list[[mat]]
    
    # plot close‑priser
    plot(df$close, type = "l", lwd = 2, col = "darkblue",
         main = paste(mat, "ESTR OIS Intraday"),
         xlab = "Hour", ylab = "pct",
         xaxt = "n")                      # sluk default x‑akse
    
    # x‑ticks
    tpos <- tick_pos(df)
    axis(1, at = tpos, labels = format(df$time[tpos], "%H:%M:%S"))
    
    # vertikale linjer (på index‑basis)
    vpos <- vline_pos(df, c("14:00:00","15:00:00","14:15:00","14:45:00"))
    abline(v = vpos[c(1,4)], col = "black", lty = 2, lwd = 2)   # sort stiplet
    abline(v = vpos[c(2,3)], col = "darkred", lty = 1, lwd = 2) # rød fuld
    
    grid(nx = NA, ny = NULL, col = "grey85")
  }
  
  dev.off()
}

cat("Alle plots gemt i:", normalizePath(out_dir), "\n")

