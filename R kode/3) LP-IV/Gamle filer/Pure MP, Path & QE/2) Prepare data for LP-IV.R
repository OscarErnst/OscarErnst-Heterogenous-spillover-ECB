# Clear workspace and console
rm(list = ls()); cat("\014")

# -------------------------------------------------------------------------
# 0. User settings: sampling window & working dir
# -------------------------------------------------------------------------
# specify your start/end in year & quarter
start_year    <- 2006
start_quarter <- 1
end_year      <- 2019
end_quarter   <- 4

# derive actual Date‑objects for filtering
# quarters start in Jan, Apr, Jul, Oct → months = 1,4,7,10
q2month <- function(q) (q - 1) * 3 + 1
start_date <- as.Date(sprintf("%04d-%02d-01", start_year,    q2month(start_quarter)))
end_date   <- as.Date(sprintf("%04d-%02d-01", end_year,      q2month(end_quarter)))

# working directory
user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "B362561") {
  setwd("C:/Users/B362561/Desktop/OscarErnst-Heterogenous-spillover-ECB-3")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# -------------------------------------------------------------------------
# 1. Load packages
# -------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(readxl)

# -------------------------------------------------------------------------
# 2. Load Controls (quarterly panel)
# -------------------------------------------------------------------------
control <- readRDS(file.path("Data", "Control Variables", "Eurozone_country_variables.rds")) %>%
  # keep only our sample window
  filter(Date >= start_date, Date <= end_date) %>%
  select(country, Date, d_HICP, d_rGDP, d_Consumption,
         HICP_log, rGDP_log, Consumption_log)

# -------------------------------------------------------------------------
# 3. Load & aggregate Bund yield to quarter
# -------------------------------------------------------------------------
size_of_bund <- "2Y"
bund_yield <- read_excel(file.path("Data", "Generic Bundesbank yield.xlsx")) %>%
  mutate(Date = as.Date(Date),
         # floor all to quarter start
         Date = floor_date(Date, unit = "quarter")) %>%
  group_by(Date) %>%
  summarise(
    bund_yield = mean(.data[[size_of_bund]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Date >= start_date, Date <= end_date)

# -------------------------------------------------------------------------
# 4. Merge controls + Bund
# -------------------------------------------------------------------------
data <- left_join(control, bund_yield, by = "Date")
rm(control, bund_yield)

# -------------------------------------------------------------------------
# 5. Load & window shock (ts object → data.frame)
# -------------------------------------------------------------------------
# read the pre‑built quarterly ts
shock_ts <- readRDS(file.path("Data","LP-IV","PureMP, Path & QE","1.stage_instrument_full_ts.rds"))

# restrict to our window
shock_ts <- window(shock_ts,
                   start = c(start_year, start_quarter),
                   end   = c(end_year,   end_quarter))

# build a Date vector for each quarter
dates_q <- seq(start_date,
               by = "quarter",
               length.out = length(shock_ts))

shock_df <- tibble(
  Date  = dates_q,
  shock = as.numeric(shock_ts)
)

# -------------------------------------------------------------------------
# 6. Save out
# -------------------------------------------------------------------------
output_dir <- file.path("Data", "LP-IV", "PureMP, Path & QE")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

saveRDS(data,     file = file.path(output_dir, "input_data.rds"))
saveRDS(shock_df, file = file.path(output_dir, "shocks.rds"))

cat("Saved input_data.rds and shock.rds for window ",
    as.character(start_date), "–", as.character(end_date), "\n")


