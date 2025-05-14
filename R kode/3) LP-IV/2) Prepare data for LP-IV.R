# Clear workspace and console
rm(list = ls()); cat("\014")

# -------------------------------------------------------------------------
# 0. User settings: sampling window & working dir
# -------------------------------------------------------------------------
# specify your start/end in year & quarter
start_year    <- 2005
start_quarter <- 1
start_month   <- 1
end_year      <- 2019
end_quarter   <- 4
end_month     <- 12

# derive actual Date‑objects for filtering
# quarters start in Jan, Apr, Jul, Oct → months = 1,4,7,10
q2month <- function(q) (q - 1) * 3 + 1
start_date <- as.Date(sprintf("%04d-%02d-01", start_year,    q2month(start_quarter)))
end_date   <- as.Date(sprintf("%04d-%02d-01", end_year,      q2month(end_quarter)))
#end_month   <- as.Date(sprintf("%04d-%02d-01", end_year,      q2month(end_month)))

# working directory
user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/OscarErnst-Heterogenous-spillover-ECB")
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

load_data <- function(path, msg) {
  if (!file.exists(path)) stop(msg)
  readRDS(path)
}


# -------------------------------------------------------------------------
# 2. Load Controls (quarterly panel)
# -------------------------------------------------------------------------
control_q <- readRDS(file.path("Data", "Control Variables", "Eurozone_country_variables.rds")) %>%
  # keep only our sample window
  filter(Date >= start_date, Date <= end_date) %>%
  dplyr::select(country, Date, d_HICP, d_rGDP, d_Consumption,
         HICP_log, rGDP_log, Consumption_log)

control_m <- load_data(
  file.path("Data","Interpolated data","control_var_m_sample.rds"),
  "control_var_m.rds not found"
) %>% filter(Date >= start_date)

# -------------------------------------------------------------------------
# 3. Load & aggregate Bund yield to quarter
# -------------------------------------------------------------------------
size_of_bund <- "2Y"
bund_yield <- read_excel(file.path("Data","Additional", "Generic Bundesbank yield.xlsx"))
bund_yield_m <- bund_yield %>% dplyr::select(Date, "2Y") %>% rename("bund_yield" = "2Y")

bund_yield_q <- bund_yield %>%
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
data_q <- left_join(control_q, bund_yield_q, by = "Date")
data_m <- left_join(control_m, bund_yield_m, by = "Date")
rm(control_q, control_m, bund_yield_q, bund_yield_m)

# -------------------------------------------------------------------------
# 5. Load & window shock (ts object → data.frame)
# -------------------------------------------------------------------------
# read the pre‑built quarterly ts
shock_ts_q <- readRDS(file.path("Data","LP-IV","1.stage_instrument_q.rds"))
shock_ts_m <- readRDS(file.path("Data","LP-IV","1.stage_instrument_m.rds"))

# restrict to our window
shock_ts_q <- window(shock_ts_q,
                   start = c(start_year, start_quarter),
                   end   = c(end_year,   end_quarter))


# build a Date vector for each quarter
dates_q <- seq(start_date,
               by = "quarter",
               length.out = length(shock_ts_q))
dates_m <- seq(start_date,
               by = "month",
               length.out = length(shock_ts_m))
shock_df_q <- tibble(
  Date  = dates_q,
  shock = as.numeric(shock_ts_q)
)
shock_df_m <- tibble(
  Date  = dates_m,
  shock = as.numeric(shock_ts_m)
)

# -------------------------------------------------------------------------
# 6. Save out
# -------------------------------------------------------------------------
output_dir <- file.path("Data", "LP-IV")

saveRDS(data_q,     file = file.path(output_dir, "input_data_q.rds"))
saveRDS(data_m,     file = file.path(output_dir, "input_data_m.rds"))

saveRDS(shock_df_q, file = file.path(output_dir, "shocks_q.rds"))
saveRDS(shock_df_m, file = file.path(output_dir, "shocks_m.rds"))


cat("Saved input_data.rds and shock.rds for window ",
    as.character(start_date), "–", as.character(end_date), "\n")


