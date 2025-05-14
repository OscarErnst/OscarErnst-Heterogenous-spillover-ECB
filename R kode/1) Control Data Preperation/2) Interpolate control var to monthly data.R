# Clear workspace and console
rm(list = ls()); cat("\014")

# -------------------------------------------------------------------------
# 0. Set working directory based on system user
# -------------------------------------------------------------------------
user <- Sys.info()[["user"]]
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "Oscar_dream") {
  setwd("HER_INDSÆT_STI_FOR_OSCAR_DREAM")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# -------------------------------------------------------------------------
# 1. Load helper functions and packages
# -------------------------------------------------------------------------
source(file.path("R kode", "Functions", "Functions.R"))
source(file.path("R kode", "Functions", "Load_Packages.R"))

library(dplyr)
library(zoo)
library(eurostat)
library(tempdisagg)

# -------------------------------------------------------------------------
# 2. Settings
# -------------------------------------------------------------------------
# monthly window
start_date    <- c(2000, 1)   # Year, Month
end_date      <- c(2023, 9)

# quarterly window (for rGDP)
end_date_q    <- c(2024, 4)

geo           <- "EA20"

# convert to numeric "time" for eurostat
start_time    <- start_date[1] + (start_date[2] - 1) / 12
end_time      <- end_date[1]   + (end_date[2]   - 1) / 12

# -------------------------------------------------------------------------
# 3. Get quarterly real GDP (for interpolation)
# -------------------------------------------------------------------------
raw_q <- get_country_dataset(geo) %>% 
  select(year, quarter, rGDP, country)

rGDP_q <- raw_q %>%
  filter(
    year >  start_date[1] | (year == start_date[1] & quarter >= start_date[2]),
    year <  end_date_q[1]  | (year == end_date_q[1]   & quarter <= end_date_q[2])
  ) %>%
  arrange(year, quarter)

rGDP_q_ts <- ts(
  data      = rGDP_q$rGDP,
  start     = start_date,
  end       = end_date_q,
  frequency = 4
)

# -------------------------------------------------------------------------
# 4. Download & filter monthly controls (unemployment & IP)
# -------------------------------------------------------------------------
# Unemployment rate
unemp_data <- suppressMessages(
  get_eurostat(
    "une_rt_m",
    time_format = "num",
    filters = list(
      geo   = geo,
      unit  = "PC_ACT",
      s_adj = "SA",
      sex   = "T",
      age   = "TOTAL"
    ),
    cache = FALSE
  )
) %>%
  filter(time >= start_time, time <= end_time)

unemployment <- ts(
  unemp_data$values,
  start     = start_date,
  frequency = 12
)

# Industrial Production
IP_data <- suppressMessages(
  get_eurostat(
    "sts_inpr_m",
    time_format = "num",
    filters = list(
      geo     = geo,
      s_adj   = "SCA",
      nace_r2 = "B-D",
      unit    = "I15"
    ),
    cache = FALSE
  )
) %>%
  filter(time >= start_time, time <= end_time)

IP <- ts(
  IP_data$values,
  start     = start_date,
  frequency = 12
)

# -------------------------------------------------------------------------
# 5. Interpolate quarterly rGDP → monthly via Chow–Lin
# -------------------------------------------------------------------------
rGDPm_td <- td(
  rGDP_q_ts ~ unemployment + IP,
  to     = 12,
  method = "chow-lin-maxlog"
)

rGDPm_level  <- rGDPm_td$values
rGDPm_logchg <- c(NA, diff(log(rGDPm_level)) * 100)

# build a monthly data.frame for rGDP
dates      <- as.Date(as.yearmon(time(rGDPm_level)))
result_df <- data.frame(
  Date       = dates,
  rGDPm      = as.numeric(rGDPm_level),
  d_rGDP_m   = as.numeric(rGDPm_logchg)
)

# clean up
rm(IP_data, raw_q, rGDP_q, rGDPm_td, unemp_data)

# -------------------------------------------------------------------------
# 6. Download & compute monthly HICP year‑on‑year
# -------------------------------------------------------------------------
hicp_data <- suppressMessages(
  get_eurostat(
    "prc_hicp_midx",
    time_format = "num",
    filters = list(
      geo    = geo,
      coicop = "CP00",
      unit   = "I15"
    ),
    cache = FALSE
  )
) %>%
  filter(time >= start_time, time <= end_time) %>%
  arrange(time)

HICP_monthly_df <- hicp_data %>%
  transmute(
    Date     = as.Date(sprintf("%04.0f-%02.0f-01",
                               floor(time),
                               (time - floor(time)) * 12 + 1)),
    # Year‑on‑year log change to remove seasonality:
    d_HICP_m = c(rep(NA, 12),
                 diff(log(values), lag = 12) * 100)
  )

# -------------------------------------------------------------------------
# 7. Merge monthly series and save
# -------------------------------------------------------------------------
control_monthly <- merge(
  HICP_monthly_df,
  result_df[, c("Date", "d_rGDP_m")],
  by = "Date"
)

saveRDS(
  control_monthly,
  file = file.path("Data", "Interpolated data", "control_var_m.rds")
)

cat("Interpolation complete. Data saved to 'Data/Interpolated data/control_var_m.rds'\n")

