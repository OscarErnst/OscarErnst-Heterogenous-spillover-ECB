# Clear environment and console
rm(list = ls())
cat("\014")

user <- Sys.info()[["user"]]

# Set working directory based on user
if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
} else if (user == "Oscar_dream") {
  setwd("HER_INDSÆT_STI_FOR_OSCAR_DREAM")
} else if (user == "Kasper") {
  setwd("HER_INDSÆT_STI_FOR_KASPER")
} else {
  stop("Ukendt bruger – tilføj sti for denne bruger.")
}

# Use relative paths
source(file.path("R kode", "Functions", "Functions.R"))
source(file.path("R kode", "Functions", "Load_Packages.R"))

library(dplyr)
library(zoo)
library(eurostat)

# Indstillinger:
start_date <- c(2001, 1)
end_date   <- c(2024, 1)
countries <- c("EA20")

# Create the full dataset and filter for EA20 for rGDP
data <- purrr::map_dfr(countries, ~ get_country_dataset(.x))
rGDP_q <- data %>% 
  dplyr::select(year, quarter, rGDP, country) %>% 
  filter(country == "EA20", year < 2025) %>% 
  arrange(year, quarter)

# Convert rGDP_q into a quarterly time series object
start_year    <- rGDP_q$year[1]
start_quarter <- rGDP_q$quarter[1]
end_year      <- rGDP_q$year[nrow(rGDP_q)]
end_quarter   <- rGDP_q$quarter[nrow(rGDP_q)]

rGDP_q_ts <- ts(
  data      = rGDP_q$rGDP,
  start     = c(start_year, start_quarter),
  end       = c(end_year, end_quarter),
  frequency = 4
)

# Download monthly indicators from Eurostat
# Unemployment
unemp_data <- get_eurostat("une_rt_m",
                           time_format = "num",
                           filters = list(
                             geo   = "EA20",
                             unit  = "PC_ACT",
                             s_adj = "SA",
                             sex   = "T",
                             age   = "TOTAL"
                           ))
unemployment <- ts(unemp_data$values, start = start_date, frequency = 12)
unemployment <- window(unemployment, start = start_date, end = end_date)

# Industrial Production
IP_data <- get_eurostat("sts_inpr_m",
                        time_format = "num",
                        filters = list(
                          geo     = "EA20",
                          s_adj   = "SCA",
                          nace_r2 = "B-D",
                          unit    = "I15"
                        ))
IP <- ts(IP_data$values, start = c(1953, 1), frequency = 12)
IP <- window(IP, start = start_date, end = end_date)

# Interpolate rGDP using Chow-Lin
rGDPm_td <- td(rGDP_q_ts ~ unemployment + IP, to = 12, method = "chow-lin-maxlog")
rGDPm_level <- rGDPm_td$values
rGDPm_logchg <- c(NA, diff(log(rGDPm_level)) * 100)

# Process HICP
HICP_q <- data %>% 
  dplyr::select(year, quarter, HICP, country) %>% 
  filter(country == "EA20", year < 2025) %>% 
  arrange(year, quarter)

start_year    <- HICP_q$year[1]
start_quarter <- HICP_q$quarter[1]
end_year      <- HICP_q$year[nrow(HICP_q)]
end_quarter   <- HICP_q$quarter[nrow(HICP_q)]

HICP_q_ts <- ts(
  data      = HICP_q$HICP,
  start     = c(start_year, start_quarter),
  end       = c(end_year, end_quarter),
  frequency = 4
)

HICPm_td <- td(HICP_q_ts ~ unemployment + IP, to = 12, method = "chow-lin-maxlog")
HICPm_level <- HICPm_td$values
HICPm_logchg <- c(NA, diff(log(HICPm_level)) * 100)

# Create data.frames
dates <- as.Date(as.yearmon(time(rGDPm_level)))
result_df <- data.frame(
  Date         = dates,
  rGDPm_level  = as.numeric(rGDPm_level),
  rGDPm_logchg = as.numeric(rGDPm_logchg)
)

dates_HICP <- as.Date(as.yearmon(time(HICPm_level)))
HICP_monthly_df <- data.frame(
  Date         = dates_HICP,
  HICPm_level  = as.numeric(HICPm_level),
  HICPm_logchg = as.numeric(HICPm_logchg)
)

# Merge and rename
d <- merge(HICP_monthly_df[, c("Date", "HICPm_logchg")],
           result_df[, c("Date", "rGDPm_logchg")],
           by = "Date") %>%
  rename(
    d_HICP_m = HICPm_logchg,
    d_rGDP_m = rGDPm_logchg
  )

# Save with relative path
saveRDS(d, file = file.path("Data", "Interpolated data", "control_var_m.rds"))

cat("Interpolation complete. Data saved to 'Data/Interpolated data/control_var_m.rds'\n")

