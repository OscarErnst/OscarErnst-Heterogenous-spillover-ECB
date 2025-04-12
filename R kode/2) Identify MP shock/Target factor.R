# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on system user
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

# Load required packages
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(zoo)
library(readxl)
library(stargazer)
library(data.table)
library(broom)
library(sandwich)

# -----------------------------
# Settings and Global Variables
# -----------------------------
monthly <- TRUE
window <- "monetary event"
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)
HFI_variables <- c("date", "OIS_3M")

# -----------------------------
# Load Data
# -----------------------------
press <- read_excel(file.path("data", "Dataset_EA-MPD.xlsx"), sheet = "Press Release Window") %>%
  replace(is.na(.), 0) %>%
  dplyr::select(all_of(HFI_variables))

conf <- read_excel(file.path("data", "Dataset_EA-MPD.xlsx"), sheet = "Press Conference Window") %>%
  replace(is.na(.), 0) %>%
  dplyr::select(all_of(HFI_variables))

me <- read_excel(file.path("data", "Dataset_EA-MPD.xlsx"), sheet = "Monetary Event Window") %>%
  replace(is.na(.), 0) %>%
  dplyr::select(all_of(HFI_variables))

data <- if (window == "release") press else if (window == "monetary event") me else conf

# -----------------------------
# Extract and Process Target Shocks
# -----------------------------
factors_scaled <- data %>%
  mutate(Target = OIS_3M) %>%
  dplyr::select(date, Target)

factors_scaled$Date <- as.Date(factors_scaled$date)
factors_scaled$YearMonth <- format(factors_scaled$Date, "%Y-%m")
factors_scaled$year <- as.integer(substr(factors_scaled$YearMonth, 1, 4))
factors_scaled$month <- as.integer(substr(factors_scaled$YearMonth, 6, 7))

################################################################################
all_dates <- seq.Date(
  from = as.Date("1999-01-01"),
  to   = as.Date("2024-12-31"),
  by   = "day"
)
all_dates_df <- data.frame(date = all_dates)

merged_df <- left_join(all_dates_df, factors_scaled, by = "date")

merged_df <- merged_df %>%
  mutate(
    Target       = replace_na(Target, 0),
    acc_target = cumsum(Target)
  )

monthly_target <- merged_df %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(target = mean(acc_target, na.rm = TRUE)) %>%
  ungroup()

# Instrument by taking first diff
instrument <- monthly_target %>%
  arrange(month) %>%
  mutate(target = target - lag(target, 1))

saveRDS(instrument, file = file.path(getwd(), "Instrumenter", "Pure MP", "KAWK_shock.rds"))

################################################################################
# -----------------------------
# Relevance Regression
# -----------------------------
FinVar <- read_excel(file.path("data", "Dataset_EA-MPD.xlsx"), sheet = "Monetary Event Window") %>%
  replace(is.na(.), 0) %>%
  dplyr::select(OIS_3M, STOXX50)

shock <- factors_scaled$Target
regdata <- cbind(FinVar, shock)
# m1 <- lm(OIS_3M ~ shock, data = regdata)
# stargazer(m1, type = "text", title = "Relevance of Target Shock", align = TRUE)

# -----------------------------
# Information Shock Filtering
# -----------------------------
stockm <- FinVar$STOXX50

png(filename = file.path(getwd(), "Graphs", "Identify MP shock", "Info_vs_MP_shock.png"),
    width = 6, height = 4, units = "in", res = 300)

plot(shock, stockm, pch = 19, col = burgundy,
     xlab = "Target Factor", ylab = "Change in STOXX50 (%)",
     main = "Target Shock vs. Stock Market Response")
abline(h = 0, col = "gray", lty = 2)
abline(v = 0, col = "gray", lty = 2)
points(shock[shock > 0 & stockm > 0], stockm[shock > 0 & stockm > 0], col = "black", pch = 19)
points(shock[shock < 0 & stockm < 0], stockm[shock < 0 & stockm < 0], col = "black", pch = 19)
legend("bottomleft", legend = c("Information shock", "Monetary policy shock"),
       pch = 19, col = c("black", burgundy), cex = 0.8)
dev.off()

pureMP <- ifelse(shock * stockm < 0, shock, 0)
InfoCB <- ifelse(shock * stockm > 0, shock, 0)
print(sum(pureMP == 0) / length(pureMP))

# -----------------------------
# Monthly Aggregation
# -----------------------------
Date <- as.Date(factors_scaled$date)
TotShocks <- data.frame(Date = Date, InfoCB = InfoCB, pureMP = pureMP) %>%
  mutate(
    YearMonth = format(Date, "%Y-%m"),
    year = as.integer(substr(YearMonth, 1, 4)),
    month = as.integer(substr(YearMonth, 6, 7))
  )

# Create a complete grid of year and month combinations
all_dates <- expand.grid(year = 1999:2024, month = 1:12)

# Merge with TotShocks and arrange the data in chronological order
merged_data <- merge(all_dates, TotShocks, by = c("year", "month"), all = TRUE) %>%
  arrange(year, month) %>%
  mutate(
    across(c(InfoCB, pureMP), ~replace_na(., 0)),
    cumulative_pureMP = cumsum(pureMP),
    cumulative_InfoCB = cumsum(InfoCB)
  )

# -----------------------------
# Monthly Aggregation (Data Frame) Matching the TS Approach
# -----------------------------
# 1. Calculate monthly means of the *cumulative* series.
# 2. Difference those monthly means (like c(NA, diff(...))).
# 3. Save two separate data frames: one for pureMP, one for InfoCB.
monthly_summary <- merged_data %>%
  mutate(YearMonth = sprintf("%04d-%02d", year, month)) %>%
  group_by(YearMonth) %>%
  summarise(
    avg_cum_pureMP = mean(cumulative_pureMP, na.rm = TRUE),
    avg_cum_InfoCB = mean(cumulative_InfoCB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YearMonth) %>%
  mutate(
    diff_pureMP = avg_cum_pureMP - lag(avg_cum_pureMP, 1),
    diff_InfoCB = avg_cum_InfoCB - lag(avg_cum_InfoCB, 1),
    # Replace first-month NAs with 0 so it matches the TS approach
    diff_pureMP = tidyr::replace_na(diff_pureMP, 0),
    diff_InfoCB = tidyr::replace_na(diff_InfoCB, 0)
  ) %>%
  # Create a Date column (first day of each month)
  mutate(Date = as.POSIXct(paste0(YearMonth, "-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S"))

# Make separate data frames for pureMP & InfoCB
data_monthly_pureMP <- monthly_summary %>%
  dplyr::select(Date, target = diff_pureMP)

data_monthly_InfoCB <- monthly_summary %>%
  dplyr::select(Date, target = diff_InfoCB)

# Create the output directory if it doesn't exist
if (!dir.exists("Instrumenter")) dir.create("Instrumenter")

# Save the monthly results data frame (pureMP + InfoCB separately)
saveRDS(data_monthly_pureMP, file = file.path("Instrumenter", "Pure MP", "PureMP_Shocks_m.rds"))
saveRDS(data_monthly_InfoCB, file = file.path("Instrumenter", "Pure MP", "InfoCB_Shocks_m.rds"))

# -----------------------------
# Quarterly Aggregation
# -----------------------------
# Summation of monthly differences across each quarter, for each shock
data_quarterly_pureMP <- data_monthly_pureMP %>%
  mutate(
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m")),
    Quarter = ceiling(Month / 3)
  ) %>%
  group_by(Year, Quarter) %>%
  summarise(
    target = sum(target, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Date = as.POSIXct(
      sprintf("%04d-%02d-01 00:00:00", Year, (Quarter - 1) * 3 + 1),
      format = "%Y-%m-%d %H:%M:%S"
    )
  ) %>%
  dplyr::select(Date, target)

data_quarterly_InfoCB <- data_monthly_InfoCB %>%
  mutate(
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m")),
    Quarter = ceiling(Month / 3)
  ) %>%
  group_by(Year, Quarter) %>%
  summarise(
    target = sum(target, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Date = as.POSIXct(
      sprintf("%04d-%02d-01 00:00:00", Year, (Quarter - 1) * 3 + 1),
      format = "%Y-%m-%d %H:%M:%S"
    )
  ) %>%
  dplyr::select(Date, target)

# Save the quarterly results (pureMP + InfoCB separately)
saveRDS(data_quarterly_pureMP, file = file.path("Instrumenter", "Pure MP", "PureMP_Shocks_q.rds"))
saveRDS(data_quarterly_InfoCB, file = file.path("Instrumenter", "Pure MP", "InfoCB_Shocks_q.rds"))

