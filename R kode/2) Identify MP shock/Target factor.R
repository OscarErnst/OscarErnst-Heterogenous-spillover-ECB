#########################################################################
# Cleaned-Up Code for Identifying and Aggregating Pure Target Shocks
#########################################################################

# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on system user
user <- Sys.info()[["user"]]

if (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB")
} else if (user == "Oscar_dream") {
  setwd("HER_INDSÆT_STI_FOR_OSCAR_DREAM")
} else if (user == "Kasper") {
  setwd("HER_INDSÆT_STI_FOR_KASPER her")
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
aggregate_method <- "mean"
window <- "monetary event"
Baseline <- TRUE
burgundy <- "#760020"
burgundy_trans <- rgb(118/255, 0, 32/255, alpha = 0.3)
HFI_variables <- c("date", "OIS_3M")
GK2015 <- FALSE
crisis_date <- "2008-09-04"

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

all_dates <- expand.grid(year = 1999:2024, month = 1:12)
merged_data <- merge(all_dates, factors_scaled, by = c("year", "month"), all = TRUE) %>%
  arrange(year, month) %>%
  mutate(Target = replace_na(Target, 0),
         cumulative_Target = cumsum(Target))

data_monthly <- merged_data %>%
  mutate(YearMonth = sprintf("%04d-%02d", year, month)) %>%
  group_by(YearMonth) %>%
  summarise(monthly_average_Target = mean(cumulative_Target, na.rm = TRUE), .groups = "drop") %>%
  mutate(monthly_diff_Target = c(NA, diff(monthly_average_Target)))

Target_m <- ts(replace_na(data_monthly$monthly_diff_Target, 0), start = c(1999, 1), frequency = 12)

# -----------------------------
# Relevance Regression
# -----------------------------
FinVar <- read_excel(file.path("data", "Dataset_EA-MPD.xlsx"), sheet = "Monetary Event Window") %>%
  replace(is.na(.), 0) %>%
  dplyr::select(OIS_3M, STOXX50)

shock <- factors_scaled$Target
regdata <- cbind(FinVar, shock)
m1 <- lm(OIS_3M ~ shock, data = regdata)
stargazer(m1, type = "text", title = "Relevance of Target Shock", align = TRUE)

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
  mutate(YearMonth = format(Date, "%Y-%m"),
         year = as.integer(substr(YearMonth, 1, 4)),
         month = as.integer(substr(YearMonth, 6, 7)))

all_dates <- expand.grid(year = 1999:2024, month = 1:12)
merged_data <- merge(all_dates, TotShocks, by = c("year", "month"), all = TRUE) %>%
  arrange(year, month) %>%
  mutate(across(c(InfoCB, pureMP), ~replace_na(., 0)),
         cumulative_pureMP = cumsum(pureMP),
         cumulative_InfoCB = cumsum(InfoCB))

data_monthly <- merged_data %>%
  mutate(YearMonth = sprintf("%04d-%02d", year, month)) %>%
  group_by(YearMonth) %>%
  summarise(
    monthly_average_pureMP = mean(cumulative_pureMP),
    monthly_average_InfoCB = mean(cumulative_InfoCB),
    .groups = "drop"
  ) %>%
  mutate(
    monthly_diff_pureMP = c(NA, diff(monthly_average_pureMP)),
    monthly_diff_InfoCB = c(NA, diff(monthly_average_InfoCB))
  )

pureMP_m <- ts(replace_na(data_monthly$monthly_diff_pureMP, 0), start = c(1999, 1), frequency = 12)
InfoCB_m <- ts(replace_na(data_monthly$monthly_diff_InfoCB, 0), start = c(1999, 1), frequency = 12)

# Ensure 'Shocks' directory exists
if (!dir.exists("Shocks")) dir.create("Shocks")

# Save monthly series
saveRDS(pureMP_m, file = file.path("Shocks", "Target Factor Shock.rds"))
saveRDS(InfoCB_m, file = file.path("Shocks", "Information Shock.rds"))

# -----------------------------
# Quarterly Aggregation
# -----------------------------
data_monthly <- data_monthly %>%
  mutate(
    Year = as.integer(substr(YearMonth, 1, 4)),
    Month = as.integer(substr(YearMonth, 6, 7)),
    Quarter = ceiling(Month / 3)
  )

data_quarterly <- data_monthly %>%
  group_by(Year, Quarter) %>%
  summarise(
    quarterly_pureMP = sum(monthly_diff_pureMP, na.rm = TRUE),
    quarterly_InfoCB = sum(monthly_diff_InfoCB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, Quarter)

pureMP_q <- ts(data_quarterly$quarterly_pureMP, start = c(1999, 1), frequency = 4)
InfoCB_q <- ts(data_quarterly$quarterly_InfoCB, start = c(1999, 1), frequency = 4)

# Save quarterly series
saveRDS(pureMP_q, file = file.path("Shocks", "Target Factor Shock_Quarterly.rds"))
saveRDS(InfoCB_q, file = file.path("Shocks", "Information Shock_Quarterly.rds"))

