# -------------------------------------------------------------
# 0. House‑keeping
# -------------------------------------------------------------
rm(list = ls()); cat("\014")

user <- Sys.info()[["user"]]
if      (user == "OscarEAM") {
  setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB")
} else if (user == "kasper") {
  setwd("/Users/kasper/Documents/GitHub/OscarErnst-Heterogenous-spillover-ECB")
} else {
  stop("Ukendt bruger – tilføj sti.")
}

source(file.path("R kode","Functions","Functions.R"))
source(file.path("R kode","Functions","Load_Packages.R"))

library(dplyr)
library(purrr)
library(eurostat)
library(readr)      # write_file()

# -------------------------------------------------------------
# 1. Country set and sample window
# -------------------------------------------------------------
countries <- c("DE", "FR", "NL", "DK", "AT",    # core
               "IT", "ES", "PT", "EL")    # periphery (EL = Greece)

start_year <- 2005
end_year   <- 2020        # filter uses < end_year  → last obs = 2019 Q4

# -------------------------------------------------------------
# 2. Download / merge raw Eurostat data
# -------------------------------------------------------------
raw_data <- map_dfr(countries, get_country_dataset)   # custom helper

raw_data <- raw_data |>
  mutate(
    HICP_log        = 100 * log(HICP),
    rGDP_log        = 100 * log(rGDP),
    Consumption_log = 100 * log(Consumption)
  )

panel <- calc_log_yoy_change(raw_data,
                             vars = c("HICP","rGDP","Consumption")) |>
  filter(year >= start_year,
         year <  end_year) |>
  rename(
    d_HICP        = HICP_yoy_log,
    d_rGDP        = rGDP_yoy_log,
    d_Consumption = Consumption_yoy_log
  )

# -------------------------------------------------------------
# 3. Descriptive statistics (mean & sd across all countries / quarters)
# -------------------------------------------------------------
# -------------------------------------------------------------
# country‑level descriptive statistics
# -------------------------------------------------------------
library(tidyr)  # pivot_wider

desc_country <- panel |>
  group_by(country) |>
  summarise(
    Real_GDP_pc = sprintf("%0.3f (%0.3f)",
                          mean(d_rGDP, na.rm = TRUE),
                          sd(  d_rGDP, na.rm = TRUE)),
    HICP        = sprintf("%0.3f (%0.3f)",
                          mean(d_HICP, na.rm = TRUE),
                          sd(  d_HICP, na.rm = TRUE)),
    HFCE        = sprintf("%0.3f (%0.3f)",
                          mean(d_Consumption, na.rm = TRUE),
                          sd(  d_Consumption, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(country)

# -------------------------------------------------------------
# write LaTeX table  (Table B.2)
# -------------------------------------------------------------
latex_rows <- apply(desc_country, 1, function(row)
  paste(row["country"], "&", row["Real_GDP_pc"], "&",
        row["HICP"], "&", row["HFCE"], "\\\\")
)

latex_table <- paste0(
  "\\begin{table}[ht]\n\\centering\n",
  "\\caption{Descriptive statistics by country (2005Q1–2019Q4)}\\label{tab:desc_country}\n",
  "\\begin{tabular}{lccc}\n\\toprule\n",
  "Country & Real GDP pc & HICP & HFCE \\\\\n\\midrule\n",
  paste(latex_rows, collapse = "\n"), "\n",
  "\\bottomrule\n\\end{tabular}\n\\end{table}"
)

#write_file(latex_table, "tables/desc_country.tex")


# -------------------------------------------------------------
# 4. Write LaTeX-ready table  (Table \ref{tab:desc_main})
# -------------------------------------------------------------
latex_table <- paste0(
  "\\begin{table}[ht]\n\\centering\n",
  "\\caption{Descriptive statistics, eight-country panel (2005Q1–2019Q4)}\\label{tab:desc_main}\n",
  "\\begin{tabular}{lccc}\n\\toprule\n",
  "Variable & Mean (sd) \\\\\n\\midrule\n",
  "Real GDP per capita & ", desc_tbl$`Real GDP per capita`, " \\\\\n",
  "HICP & ",                 desc_tbl$HICP,                  " \\\\\n",
  "HFCE & ",                 desc_tbl$HFCE,                  " \\\\\n",
  "\\bottomrule\n\\end{tabular}\n\\end{table}"
)

#write_file(latex_table, "tables/desc_main.tex")
