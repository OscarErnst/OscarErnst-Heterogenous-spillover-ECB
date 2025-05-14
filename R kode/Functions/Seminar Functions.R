read_OIS_intra <- function(maturity   = c("1M", "6M", "1Y", "5Y"),
                           sheet_date,                    # fx "06-03-25"
                           base_dir   = file.path("Data", "Bloomberg data")) {
  
  # ---- pakker --------------------------------------------------------------
  suppressPackageStartupMessages({
    library(readxl);  library(dplyr);  library(stringr)
    library(lubridate); library(readr); library(hms)
  })
  
  # ---- map løbetid -> filnavn ----------------------------------------------
  file_map <- c(`1M` = "ESTR 1M.xlsx",
                `6M` = "ESTR 6M.xlsx",
                `1Y` = "ESTR 1Y.xlsx",   # 1‑års‑filen hedder ESTR
                `5Y` = "ESTR 5Y.xlsx")
  
  maturity  <- match.arg(maturity)
  file_path <- file.path(base_dir, file_map[[maturity]])
  if (!file.exists(file_path))
    stop("Filen findes ikke: ", file_path)
  
  # ---- læs arket -----------------------------------------------------------
  df_raw <- read_excel(file_path,
                       sheet        = sheet_date,
                       .name_repair = "minimal")[, 1:3]
  names(df_raw) <- c("Dates", "Open", "Close")
  
  # ---- parse klokkeslæt ----------------------------------------------------
  # 1) Hvis Excel allerede gav en datetimE‑kolonne (POSIXct), brug den …
  if (inherits(df_raw$Dates, "POSIXct")) {
    time_vec <- as_hms(df_raw$Dates)
    
    # 2) … ellers er det formater à la "13.05.00" → lav dem selv
  } else if (all(stringr::str_detect(df_raw$Dates, "^\\d{2}\\.\\d{2}\\.\\d{2}$"))) {
    stamp <- stringr::str_replace_all(df_raw$Dates, "\\.", ":")
    my_date <- lubridate::dmy(sheet_date)
    time_vec <- as_hms(
      as.POSIXct(paste(my_date, stamp),
                 format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Copenhagen"))
  } else {
    stop("Ukendt dato/tids‑format i kolonnen 'Dates'")
  }
  
  # ---- parse værdier (håndter evt. komma‑decimal) --------------------------
  open_vec  <- if (is.numeric(df_raw$Open))  df_raw$Open
  else readr::parse_number(df_raw$Open,  locale = locale(decimal_mark = ","))
  close_vec <- if (is.numeric(df_raw$Close)) df_raw$Close
  else readr::parse_number(df_raw$Close, locale = locale(decimal_mark = ","))
  
  # ---- slutresultat --------------------------------------------------------
  tibble::tibble(time = time_vec,
                 open = open_vec,
                 close = close_vec)
}
