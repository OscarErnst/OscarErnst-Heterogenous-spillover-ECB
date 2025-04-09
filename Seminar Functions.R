OIS_data_formatter <- function(OIS_rate, sheet_name) {
  # Load required libraries (you can also load them once outside the function)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  
  # 1) Construct the file path (based on your pattern "ESTR X.xlsx")
  file_path <- paste0(
    "/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/",
    "Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis/",
    "ESTR OIS data/ESTR ", OIS_rate, ".xlsx"
  )
  
  # 2) Read the raw data
  df_raw <- read_excel(file_path, sheet = sheet_name)
  
  # 3) Identify the row that has the date, e.g. "18JUL2024_00:00:00.000000"
  date_row <- df_raw %>%
    filter(str_detect(`Time Interval`, "_00:00:00.000000"))
  
  # 4) Parse out the date (e.g. "18JUL2024") and convert to Date
  #    If that row doesn't exist, default to "1970-01-01" or handle as needed.
  if (nrow(date_row) > 0) {
    # Example "18JUL2024_00:00:00.000000" -> first 9 chars = "18JUL2024"
    date_str_raw <- date_row$`Time Interval`[1]
    date_str     <- str_sub(date_str_raw, 1, 9)        # "18JUL2024"
    # "d%b%Y" means (day)(abbrev-month)(year), e.g. 18JUL2024
    my_date      <- as.Date(date_str, format = "%d%b%Y")
  } else {
    # If no date row found, use a dummy date or stop with an error
    # my_date <- as.Date("1970-01-01")
    stop("No date row found (e.g. '18JUL2024_00:00:00.000000') in Time Interval!")
  }
  
  # 5) Remove the date row and any "Summary" row from the data
  #    Also remove lines with underscores in "Time Interval"
  #    (except the one we used for the date, which we already singled out)
  df_clean <- df_raw %>%
    filter(
      !`Time Interval` %in% c("Summary") &
        !str_detect(`Time Interval`, "_")
    )
  
  # 6) Now transform "HH:MM - HH:MM" into start/end times
  df_final <- df_clean %>%
    # Optionally remove columns you don’t need
    select(-`Tick Count`, -Volume) %>%
    
    # Split "08:20 - 08:25" into start_time_str / end_time_str
    separate(
      `Time Interval`,
      into = c("start_time_str", "end_time_str"),
      sep = " - ",
      remove = FALSE
    ) %>%
    
    # Parse those as lubridate "Period" objects
    mutate(
      start_time = hm(start_time_str),
      end_time   = hm(end_time_str)
    ) %>%
    
    # Create a POSIXct from the parsed date + start HH:MM
    mutate(
      start_time_ct = as.POSIXct(
        paste(my_date, start_time_str),
        format = "%Y-%m-%d %H:%M",
        tz = "UTC"
      )
    )
  
  return(df_final)
}

