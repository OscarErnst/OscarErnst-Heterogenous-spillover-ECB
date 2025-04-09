## [description...]
## Lastest update: 31-03-2025
## - TheErnie27 & KASP functions for seminar

cat("Loading and installing packages...","\n")

## Include function sources
if (require("AER") == TRUE){
  library("AER")
} else{
  install.packages("AER")
  library("AER")
}

if (require("car") == TRUE){
  library("car")
} else{
  install.packages("car")
  library("car")
}

if (require("data.table") == TRUE){
  library("data.table")
} else{
  install.packages("data.table")
  library("data.table")
}

if (require("dynlm") == TRUE){
  library("dynlm")
} else{
  install.packages("dynlm")
  library("dynlm")
}

if (require("eurostat") == TRUE){
  library("eurostat")
} else{
  install.packages("eurostat")
  library("eurostat")
}

if (require("lmtest") == TRUE){
  library("lmtest")
} else{
  install.packages("lmtest")
  library("lmtest")
}

if (require("lubridate") == TRUE){
  library("lubridate")
} else{
  install.packages("lubridate")
  library("lubridate")
}

if (require("MASS") == TRUE){
  library("MASS")
} else{
  install.packages("MASS")
  library("MASS")
}

if (require("NlcOptim") == TRUE){
  library("NlcOptim")
} else{
  install.packages("NlcOptim")
  library("NlcOptim")
}

if (require("purrr") == TRUE){
  library("purrr")
} else{
  install.packages("purrr")
  library("purrr")
}

if (require("readxl") == TRUE){
  library("readxl")
} else{
  install.packages("readxl")
  library("readxl")
}

if (require("stringr") == TRUE){
  library("stringr")
} else{
  install.packages("stringr")
  library("stringr")
}

if (require("tempdisagg") == TRUE){
  library("tempdisagg")
} else{
  install.packages("tempdisagg")
  library("tempdisagg")
}

if (require("tidyr") == TRUE){
  library("tidyr")
} else{
  install.packages("tidyr")
  library("tidyr")
}

if (require("xtable") == TRUE){
  library("xtable")
} else{
  install.packages("xtable")
  library("xtable")
}

if (require("zoo") == TRUE){
  library("zoo")
} else{
  install.packages("zoo")
  library("zoo")
}

if (require("dplyr") == TRUE){
  library("dplyr")
} else{
  install.packages("dplyr")
  library("dplyr")
}

