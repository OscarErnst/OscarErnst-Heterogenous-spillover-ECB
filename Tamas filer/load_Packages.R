## [description...]
## Lastest update: 07-06-2017
## - ANK

cat("Loading and installing packages...","\n")

## Include function sources
##source("I:\\R\\include\\misc.R")
##source("I:\\R\\include\\GetVariable.R")


#-------------- Packages --------------
if (require("MASS") == TRUE){
  library("MASS")
} else{
  install.packages("MASS")
  library("MASS")
}
# 
if (require("expm") == TRUE){
  library("expm")
} else{
  install.packages("expm")
  library("expm")
}
# 
if (require("urca") == TRUE){
  library("urca")
} else{
  install.packages("urca")
  library("urca")
}
# 
if (require("vars") == TRUE){
  library("vars")
} else{
  install.packages("vars")
  library("vars")
}

# if (require("glue") == TRUE){
#   library("glue")
# } else{
#   install.packages("glue")
#   library("glue")
# }


# 
if (require("actuar") == TRUE){
  library("actuar")
} else{
  install.packages("actuar")
  library("actuar")
}
# 
if (require("mvtnorm") == TRUE){
  library("mvtnorm")
} else{
  install.packages("mvtnorm")
  library("mvtnorm")
}

if (require("gdata") == TRUE){
  library("gdata")
} else{
  install.packages("gdata")
  library("gdata")
}
# 
if (require("pracma") == TRUE){
  library("pracma")
} else{
  install.packages("pracma")
  library("pracma")
}
# 
if (require("MCMCpack") == TRUE){
  library(MCMCpack)
} else{
  install.packages("MCMCpack")
  library("MCMCpack")
}
# 
if (require("readxl") == TRUE){
 library("readxl")
} else{
 install.packages("readxl")
 library("readxl")
}
# 
if (require("tis") == TRUE){
  library("tis")
} else{
  install.packages("tis")
  library("tis")
}
# 
if (require("PEIP") == TRUE){
  library("PEIP")
} else{
  install.packages("PEIP")
  library("PEIP")
}
# 
if (require("xtable") == TRUE){
  library("xtable")
} else{
  install.packages("xtable")
  library("xtable")
}
# 
if (require("zoo") == TRUE){
  library("zoo")
} else{
  install.packages("zoo")
  library("zoo")
}

if (require("mFilter") == TRUE){
  library("mFilter")
} else{
  install.packages("mFilter")
  library("mFilter")
}

if (require("readxl") == TRUE){
  library("readxl")
} else{
  install.packages("readxl")
  library("readxl")
}



if (require("MSBVAR") == TRUE){
  library("MSBVAR")
}else{
if (require("devtools") ==TRUE){
  library("devtools")
}else{
  install.packages("devtools")
  library("devtools")
}
  install_version("MSBVAR",version="0.9-0",repos="http://cran.us.r-project.org")
  library("MSBVAR")
}

#if (require("MAKRO")==TRUE){
#  library("MAKRO")
#}else{
# library(devtools)
# devtools::install("P:/ank/LP_MAKRO")   ## install the MAKRO package for LP  
#}



# 
if (require("lattice") == TRUE){
  library("lattice")
} else{
  install.packages("lattice")
  library("lattice")
}
# 
if (require("car") == TRUE){
  library("car")
} else{
  install.packages("car")
  library("car")
}
# 
if (require("matrixStats") == TRUE){
  library("matrixStats")
} else{
  install.packages("matrixStats")
  library("matrixStats")
}
# 
if (require("graphics") == TRUE){
  library("graphics")
} else{
  install.packages("graphics")
  library("graphics")
}
# 
if (require("lmtest") == TRUE){
  library("lmtest")
} else{
  install.packages("lmtest")
  library("lmtest")
}
# 
if (require("seasonal") == TRUE){
  library("seasonal")
} else{
  install.packages("seasonal")
  library("seasonal")
}
#
if (require("lrmest") == TRUE){
  library("lrmest")
} else{
  install.packages("lrmest")
  library("lrmest")
}
#
if (require("pcaMethods") == TRUE){
  library("pcaMethods")
} else{
  if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
  BiocManager::install("pcaMethods")
  library("pcaMethods")
}
#
if (require("forecast") == TRUE){
  library("forecast")
} else{
  install.packages("forecast")
  library("forecast")
}
#
if (require("readxl") == TRUE){
  library("readxl")
} else{
  install.packages("readxl")
  library("readxl")
}
