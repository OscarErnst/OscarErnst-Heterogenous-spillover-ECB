# Clear workspace and console
rm(list = ls())
cat("\014")

# Set working directory based on system user
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

monthly=TRUE
aggregate = "mean"
window="monetary event" #or "conference"

Baseline = TRUE
# Load required packages
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(zoo)
library(MASS)
library(NlcOptim)
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(stringr)
library(xtable)
library(readxl)
library(stargazer)
library(data.table)
library(broom)
library(sandwich)

#Name variables to be used in data
HFI_variables <- c("date","OIS_1M", "OIS_3M", "OIS_6M", "OIS_1Y", "OIS_2Y", "OIS_5Y", "OIS_10Y")


#Specify method - from daily to month
GK2015 = FALSE

#Specify crisis date for identification of the 3rd factor
crisis_date="2008-09-04"

press <- read_excel("Data/Additional/Dataset_EA-MPD.xlsx", sheet = "Press Release Window")
press[is.na(press)] <- 0
press <- subset(press, select = HFI_variables)

conf <- read_excel("Data/Additional/Dataset_EA-MPD.xlsx", sheet = "Press Conference Window")
conf[is.na(conf)] <- 0
conf <- subset(conf, select = HFI_variables)

me <- read_excel("Data/Additional/Dataset_EA-MPD.xlsx", sheet = "Monetary Event Window")
me[is.na(me)] <- 0
me <- subset(me, select = HFI_variables)

#Choose window -> press release or press conference
if (window == "release"){
  data = press
}else if (window == "monetary event"){
  data = me
} else {
  data =conf
}


HFI_matrix <- as.matrix(data[,2:8])
date_vector<-data[,1] %>%
  pull(date)
Tn<-nrow(HFI_matrix)
nn<-ncol(HFI_matrix)

#estimate factormodel
factor_model <- function(Z,center=F,scaleZ=T){
  Tn<-nrow(Z)
  nn<-ncol(Z)
  if(center==T){
    meanZ=colMeans(Z)
  }else{
    meanZ=rep(0,nn)
  }
  if(scaleZ==T){
    sdZ=apply(Z, 2, sd)
  }else{
    sdZ=rep(1,nn)
  }
  
  X=sweep(sweep(Z,2,meanZ), 2, sdZ, "/")
  
  ev<-eigen(t(X)%*%X,only.values=F)
  neg<-which(ev$values<0)
  
  if(!length(neg)==0){
    break
  }
  lamda<-ev$values
  sigma<-sqrt(lamda/Tn)
  v_k<-sigma^2/sum(sigma^2)
  Lambda<-sqrt(nn)*ev$vectors
  Fa=X%*%Lambda/nn
  
  return(list(factors=Fa,loadings=Lambda,eigenvalues=lamda,center=meanZ,scale=sdZ,data=Z))
}

fm<-factor_model(HFI_matrix)


#scale (not needed)
scale<-apply(fm$factors, 2, sd) #scale the estimated factors
Factors<-sweep(fm$factors, 2, scale, "/")[,1:3] #maximum of 3 factors



#######################################
###How much do the first 3PC explain###
#######################################

# Perform PCA and standardize the data
pca_result <- prcomp(HFI_matrix, scale. = TRUE)
# Extract the first three principal components
pca_scores <- pca_result$x[, 1:3]
print(pca_scores)
# Get the proportion of variance explained by each component
pve <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# Get the cumulative proportion of variance explained by the first three components
cumulative_pve <- cumsum(pve)
print(pve)  # individual PVE
print(cumulative_pve)  # cumulative PVE
# The cumulative proportion of variance explained by the first three principal components
variance_explained_first_3 <- cumulative_pve[3]
print(variance_explained_first_3)



###########################################################
idx_pre<-1:(which(date_vector==as.POSIXlt(crisis_date,tz="UTC"))-1)

ID<-list(Fa=Factors[idx_pre,],L=(fm$loadings[,1:3]*scale[1:3]))

#function to be optimized
if(window=="release" | window == "monetary event"){
  obj<-function(x){
    U=matrix(c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]),nrow=3)
    #xx<-ID$Fa%*%U[,2]
    xx<-ID$Fa%*%U[,3]
    
    out<-0.5*t(xx)%*%xx/length(xx)
    as.numeric(out)
  }
}else{
  obj<-function(x){
    U=matrix(c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]),nrow=3)
    xx<-ID$Fa%*%U[,3]
    
    out<-0.5*t(xx)%*%xx/length(xx)
    as.numeric(out)
  }
}



#non-linear constraint function
con=function(x){
  loading<-ID$L
  f=NULL
  #orthogonal restrictions
  f=rbind(f,x[1]^2 + x[4]^2 + x[7]^2-1)
  f=rbind(f,x[2]^2 + x[5]^2 + x[8]^2-1)
  f=rbind(f,x[3]^2 + x[6]^2 + x[9]^2-1)
  f=rbind(f,x[1]*x[2] + x[4]*x[5] + x[7]*x[8]-0)
  f=rbind(f,x[1]*x[3] + x[4]*x[6] + x[7]*x[9]-0)
  f=rbind(f,x[2]*x[3] + x[5]*x[6] + x[8]*x[9]-0)
  #second and third factors does not load on one month rate
  f=rbind(f,x[4]*loading[1,1] + x[5]*loading[1,2] + x[6]*loading[1,3]-0)
  f=rbind(f,x[7]*loading[1,1] + x[8]*loading[1,2] + x[9]*loading[1,3]-0)
  return(list(ceq=f,c=NULL))
}

sol<-solnl(c(diag(3)),objfun=obj,confun=con)

#rotate factors
rotate_factors<-Factors%*%matrix(sol$par,nrow=3) %>%
  as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))

#rename and scale based on corresponding ois rate
if(window=="release" | window == "monetary event"){
  
  rotate_factors<-rotate_factors %>%
    dplyr::select(1:3) %>%
    dplyr::rename(Target=1, Path = 2, QE = 3)
  
  full<-bind_cols(data %>%
                    dplyr::select(date),rotate_factors,HFI_matrix %>% as_tibble(.))
  
  scale_1 <-coef(lm(OIS_1M~Target, data = full))[2]
  ###As in Swanson2021 & Miranda-Agrippino & Nenova 2022
  scale_2 <-coef(lm(OIS_1Y~Path, data = full))[2] #could be changed OIS_3m
  scale_3 <-coef(lm(OIS_10Y~QE, data = full))[2] #5Y
  
  rotate_factors<-rotate_factors %>%
    dplyr::mutate(Target = Target*scale_1,
                  Path = Path*scale_2,
                  QE = QE*scale_3)
} else{
  rotate_factors<-rotate_factors %>%
    dplyr::select(1:3) %>%
    dplyr::rename(Timing=1,FG=2,QE=3)
  
  full<-bind_cols(data %>%
                    dplyr::select(date),rotate_factors,HFI_matrix %>% as_tibble(.))
  scale_4 <-coef(lm(OIS_6M~Timing, data = full))[2]
  scale_5 <-coef(lm(OIS_2Y~FG, data = full))[2]
  scale_6 <-coef(lm(OIS_10Y~QE, data = full))[2]
  
  rotate_factors<-rotate_factors %>%
    dplyr::mutate(Timing = Timing*scale_4,
                  FG = FG*scale_5,
                  QE = QE*scale_6)
}

factors_scaled<-bind_cols(data %>%
                            dplyr::select(date),rotate_factors)


loadings<-function(ois_data,factors){
  loadings_release<-full_join(ois_data,factors,by="date") %>%
    dplyr::select(-date)%>%
    pivot_longer(cols=!starts_with("OIS"),names_to = "factor",values_to = "shock") %>%
    pivot_longer(cols=starts_with("OIS"),names_to = "ois",values_to = "ois_value")  %>%
    split(list(.$factor,.$ois))%>%
    map(~ lm(ois_value ~ shock, data = .x))%>%
    map( function(u) tibble(coef=coef(u)[2],ser=sqrt(vcovHC(u)[2,2]),r2=summary(u)$r.squared)) %>%
    rbindlist(idcol = TRUE ) %>%
    tibble() %>%
    separate(.id,c("shock","ois"),"\\.") %>%
    mutate(ois=str_remove(ois,"_release|_conference"))
  return(loadings_release)
}
if (window=="release" | window == "monetary event"){
  loadings_release<-loadings(data,factors_scaled)
}else{
  loadings_conference<-loadings(data,factors_scaled)
}


####Aggregating shocks#####

if(window == "release" | window == "monetary event"){
  
factors_scaled$Date <- as.Date(factors_scaled$date)

    # Aggregate the data by month
    factors_scaled$YearMonth <- format(factors_scaled$Date, "%Y-%m")
    
    factors_scaled_m <- aggregate(. ~ YearMonth, factors_scaled, aggregate) # or sum, median, etc. depending on your needs
    
    
    # Split 'YearMonth' into separate 'year' and 'month' columns
    factors_scaled_m$YearMonth <- as.character(factors_scaled_m$YearMonth)
    year_month <- strsplit(factors_scaled_m$YearMonth, "-")
    factors_scaled_m$year <- sapply(year_month, function(x) as.integer(x[1]))
    factors_scaled_m$month <- sapply(year_month, function(x) as.integer(x[2]))
    
    
    # Create a sequence of all months from January 1999 to January 2024
    all_dates <- data.frame(year = rep(1999:2024, each = 12),
                            month = rep(1:12, times = 26))
    
    
    # Merge existing data with the full sequence of dates
    merged_data <- merge(all_dates, factors_scaled_m, by = c("year", "month"), all = TRUE)
    
    Target_m <- merged_data$Target
    
    Path_m <- merged_data$Path
    
    QE_m <- merged_data$QE
    
    # Fill missing values with zeros
    
    Target_m[is.na(Target_m)]<- 0
    
    Path_m[is.na(Path_m)]<- 0
    
    QE_m[is.na(QE_m)]<- 0
    
    
}


######################################################
#Follow Swanson 2021 to check relevance of the shocks#
######################################################
#Name financial variables to be used in the regression
HFI_FinVar <- c("OIS_1M", "OIS_3M", "OIS_6M", "OIS_1Y", "OIS_2Y", "OIS_5Y", "OIS_10Y", "STOXX50")

if (window == "release"){
  FinVar <- read_excel("Data/Additional/Dataset_EA-MPD.xlsx", sheet = "Press Release Window") 
  FinVar[is.na(FinVar)] <- 0
  FinVar <- subset(FinVar, select = HFI_FinVar)
  shock <- factors_scaled$Target
  shocks <-cbind(factors_scaled$Target, factors_scaled$Path, factors_scaled$QE)
  regdata <- cbind(FinVar, shock)
}else if (window == "monetary event"){
  FinVar <- read_excel("Data/Additional/Dataset_EA-MPD.xlsx", sheet = "Monetary Event Window") 
  FinVar[is.na(FinVar)] <- 0
  FinVar <- subset(FinVar, select = HFI_FinVar)
  shock <- factors_scaled$Target
  shocks <-cbind(factors_scaled$Target, factors_scaled$Path, factors_scaled$QE)
  regdata <- cbind(FinVar, shocks) #NOTE! shock
} else {
  FinVar <- read_excel("Data/Additional/Dataset_EA-MPD.xlsx", sheet = "Press Conference Window") 
  FinVar[is.na(FinVar)] <- 0
  FinVar <- subset(FinVar, select = HFI_FinVar)
  shock <- factors_scaled$FG
  regdata <- cbind(FinVar, shock)
}

library(stargazer) #https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf


#m1 <- lm(get("OIS_1M") ~ shocks, data = regdata)
#m2 <- lm(get("OIS_3M") ~ shocks, data = regdata)
#m3 <- lm(get("OIS_1Y") ~ shocks, data = regdata)
# m4 <- lm(get("OIS_2Y") ~ shocks, data = regdata)
# m5 <- lm(get("OIS_5Y") ~ shocks, data = regdata)
# m6 <- lm(get("OIS_10Y") ~ shocks, data = regdata)
# m7 <- lm(get("STOXX50") ~ shocks, data = regdata)

#stargazer(m1,m2,m3,m4,m5,m6,m7, type = "text",  title="Results", align=TRUE)
#stargazer(m1,m2,m3,m4,m5,m6,m7, type = "latex",  title="Results", align=TRUE)


#######################################################################
#Make poor-man mp shock identification a la Jarocinski and Karadi 2020#
#######################################################################
stockm <- FinVar$STOXX50      # <- make sure this line sits ABOVE the next block

#Plot 
# ── paths & file name ──────────────────────────────────────────────────────
out_dir  <- "/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/OscarErnst-Heterogenous-spillover-ECB/Graphs/Identify MP shock"
out_file <- file.path(out_dir, "MP_shock_scatter_panel.pdf")   # or .png

# ── open graphics device (choose one) ──────────────────────────────────────
png(out_file, width = 1600, height = 500, res = 150)        # alternative

# ── 1 × 3 scatter-plot panel ───────────────────────────────────────────────
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))   # 1×3 panel

shock_labs <- c("Target / pureMP", "Path", "QE")

for (j in 1:3) {
  x <- shocks[, j]
  
  plot(x, stockm,
       pch  = 19,
       xlab = shock_labs[j],
       ylab = expression(Delta~STOXX50~" (%)"),   # ← plot-math label
       main = shock_labs[j])
  
  abline(h = 0); abline(v = 0)
  
  info_idx <- x * stockm > 0
  points(x[info_idx], stockm[info_idx], pch = 19, col = "blue")
  
  if (j == 1) {
    legend("topright",
           legend = c("Information shock", "Monetary policy shock"),
           col    = c("blue", "black"),
           pch    = 19,
           cex    = 0.8)
  }
}
par(mfrow = c(1, 1))          # reset layout
dev.off()              


##############################################################################
##  Shock decomposition: raw factors, pure-MP shocks, information shocks
##############################################################################

## STOXX-50 intraday change series (used for sign test)
stockm <- FinVar$STOXX50      # <- make sure this line sits ABOVE the next block

if (window %in% c("release", "monetary event")) {
  
  ## shocks  : n × 3 matrix  (1 = Target, 2 = Path, 3 = QE)
  ## stockm  : n-vector of STOXX50 intraday returns  ← defined right above
  
  info_mask <- shocks * stockm > 0              # same sign  ⇒ info shock
  
  info_mat <- shocks;  info_mat[!info_mask] <- 0
  pure_mat <- shocks;  pure_mat[ info_mask] <- 0
  
  Dates <- as.Date(press$date)
  
  ShocksDF     <- data.frame(Date = Dates,
                             Target_m = pure_mat[, 1],
                             Path_m   = shocks[, 2],
                             QE_m     = shocks[, 3])
  
  PureShocksDF <- data.frame(Date = Dates,
                             pureTarget_m = pure_mat[, 1],
                             purePath_m   = pure_mat[, 2],
                             pureQE_m     = pure_mat[, 3])
  
  InfoShocksDF <- data.frame(Date = Dates,
                             infoTarget = info_mat[, 1],
                             infoPath   = info_mat[, 2],
                             infoQE     = info_mat[, 3])
  
}

## ── full calendar grid 1999-01 … 2024-12 ─────────────────────────────────
month_grid <- data.frame(
  year  = rep(1999:2024, each = 12),
  month = rep(1:12,     times = 26)
) |>
  mutate(Date = as.Date(sprintf("%04d-%02d-01", year, month)))

## ── helper: fill missing months with zeros ───────────────────────────────
make_monthly <- function(df, value_cols) {
  df |>
    mutate(year  = lubridate::year(Date),
           month = lubridate::month(Date)) |>
    group_by(year, month) |>
    summarise(across(all_of(value_cols), sum, .names = "{.col}"), .groups = "drop") |>
    right_join(month_grid, by = c("year", "month")) |>
    arrange(year, month) |>
    mutate(across(all_of(value_cols), ~ replace_na(.x, 0))) |>
    dplyr::select(Date, all_of(value_cols))
}

## ── 1. raw factors, 2. pure MP, 3. information shocks ───────────────────
Shocks_m     <- make_monthly(ShocksDF,
                             c("Target_m", "Path_m", "QE_m"))

PureShocks_m <- make_monthly(PureShocksDF,
                             c("pureTarget_m", "purePath_m", "pureQE_m")) %>% rename(
                               Target_m = pureTarget_m, 
                               Path_m = purePath_m, 
                               QE_m = pureQE_m)

InfoShocks_m <- make_monthly(InfoShocksDF,
                             c("infoTarget", "infoPath", "infoQE")) 

## ── save only these three monthly files ─────────────────────────────────
out_dir <- "Data/Shocks"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(Shocks_m,     file = file.path(out_dir, "Shocks_m.rds"))
saveRDS(PureShocks_m, file = file.path(out_dir, "PureShocks_m.rds"))
saveRDS(InfoShocks_m, file = file.path(out_dir, "InfoShocks_m.rds"))

cat("Monthly shock files saved in", normalizePath(out_dir), "\n")

