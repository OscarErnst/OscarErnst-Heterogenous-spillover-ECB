rm(list=ls())
cat("\014")
setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Data")
set.seed(4)
library(MASS)
library(NlcOptim)
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(stringr)
library(xtable)
#Code inspired by https://github.com/martinbaumgaertner/hfdshocks/blob/main/R/rotate.R
source("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Tamas filer/load_Packages.R")

monthly=TRUE
aggregate = "mean"
window="monetary event" #or "conference"

Robust1=FALSE 
Robust2=FALSE
Robust3=FALSE
Baseline = TRUE


#Name variables to be used in data
HFI_variables <- c("date","OIS_1M", "OIS_3M", "OIS_6M", "OIS_1Y", "OIS_2Y", "OIS_5Y", "OIS_10Y")


#Specify method - from daily to month
GK2015 = FALSE

#Specify crisis date for identification of the 3rd factor
crisis_date="2008-09-04"

press <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Press Release Window")
press[is.na(press)] <- 0
press <- subset(press, select = HFI_variables)

conf <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Press Conference Window")
conf[is.na(conf)] <- 0
conf <- subset(conf, select = HFI_variables)

me <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Monetary Event Window")
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
  
  if(GK2015 == TRUE) {
    factors_scaled$YearMonth <- format(factors_scaled$Date, "%Y-%m")
    
    # Split 'YearMonth' into separate 'year' and 'month' columns
    factors_scaled$YearMonth <- as.character(factors_scaled_m$YearMonth)
    year_month <- strsplit(factors_scaled$YearMonth, "-")
    factors_scaled$year <- sapply(year_month, function(x) as.integer(x[1]))
    factors_scaled$month <- sapply(year_month, function(x) as.integer(x[2]))
    
    
    # Create a sequence of all months from January 1999 to January 2024
    all_dates <- data.frame(year = rep(1999:2024, each = 12),
                            month = rep(1:12, times = 26))
    
    
    # Merge existing data with the full sequence of dates
    merged_data <- merge(all_dates, factors_scaled, by = c("year", "month"), all = TRUE)
    
    # Fill missing values with zeros
    
    merged_data$Target[is.na(merged_data$Target)]<- 0
    
    merged_data$Path[is.na(merged_data$Path)]<- 0
    
    # Step 1: Create a cumulative daily surprise series
    merged_data$cumulative_Target <- cumsum(merged_data$Target)
    
    merged_data$cumulative_Path <- cumsum(merged_data$Path)

    # Step 2: Take monthly averages
    library(dplyr)
    data_monthly <- merged_data %>%
      group_by(month = format(date, "%Y-%m")) %>%
      summarise(monthly_average_Target = mean(cumulative_Target),
      monthly_average_Path = mean(cumulative_Path))

    # Step 3: Obtain monthly average surprises as the first difference of this series
    data_monthly$monthly_diff_Target <- c(NA, diff(data_monthly$monthly_average_Target))
    
    data_monthly$monthly_diff_Path <- c(NA, diff(data_monthly$monthly_average_Path))
    
    Target_m <- data_monthly$monthly_diff_Target
    
    Target_m <- ts(cbind(Target_m=Target_m), start = c(1999,1), end = c(2023,10), frequency = 12)

    Target_m[is.na(Target_m)]<- 0

    Path_m <-  data_monthly$monthly_diff_Path
    
    Path_m <- ts(cbind(Path_m=Path_m), start = c(1999,1), end = c(2023,10), frequency = 12)
    
    Path_m[is.na(Path_m)]<- 0
    
    
  }else{
    
    if(monthly == TRUE){
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
    
    
    } else {
    # Aggregate the data by quarter
   
    
    factors_scaled$YearQuarter <- as.yearqtr(factors_scaled$Date)
    
    factors_scaled_q <- aggregate(. ~ YearQuarter, factors_scaled, mean) # or sum, median, etc. depending on your needs
    
    
    # Split 'YearMonth' into separate 'year' and 'quarter' columns
    factors_scaled_q$YearQuarter <- as.character(factors_scaled_q$YearQuarter)
    year_quarter <- strsplit(factors_scaled_q$YearQuarter, "Q")
    factors_scaled_q$year <- sapply(year_quarter, function(x) as.integer(x[1]))
    factors_scaled_q$quarter <- sapply(year_quarter, function(x) as.integer(x[2]))
    
    
    # Create a sequence of all quarters from Q1 1999 to Q1 2024
    all_quarters <- data.frame(year = rep(1999:2023, each = 4),
                            quarter = rep(1:4, times = 25))
    
    
    # Merge existing data with the full sequence of quarters
    merged_data_q <- merge(all_quarters, factors_scaled_q, by = c("year", "quarter"), all = TRUE)    
    
    Target_q <- merged_data_q$Target
    
    Path_q <- merged_data_q$Path
    
    # Fill missing values with zeros
    
    Target_q[is.na(Target_q)]<- 0
    
    Path_q[is.na(Path_q)]<- 0
  }
  }

  

}else{
  Timing <-factors_scaled$Timing
  FG <- factors_scaled$FG
  QE <- factors_scaled$QE
  
  factors_scaled$dateR <- as.Date(factors_scaled$date)
  
  factors_scaled$Month <- as.yearmon(factors_scaled$dateR)
  
  factors_scaled$Quarter <- as.yearqtr(factors_scaled$dateR)
  
  # Aggregate the data by month
  FG_m <- aggregate(factors_scaled$FG, by = list(Month = factors_scaled$Month), FUN = sum )
  
  FG_m[is.na(FG_m)]<- 0
  
  # Aggregate the data by quarter
  FG_q <- aggregate(factors_scaled$FG,  by = list(Quarter = factors_scaled$Quarter), FUN = sum )
  
}



######################################################
#Follow Swanson 2021 to check relevance of the shocks#
######################################################
#Name financial variables to be used in the regression
HFI_FinVar <- c("OIS_1M", "OIS_3M", "OIS_6M", "OIS_1Y", "OIS_2Y", "OIS_5Y", "OIS_10Y", "STOXX50")

if (window == "release"){
  FinVar <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Press Release Window") 
  FinVar[is.na(FinVar)] <- 0
  FinVar <- subset(FinVar, select = HFI_FinVar)
  shock <- factors_scaled$Target
  shocks <-cbind(factors_scaled$Target, factors_scaled$Path, factors_scaled$QE)
  regdata <- cbind(FinVar, shock)
}else if (window == "monetary event"){
  FinVar <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Monetary Event Window") 
  FinVar[is.na(FinVar)] <- 0
  FinVar <- subset(FinVar, select = HFI_FinVar)
  shock <- factors_scaled$Target
  shocks <-cbind(factors_scaled$Target, factors_scaled$Path, factors_scaled$QE)
  regdata <- cbind(FinVar, shocks) #NOTE! shock
} else {
  FinVar <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Press Conference Window") 
  FinVar[is.na(FinVar)] <- 0
  FinVar <- subset(FinVar, select = HFI_FinVar)
  shock <- factors_scaled$FG
  regdata <- cbind(FinVar, shock)
}

library(stargazer) #https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf


m1 <- lm(get("OIS_1M") ~ shocks, data = regdata)
m2 <- lm(get("OIS_3M") ~ shocks, data = regdata)
m3 <- lm(get("OIS_1Y") ~ shocks, data = regdata)
m4 <- lm(get("OIS_2Y") ~ shocks, data = regdata)
m5 <- lm(get("OIS_5Y") ~ shocks, data = regdata)
m6 <- lm(get("OIS_10Y") ~ shocks, data = regdata)
m7 <- lm(get("STOXX50") ~ shocks, data = regdata)

stargazer(m1,m2,m3,m4,m5,m6,m7, type = "text",  title="Results", align=TRUE)

stargazer(m1,m2,m3,m4,m5,m6,m7, type = "latex",  title="Results", align=TRUE)


#######################################################################
#Make poor-man mp shock identification a la Jarocinski and Karadi 2020#
#######################################################################

#Plot 
stockm <- FinVar$STOXX50
plot(shocks[,1], stockm, pch = 19, xlab = "Target Factor", ylab = "Change in STOXX50 (%)")
abline(h = 0) # horizontal line
abline(v = 0) # vertical line
# Color points based on position (positive/negative for both variables)
points(shocks[,1][shocks[,1] > 0 & stockm > 0], stockm[shocks[,1] > 0 & stockm > 0], col = "blue") # Blue if both x and y are positive
points(shocks[,1][shocks[,1] < 0 & stockm < 0], stockm[shocks[,1] < 0 & stockm < 0], col = "blue") # Blue if both x and y are negative
legend("topright", legend = c("Information shock", "Monetary policy shock"), pch = 19, col = c("blue", "black"), cex = 0.8)

if (window == "release" || window == "monetary event") {
  
   pureMP <- ifelse(shock * stockm < 0, shock, 0)
   
   purePath <-ifelse(shocks[,2] * stockm < 0, shocks[,2], 0)
   
   InfoCB <- ifelse(shock * stockm > 0, shock, 0)
   
   # Count the number of shocks that are interpreted as info shocks
   infoshock_count <- sum(pureMP == 0)/length(pureMP)
   
   #Regress on fin variables to check pure MP shock relevance
   regdata <- cbind(FinVar, pureMP)
   
   
   m1 <- lm(get("OIS_1M") ~ pureMP + InfoCB , data = regdata)
   m2 <- lm(get("OIS_3M") ~ pureMP + InfoCB, data = regdata)
   m3 <- lm(get("OIS_1Y") ~ pureMP + InfoCB, data = regdata)
   m4 <- lm(get("OIS_2Y") ~ pureMP + InfoCB, data = regdata)
   m5 <- lm(get("OIS_5Y") ~ pureMP + InfoCB, data = regdata)
   m6 <- lm(get("OIS_10Y") ~ pureMP + InfoCB, data = regdata)
   m7 <- lm(get("STOXX50") ~ pureMP + InfoCB, data = regdata)
   
   stargazer(m1,m2,m3,m4,m5,m6,m7, type = "text",  title="Results", align=TRUE)
   
   
   pureMP <-as.data.frame(pureMP)
   
   Date <-as.Date(press$date)
   
   pureMP<-cbind(Date,pureMP)
   
   purePath <-as.data.frame(purePath)
   
   purePath<-cbind(Date,purePath)
   
   InfoCB <-as.data.frame(InfoCB) 
   
   InfoCB<-cbind(Date,InfoCB)
   
   TotShocks <- cbind(Date,InfoCB,pureMP, purePath)
   
   # Aggregate the data by month
   
   if(GK2015 == TRUE) {
     TotShocks$YearMonth <- format(TotShocks$Date, "%Y-%m")
     
     # Split 'YearMonth' into separate 'year' and 'month' columns
     TotShocks$YearMonth <- as.character(TotShocks$YearMonth)
     year_month <- strsplit(factors_scaled$YearMonth, "-")
     TotShocks$year <- sapply(year_month, function(x) as.integer(x[1]))
     TotShocks$month <- sapply(year_month, function(x) as.integer(x[2]))
     
     
     # Create a sequence of all months from January 1999 to January 2024
     all_dates <- data.frame(year = rep(1999:2024, each = 12),
                             month = rep(1:12, times = 26))
     
     
     # Merge existing data with the full sequence of dates
     merged_data <- merge(all_dates, TotShocks, by = c("year", "month"), all = TRUE)
     
     # Fill missing values with zeros
     
     merged_data$InfoCB[is.na(merged_data$InfoCB)]<- 0
     
     merged_data$pureMP[is.na(merged_data$pureMP)]<- 0
     
     # Step 1: Create a cumulative daily surprise series
     merged_data$cumulative_pureMP <- cumsum(merged_data$pureMP)
     
     merged_data$cumulative_InfoCB <- cumsum(merged_data$InfoCB)
     
     # Step 2: Take monthly averages
     data_monthly <- merged_data %>%
       group_by(month = format(Date, "%Y-%m")) %>%
       summarise(monthly_average_pureMP = mean(cumulative_pureMP),
                 monthly_average_InfoCB = mean(cumulative_InfoCB))
     
     # Step 3: Obtain monthly average surprises as the first difference of this series
     data_monthly$monthly_diff_pureMP <- c(NA, diff(data_monthly$monthly_average_pureMP))
     
     data_monthly$monthly_diff_InfoCB <- c(NA, diff(data_monthly$monthly_average_InfoCB))
     
     pureMP_m <- data_monthly$monthly_diff_pureMP
     
     pureMP_m <- ts(cbind(pureMP_m=pureMP_m), start = c(1999,1), end = c(2023,10), frequency = 12)
     
     pureMP_m[is.na(pureMP_m)]<- 0
     
     InfoCB_m <-  data_monthly$monthly_diff_InfoCB
     
     InfoCB_m <- ts(cbind(InfoCB_m=InfoCB_m), start = c(1999,1), end = c(2023,10), frequency = 12)
     
     InfoCB_m[is.na(InfoCB_m)]<- 0
     
   }else{
     if (monthly==TRUE){
     
     # Aggregate the data by month
     
     TotShocks$YearMonth <- format(TotShocks$Date, "%Y-%m")
     
     TotShocks_m <- aggregate(. ~ YearMonth, TotShocks, aggregate) # mean! or sum, median, etc. depending on your needs
     
     
     # Split 'YearMonth' into separate 'year' and 'month' columns
     TotShocks_m$YearMonth <- as.character(TotShocks_m$YearMonth)
     year_month <- strsplit(TotShocks_m$YearMonth, "-")
     TotShocks_m$year <- sapply(year_month, function(x) as.integer(x[1]))
     TotShocks_m$month <- sapply(year_month, function(x) as.integer(x[2]))
     
     
     # Merge existing data with the full sequence of dates
     merged_data <- merge(all_dates, TotShocks_m, by = c("year", "month"), all = TRUE)
     
     pureMP_m <- merged_data$pureMP
     
     InfoCB_m <- merged_data$InfoCB
     
     purePath_m <- merged_data$purePath
     
     # Fill missing values with zeros
     
     pureMP_m[is.na(pureMP_m)]<- 0
     
     InfoCB_m[is.na(InfoCB_m)]<- 0
     
     purePath_m[is.na(purePath_m)]<- 0
     
     TargetMP_m <-pureMP_m
     save( Target_m, TargetMP_m,InfoCB_m,purePath_m, file="Own_mpshock_release.RData")
     } else {
     # Aggregate the data by quarter
     
     TotShocks$YearQuarter <- as.yearqtr(TotShocks$Date)
     
     TotShocks_q <- aggregate(. ~ YearQuarter, TotShocks, mean) # or sum, median, etc. depending on your needs
     
     
     # Split 'YearMonth' into separate 'year' and 'quarter' columns
     TotShocks_q$YearQuarter <- as.character(TotShocks_q$YearQuarter)
     year_quarter <- strsplit(TotShocks_q$YearQuarter, "Q")
     TotShocks_q$year <- sapply(year_quarter, function(x) as.integer(x[1]))
     TotShocks_q$quarter <- sapply(year_quarter, function(x) as.integer(x[2]))
     
     # Merge existing data with the full sequence of quarters
     merged_data_q <- merge(all_quarters, TotShocks_q, by = c("year", "quarter"), all = TRUE)    
     
     pureMP_q <- merged_data_q$pureMP
     
     InfoCB_q <- merged_data_q$InfoCB
     
     purePath_q <- merged_data_q$purePath
     
     }
     
   }
   
  
   
} else{
  
  FGMP <- ifelse(shock * stockm < 0, shock, 0)
  
  # Count the number of shocks that are interpreted as info shocks
  infoshock_count <- sum(FGMP == 0)/length(FGMP)
  
  #Regress on fin variables to check pure MP shock relevance
  regdata <- cbind(FinVar, FGMP)
  
  
  m1 <- lm(get("DE3M") ~ FGMP, data = regdata)
  m2 <- lm(get("DE2Y") ~ FGMP, data = regdata)
  m3 <- lm(get("DE5Y") ~ FGMP, data = regdata)
  m4 <- lm(get("DE10Y") ~ FGMP, data = regdata)
  m5 <- lm(get("STOXX50") ~ FGMP, data = regdata)
  
  stargazer(m1,m2,m3,m4,m5, type = "text",  title="Results", align=TRUE)
  
  FGMP <-as.data.frame(FGMP)
  
  date <-as.Date(press$date)
  
  FGMP<-cbind(date,FGMP)
  
  FGMP$Month <- as.yearmon(FGMP$date)
  
  FGMP$Quarter <- as.yearqtr(FGMP$date)
  
  # Aggregate the data by month
  FGMP_m <- aggregate(FGMP$FGMP, by = list(Month = FGMP$Month), FUN = sum )
  
  FGMP_m[is.na(FGMP_m)]<- 0
  
  # Aggregate the data by quarter
  FGMP_q <- aggregate(FGMP$FGMP,  by = list(Quarter = FGMP$Quarter), FUN = sum )
  
  save(FG_q, FG_m,FGMP_q, FGMP_m, file="Own_mpshock_conference.RData")  
}
save(pureMP_m, Path_m, QE_m,file ="Pure_shocks")
saveRDS(pureMP_m, file ="pureMP_m.rds")
saveRDS(Path_m, file ="Path_m.rds")
saveRDS(QE_m, file ="QE_m.rds")


load("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Heterogenous-spillover-ECB/Data/Pure_shocks")
##############################################################
##Estimating fitted ECB interest rate  a la Stock&Watson2018##
##############################################################
load("C:/Foreign_SVAR/load_raw_data.RData")
load("interpolatedrGDP_E20.RData")
load("Pure_shocks")
if (monthly == TRUE){
  
  t0        <- c(2004,9)#c(2004,12)                                      
  tEnd      <- c(2024,6) #2020,12  
  
  HCIP_eurostat_m <- window(HCIP_eurostat_m,start = t0, end = tEnd )
  
  rGDPm <- window(rGDPm, start = t0, end =tEnd)
  
  # Eonia <- window(Eonia, start = t0, end =tEnd)
  # 
  # dEonia <- window(dEonia, start = t0, end =tEnd)
  # 
  # Euribor_3m <- window(Euribor_3m, start = t0, end =tEnd)
  # 
  # dEuribor_3m <- window(dEuribor_3m, start = t0, end =tEnd)
  # 
  # Euribor_6m <- window(Euribor_6m, start = t0, end =tEnd)
  # 
  # GerBund1y_m <- window(GerBund1y_m, start = t0, end =tEnd)
  # 
  # dGerBund1y_m <- window(dGerBund1y_m, start = t0, end =tEnd)
  # 
  # dEuribor_6m <- window(dEuribor_6m, start = t0, end =tEnd)
  
  dEuro_1y_m <- window(dEuro_1y_m, start = t0, end =tEnd)
  
  Euro_1y_m <- window(Euro_1y_m, start = t0, end =tEnd)
  
  DK_12m <- window(DK_12m, start = t0, end =tEnd)
  
  dDK_12m <- window(dDK_12m, start = t0, end =tEnd)
  
} else {
  t0        <- c(2004,4)                                      
  tEnd      <- c(2020,4)  
  
  HCIP_euroECB_q <- window(HCIP_euroECB_q, start = t0, end =tEnd)
  
  rGDP_eurozone <- window(rGDP_eurozone, start = t0, end =tEnd)
  
  Euro_1y_q  <- window(Euro_1y_q, start = t0, end =tEnd)
  
}


if (window == "release" || window == "monetary event") {
  if (monthly == TRUE){
  
 # Target_m <- ts(cbind(Target_m=Target_m), start = c(1999,1), end =  c(2024,12), frequency = 12)
  
#  Target_m <- window(Target_m, start = t0, end =tEnd)
  
  pureMP_m <- ts(cbind(pureMP_m=pureMP_m), start = c(1999,1), end = c(2024,12), frequency = 12)
  
  pureMP_m <- window(pureMP_m, start = t0, end =tEnd)
  
#  InfoCB_m <- ts(cbind(InfoCB_m=InfoCB_m), start = c(1999,1), end = c(2024,12), frequency = 12)
  
#  InfoCB_m <- window(InfoCB_m, start = t0, end =tEnd)
  
  Path_m <- ts(cbind(Path_m=Path_m), start = c(1999,1), end = c(2024,12), frequency = 12)
  
  Path_m <- window(Path_m, start = t0, end =tEnd)
  
 # purePath_m <- ts(cbind(purePath_m=purePath_m), start = c(1999,1), end = c(2024,12), frequency = 12)
  
#  purePath_m <- window(purePath_m, start = t0, end =tEnd)
  
  QE_m <- ts(cbind(QE_m=QE_m), start = c(1999,1), end = c(2024,12), frequency = 12)
  
  QE_m <- window(QE_m, start = t0, end =tEnd)
  } else {
    pureMP_q <- ts(cbind(pureMP_q=pureMP_q), start = c(1999,1), end =  c(2023,4), frequency = 4)
    pureMP_q <- window(pureMP_q, start = t0, end =tEnd)
    
    InfoCB_q <- ts(cbind(InfoCB_q=InfoCB_q), start = c(1999,1), end =  c(2023,4), frequency = 4)
    InfoCB_q <- window(InfoCB_q, start = t0, end =tEnd)
    
    Path_q <-ts(cbind(Path_q=Path_q), start = c(1999,1), end =  c(2023,4), frequency = 4)
    Path_q <- window(Path_q, start = t0, end =tEnd) 
    
    purePath_q <- ts(cbind(purePath_q=purePath_q), start = c(1999,1), end =  c(2023,4), frequency = 4)
    purePath_q <- window(purePath_q, start = t0, end =tEnd)
    
  }

} else {
  
  FGMP_m <- FGMP_m[,2]
  
  FGMP_m <- ts(cbind(FGMP_m=FGMP_m), start = c(1999,1), end = c(2023,10), frequency = 12)
  
  FGMP_m <- window(FGMP_m, start = t0, end =tEnd)
  
}

# Run 1st stage regression 
library(dplyr)
#library(dyn)
library(dynlm)


#regdata <- cbind(Eonia, rGDPm,HCIP_eurostat_m, TargetMP_m)

if (monthly==TRUE){
  if (Baseline == TRUE){
    
    #Baseline
    FirstStage <- dynlm(Euro_1y_m ~ pureMP_m + Path_m + QE_m +
                          L(pureMP_m,1:6) + L(Path_m,1:6) + L(QE_m,1:6)
                          + L(rGDPm,1:6) +  L(HCIP_eurostat_m,1:6) + L(Euro_1y_m, 1:6)) 
    summary(FirstStage)
    
  } if (Robust1 == TRUE){
    
    #Robust 1
    
    FirstStage <- dynlm(Euro_1y_m ~ pureMP_m + Path_m +  QE_m +x
                          L(pureMP_m,1:6) + L(Path_m,1:6) + L(QE_m,1:6) + 
                        + L(log(rGDPm),1:6) +  L(log(HCIP_eurostat_m),1:6) +L(Euro_1y_m, 1:6) )
    
    summary(FirstStage)
    
    
  } if (Robust2 == TRUE) {
    
    FirstStage <- dynlm(Euro_1y_m ~  Path_m +
                          + L(Path_m,1:2) 
                        + L(rGDPm,1:2) +  L(HCIP_eurostat_m,1:2) +L(Euro_1y_m, 1:2)) 
    
  } if (Robust3 == TRUE){
    
    #Baseline
    FirstStage <- dynlm(Euro_1y_m ~ QE_m +
                          L(QE_m,1:2)
                        + L(rGDPm,1:2) +  L(HCIP_eurostat_m,1:2) +L(Euro_1y_m,1:2)) 
    
  }
  
  
  # Calculate Newey-West standard errors using coeftest
  
  newey_west_se <- coeftest(FirstStage, vcov = NeweyWest(FirstStage))
  
#Fit FirstStage regression to data
  Euro_1y_m_hat <- fitted.values(FirstStage)
 
#plot fitted variable VS actual variable
 windows(10,10)
  plot(Euro_1y_m)
  lines(Euro_1y_m_hat, col ="red")
  
  shock_var <-Euro_1y_m_hat
  
  shock_var_zoo <- zoo(shock_var)
  
  shock_var_q <- aggregate(shock_var_zoo, as.yearqtr, mean) #sum
  
  # Convert the resulting zoo object back to a dataframe
  shock_var_q <- data.frame(dates = as.Date(index(shock_var_q)),
                               values = coredata(shock_var_q))
  
  #shock_var_q <-ts(shock_var_q$values, start = c(2005,1), end = c(2020,4),  frequency = 4)
  shock_var_q <-ts(shock_var_q$values, start = c(2005,1), end = c(2023,1),  frequency = 4) #c(2020,2)

  
  #save(pureMP_m, Path_m,QE_m, file="P:/tav/Foreign_SVAR/lpirfs_package_data/shocks.RData")
  save( shock_var, shock_var_q, file="IV_mpshock_release.RData")
  
  
} else{
  FirstStage <- dynlm(Euro_1y_q ~ pureMP_q + Path_q + L(pureMP_q,1:2) + L(Path_q,1:2) +L(rGDP_eurozone,1:2) +  L(HCIP_euroECB_q,1:2) +L(Euro_1y_q,1:2) )
  
  #FirstStage <- dynlm(GerBund1y_m ~ pureMP_m + L(pureMP_m,1:2) +L(rGDPm,1:2) +  L(HCIP_eurostat_m,1:2) + L(GerBund1y_m,1:2) )
  
  summary(FirstStage)
  
  # Calculate Newey-West standard errors using coeftest
  newey_west_se <- coeftest(FirstStage, vcov = NeweyWest(FirstStage))
  
  
  Euro_1y_q_hat <- fitted.values(FirstStage)
  windows(10,10)
  plot(Euro_1y_q)
  lines(Euro_1y_q_hat, col ="red")
  
  shock_var <- Euro_1y_q_hat
  
}




####################################################################
##Compare with Miranda Aggippino and Nenova 2022 type of ECB shock##
####################################################################
EA_shocks<- read_excel("P:/tav/Foreign_SVAR/SVAR/Data/MirandaAgrippionoNenova2022.xlsx", skip =2, sheet = "EA shocks")
EA_shocks[is.na(EA_shocks)]<- 0


EA_shocks$date <- as.Date(EA_shocks$date)
EA_shocks$YearMonth <- format(EA_shocks$date, "%Y-%m")

EA_shocks_m <- aggregate(. ~ YearMonth, EA_shocks, mean) # or sum, median, etc. depending on your needs


# Split 'YearMonth' into separate 'year' and 'month' columns
EA_shocks_m$YearMonth <- as.character(EA_shocks_m$YearMonth)
year_month <- strsplit(EA_shocks_m$YearMonth, "-")
EA_shocks_m$year <- sapply(year_month, function(x) as.integer(x[1]))
EA_shocks_m$month <- sapply(year_month, function(x) as.integer(x[2]))


# Merge existing data with the full sequence of dates
EA_merged_data <- merge(all_dates, EA_shocks_m, by = c("year", "month"), all = TRUE)

TARGETEA_MP <- EA_merged_data$TARGETEA_MP

TARGETEA_MP <- ts(cbind(TARGETEA_MP=TARGETEA_MP), start = c(1999,1), end =  c(2024,12), frequency = 12)

TARGETEA_MP <- window(TARGETEA_MP, start = t0, end =tEnd)

FirstStage <- dynlm(dEuribor_3m ~ TARGETEA_MP + L(TARGETEA_MP,1) + L(TARGETEA_MP,2) +L(rGDPm,1) + L(rGDPm,2)+ L(HCIP_eurostat_m,1) + L(HCIP_eurostat_m,2))

summary(FirstStage)

###############
##PLOT shocks##
###############
windows(10,10)
plot( pureMP_m, type = "l", col = "blue",
     xlab = "Time", ylab = "Shock size")

# Add the second time series to the plot
lines(Path_m, col = "red")
lines(QE_m, col = "black",lty =2)

# Add a legend
legend("topright", legend = c("Pure MP shock", "Path shock", "QE shock"), col = c("blue", "red", "black"), lty = 1)

############
###IV_reg###
############
# library(ivreg)
# 
# lagmatrix <- function( X , lag ){
#   if (lag>0) {
#     X <- as.matrix(X)
#     N <- ncol(X)
#     T <- nrow(X)
#     X <- rbind( matrix(NA,lag,N) , X )
#     X <- embed(X,lag)
#     #X[ is.na(X) ] <- 0
#     X[1:T,]
#   }
# }
# 
# Y <- cbind(Euro_1y_m, log(rGDPm), log(HCIP_eurostat_m) )
# lagY <- lagmatrix(Y, 4)
# Y <- unname(as.matrix(Y))
# shock <- as.matrix(pureMP_m)
# 
# 
# regdata <- data.frame(cbind(Y, lagY, pureMP_m))
# 
# 
# nona <- rowSums(is.na(regdata)) == 0
# regdata <- regdata[nona, ]


ivreg(Y.2 ~ 0 + . | pureMP_m, data = regdata)







