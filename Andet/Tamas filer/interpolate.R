rm(list=ls())
cat("\014")
setwd("P:/tav/Foreign_SVAR/SVAR/Data")
set.seed(4)

library(tempdisagg)
library(eurostat) #https://cran.r-project.org/web/packages/eurostat/eurostat.pdf 

##################################
##Download data from eurostat#####
##################################

#clean_eurostat_cache(cache_dir = NULL, config = FALSE)
start_date <- c(2001-01)
end_date <- c(2024-01)


unemployment<- get_eurostat("une_rt_m", time_format = "num", filters = list(geo="EA20", unit="PC_ACT", s_adj="SA",
                                                                            sex = "T", age = "TOTAL"))
                                                                   

unemployment <- ts(unemployment$values, start = c(1983,1), end = c(2024,1),  frequency = 12)

unemployment <- window(unemployment,start = start_date, end = end_date )

IP <- get_eurostat("sts_inpr_m",time_format = "num", filters = list(geo="EA20", s_adj="SCA", nace_r2="B-D", unit="I15"))

IP <- ts(IP$values, start = c(1953,1), end = c(2023,12),  frequency = 12)

IP <- window(IP,start = start_date, end = end_date )

rGDP <- get_eurostat("namq_10_gdp",time_format = "num", filters = list(geo="EA20", s_adj="SCA", unit="CLV_I10"))

rGDP <- ts(rGDP$values, start = c(1975,1), end = c(2023,4),  frequency = 4)

rGDP <- window(rGDP,start = start_date, end = end_date )


#############
#Interpolate#
#############

# Methods to be used
methods <- c('chow-lin-maxlog',"chow-lin-minrss-ecotrim",
                 "chow-lin-minrss-quilis", "litterman-maxlog","litterman-minrss",
                 "dynamic-maxlog", "dynamic-minrss","dynamic-fixed" )

# Pre-allocation of object for interpolation results:
intrawdata <- list()

for (j in 1:length(methods)){
  
    tempresult <- c()
  
    # Interpolating:
    temptd  <- td(rGDP~unemployment + IP, to = 12, method = methods[j])
    
    # Appending results of countries:
    tempresult <- cbind(tempresult,temptd$values)
    
    
    intrawdata[[j]] <- log(tempresult)
    # Saving R-squared:
    #r2[i,j] <- summary(temptd)$r.squared
    
  }
  
rGDPm  <- td(rGDP~unemployment + IP, to = 12, method = 'chow-lin-maxlog')

rGDPm <-rGDPm$values

save(rGDPm, file="interpolatedrGDP_E20.RData")