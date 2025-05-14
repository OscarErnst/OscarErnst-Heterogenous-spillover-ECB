rm(list=ls())
cat("\014")
#setwd("C:/Foreign_SVAR")

library(readxl)
library(OECD) #https://github.com/expersso/OECD
library(eurostat) #https://cran.r-project.org/web/packages/eurostat/eurostat.pdf 
library(bundesbank) #http://enricoschumann.net/R/packages/bundesbank/bundesbank-manual.pdf
library(zoo)
library(ecb) #https://github.com//ecb
library(dkstat)
#install.packages("ape") # used to open .gbk file


## Import data from files

mona2  = read_excel("SVAR/Data/monadata2024.xlsx", col_names = FALSE)

load("SVAR/Data/ECBrate_shadow_q.RData") 

load("SVAR/Data/HCIP.RData") 

load("SVAR/Data/GDP_defF.RData") 

load("SVAR/Data/HCIP_DK.RData") 

load("SVAR/Data/EX_nonEU.RData") 

load("SVAR/Data/shocks_q.RData")

load("SVAR/Data/efkrks_DN.RData")

load("SVAR/Data/Export_pharma.RData")

load("SVAR/Data/Export_detailed.RData")

load("SVAR/Data/Export_detailed_EA.RData")

load("SVAR/Data/Export_detailed_BEL_NL_SWE.RData")

load("SVAR/Data/Export_detailed_STIC59.RData")

load("SVAR/Data/Euro_rates.RData") #money market rates, 1month & 3month

load("SVAR/Data/Import_DK.RData")

load("SVAR/Data/rPBol_house.RData")

load("SVAR/Data/rPBol_apartment.RData")

load("SVAR/Data/PPI_nondom_euro20.RData")

load("SVAR/Data/ZEW_sentiment.RData")

load("SVAR/Data/GSCPI.RData")

load("SVAR/Data/Import_EA.RData")

load("SVAR/Data/PEX_PF_DREAM.RData")

Euro_rate_1mo <- Euro_rates[,1]
Euro_rate_3mo <- Euro_rates[,2]

load("SVAR/Data/Euro_1y.RData.")

Euro_1y <- Euro_1y[,2]

Euro_1y <- ts(cbind(Euro_1y=Euro_1y), start = c(1970,1), end = c(2023,3), frequency = 4)

load("SVAR/Data/Euro_2y.RData.")

Euro_2y <- Euro_2y[,2]

Euro_2y <- ts(cbind(Euro_2y=Euro_2y), start = c(1970,1), end = c(2023,3), frequency = 4)

load("SVAR/Data/Euribor_q.RData")

Euribor3m_q <- Euribor_q[,3]

load("SVAR/Data/GDP_euro.RData.") 

GDP_euro <- GDP_euro[,2]

load("SVAR/Data/HCIP_euro.RData") 

HCIP_euro <- HCIP_euro[,2]

load("SVAR/Data/HCIP_euro_exl_energy.RData") 

HCIP_euro_exl_energy <- HCIP_euro_exl_energy[,2]

############################
###Obtain data from MONA####
############################

##########################Order data from MONA#################################

# Arbejdsstyrke inkl. efterløn og orlovstagere [1000 personer] -> U
# Offentlig sektor, besk?ftigelse [1000 personer] -> QO
# Privat besk?ftigelse [1000 personer] -> QP
# Selvstændige [1000 personer] -> QS
# Effektiv kronekurs -> efkrks
# Arbejdsløse -> ul
# GDP in fixed prices -> fY [Mia kr.]
# NGDP -> Y [Mia kr.]
# Privat forbrug for hh and firms -> fCp 
# Privat forbrug ekskl. bilk?b, bolig og nettoturistk?b, deflator ekskl. afgifter -> qcq 
# Privat forbrug, deflator -> pcp
# Eksport af tjenester eksl. turistindtægter, deflator -> pEs
# Eksport af tjenester ekkl. turistindtægter [Mia 95-kr] -> FES
# Eksport af varer [Mia. kr.] -> Ev
# Eksport af varer og tjenester [Mia kr.] ->E
# Eksport af energivarer [Mia. 95-kr.] -> febra
# Energieksport, deflator -> pebra
# Eksport af skibe og fly [Mia. 95-kr.] -> fey
# Eksport af skive og fly, deflator -> pey
# Eksportmarkedets importpris i udenlandsk valuta -> pxudl
# Råoliepris [$/tønde] -> praoil
# Gennemsnitsløn [Mio kr.] -> dagl
# Timeløn, industriarbejde -> lna
# Marked for industrieksport, indeks -> FEU
# Eksport af varer og tjenester, deflator -> pE
# PXUDL
# Boliginvestering  -> fih
# Erhvervenes bygge- og anl?gsinvestering  -> fipb
# Erhvervenes materielinvestering -> fipm
# Huspris (kontantpris)  -> kp
# IMI-index, ses som et prisindeks fori ndenlandsk markedsbestemt v?rditilv?kst -> restx
# Import, engrosprisindex -> pimpor
# Obligationsrente efter skat -> rente
# Import af varer -> fmv
# Export af varer og tjenester -> fe
# Tysk obligationsrente -> ibodem
# Pengemarkedsrenten -> imm
# Gennemsnitlig obligationsrente -> ibz
# Import af varer ekskl. energi, skibe og fly -> fmvx
#Import af varer og tjenester -> fM
#Import af varer -> fmv
#exportpris -> pxden
#industrieksport -> feind
#importpris -> pmvx
#industrieksport, deflator -> peind
#Eksport af varer og tjenester, deflator -> pE
#Import af varer og tjenester, deflator -> pM
#Import af varer, deflator -> pMv
#Byerhvervenes besk?ftigelse arbejdstidskorrigeret, hj?lpevariabel  -> qbyx
#Privat disponibel indkomst -> ydp
#Offentligt forbrug -> fco


var.list.MONA     <-   c("u","QO","QP","QS", "efkrks", "ul", "fy", "y", "fcp","pcp","qcq", "pes", "fes", "fev", "Ev", "E", 
                         "febra", "pebra", "fey", "pey", "pxudl", "praoli", "dagl", "lna", "feu", "pe", "pxudl",
                         "fih", "fipb", "fipm", "kp", "restx", "pimpor", "rente", "fmv" , "fe", "ibodem", "imm",
                         "ibz", "fmvx", "fm", "fmv", "pxden", "feind", "pmvx", "peind", "pe", "pM", 
                         "pMv","qbyx", "ydp", "fco", "fipbxe", "fipnb")


t0.MONA   <-   c(1966,1)#c(1971,1) 
#tEnd.MONA <-   c(2022,2)


for(k in 1:length(var.list.MONA)){
  var   <-  mona2[mona2[,1]==var.list.MONA[k],]
  var   <-  as.numeric(var[2,2:ncol(var)])
  #var   <-  ts(cbind(var=var),start=t0.MONA,end=tEnd.MONA,freq=4)
  var   <-  ts(cbind(var=var),start=t0.MONA,freq=4)
  assign(var.list.MONA[k],var)
}


############################
###Import data from FRED####
############################

library(fredr)

# info about API: https://sboysel.github.io/fredr/articles/fredr.html 

fredr_set_key("6c537d3a94858ca1594a68fc9ac067fd")

Ger_rGDP<-fredr(
  series_id = "CLVMEURSCAB1GQEA19",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

Ger_rGDP <-ts(Ger_rGDP$value, start = c(1999,1), end = c(2023,2), frequency = 4)


HCIP_euro <- fredr(
  series_id = "CP0000EZ19M086NEST",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

HCIP_euro <-ts(HCIP_euro$value, start = c(1994,1), end = c(2023,2), frequency = 4)

chHCIP_euro <- fredr(
  series_id = "CP0000EZ19M086NEST",
  observation_start = as.Date("1999-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q",#, # quarterly
  units = "pc1" # change over previous value
)

chHCIP_euro <-ts(chHCIP_euro$value, start = c(1999,1), end = c(2023,2), frequency = 4)


rGDP_eurozone <- fredr(
  series_id = "CLVMNACSCAB1GQEA19",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

rGDP_eurozone <-ts(rGDP_eurozone$value, start = c(1994,1), end = c(2023,2), frequency = 4)


chrGDP_eurozone <- fredr(
  series_id = "CLVMNACSCAB1GQEA19",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q",#, # quarterly
  units = "pc1" # change over previous value
)

chrGDP_eurozone <-ts(chrGDP_eurozone$value, start = c(1994,1), frequency = 4)

nGDP_eurozone <- fredr(
  series_id = "EUNNGDP",
  observation_start = as.Date("1999-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

nGDP_eurozone <-ts(nGDP_eurozone$value, start = c(1999,1), end = c(2023,2), frequency = 4)

GDP_def_euro <- nGDP_eurozone/rGDP_eurozone

i_eurozone <- fredr(
  series_id = "IRLTCT01DEM156N",#"IR3TIB01EZM156N",
  observation_start = as.Date("1999-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

i_eurozone <-ts(i_eurozone$value, start = c(1999,1), end = c(2023,2), frequency = 4)

Brent_oil <- fredr(
  series_id = "POILBREUSDM",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "m"#, # quarterly
  #units = "chg" # change over previous value
)

Brent_oil <-ts(Brent_oil$value, start = c(1994,1), end = c(2023,2), frequency = 12)


qBrent_oil <- fredr(
  series_id = "POILBREUSDM",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

qBrent_oil <-ts(qBrent_oil$value, start = c(1994,1), end = c(2023,2), frequency = 4)


WTI_oil <-fredr(
  series_id = "WTISPLC",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "m"#, # quarterly
  #units = "chg" # change over previous value
)

WTI_oil <-ts(WTI_oil$value, start = c(1994,1), end = c(2023,2), frequency = 12)


qWTI_oil <- fredr(
  series_id = "WTISPLC",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2023-08-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

qWTI_oil <-ts(qWTI_oil$value, start = c(1994,1), end = c(2023,2), frequency = 4)


Spread <-fredr(
  series_id = "BAA10Y",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "m"#, # quarterly
  #units = "chg" # change over previous value
)

Spread <-ts(Spread$value, start = c(1994,1), end = c(2024,6), frequency = 12)

Spread_q <-fredr(
  series_id = "BAA10Y",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

Spread_q <-ts(Spread_q$value, start = c(1994,1), end = c(2024,2), frequency = 4)

rGDP_US <-fredr(
  series_id = "GDPC1",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

rGDP_US <-ts(rGDP_US$value, start = c(1969,1), frequency = 4)

CPI_US <-fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "m"#, # quarterly
  #units = "chg" # change over previous value
)

CPI_US <-ts(CPI_US$value, start = c(1990,1), frequency = 12)

CPI_USq <-fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

CPI_USq <-ts(CPI_USq$value, start = c(1969,1), frequency = 4)

CPI_UScoreq <-fredr(
  series_id = "CPILFESL",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

CPI_UScoreq <-ts(CPI_UScoreq$value, start = c(1969,1), frequency = 4)


GPDI_US <- fredr(
  series_id = "GPDI",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

GPDI_US <-ts(GPDI_US$value, start = c(1969,1), frequency = 4)

U_US <-fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "m"#, # quarterly
  #units = "chg" # change over previous value
)

U_US <-ts(U_US$value, start = c(1990,1), frequency = 12)

U_USq <-fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

U_USq <-ts(U_USq$value, start = c(1990,1), frequency = 4)

FSI_US <-fredr(
  series_id = "STLFSI4",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

FSI_US <-ts(FSI_US$value, start = c(1990,1), frequency = 4)

PIC_US <-fredr(
  series_id = "PPIACO",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

PIC_US <-ts(PIC_US$value, start = c(1990,1), frequency = 4)

PCE_US <-fredr(
  series_id = "PCE",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

PCE_US <-ts(PCE_US$value, start = c(1969,1), frequency = 4)


BAA_spread <-fredr(
  series_id = "BAA10Y",
  observation_start = as.Date("1994-01-01"),
  observation_end = as.Date("2024-09-18"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

BAA_spread <-ts(BAA_spread$value, start = c(1994,1), frequency = 4)


DKK_USD <-fredr(
  series_id = "CCUSMA02DKM661N",
  observation_start = as.Date("1999-01-01"),
  observation_end = as.Date("2023-12-01"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

DKK_USD <-ts(DKK_USD$value, start = c(1990,1), frequency = 4)

#US private consumption
C_US <-fredr(
  series_id = "USAPFCEQDSNAQ",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

C_US <-ts(C_US$value, start = c(1969,1), frequency = 4)

#Median weekly real earnings
W_real_US <- fredr(
  series_id = "LES1252881600Q",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

W_real_US <-ts(W_real_US$value, start = c(1969,1), frequency = 4)

#Real gross private domestic investment
rGPDI <- fredr(
  series_id = "GPDIC1",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

rGPDI <-ts(rGPDI$value, start = c(1990,1), frequency = 4)

#Federal Funds Effective Rate
ffer <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

ffer <-ts(ffer$value, start = c(1969,1), frequency = 4)

#Market Yield on U.S. Treasury Securities at 1-Year Constant Maturity, Quoted on an Investment Basis 
US_1y <- fredr(
  series_id = "DGS1",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

US_1y <-ts(US_1y$value, start = c(1969,1), frequency = 4)

#Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity, Quoted on an Investment Basis 
US_2y <- fredr(
  series_id = "DGS2",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

US_2y <-ts(US_2y$value, start = c(1969,1), frequency = 4)


#Real US import service and goods
rIMP_US <- fredr(
  series_id = "IMPGSC1",
  observation_start = as.Date("1969-01-01"),
  observation_end = as.Date("2024-06-14"),
  frequency = "q"#, # quarterly
  #units = "chg" # change over previous value
)

rIMP_US <-ts(rIMP_US$value, start = c(1969,1), frequency = 4)




#################################
###Import data from Bundesbank###
#################################
GerBund1y <- getSeries("BBSSY.D.REN.EUR.A640.000000WT3030.A")
GerBund1y$dates <- as.Date(GerBund1y$dates)
zoo_data <- zoo(GerBund1y$values, order.by = GerBund1y$dates)
# Convert the daily frequency to monthly frequency by taking the average
GerBund1y_m <- aggregate(zoo_data, as.yearmon, mean) # tail, n = 1)
# Convert the resulting zoo object back to a dataframe
GerBund1y_m <- data.frame(dates = as.Date(index(GerBund1y_m)),
                          values = coredata(GerBund1y_m))
GerBund1y_m <-ts(GerBund1y_m$value, start = c(1993,4), end = c(2024,1), frequency = 12)

# Convert the daily frequency to quarterly frequency by taking the average
GerBund1y_q <- aggregate(zoo_data, as.yearqtr, mean) # tail, n = 1)
# Convert the resulting zoo object back to a dataframe
GerBund1y_q <- data.frame(dates = as.Date(index(GerBund1y_q)),
                          values = coredata(GerBund1y_q))
GerBund1y_q <-ts(GerBund1y_q$value, start = c(1993,4), end = c(2024,1), frequency = 4)

############################################
###Import data from ECB - SDW - series######
############################################
#Get Euro rate 1Y
Euro_1y <- get_data("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y")

Euro_1y$obstime <- convert_dates(Euro_1y$obstime)
Euro_1y$obstime <- as.Date(Euro_1y$obstime)
zoo_data <- zoo(Euro_1y$obsvalue, order.by = Euro_1y$obstime)
# Convert the monthly frequency to quarterly frequency by taking the average
index_year_month <- as.yearmon(index(zoo_data))
Euro_1y_m <- aggregate(zoo_data, index_year_month, mean)

# Convert the resulting zoo object back to a dataframe
Euro_1y_m <- data.frame(dates = as.Date(index(Euro_1y_m)),
                        values = coredata(Euro_1y_m))
Euro_1y_m <-ts(Euro_1y_m$values, start = c(2004,9),  frequency = 12)

dEuro_1y_m <- diff(Euro_1y_m)

zoo_data <- zoo(Euro_1y$obsvalue, order.by = Euro_1y$obstime)
index_year_quarter <- as.yearqtr(index(zoo_data))

Euro_1y_q <- aggregate(zoo_data, index_year_quarter, mean)

# Convert the resulting zoo object back to a dataframe
Euro_1y_q <- data.frame(dates = as.Date(index(Euro_1y_q)),
                        values = coredata(Euro_1y_q))
Euro_1y_q <-ts(Euro_1y_q$values, start = c(2004,3),  frequency = 4)


#Get Euro rate 2Y

Euro_2y <- get_data("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_2Y")

Euro_2y$obstime <- convert_dates(Euro_2y$obstime)
Euro_2y$obstime <- as.Date(Euro_2y$obstime)
zoo_data <- zoo(Euro_2y$obsvalue, order.by = Euro_2y$obstime)
# Convert the monthly frequency to quarterly frequency by taking the average
index_year_month <- as.yearmon(index(zoo_data))
Euro_2y_m <- aggregate(zoo_data, index_year_month, mean)

# Convert the resulting zoo object back to a dataframe
Euro_2y_m <- data.frame(dates = as.Date(index(Euro_2y_m)),
                        values = coredata(Euro_2y_m))
Euro_2y_m <-ts(Euro_2y_m$values, start = c(2004,9),   frequency = 12)

dEuro_2y_m <- diff(Euro_2y_m)

#Get monthly HCIP data 
HCIP_euroECB <- get_data("ICP.M.U2.Y.000000.3.INX")
HCIP_euroECB$obstime <- convert_dates(HCIP_euroECB$obstime)
HCIP_euroECB$obstime <- as.Date(HCIP_euroECB$obstime)
HCIP_euroECB_m<- ts(HCIP_euroECB$obsvalue, start = c(1997,1),  frequency = 12)

zoo_data <- zoo(HCIP_euroECB$obsvalue, order.by = HCIP_euroECB$obstime)
# Convert the monthly frequency to quarterly frequency by taking the average
HCIP_euroECB_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
HCIP_euroECB_q <- data.frame(dates = as.Date(index(HCIP_euroECB_q)),
                             values = coredata(HCIP_euroECB_q))
HCIP_euroECB_q <-ts(HCIP_euroECB_q$values, start = c(1997,1),  frequency = 4)

#Get monthly IP data
IP_eurozone <- get_data("STS.M.I9.Y.PROD.NS0020.4.000")
IP_eurozone$obstime <- convert_dates(IP_eurozone$obstime)
IP_eurozone$obstime <- as.Date(IP_eurozone$obstime)
IP_eurozone_m <-ts(IP_eurozone$obsvalue, start = c(1991,1),  frequency = 12)

zoo_data <- zoo(IP_eurozone$obsvalue, order.by = IP_eurozone$obstime)
# Convert the monthly frequency to quarterly frequency by taking the average
IP_eurozone_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
IP_eurozone_q <- data.frame(dates = as.Date(index(IP_eurozone_q)),
                            values = coredata(IP_eurozone_q))
IP_eurozone_q <-ts(IP_eurozone_q$values, start = c(1991,1),   frequency = 4)

#Get CISS - Composite Indicator of Systemic Stress
CISS <- get_data("CISS.M.U2.Z0Z.4F.EC.SOV_EW.IDX")
CISS$obstime <- convert_dates(CISS$obstime)
CISS$obstime <- as.Date(CISS$obstime)
CISS_m <-ts(CISS$obsvalue, start = c(2000,9),  frequency = 12)

# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(CISS$obsvalue, order.by = CISS$obstime)
CISS_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
CISS_q <- data.frame(dates = as.Date(index(CISS_q)),
                            values = coredata(CISS_q))
CISS_q <-ts(CISS_q$values, start = c(2000,3),  frequency = 4)

#Get ECB commodity price index

ECB_cpi <- get_data("STS.M.I9.N.ECPE.CNFOOD.3.000")
ECB_cpi$obstime <- convert_dates(ECB_cpi$obstime)
ECB_cpi$obstime <- as.Date(ECB_cpi$obstime)
ECB_cpi_m <-ts(ECB_cpi$obsvalue, start = c(1986,4),  frequency = 12)

# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(ECB_cpi $obsvalue, order.by = ECB_cpi $obstime)
ECB_cpi_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
ECB_cpi_q <- data.frame(dates = as.Date(index(ECB_cpi_q)),
                     values = coredata(ECB_cpi_q))
ECB_cpi_q <-ts(ECB_cpi_q$values, start = c(1986,2),  frequency = 4)

#Negotiated wages

neg_wages <- get_data("STS.Q.I9.N.INWR.000000.3.ANR")
neg_wages$obstime <- convert_dates(neg_wages$obstime)
neg_wages$obstime <- as.Date(neg_wages$obstime)
neg_wages <-ts(neg_wages$obsvalue, start = c(1992,2),  frequency = 4)

#Nominal effective euro rate
euro_ekr <- get_data("EXR.Q.E03.EUR.EN00.A")
euro_ekr$obstime <- convert_dates(euro_ekr$obstime)
euro_ekr$obstime <- as.Date(euro_ekr$obstime)
euro_ekr <-ts(euro_ekr$obsvalue, start = c(1993,1),  frequency = 4)

#Real effective euro rate
euro_rekr <- get_data("EXR.Q.E03.EUR.ERC0.A")
euro_rekr$obstime <- convert_dates(euro_rekr$obstime)
euro_rekr$obstime <- as.Date(euro_rekr$obstime)
euro_rekr <-ts(euro_rekr$obsvalue, start = c(1993,1),  frequency = 4)

#Exports of goods to rest of the world, DK
Export_DK_ECBdata <- get_data("QSA.Q.N.DK.W1.S1.S1.N.D.P61._Z._Z._Z.XDC._T.S.V.N._T")
Export_DK_ECBdata$obstime <- convert_dates(Export_DK_ECBdata$obstime)
Export_DK_ECBdata$obstime <- as.Date(Export_DK_ECBdata$obstime)
Export_DK_ECBdata <-ts(Export_DK_ECBdata$obsvalue, start = c(1999,1),  frequency = 4)



###################################################################
###########Import data from Eurostat###############################
###################################################################

#clean_eurostat_cache(cache_dir = NULL, config = FALSE)

HCIP_eurostat <- get_eurostat("prc_hicp_midx", time_format = "num", filters = list(geo="EA", coicop= "CP00", unit="I15"))
#HCIP_eurostat <- get_eurostat("tec00118", time_format = "num", filters = list(geo="EA20", coicop= "CP00", unit="I15"))

HCIP_eurostat_m<- ts(HCIP_eurostat$values, start = c(1996,1),  frequency = 12)


# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(HCIP_eurostat$values, order.by = HCIP_eurostat$time)
HCIP_eurostat_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
HCIP_eurostat_q <- data.frame(dates = as.Date(index(HCIP_eurostat_q)),
                       values = coredata(HCIP_eurostat_q))
HCIP_eurostat_q <-ts(HCIP_eurostat_q$values, start = c(1996,1),  frequency = 4)


Eonia <-  get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_DTD"))

Eonia <- ts(Eonia$values, start = c(1970,1),  frequency = 12)

Euribor_1m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M1"))

Euribor_1m <- ts(Euribor_1m$values, start = c(1970,1),   frequency = 12)

Euribor_3m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M3"))

Euribor_3m <- ts(Euribor_3m$values, start = c(1970,1), frequency = 12)

Euribor_6m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M6"))

Euribor_6m <- ts(Euribor_6m$values, start = c(1970,1),  frequency = 12)

DK_3mECB <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "DK", int_rt = "IRT_M3"))

DK_3m <- ts(DK_3mECB$values, start = c(1970,1),   frequency = 12)

DK_12m <-get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "DK", int_rt = "IRT_M12"))

DK_12m <- ts(DK_12m$values, start = c(1970,1),  frequency = 12)

zoo_data <- zoo(DK_3mECB$values, order.by = DK_3mECB$time)
# Convert the monthly frequency to quarterly frequency by taking the average
DK_3m_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
DK_3m_q <- data.frame(dates = as.Date(index(DK_3m_q)),
                      values = coredata(DK_3m_q))
DK_3m_q <-ts(DK_3m_q$values, start = c(1970,1),  frequency = 4)

DK_12m <- na.omit(DK_12m)

DK_12m <- as.ts(DK_12m)

DK_12m_q <- aggregate(DK_12m, nfrequency = 4, FUN = mean)

#euro area unemployment
euro_u <- get_eurostat("une_rt_m", time_format = "num", filters = list(geo = "EA20", age="TOTAL", s_adj="SA", sex = "T", unit="PC_ACT" ))

euro_u_m <- ts(euro_u$values, start = c(1983,1),   frequency = 12)

# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(euro_u$values, order.by = euro_u$time)
euro_u_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
euro_u_q <- data.frame(dates = as.Date(index(euro_u_q)),
                      values = coredata(euro_u_q))
euro_u_q <-ts(euro_u_q$values, start = c(1983,1),  frequency = 4)

#DK unemployment
u_DK <- get_eurostat("une_rt_m", time_format = "num", filters = list(geo = "DK", age="TOTAL", s_adj="SA", sex = "T", unit="PC_ACT" ))

u_DK_m <- ts(u_DK$values, start = c(1983,1),   frequency = 12)

# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(u_DK$values, order.by = euro_u$time)
u_DK_q <- aggregate(zoo_data, as.yearqtr, mean)
# Convert the resulting zoo object back to a dataframe
u_DK_q <- data.frame(dates = as.Date(index(euro_u_q)),
                       values = coredata(euro_u_q))
u_DK_q <-ts(u_DK_q$values, start = c(1983,1),  frequency = 4)



# euro are labour productivity
euro_prod <- get_eurostat("namq_10_lp_ulc", time_format = "num", filters = list(geo = "EA", na_item = "RLPR_HW", s_adj="SCA",  unit="I20" ))

euro_prod <- ts(euro_prod$values, start = c(1980,1),   frequency = 4)

#real GDP
rGDP_swe <-  get_eurostat("naidq_10_gdp", time_format = "num", filters = list(geo ="SE", na_item = "B1GQ", s_adj = "SCA",
      unit = "CLV_I10"))
rGDP_swe <-ts(rGDP_swe$values, start = c(1975,1),  frequency = 4)

rGDP_NL <- get_eurostat("naidq_10_gdp", time_format = "num", filters = list(geo ="NL", na_item = "B1GQ", s_adj = "SCA",
                        unit = "CLV_I10"))
rGDP_NL <-ts(rGDP_NL$values, start = c(1975,1),  frequency = 4)

rGDP_BE <- get_eurostat("naidq_10_gdp", time_format = "num", filters = list(geo ="BE", na_item = "B1GQ", s_adj = "SCA",
                        unit = "CLV_I10"))
rGDP_BE <-ts(rGDP_BE$values, start = c(1975,1),  frequency = 4)

rGDP_DK <-  get_eurostat("naidq_10_gdp", time_format = "num", filters = list(geo ="DK", na_item = "B1GQ", s_adj = "SCA",
                                                                              unit = "CLV_I10"))
rGDP_DK <-ts(rGDP_DK$values, start = c(1975,1),  frequency = 4)

rGDP_EA <- get_eurostat("namq_10_gdp",time_format = "num", filters = list(geo="EA", na_item = "B1GQ", s_adj="SCA", unit="CLV_I10"))

rGDP_EA <- ts(rGDP_EA$values, start = c(1975,1),  frequency = 4)


# Final consumption by households
C_swe <-  get_eurostat("namq_10_fcs", time_format = "num", filters = list(geo ="SE", na_item = "P31_S14", s_adj = "SCA",
                                                                              unit = "CLV10_MEUR"))
C_swe <-ts(C_swe$values, start = c(1978,1),  frequency = 4)

C_NL <- get_eurostat("namq_10_fcs", time_format = "num", filters = list(geo ="NL", na_item = "P31_S14", s_adj = "SCA",
                                                                            unit = "CLV10_MEUR"))
C_NL <-ts(C_NL$values, start = c(1978,1),  frequency = 4)

C_BE <- get_eurostat("namq_10_fcs", time_format = "num", filters = list(geo ="BE", na_item = "P31_S14", s_adj = "SCA",
                                                                            unit = "CLV10_MEUR"))
C_BE <-ts(C_BE$values, start = c(1978,1),  frequency = 4)

C_DK <-  get_eurostat("namq_10_fcs", time_format = "num", filters = list(geo ="DK", na_item = "P31_S14", s_adj = "SCA",
                                                                             unit = "CLV10_MEUR"))
C_DK <-ts(C_DK$values, start = c(1978,1),  frequency = 4)

C_euro <-  get_eurostat("namq_10_fcs", time_format = "num", filters = list(geo ="EA", na_item = "P31_S14", s_adj = "SCA",
                                                                         unit = "CLV10_MEUR"))
C_euro <-ts(C_euro$values, start = c(1978,1),  frequency = 4)

# Inflation (HICP)

# HCIP_eurostat <- get_eurostat("prc_hicp_midx", time_format = "num", filters = list(geo="EA", coicop= "CP00", unit="I15"))
# #HCIP_eurostat <- get_eurostat("tec00118", time_format = "num", filters = list(geo="EA20", coicop= "CP00", unit="I15"))
# 
# HCIP_eurostat_m<- ts(HCIP_eurostat$values, start = c(1996,1), end = c(2024,1),  frequency = 12)
# 
# 
# # Convert the monthly frequency to quarterly frequency by taking the average
# zoo_data <- zoo(HCIP_eurostat$values, order.by = HCIP_eurostat$time)
# HCIP_eurostat_q <- aggregate(zoo_data, as.yearqtr, mean)
# # Convert the resulting zoo object back to a dataframe
# HCIP_eurostat_q <- data.frame(dates = as.Date(index(HCIP_eurostat_q)),
#                               values = coredata(HCIP_eurostat_q))
# HCIP_eurostat_q <-ts(HCIP_eurostat_q$values, start = c(1996,1),  frequency = 4)

# Define the list of countries
geo_list <- c("DK", "BE", "NL", "SE", "EA")

# Initialize an empty list to store the results for each country
HCIP_results <- list()

# Loop through each geo in the geo_list
for (geo in geo_list) {
  
  # Fetch the data from Eurostat
  HCIP_eurostat <- get_eurostat("prc_hicp_midx", time_format = "num", 
                                filters = list(geo = geo, coicop = "CP00", unit = "I15"))
  
  # Create a time series from the data
  HCIP_eurostat_m <- ts(HCIP_eurostat$values, start = c(1996,1),  frequency = 12)
  
  # Convert the monthly frequency to quarterly frequency by taking the average
  zoo_data <- zoo(HCIP_eurostat$values, order.by = HCIP_eurostat$time)
  HCIP_eurostat_q <- aggregate(zoo_data, as.yearqtr, mean)
  
  # Convert the resulting zoo object back to a data frame
  HCIP_eurostat_q <- data.frame(dates = as.Date(index(HCIP_eurostat_q)),
                                values = coredata(HCIP_eurostat_q))
  
  # Convert the data frame back to a time series with quarterly frequency
  HCIP_eurostat_q_ts <- ts(HCIP_eurostat_q$values, start = c(1996,1), frequency = 4)
  
  # Store the result in the list, using the geo code as the key
  HCIP_results[[geo]] <- HCIP_eurostat_q_ts
}

# You can access each country's data like this: HCIP_results[["DK"]], HCIP_results[["BE"]], etc.

HCIP_Swe <- HCIP_results[["SE"]]
HCIP_BE <- HCIP_results[["BE"]]
HCIP_NL <- HCIP_results[["NL"]]
HCIP_EA <- HCIP_results[["EA"]]
HCIP_DK <- HCIP_results[["DK"]]


#Euro zone inflation excl. energy
HCIP_EA_excl_energy <- get_eurostat("prc_hicp_midx", time_format = "num", 
                              filters = list(geo = "EA", coicop = "TOT_X_NRG", unit = "I15"))

# Create a time series from the data
HCIP_EA_excl_energy_m <- ts(HCIP_EA_excl_energy$values, start = c(1996,1),  frequency = 12)

# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(HCIP_EA_excl_energy$values, order.by = HCIP_EA_excl_energy$time)
HCIP_EA_excl_energy_q <- aggregate(zoo_data, as.yearqtr, mean)

# Convert the resulting zoo object back to a data frame
HCIP_EA_excl_energy_q <- data.frame(dates = as.Date(index(HCIP_EA_excl_energy_q)),
                              values = coredata(HCIP_EA_excl_energy_q))

# Convert the data frame back to a time series with quarterly frequency
HCIP_EA_excl_energy_q <- ts(HCIP_EA_excl_energy_q$values, start = c(1996,1), frequency = 4)


# Gross fixed capital formation (investments)

# Define the list of countries
geo_list <- c("DK", "BE", "NL", "SE", "EA20")

# Initialize an empty list to store the results for each country
GFCF <- list()

# Loop through each geo in the geo_list
for (geo in geo_list) {
  
  # Fetch the data from Eurostat
  GFCF_eurostat <- get_eurostat("naidq_10_gdp", time_format = "num", 
                                filters = list(geo = geo, na_item = "P51G", s_adj ="SCA", unit = "CP_MEUR"))
  
  # Convert the data frame back to a time series with quarterly frequency
  GFCF_eurostat <- ts(GFCF_eurostat$values, start = c(1975,1), end = c(2024,2), frequency = 4)
  
  # Store the result in the list, using the geo code as the key
  GFCF[[geo]] <- GFCF_eurostat
}

# You can access each country's data like this: HCIP_results[["DK"]], HCIP_results[["BE"]], etc.

GFCF_Swe <- GFCF[["SE"]]
GFCF_BE <- GFCF[["BE"]]
GFCF_NL <- GFCF[["NL"]]
GFCF_EA <- GFCF[["EA"]]
GFCF_DK <- GFCF[["DK"]]


# Export (value)
EX_dk <- get_eurostat("namq_10_exi", time_format = "num", filters = list(geo="DK", na_item = "P6", s_adj = "SCA", unit="CLV10_MEUR"))
EX_dk = EX_dk[-c(1:5),]                                                                        
EX_dk <-ts(EX_dk$values, start = c(1991,1),  frequency = 4)

EX_dk_euro <- get_eurostat("namq_10_exi", time_format = "num", filters = list(geo="DK", na_item = "P6_S2111", s_adj = "SCA", unit="CLV10_MEUR"))
EX_dk_euro = EX_dk_euro[-c(1),]                                                                        
EX_dk_euro <-ts(EX_dk_euro$values, start = c(1990,1),  frequency = 4)


EX_dk_currentP <- get_eurostat("namq_10_exi", time_format = "num", filters = list(geo="DK", na_item = "P6", s_adj = "SCA", unit="CP_MNAC"))
EX_dk_currentP = EX_dk_currentP[-c(1),]                                                                        
EX_DK_currentP <-ts(EX_dk_currentP$values, start = c(1990,1),  frequency = 4)



# Define the list of countries
geo_list <- c("DK", "BE", "NL", "SE")

#Generate time periods from 1999Q1 to 2024Q2 in the correct chronological order
years <- 1999:2024
quarters <- c("Q1", "Q2", "Q3", "Q4")

# Create a time range where quarters are ordered for each year
time_range <- c(sapply(years[1:25], function(year) paste0(year, "-", quarters)),
                paste0(2024, "-Q", 1:2))
# Initialize an empty list to store the results for each country
Export_results <- list()

# Loop through each geo in the geo_list
for (geo in geo_list) {
  
  # Fetch the data from Eurostat
  Export <- get_eurostat("namq_10_exi", time_format = "num", filters = list(geo=geo, na_item = "P6", s_adj = "SCA",
                                                                            time = time_range,unit="CLV10_MEUR"))

  # # Convert the monthly frequency to quarterly frequency by taking the average
  # zoo_data <- zoo(Export$values, order.by = Export$time)
  # Export_q <- aggregate(zoo_data, as.yearqtr, mean)
  # 
  # # Convert the resulting zoo object back to a data frame
  # Export_q <- data.frame(dates = as.Date(index(Export_q)),
  #                               values = coredata(Export_q))
  
  # Convert the data frame back to a time series with quarterly frequency
  Export_q <- ts(Export$values, start = c(1999,1), frequency = 4)
  
  # Store the result in the list, using the geo code as the key
  Export_results[[geo]] <- Export_q
}

Export_Swe <- Export_results[["SE"]]
Export_BE <- Export_results[["BE"]]
Export_NL <- Export_results[["NL"]]
Export_DK <- Export_results[["DK"]]

#DK export to eurozone and non-eurozone
Export_DK_to_EMU <- get_eurostat("namq_10_exi", time_format = "num", filters = list(geo="DK", na_item = "P6_S2111", s_adj = "SCA",
                                                                          time = time_range,unit="CLV10_MEUR"))
Export_DK_to_EMU_q <- ts(Export_DK_to_EMU$values, start = c(1999,1), frequency = 4)

Export_DK_to_nonEMU <- get_eurostat("namq_10_exi", time_format = "num", filters = list(geo="DK", na_item = "P6_S2112", s_adj = "SCA",
                                                                                    time = time_range,unit="CLV10_MEUR"))
Export_DK_to_nonEMU_q <- ts(Export_DK_to_nonEMU$values, start = c(1999,1), frequency = 4)

                                                                      

# Money market interest rates 
# Define the list of countries
geo_list <- c("DK", "SE", "EA")

# Initialize an empty list to store the results for each country
IBM3_results <- list()

# Loop through each geo in the geo_list
for (geo in geo_list) {
  
  # Fetch the data from Eurostat
  IBM3 <- get_eurostat("irt_st_m", time_format = "num", 
                                filters = list(geo = geo, int_rt = "IRT_M3"))
  
  # Convert the monthly frequency to quarterly frequency by taking the average
  zoo_data <- zoo(IBM3$values, order.by = IBM3$time)
  IBM3_results_q <- aggregate(zoo_data, as.yearqtr, mean)
  
  # Convert the resulting zoo object back to a data frame
  IBM3_results_q <- data.frame(dates = as.Date(index(IBM3_results_q)),
                                values = coredata(IBM3_results_q))
  
  # Convert the data frame back to a time series with quarterly frequency
  IBM3_results_q_ts <- ts(IBM3_results_q$values, start = c(1970,1), frequency = 4)
  
  # Store the result in the list, using the geo code as the key
  IBM3_results[[geo]] <- IBM3_results_q_ts
}

# You can access each country's data like this: HCIP_results[["DK"]], HCIP_results[["BE"]], etc.

IBM3_Swe <- IBM3_results[["SE"]]
IBM3_EA <- IBM3_results[["EA"]]
IBM3_DK <- IBM3_results[["DK"]]

# Initialize an empty list to store the results for each country
IBM12_results <- list()

# Loop through each geo in the geo_list
for (geo in geo_list) {
  
  # Fetch the data from Eurostat
  IBM12 <- get_eurostat("irt_st_m", time_format = "num", 
                       filters = list(geo = geo, int_rt = "IRT_M12"))
  
  # Convert the monthly frequency to quarterly frequency by taking the average
  zoo_data <- zoo(IBM12$values, order.by = IBM12$time)
  IBM12_results_q <- aggregate(zoo_data, as.yearqtr, mean)
  
  # Convert the resulting zoo object back to a data frame
  IBM12_results_q <- data.frame(dates = as.Date(index(IBM12_results_q)),
                               values = coredata(IBM12_results_q))
  
  # Convert the data frame back to a time series with quarterly frequency
  IBM12_results_q_ts <- ts(IBM12_results_q$values, start = c(1970,1), frequency = 4)
  
  # Store the result in the list, using the geo code as the key
  IBM12_results[[geo]] <- IBM12_results_q_ts
}

# You can access each country's data like this: HCIP_results[["DK"]], HCIP_results[["BE"]], etc.

IBM12_Swe <- IBM12_results[["SE"]]
IBM12_EA <- IBM12_results[["EA"]]
IBM12_DK <- IBM12_results[["DK"]]


# interest rates
Eonia <-  get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_DTD"))

Eonia <- ts(Eonia$values, start = c(1970,1),  frequency = 12)

dEonia <- diff(Eonia)

Euribor_1m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M1"))

Euribor_1m <- ts(Euribor_1m$values, start = c(1970,1),   frequency = 12)

Euribor_3m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M3"))

Euribor_3m <- ts(Euribor_3m$values, start = c(1970,1),   frequency = 12)

Euribor_6m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M6"))

Euribor_6m <- ts(Euribor_6m$values, start = c(1970,1),  frequency = 12)

Euribor_12m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "EA", int_rt = "IRT_M12"))

Euribor_12m <- ts(Euribor_12m$values, start = c(1994,1),   frequency = 12)

DK_3m <- get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "DK", int_rt = "IRT_M3"))

DK_3m <- ts(DK_3m$values, start = c(1970,1), frequency = 12)

DK_12m <-get_eurostat("irt_st_m", time_format = "num", filters = list(geo = "DK", int_rt = "IRT_M12"))

DK_12m <- ts(DK_12m$values, start = c(1970,1),  frequency = 12)

dEuribor_3m <- diff(Euribor_3m)

dEuribor_6m <- diff(Euribor_6m)

dEuribor_12m <- diff(Euribor_12m)

dDK_12m <- diff(DK_12m)

#Import prices
PImp_eurozone <- get_eurostat("sts_inpr_m",time_format = "num", filters = list(geo="EA20", s_adj="SCA", nace_r2="B-D", unit="I21"))

# Convert the monthly frequency to quarterly frequency by taking the average
zoo_data <- zoo(PImp_eurozone$values, order.by = PImp_eurozone$time)
PImp_eurozone_q <- aggregate(zoo_data, as.yearqtr, mean)

# Convert the resulting zoo object back to a data frame
PImp_eurozone_q <- data.frame(dates = as.Date(index(PImp_eurozone_q)),
                              values = coredata(PImp_eurozone_q))

# Convert the data frame back to a time series with quarterly frequency
PImp_eurozone_q <- ts(PImp_eurozone_q$values, start = c(1953,1), frequency = 4)


###############################################
###########Import data from DST################
###############################################
exEU_meta <- dst_meta("UHV2", lang = "da")
# 
# ex_EU <- dst_get_data(table = "UHV2", INDUD = "Eksport (ekskl. skibe og fly, br?ndsel mv.)",
#                       S?SON = "S?sonkorrigeret", LAND = "EU-27 (uden Storbritannien)",
#                       Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT
# 
# ex_EU<- ts(ex_EU$value, start = c(1997,1), end = c(2024,2),  frequency = 12)
# 
# ex_EU_zoo <- zoo(ex_EU)
# 
# ex_EU_q <- aggregate(ex_EU_zoo, as.yearqtr, mean)
# 
# # Convert the resulting zoo object back to a dataframe
# ex_EU_q <- data.frame(dates = as.Date(index(ex_EU_q)),
#                       values = coredata(ex_EU_q))
# 
# ex_EU_q <-ts(ex_EU_q$values, start = c(1997,1), end = c(2024,1),  frequency = 4)
# 

Export_DK_excl_ship_etc <- dst_get_data(table = "UHV2", INDUD = "Eksport (ekskl. skibe og fly, brændsel mv.)",
                      SæSON = "Sæsonkorrigeret", LAND = "LANDE I ALT",
                      Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

Export_DK_excl_ship_etc<- ts(Export_DK_excl_ship_etc$value, start = c(1997,1),  frequency = 12)

Export_DK_excl_ship_etc_zoo <- zoo(Export_DK_excl_ship_etc)

Export_DK_excl_ship_etc <- aggregate(Export_DK_excl_ship_etc_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
Export_DK_excl_ship_etc <- data.frame(dates = as.Date(index(Export_DK_excl_ship_etc)),
                      values = coredata(Export_DK_excl_ship_etc))

Export_DK_excl_ship_etc <-ts(Export_DK_excl_ship_etc$values, start = c(1997,1),  frequency = 4)



Export_DK_goods <- dst_get_data(table = "UHV1", INDUD = "Eksport",
                                     SæSON = "Sæsonkorrigeret", ART = "I ALT",
                                     Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

Export_DK_goods<- ts(Export_DK_goods$value, start = c(1997,1),  frequency = 12)

Export_DK_goods_zoo <- zoo(Export_DK_goods)

Export_DK_goods <- aggregate(Export_DK_goods_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
Export_DK_goods <- data.frame(dates = as.Date(index(Export_DK_goods)),
                                   values = coredata(Export_DK_goods))

Export_DK_goods <-ts(Export_DK_goods$values, start = c(1997,1),  frequency = 4)

# total export (goods and services) in 2020 prices from revised national accounts

Export_DK_total_DST <- dst_get_data(table = "NKN1", TRANSAKT = "P.6 Eksport af varer og tjenester",
                           PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

Export_DK_total_DST <-ts(Export_DK_total_DST$value, start = c(1990,1),  frequency = 4)

# total export goods in 2020 prices from revised national accounts
Export_DK_goods_DST <- dst_get_data(table = "NKN1", TRANSAKT = "P.61 Eksport af varer",
                                    PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

Export_DK_goods_DST <-ts(Export_DK_goods_DST$value, start = c(1990,1),  frequency = 4)


#Forbrugprisindex
CPI_DK_meta <- dst_meta("PRIS111", lang = "da")

CPI_DK <- dst_get_data(table = "PRIS111",VAREGR = "00 Forbrugerprisindekset i alt", ENHED = "Indeks",Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

CPI_DK<- ts(CPI_DK$value, start = c(2001,1),  frequency = 12)


CPI_DK_zoo <- zoo(CPI_DK)

CPI_DK_q <- aggregate(CPI_DK_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
CPI_DK_q <- data.frame(dates = as.Date(index(CPI_DK_q)),
                      values = coredata(CPI_DK_q))

CPI_DK_q <-ts(CPI_DK_q$values, start = c(2001,1),   frequency = 4)

CPI_DK_excl_energy <- dst_get_data(table = "PRIS111",VAREGR = "15.1 Forbrugerprisindeks ekskl. energi", ENHED = "Indeks",Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

CPI_DK_excl_energy<- ts(CPI_DK_excl_energy$value, start = c(2001,1), frequency = 12)


CPI_DK_excl_energy_zoo <- zoo(CPI_DK_excl_energy)

CPI_DK_excl_energy_q <- aggregate(CPI_DK_excl_energy_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
CPI_DK_excl_energy_q <- data.frame(dates = as.Date(index(CPI_DK_excl_energy_q)),
                       values = coredata(CPI_DK_excl_energy_q))

CPI_DK_excl_energy_q <-ts(CPI_DK_excl_energy_q$values, start = c(2001,1),  frequency = 4)

GDP_DK_dst <-dst_meta("NKN1", lang = "da")

GDP_DK_dst <- dst_get_data(table = "NKN1", TRANSAKT = "B.1*g Bruttonationalprodukt, BNP",
                           PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") #"EU-27 (uden Storbritannien)" LANDE I ALT

GDP_DK_dst<- ts(GDP_DK_dst$value, start = c(1990,1), frequency = 4)

#real effective DKK 
refkrks <- dst_get_data(table = "DNVALQ", VALUTA = "Nominel effektiv kronekurs (Jan. 1970-)",
                           KURTYP = "Real effektiv kronekurs baseret på timelønninger, indeks 1980=100",
                        OPGOER = "Beregnet", Tid = "*") 

refkrks<- ts(refkrks$value, start = c(1970,1), frequency = 4)

#Exportprice 

pex <- dst_get_data(table = "PRIS4315", ENHED = "Indeks", HOVEDGRP = "C Fremstillingsvirksomhed", 
                    MARKED1 = "Eksportmarked", Tid = "*") 
pex<- ts(pex$value, start = c(2005,1), frequency = 12)

pex_zoo <- zoo(pex)

pex_q <- aggregate(pex_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
pex_q <- data.frame(dates = as.Date(index(pex_q)),
                       values = coredata(pex_q))

pex_q <-ts(pex_q$values, start = c(2005,1),   frequency = 4)

#Gross investment less housing

gross_inv <- dst_get_data(table = "NKHI", AKTIV = "Bruttoinvesteringer",
                          PRISENHED = "2020-priser, kædede værdier",SÆSON = "Sæsonkorrigeret", Tid = "*") 

gross_inv<- ts(gross_inv$value, start = c(1990,1), frequency = 4)


gross_inv_fixed <- dst_get_data(table = "NKHI", AKTIV = "Faste bruttoinvesteringer",
                          PRISENHED = "2020-priser, kædede værdier",SÆSON = "Sæsonkorrigeret", Tid = "*") 

gross_inv_fixed<- ts(gross_inv_fixed$value, start = c(1990,1), frequency = 4)

housing_inv <- dst_get_data(table = "NKHI", AKTIV = "Boliger",
                          PRISENHED = "2020-priser, kædede værdier",SÆSON = "Sæsonkorrigeret", Tid = "*") 

housing_inv<- ts(housing_inv$value, start = c(1990,1), frequency = 4)

Other_building_inv <- dst_get_data(table = "NKHI", AKTIV = "Andre bygninger",
                           PRISENHED = "2020-priser, kædede værdier",SÆSON = "Sæsonkorrigeret", Tid = "*") 

Other_building_inv<- ts(Other_building_inv$value, start = c(1990,1), frequency = 4)

Facility_inv <- dst_get_data(table = "NKHI", AKTIV = "Anlæg",
                                   PRISENHED = "2020-priser, kædede værdier",SÆSON = "Sæsonkorrigeret", Tid = "*") 

Facility_inv<- ts(Facility_inv$value, start = c(1990,1), frequency = 4)

gross_inv_excl_house_facility <- gross_inv - housing_inv -Other_building_inv -Facility_inv

gross_invfixed_excl_house_facility <- gross_inv_fixed - housing_inv -Other_building_inv -Facility_inv


housing_facility_inv <- housing_inv + Other_building_inv + Facility_inv

#Business indicator for building and facilities

BC_indicator_housing_facilities <- dst_get_data(table = "KBYG44", INDIKATOR = "Sammensat konjunkturindikator, i alt",
                             SÆSON = "Sæsonkorrigeret", Tid = "*") 

BC_indicator_housing_facilities<- ts(BC_indicator_housing_facilities$value, start = c(1998,1), frequency = 12)

BC_indicator_housing_facilities_zoo <- zoo(BC_indicator_housing_facilities)

BC_indicator_housing_facilities_q <- aggregate(BC_indicator_housing_facilities_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
BC_indicator_housing_facilities_q <- data.frame(dates = as.Date(index(BC_indicator_housing_facilities_q)),
                    values = coredata(BC_indicator_housing_facilities_q))

BC_indicator_housing_facilities_q  <-ts(BC_indicator_housing_facilities_q $values, start = c(1998,1),   frequency = 4)

#DK import of goods and services
Import_goods_services_DK <- dst_get_data(table = "NKN1", TRANSAKT = "P.7 Import af varer og tjenester",
                           PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") 

Import_goods_services_DK<- ts(Import_goods_services_DK$value, start = c(1990,1), frequency = 4)

#DK export of goods and services
Export_goods_services_DK <- dst_get_data(table = "NKN1", TRANSAKT = "P.6 Eksport af varer og tjenester",
                                         PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") 

Export_goods_services_DK<- ts(Export_goods_services_DK$value, start = c(1990,1), frequency = 4)
#Private consumption
Private_consumption_DK <- dst_get_data(table = "NKN1", TRANSAKT = "P.31 Privatforbrug",
                                         PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") 

Private_consumption_DK<- ts(Private_consumption_DK$value, start = c(1990,1), frequency = 4)

# Gross investment 
Gross_investment_DK <- dst_get_data(table = "NKN1", TRANSAKT = "P.5g Bruttoinvesteringer",
                                       PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",SÆSON = "Sæsonkorrigeret", Tid = "*") 

Gross_investment_DK<- ts(Gross_investment_DK$value, start = c(1990,1), frequency = 4)

#Importprice from DST

PM_dst <- dst_get_data(table = "PRIS4321", ENHED = "Indeks", HOVEDGRP = "C33 Fremstillingsindustri ekskl. forarb. af kød og fisk samt ekskl. fremst. af tobak, koks og raffinerede mineral olieprodukter", 
                    MARKED1 = "Import", Tid = "*") 
PM_dst<- ts(PM_dst$value, start = c(2005,1), frequency = 12)

PM_dst_zoo <- zoo(PM_dst)

PM_dst_q <- aggregate(PM_dst_zoo, as.yearqtr, mean)

# Convert the resulting zoo object back to a dataframe
PM_dst_q <- data.frame(dates = as.Date(index(PM_dst_q)),
                    values = coredata(PM_dst_q))

PM_dst_q <-ts(PM_dst_q$values, start = c(2005,1),   frequency = 4)

# Housing prices




load("SVAR/Data/interpolatedrGDP_E20.RData")
#Interpolate data
library(tempdisagg)

#set variables to the same date
start_date <- c(2000,1)
end_date <- c(2024,6)

rGDP_EA_int <- window(rGDP_EA, start =c(2000,1), end= c(2024,2))
unemployment <- window(euro_u_m, start =start_date, end= end_date)
IP_int <- window(IP_eurozone_m, start =start_date, end= end_date)
# Methods to be used
methods <- c('chow-lin-maxlog',"chow-lin-minrss-ecotrim",
             "chow-lin-minrss-quilis", "litterman-maxlog","litterman-minrss",
             "dynamic-maxlog", "dynamic-minrss","dynamic-fixed" )

# Pre-allocation of object for interpolation results:
intrawdata <- list()

for (j in 1:length(methods)){
  
  tempresult <- c()
  
  # Interpolating:
  temptd  <- td(rGDP_EA_int~unemployment + IP_int, to = 12, method = methods[j])
  
  # Appending results of countries:
  tempresult <- cbind(tempresult,temptd$values)
  
  
  intrawdata[[j]] <- log(tempresult)
  # Saving R-squared:
  #r2[i,j] <- summary(temptd)$r.squared
  
}

rGDPm  <- td(rGDP_EA_int~unemployment + IP_int, to = 12, method = 'chow-lin-maxlog')

rGDPm <-rGDPm$values


MPshock = FALSE
save.image("load_raw_data_FE.RData")