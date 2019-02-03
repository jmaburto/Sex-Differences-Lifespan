library(data.table)
library(reshape2)
library(HMDHFDplus)

# #get data from HMD
XYZ <- getHMDcountries()
us <- "jmaburto@colmex.mx"
pw <- "kolmogorov"

setwd(  "C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan-Equality/Big picture project/")
# now grab all the lifetables and mesh together..
# grab them all
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY
}, us = us, pw = pw))

HMDL <- data.table(HMDL)
save(HMDL,file="Data/HMDLT.RData")

HMD_Counts <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Deaths          <- readHMDweb(x,"Deaths_1x1",username=us,password=pw)
  Exposures       <- readHMDweb(x,"Exposures_1x1",username=us,password=pw)
  Deaths$Type     <- 'Deaths'
  Exposures$Type  <- 'Exposures'
  CTRY         <- rbind(Deaths, Exposures)
  CTRY$PopName <- x
  CTRY
}, us = us, pw = pw))

HMD_Counts <- data.table(HMD_Counts)
save(HMD_Counts,file="Data/HMD_Counts.RData")

