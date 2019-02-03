library(rsconnect)

setwd("C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan/R/SexDiff_App/")

rsconnect::setAccountInfo(name='population-health',
                          token='D7E026818B6A1E89694B76F6542F5716',
                          secret='1z2po2GHWx0eQ5cfqUgtAG6LL+U9sBX0cVjtFjkW')

rsconnect::deployApp(account = 'population-health') 
Y
runApp()

countries <- unique(HMDL$PopName)

#to try the app
country  <- country.ind <- 'AUS'
year1    <- year1.ind <- 2000
year2    <- year2.ind <- 2010
year.sex <- yearsex.ind <- 2005

#sensitivy
Data[,list(sum(eo),sum(ed)), by = list(sex)]

HMDL[HMDL$Age == 0 & HMDL$Year %in% c(2010) & HMDL$PopName == 'AUS',]$ex -
  HMDL[HMDL$Age == 0 & HMDL$Year %in% c(2000) & HMDL$PopName == 'AUS',]$ex

HMDL[HMDL$Age == 0 & HMDL$Year %in% c(2010) & HMDL$PopName == 'AUS',]$ed -
  HMDL[HMDL$Age == 0 & HMDL$Year %in% c(2000) & HMDL$PopName == 'AUS',]$ed
