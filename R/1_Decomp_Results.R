############# Written by JMA
############# Project with Virginia Zarulli, Ridhi Kashyap and Alyson van Raalte
rm(list=ls(all=TRUE))
library(reshape2)
library(data.table)
library(parallelsugar)
library(DemoDecomp)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan-Equality/Big picture project/")
#source("R/0_Get_HMD.R") # Just in case you want updated HMD Data

#load life expectancy and lifespan variation functions
source('R/Functions_1.R')

#load data
load('Data/HMDLT.RData')


#Get deocomposition over time of ecery country in HMD
countries <- sort(unique(HMDL$PopName))
sex       <- sort(unique(HMDL$Sex))

#countries <- countries[1]


######## Algorith to decompose over time all HMD countries life expectancy and lifespan disparity
Decomp_time <- lapply(countries,function(x, HMDL,sex,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx){
  
  y <- HMDL[HMDL$PopName == x,]
  
  decomp.country <- lapply(sex, function(z,y,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx){
    
    w     <- y[y$Sex == z]
    years <- unique(w$Year)
    #years <- 1921:1923
    #yr <- 1921
    
    decomp.list <- mclapply(years[-length(years)], function(yr,w,LifeExpectancy,edagger.frommx,z){
      
      e0.decomp <- horiuchi(func = LifeExpectancy, pars1 = w[w$Year == yr,]$mx, pars2 = w[w$Year == (yr+1),]$mx, sex = z,N = 25)
      ed.decomp <- horiuchi(func = edagger.frommx, pars1 = w[w$Year == yr,]$mx, pars2 = w[w$Year == (yr+1),]$mx, sex = z,N = 25)
      
      e0.decomp           <- data.table(cbind(e0.decomp,yr))
      e0.decomp$ed.decomp <- ed.decomp
      
      e0.decomp
      
    },w =w,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx,z=z, mc.cores = 4)
    
    names(decomp.list) <- years[-length(years)]
    
    decomp.list
    
  }, y = y,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx)
  
  names(decomp.country) <- sex
  
  decomp.country
  
}, HMDL=HMDL,sex= sex,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx)

names(Decomp_time) <- countries

save(Decomp_time,file='Data/Decomp_timeHMD.RData')

######## Algorith to decompose by all HMD countries life expectancy and lifespan disparity

Decomp_Sex <- lapply(countries,function(x, HMDL,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx){
  
  y     <- HMDL[HMDL$PopName == x,]
  years <- unique(y$Year)
    
    decomp.list <- mclapply(years, function(yr,LifeExpectancy,edagger.frommx,y){
      
      e0.decomp <- horiuchi(func = LifeExpectancy, pars1 = y[y$Year == yr & y$Sex == 'f',]$mx, pars2 = y[y$Year == yr & y$Sex == 'm',]$mx, sex = 'f',N = 25)
      ed.decomp <- horiuchi(func = edagger.frommx, pars1 = y[y$Year == yr  & y$Sex == 'f',]$mx, pars2 = y[y$Year == yr & y$Sex == 'm',]$mx, sex = 'f',N = 25)
      
      e0.decomp           <- data.table(cbind(e0.decomp,yr))
      e0.decomp$ed.decomp <- ed.decomp
      
      e0.decomp
      
    },LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx,y=y, mc.cores = 4)
    
    names(decomp.list) <- years
    
    decomp.list
    
  
}, HMDL=HMDL,LifeExpectancy=LifeExpectancy,edagger.frommx=edagger.frommx)

names(Decomp_Sex) <- countries


save(Decomp_Sex,file='Data/Decomp_SexHMD.RData')


######## Add lifespan disparity to HMD Data
 #mx <- HMDL[HMDL$Year == 2000 & HMDL$Sex == 'f' & HMDL$PopName == 'AUS',]$mx
 #sex <- 'f'
 
HMDL <- HMDL[,ed:= edagger.x(mx,Sex[1]), by = list(Year,Sex,PopName)]

save(Decomp_time,Decomp_Sex,HMDL, file='Data/DecompResultHMD.RData')


