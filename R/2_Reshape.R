############# Written by JMA
############# Project with Virginia Zarulli, Ridhi Kashyap and Alyson van Raalte
rm(list=ls(all=TRUE))
library(reshape2)
library(data.table)

setwd(  "C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan-Equality/Big picture project/")
#source("R/0_Get_HMD.R") # Just in case you want updated HMD Data

#load data
load('Data/DecompResultHMD.RData')

#Gonna be easir to transfomt to data.table all the results

DT.Decomp.time <-do.call(rbind, lapply(names(Decomp_time),function(country,y){
  x <- y[[country]]
  
  kk <- lapply(names(x),function(sex,x,country){
    z     <- x[[sex]]
    years <- names(z)
    
    k <- lapply(years,function(year,sex,country,z){
      zz <- z[[year]]
      zz <- cbind(zz,sex)
      zz <- cbind(zz,country)
      zz <- cbind(zz,0:110)
      zz
      
      },sex=sex,country=country,z=z)
    
    k <- do.call(rbind,k)
    
    k
    
  },x=x, country = country)
  
  kk <- do.call(rbind,kk)
  
  kk
  
},y=Decomp_time))

DT.Decomp.time <- DT.Decomp.time[,c('country','sex','yr','V2' ,'e0.decomp','ed.decomp')]
names(DT.Decomp.time) <- c('country','sex','year','age','eo.decomp','ed.decomp')


DT.Decomp.sex <-do.call(rbind, lapply(names(Decomp_Sex),function(country,y){
  z <- y[[country]]
    
    k <- lapply(names(z),function(year,country,z){
      zz <- z[[year]]
      zz <- cbind(zz,country)
      zz <- cbind(zz,0:110)
      zz
      
    },country=country,z=z)
    
    k <- do.call(rbind,k)
    
    k
    
},y=Decomp_Sex))

DT.Decomp.sex$e0.decomp <-  -DT.Decomp.sex$e0.decomp
DT.Decomp.sex$ed.decomp <-  -DT.Decomp.sex$ed.decomp

names(DT.Decomp.sex) <- c('e0.decomp','year','ed.decomp','country','age')
DT.Decomp.sex <-DT.Decomp.sex[,c('country','year','age','e0.decomp','ed.decomp')]

save(DT.Decomp.sex,DT.Decomp.time,HMDL, file= 'Data/ResultsSexDiff.RData')

save(DT.Decomp.sex,DT.Decomp.time,HMDL, file= 'R/SexDiff_App//ResultsSexDiff.RData')

gdata:: keep(DT.Decomp.sex,DT.Decomp.time,HMDL, sure=T)

