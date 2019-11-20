library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Sex-Differences-Lifespan/R/COD DK")

#2  Cancer, amenable to smoking
#3  Cancer, not amenable to smoking
#5  Cardiovascular & Diabetes mellitus (move to 5)
#6  Respiratory, infectious
#7  Respiratory, non-infectious
#8  External
#9  Other & Infectious, non-respiratory

#load data
HMD               <- get(load('HMD_Data.RData'))
Countries         <- c('DNK', 'SWE', 'NOR')
Data2             <- HMD
Data              <- HMD[HMD$PopName %in% Countries & HMD$Year >= 1960,]
Data$Country.name <- Data$PopName
Data              <- Data[,c(1,12,14,2,3)]

#gdata::keep(Data,Countries,sure=T)
COD             <- local(get(load('Single_COD.RData')))
range(COD$Year)
COD             <- COD[COD$Year >= 1960 & COD$Year <= 2014,]
COD$Sex         <- as.character(COD$Sex)
levels(COD$Country.name) <- Countries
COD$Country.name         <- as.character(COD$Country.name)

#get proportions by age
COD       <- COD[, Dx.p:= Dx/sum(Dx), by= list(Country, Country.name, ICD, Year, Sex,Age)]
COD[is.na(Dx.p),]$Dx.p <- 0
COD       <- COD[,c(4,5,2,6,8,9)]
COD.cast  <- dcast(COD,Year+Sex+Country.name+Age ~ Cat,value.var = 'Dx.p')

#Merge datsets
mx.COD        <- merge(COD,Data,by = c('Year','Sex','Country.name','Age'), all = T)
mx.COD$mx.COD <- mx.COD$Dx.p * mx.COD$mx 
mx.COD        <- mx.COD[order(Country.name, Year, Sex, Cat, Age),]
mx.COD.cast   <- dcast.data.table(mx.COD, Year+Sex+Country.name+Age ~ Cat, value.var = 'mx.COD')

# I have to select the years to do decomp
# first calculate variation and life expectancy 
source('Functions_1.R')

Disparity.Ind <- Data2[, list(eo = LifeExpectancy(mx = mx, sex = Sex[1]), sd = sd.frommx(mx = mx,sex = Sex[1]), ed = ed.frommx(mx = mx,sex = Sex[1])), 
               by = list(PopName,Year,Sex)]

# crossover with sd
crossover.countries.sd <- c('AUS','AUT','BEL','CAN','CHE','CZE','DEUTE','DEUTW','DNK','ESP','FIN','FRATNP','GBR_NIR',
                            'GBR_SCO','GBRTENW','IRL','ISL','ISR','ITA','JPN','NLD','NOR',"NZL_NM",'PRT','SVN','SWE','TWN','USA')

r <-ggplot(Disparity.Ind[Year >= 1960 & PopName %in% crossover.countries.sd & eo >= 70] , aes(x = eo,y = sd)) +
  ggtitle('Life expectancy vs lifespan inequality (standard deviation)') +
  geom_point(aes(group = PopName,colour=Sex), size= 1.2) +
  facet_wrap(~PopName)+
  theme_light()+
  labs(x = "Life expectancy", y = "Lifespan inequality")+
  scale_colour_manual('Sex', values = c('orange','lightblue'), labels = c('Females', 'Males')) + 
  theme(text = element_text(size=12),legend.position = c(.85, 0.1),
        strip.text.x = element_text(size = 12, colour = "black"))
r

#crossover with e-dagger
#crossover.countries.ed <- unique(Disparity.Ind$PopName)
crossover.countries.ed <- c('AUS','AUT','CAN','DEUTE','DNK','ESP','FIN','FRATNP','GBR_NIR',
                            'GBR_SCO','GBRTENW','IRL','ITA','JPN','NOR','POL','PRT','SWE','TWN','USA')

s <-ggplot(Disparity.Ind[ PopName %in% crossover.countries.ed & eo >= 50] , aes(x = eo,y = ed)) +
  ggtitle('Life expectancy vs lifespan inequality (Life years lost)') +
  geom_point(aes(group = PopName,colour=Sex), size= 1.2) +
  facet_wrap(~PopName)+
  theme_light()+
  labs(x = "Life expectancy", y = "Lifespan inequality")+
  scale_colour_manual('Sex', values = c('orange','lightblue'), labels = c('Females', 'Males')) + 
  theme(text = element_text(size=16),legend.position = c(.15, 0.952),
        strip.text.x = element_text(size = 14, colour = "black"))
s

pdf(file="Fig_ed_crossover_Ridhi.pdf",width=12,height=10,pointsize=4,useDingbats = F)
s
dev.off()


crossover.countries.ed2 <- c('AUT','CAN','DEUTE','FIN','FRATNP','GBR_NIR',
                            'GBR_SCO','GBRTENW','IRL','JPN','POL','PRT','TWN','USA')

t <-ggplot(Disparity.Ind[ PopName %in% crossover.countries.ed2 & eo >= 50] , aes(x = eo,y = ed)) +
  ggtitle('Life expectancy vs lifespan inequality (Life years lost)') +
  geom_point(aes(group = PopName,colour=Sex), size= 1.2) +
  facet_wrap(~PopName)+
  theme_light()+
  labs(x = "Life expectancy", y = "Lifespan inequality")+
  scale_colour_manual('Sex', values = c('orange','lightblue'), labels = c('Females', 'Males')) + 
  theme(text = element_text(size=16),legend.position = c(.6, .15),
        strip.text.x = element_text(size = 14, colour = "black"))
t

pdf(file="Fig_ed_crossover_Ridhi_2.pdf",width=12,height=10,pointsize=4,useDingbats = F)
t
dev.off()


### Focus on Denmark, Sweden and Norway
library(plotly)
install.packages('plotly')

u <-ggplot(Disparity.Ind[ PopName %in% c('DNK','NOR','SWE') &  Year >= 1960] , aes(x = eo,y = ed)) +
  ggtitle('Life expectancy vs lifespan inequality (Life years lost)') +
  geom_point(aes(group = PopName,colour=Sex), size= 1.2) +
  facet_wrap(~PopName)+
  theme_light()+
  labs(x = "Life expectancy", y = "Lifespan inequality")+
  scale_colour_manual('Sex', values = c('orange','lightblue'), labels = c('Females', 'Males')) + 
  theme(text = element_text(size=12),legend.position = 'right',
        strip.text.x = element_text(size = 12, colour = "black"))
u


# perform decomposition
sexes     <- unique(mx.COD.cast$Sex)
years     <- unique(mx.COD.cast$Year)
countries <- unique(mx.COD.cast$Country.name)
ages      <- sort(unique(mx.COD.cast$Age))
causes    <- colnames(mx.COD.cast)[5:11]
Empty     <- matrix(0,nrow = length(ages),ncol = length(causes),dimnames = list(ages,causes))

# get matrices in lists to do faster the calculation
Mat.list <- lapply(sexes, function(sx,LTC2,years,Empty,ages,causes){
  LTC    <- mx.COD.cast[mx.COD.cast$Sex == sx,]
Sex.List <- lapply(countries, function(st,LTC,Empty,years,ages,causes){
  Mat    <- LTC[LTC$Country == st,]
  YRlist <- lapply(years, function(yr,Mat,Empty,causes,ages){
    Mat2           <- as.matrix(Mat[Mat$Year == yr,c(causes), with = F])
    rownames(Mat2) <- ages
    Empty[rownames(Mat2),colnames(Mat2)] <- Mat2
    Empty
  }, Mat = Mat, Empty = Empty, causes = causes, ages= ages)
  names(YRlist) <- years
  YRlist
}, LTC = LTC, years=years, Empty = Empty, causes = causes, ages=ages)
names(Sex.List) <- countries
Sex.List
},LTC2 = mx.COD.cast, years=years, Empty = Empty, causes = causes, ages=ages)
names(Mat.list) <- sexes




library(parallelsugar)

Decomp.results.list   <- list()
country.list          <- list()


  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in countries){
      print(j)
      y <- x[[as.character(j)]]
      
      Decomp.list <- mclapply(years[-length(years)],function(yr,y,e0frommxc,i){
        contrib           <- Decomp(func = e0frommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,e0frommxc = e0frommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years[-length(years)]
      country.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.list[[as.character(i)]]  <- country.list
  }

save(Decomp.results.list, file =  'Data/DecompResults_ex_List.RData')
gc()
# bruger   system forløbet 
# 44.07    60.37  6009.63 




Decomp.results.cv <- list()
country.list          <- list()

  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in countries){
      print(j)
      y <- x[[as.character(j)]]
      
      Decomp.list <- mclapply(years[-length(years)],function(yr,y,cvfrommxc,i){
        
        contrib           <- Decomp(func = cvfrommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,cvfrommxc = cvfrommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years[-length(years)]
      country.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.cv[[as.character(i)]]  <- country.list
  }

save(Decomp.results.cv, file =  'Data/DecompResults_cv_List.RData')
gc()



