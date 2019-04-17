setwd("C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan/R/SexDiff_App/")

library(ggplot2)
library(data.table)
library(DemoDecomp)

load('ResultsSexDiff.RData')

#get the coefficient of variation
Data <- data.table(HMDL)
Data$Sex2 <- Data$Sex
head(Data)

Data[Data$Sex2 == 'f',]$Sex2 <- 'Females'
Data[Data$Sex2 == 'm',]$Sex2 <- 'Males'
unique(Data$PopName)
#Data <- Data[Data$PopName %in% c("GBRCENW","GBR_SCO") & Data$Age == 0,]
Data <- Data[Data$PopName %in% c("DNK") & Data$Age == 0,]



r <-ggplot(Data, aes(x = ex,y = ed)) +
  ggtitle('C Life expectancy at birth vs lifespan inequality') +
  geom_point(aes(group = Sex2,colour=Sex2), size= 1.2) +
  theme_light()+
  labs(x = "Life expectancy", y = "Lifespan inequality")+
  scale_colour_manual('Sex', values = c('orange','lightblue'), labels = c('Females', 'Males')) + 
  theme(text = element_text(size=12),legend.position = c(.8, 0.85),
        strip.text.x = element_text(size = 12, colour = "black"))
r


p <-ggplot(Data, aes(x = Year,y = ex)) +
  ggtitle('A Life expectancy at birth') +
  geom_line(aes(group = Sex2,colour=Sex2), size= 1.2,show.legend = F) +
  #facet_wrap(~Sex2)+
  scale_x_continuous(breaks = c(1850,1900,1950,2016))+
  theme_light()+
  labs(y = "Years")+
  scale_colour_manual('Country', values = c('orange','lightblue'), labels = c('Scotland', 'England & Wales')) + 
  theme(text = element_text(size=12),legend.position = c(0.57, 0.8),
        strip.text.x = element_text(size = 12, colour = "black"))+
  theme(legend.position = c(.15,.9))
p

q <-ggplot(Data, aes(x = Year,y = ed)) +
  ggtitle('B Lifespan inequality') +
  geom_line(aes(group = Sex2,colour=Sex2), size= 1.2,show.legend = F) +
  #facet_wrap(~Sex2)+
  scale_x_continuous(breaks = c(1850,1900,1950,2016))+
  theme_light()+
  labs(y = "Years")+
  scale_colour_manual('Country', values = c('orange','lightblue'), labels = c('Scotland', 'England & Wales')) + 
  theme(text = element_text(size=12),legend.position = c(0.57, 0.8),
        strip.text.x = element_text(size = 12, colour = "black"))+
  theme(legend.position = c(.15,.9))
q

require(gridExtra)
pdf(file="Fig_DK.pdf",width=10,height=5,pointsize=4,useDingbats = F)
grid.arrange(p,q,r,layout_matrix = rbind(c(1,3),c(2,3)))
dev.off()

