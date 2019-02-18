
setwd("C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan/R/SexDiff_App/")
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(data.table)
library(DemoDecomp)
library(gridExtra)

load('ResultsSexDiff.RData')
country <- 'SWE'
year1   <-  1850
year2   <-  2016

      
      Data      <- HMDL[HMDL$PopName == country & HMDL$Year%in% c(year1,year2),]
      
       p1 <- ggplot(data = Data, aes(x = Age,y = dx,colour = Sex, lty = factor(Year))) +
        ggtitle(paste0('Age at death distribution (100,000 population), ', country)) +
        geom_line(aes(group = interaction(Sex,Year)), size = 1.5,show.legend = F) +
        scale_colour_manual('Sex', values = c('orange', 'lightblue'),labels = c('Females','Males')) + 
        theme_light()+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))
       
       p1
       
       p2 <- ggplot(data = Data, aes(x = Age,y = mx,colour = Sex, lty = factor(Year))) +
         ggtitle(paste0('Age-specific death rates, ', country)) +
         geom_line(aes(group = interaction(Sex,Year)), size = 1.5) +
         scale_y_log10('Log mx, base 10')+
         scale_colour_manual('Sex', values = c('orange', 'lightblue'),labels = c('Females','Males')) + 
         theme_light()+
         theme(text = element_text(size=10),
               strip.text.x = element_text(size = 12, colour = "black"))+
         theme(legend.position = c(.85,.3))
       p2
      
       
       pdf(file = 'Sex_Plot1.pdf',width = 8,height = 4,useDingbats = T)
       grid.arrange(p1,p2,nrow =1)
       dev.off()
      
      Data    <- DT.Decomp.sex[DT.Decomp.sex$country == 'SWE' & DT.Decomp.sex$year %in% c(year1,year2),]
      Data    <- Data[, cumx := cumsum(e0.decomp)/sum(e0.decomp), by = list(country,year)]
      
      Sx.T <- round(c(Data[,sum(e0.decomp), by = list(year)]$V1),2)
      
        p3 <- ggplot(data = Data, aes(x = age, y = cumx,  width=.7, colour = factor(year))) +
        ggtitle('Age-contribution to female-male difference in life expectancty.',
                subtitle = paste0('Difference: ',Sx.T[1],' in 1900, ', Sx.T[2], ' in 2016.'))+
        geom_step(aes(group = year), size = 1.5) +
          scale_colour_manual('Year', values = c('darkblue', 'lightblue'),labels = c('1900','2016')) + 
        theme_light()+
          geom_hline(yintercept = .5, size = 1, lty = 2)+
        theme(text = element_text(size=10))+
        labs(x = " Age", y = "Cumulative contribution",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      p3
      
      
      pdf(file = 'Sex_Plot.pdf',width = 6,height = 4,useDingbats = T)
      grid.arrange(p3,nrow =1)
      dev.off()