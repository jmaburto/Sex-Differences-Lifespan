
#setwd("C:/Users/jmaburto/Documents/GitHub/Sex-Differences-Lifespan-Equality/Big picture project/R/SexDiff_App/")
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(data.table)
library(DemoDecomp)

load('ResultsSexDiff.RData')



shinyServer(
  
  function(input, output) {
    #options(shiny.sanitize.errors = T)
    
    var1 <- reactive(seq(as.integer(input$initial.ind)+1,2015,1))

    output$vx1 <- renderUI({
      years <- unique(HMDL[HMDL$PopName == input$country.ind]$Year)
      selectInput('year1.ind','Initial year for decomposition over time',years,selected = min(years))
    })
    
    output$vx2 <- renderUI({
      year.max <- max(unique(HMDL[HMDL$PopName == input$country.ind]$Year))
      selectInput('year2.ind','Final year for decomposition over time',seq(as.numeric(input$year1.ind)+1,year.max,1))
    })
    
    output$vx3 <- renderUI({
      selectInput( 'yearsex.ind','Year for female-male decomposition (between intital and final year)',seq(as.numeric(input$year1.ind)+1,as.numeric(input$year2.ind)-1,1))
    })
    
    output$vx4 <- renderUI({
      years <- unique(HMDL[HMDL$PopName == input$country.ind]$Year)
      selectInput('year.males','Year for males',years,selected = min(years))
    })
    
    output$vx5 <- renderUI({
      years <- unique(HMDL[HMDL$PopName == input$country.ind]$Year)
      selectInput('year.females','Year for females',years,selected = min(years))
    })
    
    AKm02a0        <- function(m0, sex = "m"){
      sex <- rep(sex, length(m0))
      ifelse(sex == "m", 
             ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                    ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
             # f
             ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                    ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
      )
    }
    
    LifeExpectancy <- compiler::cmpfun(function(mx,sex = "f"){
      i.openage <- length(mx)
      OPENAGE   <- i.openage - 1
      RADIX     <- 1
      ax        <- mx * 0 + .5
      ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
      qx        <- mx / (1 + (1 - ax) * mx)
      qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
      ax[i.openage]       <- 1 / mx[i.openage]                   
      px 				    <- 1 - qx
      px[is.nan(px)]      <- 0
      lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
      dx 				    <- lx * qx
      Lx 				    <- lx - (1 - ax) * dx
      Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
      Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
      ex 				    <- Tx / lx
      ex[1]
    })
    
    
    edagger.frommx <- compiler::cmpfun(function(mx,sex){
      i.openage <- length(mx)
      OPENAGE   <- i.openage - 1
      RADIX     <- 1
      ax        <- mx * 0 + .5
      ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
      qx        <- mx / (1 + (1 - ax) * mx)
      qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
      ax[i.openage]       <- 1 / mx[i.openage]                   
      px 				    <- 1 - qx
      px[is.nan(px)]      <- 0
      lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
      dx 				    <- lx * qx
      Lx 				    <- lx - (1 - ax) * dx
      Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
      Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
      ex 				    <- Tx / lx
      l <- length(ex)
      v <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
      return(v)
    })
    
    output$dx.plot <- renderPlotly({
      
      country   <- input$country.ind
      year1     <- input$year1.ind
      year2     <- input$year2.ind
      year.sex  <- input$yearsex.ind
      
      Data      <- HMDL[HMDL$PopName == country & HMDL$Year%in% c(year1,year2,year.sex),]
      
      p <- ggplot(data = Data, aes(x = Age,y = dx,colour = Sex)) +
        ggtitle(paste0('Age at death distribution (100,000 population), ', country)) +
        geom_line(aes(group = Sex), size= 1) +
        facet_wrap(~Year)+
        scale_colour_manual('Sex', values = c('orange', 'lightblue'),labels = c('Females','Males')) + 
        theme_light()+
        theme(text = element_text(size=14),
             strip.text.x = element_text(size = 14, colour = "black"))
      
      fig <- ggplotly(p,width = 1500, height = 400, tooltip= c('Age','dx'))
      fig <- fig %>% layout(margin = list(l = 120, b=90))
      fig[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
      #fig$x$layout$xaxis$title = "Age"
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    output$Summary.table.1 = renderDataTable({
      #y1 <- '2000-2005' 
      country   <- input$country.ind
      year1     <- input$year1.ind
      year2     <- input$year2.ind
      year.sex  <- input$yearsex.ind
      
      Data      <- HMDL[HMDL$Age == 0 & HMDL$PopName == country & HMDL$Year%in% c(year1,year2,year.sex),]
      Data      <- Data[,c('Year','Sex','ex','ed')]
      Data$ex   <- round(Data$ex,2)
      Data$ed   <- round(Data$ed,2)
      
      
      datatable(Data, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = 'Life expectancy and lifespan variation at birth')
    })
    
    output$e0.ed.plot <- renderPlotly({
      
      country   <- input$country.ind
      
      Data      <- HMDL[HMDL$PopName == country & HMDL$Age == 0,]
      
      
      p <- ggplot(data = Data, aes(x = ex,y = ed,colour = Sex, label= Year)) +
        ggtitle(paste0('Life expectancy by lifespan variation, ', country)) +
        geom_point(aes(group = Sex), size= 1) +
        scale_colour_manual('Sex', values = c('orange', 'lightblue'),labels = c('Females','Males')) + 
        theme_light()+
        xlab('Life expectancy')+
        ylab('Lifespan variation')+
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      
      
      fig <- ggplotly(p,width = 700, height = 700,tooltip = c('ex','ed','Year'))
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('pan2d', 'toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    output$e0.decomp.time <- renderPlotly({
      
      year1     <- input$year1.ind
      year2     <- input$year2.ind
      
      Data    <- DT.Decomp.time[DT.Decomp.time$country %in% input$country.ind & (DT.Decomp.time$year >= year1 &  DT.Decomp.time$year < year2),]
      
 
       Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                                  '40-44','45-49','50-54','55-59','60-64','65-69',
                                  "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
      
       Data$Age       <- cut(Data$age+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age)
      
       Data           <- Data[,list(eo = round(sum(eo.decomp),2), ed = round(sum(ed.decomp),2)), by = list(sex,Age)]
      
        p <- ggplot(data = Data, aes(x = Age, y = eo,  width=.7)) +
         ggtitle(
           paste0('Age-contribution to changes in life expectancy by period ', year1,'-',year2,'. ',
                  round(sum(Data[Data$sex == 'f',]$eo),2),' and ' ,round(sum(Data[Data$sex == 'm',]$eo),2), 
                  ' years for f and m, respectively'))+
         geom_bar(stat = "identity",position = "stack", fill= 'red')+
          #annotate("text", label = paste0('Change = ',round(sum(Data[Data$sex == 'f',]$eo))), size = 4, x = '85-89', 
          #         y = .1)+
          facet_wrap(~sex)+
           theme_light()+
          theme(text = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1))+
          labs(x = " ", y = "Years ",size=10)+
          theme(text = element_text(size=10),
                strip.text.x = element_text(size = 12, colour = "black"))+
          geom_hline(yintercept = 0)
         
      
       fig <- ggplotly(p,width = 1350, height = 400)
       fig <- fig %>% layout(margin = list(l = 120, b=90))
       ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
       
      
    })
    
    output$ed.decomp.time <- renderPlotly({
      
      year1     <- input$year1.ind
      year2     <- input$year2.ind
      
      Data    <- DT.Decomp.time[DT.Decomp.time$country %in% input$country.ind & (DT.Decomp.time$year >= year1 &  DT.Decomp.time$year < year2),]
      
      
      Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                                 '40-44','45-49','50-54','55-59','60-64','65-69',
                                 "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
      
      Data$Age       <- cut(Data$age+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age)
      
      Data           <- Data[,list(eo = round(sum(eo.decomp),2), ed = round(sum(ed.decomp),2)), by = list(sex,Age)]
      
      p <- ggplot(data = Data, aes(x = Age, y = ed,  width=.7)) +
        ggtitle(
          paste0('Age-contribution to changes in lifespan variation by period ', year1,'-',year2,'. ',
                 round(sum(Data[Data$sex == 'f',]$ed),2),' and ' ,round(sum(Data[Data$sex == 'm',]$ed),2), 
                 ' years for f and m, respectively'))+
        geom_bar(stat = "identity",position = "stack", fill= 'blue')+
        facet_wrap(~sex)+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "Age", y = "Years ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      
      
      fig <- ggplotly(p,width = 1350, height = 400)
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    output$e0.decomp.sex <- renderPlotly({
      
      year1     <- input$year1.ind
      year2     <- input$year2.ind
      year.sex  <- input$yearsex.ind
      
      Data    <- DT.Decomp.sex[DT.Decomp.sex$country %in% input$country.ind & DT.Decomp.sex$year %in% c(year1,year2,year.sex),]
      
      #Data    <- DT.Decomp.sex[DT.Decomp.sex$country %in% 'AUS' & DT.Decomp.sex$year %in% c(year1,year2,year.sex),]
      
      
      Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                                 '40-44','45-49','50-54','55-59','60-64','65-69',
                                 "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
      
      Data$Age       <- cut(Data$age+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age)
      
      Data           <- Data[,list(eo = round(sum(e0.decomp),2)), by = list(year,Age)]
    
      Data$year <- as.factor(Data$year)
      
      Sx.T <- c(Data[,sum(eo), by = list(year)]$V1)
      
      p <- ggplot(data = Data, aes(x = Age, y = eo,  width=.7)) +
        ggtitle(paste0('Age-contribution to female-male difference in life expectancty. Difference: ',
                       Sx.T[1],', ', Sx.T[2],', ', Sx.T[3], '.'))+
        geom_bar(stat = "identity",position = "stack",fill = 'red')+
        facet_wrap(~year)+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "", y = "Years ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      
      
      fig <- ggplotly(p,width = 1350, height = 400)
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    
    output$ed.decomp.sex <- renderPlotly({
      
      year1     <- input$year1.ind
      year2     <- input$year2.ind
      year.sex  <- input$yearsex.ind
      
      Data    <- DT.Decomp.sex[DT.Decomp.sex$country %in% input$country.ind & DT.Decomp.sex$year %in% c(year1,year2,year.sex),]
      
      #Data    <- DT.Decomp.sex[DT.Decomp.sex$country %in% 'AUS' & DT.Decomp.sex$year %in% c(year1,year2,year.sex),]
      
      
      Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                                 '40-44','45-49','50-54','55-59','60-64','65-69',
                                 "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
      
      Data$Age       <- cut(Data$age+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age)
      
      Data           <- Data[,list(ed = round(sum(ed.decomp),2)), by = list(year,Age)]
      
      
      Data$year <- as.factor(Data$year)
      
      Sx.T <- c(Data[,sum(eo), by = list(year)]$V1)
      
      
      Data$year <- as.factor(Data$year)
      
      p <- ggplot(data = Data, aes(x = Age, y = ed,  width=.7)) +
        ggtitle(paste0('Age-contribution to female-male difference in lifes variation. Difference: ',
                       Sx.T[1],', ', Sx.T[2],', ', Sx.T[3], '.'))+
        geom_bar(stat = "identity",position = "stack",fill = 'blue')+
        facet_wrap(~year)+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "", y = "Years ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      
      
      fig <- ggplotly(p,width = 1350, height = 400)
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    output$dx.plot2 <- renderPlotly({
      
      country   <- input$country.ind
      year1     <- input$year.males
      year2     <- input$year.females
      
      Data      <- HMDL[HMDL$PopName == country & ((HMDL$Year == year1 & HMDL$Sex == 'm' )
                        | (HMDL$Year == year2 & HMDL$Sex == 'f' )),]
      
      p <- ggplot(data = Data, aes(x = Age,y = dx,colour = Sex, label= Year)) +
        ggtitle(paste0('Age at death distribution ', country)) +
        geom_line(aes(group = Sex), size= 1) +
        scale_colour_manual('Sex', values = c('orange', 'lightblue'),labels = c('Females','Males')) + 
        theme_light()+
        theme(text = element_text(size=12),
              strip.text.x = element_text(size = 12, colour = "black"))
      
      fig <- ggplotly(p,width = 500, height = 500,tooltip=c('Year','Age','dx'))
            fig <- fig %>% layout(margin = list(l = 120, b=70))
            ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
            
      
    })
    
    output$mx.plot2 <- renderPlotly({
      
      country   <- input$country.ind
      year1     <- input$year.males
      year2     <- input$year.females
      
      Data      <- HMDL[HMDL$PopName == country & ((HMDL$Year == year1 & HMDL$Sex == 'm' )
                                                   | (HMDL$Year == year2 & HMDL$Sex == 'f' )),]
      
      p <- ggplot(data = Data, aes(x = Age,y = log(mx),colour = Sex, label= Year)) +
        ggtitle(paste0('Log mortality), ', country)) +
        geom_line(aes(group = Sex), size= 1) +
        scale_colour_manual('Sex', values = c('orange', 'lightblue'),labels = c('Females','Males')) + 
        theme_light()+
        theme(text = element_text(size=12),
              strip.text.x = element_text(size = 12, colour = "black"))
      
      fig <- ggplotly(p,width = 500, height = 500, tooltip  =c('Year','Age','log(mx)'))
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    output$eo.decomp.sex2 <- renderPlotly({
      
      country   <- input$country.ind
      year1     <- input$year.males
      year2     <- input$year.females
      
      Data      <- HMDL[HMDL$PopName == country & ((HMDL$Year == year1 & HMDL$Sex == 'm' )
                                                   | (HMDL$Year == year2 & HMDL$Sex == 'f' )),]
      
      mx2 <- Data[Data$Sex == 'f',]$mx
      mx1 <- Data[Data$Sex == 'm',]$mx
      
      Decomp  <- horiuchi(LifeExpectancy,pars1 = mx1,pars2 = mx2,N = 25)
      
      Data <- Data[1:111,]
      Data <- cbind(Data,Decomp)
      
      Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                                 '40-44','45-49','50-54','55-59','60-64','65-69',
                                 "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
      
      Data$Age       <- cut(Data$Age+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age)
      
      Data           <- Data[,list(eo = round(sum(Decomp),2)), by = list(Age)]


      p <- ggplot(data = Data, aes(x = Age, y = eo,  width=.7)) +
        ggtitle(paste0('Female (',year2,') - male (',year1,') (life expectancy)'))+
        geom_bar(stat = "identity",position = "stack",fill = 'blue')+
        theme_light()+
        theme(text = element_text(size=12),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "Age group", y = "Years ",size=12)+
        theme(text = element_text(size=12),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)


      fig <- ggplotly(p,width = 500, height = 500)
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })

    output$ed.decomp.sex2 <- renderPlotly({

      country   <- input$country.ind
      year1     <- input$year.males
      year2     <- input$year.females

      Data      <- HMDL[HMDL$PopName == country & ((HMDL$Year == year1 & HMDL$Sex == 'm' )
                                                   | (HMDL$Year == year2 & HMDL$Sex == 'f' )),]

      mx2 <- Data[Data$Sex == 'f',]$mx
      mx1 <- Data[Data$Sex == 'm',]$mx

      Decomp  <- horiuchi(edagger.frommx,pars1 = mx1,pars2 = mx2,N = 25,sex = 'f')

      Data <- Data[1:111,]
      Data <- cbind(Data,Decomp)

      Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                                 '40-44','45-49','50-54','55-59','60-64','65-69',
                                 "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")

      Data$Age       <- cut(Data$Age+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age)

      Data           <- Data[,list(ed = round(sum(Decomp),2)), by = list(Age)]


      p <- ggplot(data = Data, aes(x = Age, y = ed,  width=.7)) +
        ggtitle(paste0('Female (',year2,') - male (',year1,') (lifespan variation)'))+
        geom_bar(stat = "identity",position = "stack",fill = 'blue')+
        theme_light()+
        theme(text = element_text(size=12),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "Age group", y = "Years ",size=12)+
        theme(text = element_text(size=12),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)


      fig <- ggplotly(p,width = 500, height = 500)
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      

    })
    
    output$Refresh <- renderText({
      country   <- input$country.ind
      year1     <- input$year.males
      year2     <- input$year.females
      Data      <- HMDL[HMDL$PopName == country & ((HMDL$Year == year1 & HMDL$Sex == 'm' )
                                                   | (HMDL$Year == year2 & HMDL$Sex == 'f' )),]
      
      ex.males <- round(Data[Data$Sex == 'm' & Data$Age == 0,]$ex,2)
      ex.females <- round(Data[Data$Sex == 'f' & Data$Age == 0,]$ex,2)
      
      ed.males <- round(Data[Data$Sex == 'm' & Data$Age == 0,]$ed,2)
      ed.females <- round(Data[Data$Sex == 'f' & Data$Age == 0,]$ed,2)
      
      toString(paste0('Life expectancy males = ',ex.males, ' for females = ',ex.females,'; difference = ',
                     round(ex.females - ex.males,2), '.', 'Life disparity males = ',ed.males, ' for females = ',ed.females,'; difference = ',
               round(ed.females - ed.males,0), '.'))
      
      
    })
    
})
