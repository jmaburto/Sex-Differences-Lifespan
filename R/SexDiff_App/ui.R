library(shiny)
library(data.table)
library(DT)
library(plotly)

### Countries in the HMD
  countries <- c("AUS","AUT","BEL","BGR","BLR","CAN","CHL","HRV","CHE","CZE","DEUTNP","DEUTE","DEUTW","DNK","ESP","EST","FIN","FRATNP","FRACNP","GRC",    
              "HUN","IRL","ISL","ISR","ITA","JPN","KOR","LTU","LUX","LVA","NLD","NOR","NZL_NP", "NZL_MA","NZL_NM","POL","PRT","RUS","SVK","SVN",
              "SWE","TWN","UKR","GBR_NP","GBRTENW","GBRCENW","GBR_SCO", "GBR_NIR", "USA") 
  
  
  Initial.year <- 1995:2014
  
  shinyUI(
    fluidPage(
      titlePanel('Sex differences in lifespan variation and life expectancy'),
      #navbarPage(
       # 'App by Aburto JM, Kashyap R, van Raalte A & Zarulli V (alphabetically), Sex differences in lifespan variation.',
      #  position = c("fixed-bottom")
        #),
      
      tags$style(type = 'text/css', '.navbar { background-color: white}',
                 '.navbar-default .navbar-brand {
                 color: grey;
                 font-size: 13px}'),
      
      tabsetPanel(
        
        tabPanel("Relationship between life expectancy and lifespan variation",
                 sidebarPanel(
                   selectInput( 'country.ind','Country',countries, selected = 'AUS'),
                   br(),
                   p('Select a country. This country is the subject of analysis in the subsequent panels.'),
                   br(),
                   p('App by Aburto JM, Kashyap R, van Raalte A & Zarulli V (alphabetically), Sex differences in lifespan variation.'),
                   width = 2
                 ),
                 mainPanel(
                   tabPanel("Relationship between life expectancy and lifespan variation",
                                        fluidRow(plotlyOutput('e0.ed.plot',width = '100%'))
                   )
                 )
        )
        ,
        tabPanel("Traditional analysis over time and sex",
                   sidebarPanel(
                     uiOutput("vx1"),
                     br(),
                     uiOutput("vx2"),
                     br(),
                     uiOutput("vx3"),
                     width = 2
                   ),
                   mainPanel(
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"),
                     tabsetPanel(tabPanel("Age-at-death distribution by sex",
                                          plotlyOutput("dx.plot"),
                                          dataTableOutput('Summary.table.1')),
                                 tabPanel("Time decomposition",
                                          plotlyOutput("e0.decomp.time"),
                                          plotlyOutput("ed.decomp.time")),
                                 tabPanel("Sex decomposition",
                                          plotlyOutput("e0.decomp.sex"),
                                          plotlyOutput("ed.decomp.sex"))
                     )
                   )
                 ),
        
        tabPanel("Flexible sex decomposition in different periods ",
                 sidebarPanel(
                   uiOutput("vx4"),
                   br(),
                   uiOutput("vx5"),
                   textOutput("Refresh"),
                   width = 2
                 ),
                 mainPanel(
                   tabPanel("",
                            fluidPage(
                              fluidRow(
                                column(6,
                                       plotlyOutput("mx.plot2")
                                ),
                                column(6,
                                       plotlyOutput("dx.plot2")
                                )
                              ),
                              
                              fluidRow(
                                column(6,div(style = "height:75px;background-color: transparent;"),
                                       plotlyOutput("eo.decomp.sex2")
                                ),
                                column(6,div(style = "height:75px;background-color: transparent;"),
                                       plotlyOutput("ed.decomp.sex2")
                                )
                              ),
                              fluidRow(
                                column(6,div(style = "height:50px;background-color: transparent;")
                                ),
                                column(6,div(style = "height:50px;background-color: transparent;")
                                )
                              )
                              )
                            )
                   
                 )
        )
      )
      
      
      
      )
    )
  
  
#  devtools::install_github('hadley/ggplot2')
  