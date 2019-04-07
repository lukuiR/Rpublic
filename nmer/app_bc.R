#dt ext https://rstudio.github.io/DT/extensions.html

#DT ino: https://yihui.shinyapps.io/DT-info/

###

###, graph with differenssec, add dots, label 45, not map for, retirement, add tab, 

# add loading

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(readxl)
library(data.table)
library(DT)
library(tidyverse)
library(shinyBS)

source('prep_dat.R')

title <- tags$a(href='https://www.sabic.com/en',
                tags$img(src="sabic.png", height = '55'), target="_blank")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = title ,titleWidth = 200
                  ,tags$li(class= 'dropdown',
                           tags$style(".main-header {max-height: 55px;}"),
                           tags$style(".main-header .logo {height: 55px;}"),
                           tags$style(".sidebar-toggle {height: 55px; padding-top: 18px !important;}"),
                           tags$style(".navbar {min-height:55px !important}"),
                           tags$style(".navbar-custom-menu, .main-header .navbar-right {float: left !important;}"),HTML( '<b><font color=#ffffff> <font size="6"> Strategic Workforce Planning Platform</font></font></b>'))
              
  ),
  dashboardSidebar(
    width = 200,
    #####
    
    tags$style(".left-side, .main-sidebar {padding-top: 55px}"),
    
    #tags$style(".left-side, .main-sidebar {width: 200px}"),
    
    sidebarMenu(
      
      id = "tabs",
      
      menuItem("Instruction", icon = icon("info"), tabName = "tab1"),
      
      menuItem("Model Setup", icon = icon("glyphicon glyphicon-cog", lib ='glyphicon'), tabName = "tab2"),
      
      menuItem("Executive View", icon = icon("line-chart"), tabName = "tab3"),
      
      menuItem("Deep Dive View", icon = icon("glyphicon glyphicon-stats", lib ='glyphicon'), tabName = "tab4"),
      
      menuItem("Data View", icon = icon("glyphicon glyphicon-folder-open", lib ='glyphicon'), tabName = "tab5")
      
    )
    
    #####
  ),
  # end
  #dashbody  
  dashboardBody(
    tabItems(
      #tab1
      #####
      
      tabItem(tabName = "tab1",includeCSS("styles.css"),
              
              img(src="sabic.png", width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),
              
              h2("Strategic Workforce Interactive Scenario Scoper",align="center",style="color:dark blue"),
              
              fluidRow(
                
                box(p("This tool is designed to enable detailed forecasting of workforce requirements throughout Sabic's
                      
                      workforce over a three year time period.",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    h2("Instructions",style = "font-family: 'Source Sans Pro';"),
                    
                    p("First, go to the Model Setup tab. Here you must generate data with the button at the top, before you
                      
                      are able to access the other tabs. Once data is generated, the executive view and deep dive view show both the
                      
                      resulting workforce forecasts, and the details behind them.",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    p("The tool is divided into three sections:",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    h4("1. Model Setup",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Model Setup must be visited first to generate the data, and adjust underlying assumptions if desired.
                      
                      Driving the tool is data on both the current workforce state and the key levers that influence future demand: driver changes, automation, training and outsourcing,
                      
                      produced from both Sabic's data and market research. These values are preloaded for every individual section based on
                      
                      a combination of research and Sabic's own data, however should you wish to adjust these underlying assumptions in bulk you may do so in this tab.",style = "font-family: 'Source Sans Pro';"),
                    
                    h4("2. Executive View",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Executive View provides a high level overview of Sabic and its largest functions.
                      
                      It displays the workforce forecasts, details the impact of the different workforce levers,
                      
                      and highlights the supply/demand gap.",style = "font-family: 'Source Sans Pro';"),
                    
                    h4("3. Deep Dive",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Deep Dive provides key demographic information at every level of the organisation, and
                      
                      allows for detailed section level planning.",style = "font-family: 'Source Sans Pro';")
                    
                    
                    
                    ,width=12)
                
                    ),
              
              img(src="mercer-logo.png", width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),
              
              p("Tool code and materials are copyrighted works owned exclusively by Mercer and may not be copied,
                
                modified, sold, transformed into any other media, or otherwise transferred in whole or in part
                
                to any party other than Sabic, without prior written consent from Mercer. All rights reserved.",
                
                style = "font-family: 'Source Sans Pro';")
              
                    ),
      
      #####
      #end tab1
      #tab2
      ######
      tabItem(tabName = "tab2",
              h2("Model Initialisation"),
              #introduction
              fluidRow(
                {
                box(
                  
                  title =HTML( '<font color=#ffffff> <font size="5">Introduction</font></font>'),
                  
                  status="primary",solidHeader = TRUE, colour="light blue",collapsible = TRUE,width = '100%',
                  
                  p("This is the Model Initialisation page. Here you are able to adjust the underlying model parameters
                    
                    for the three different scenarios. When you are finished, even if you did not make any
                    
                    changes, click the 'Generate' button just below this box to calculate forecast headcounts and enable the other tabs.",
                    
                    style = "font-family: 'Source Sans Pro';"),
                  
                  p("All values are preloaded with the results of research and Sabic data. An average of these, weighted on
                    
                    baseline headcount, is used to preload the sliders below.",
                    
                    style = "font-family: 'Source Sans Pro';"),
                  
                  p("There are two types of parameter that can be adjusted: Global Parameters, which apply to the whole organisation, and
                    
                    Specific Parameters, which apply only to the selected area of the organisation.",
                    
                    style = "font-family: 'Source Sans Pro';")
                  
                )
                }
              ),
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Right-sizing Scenario Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                  tags$div(title="As-Is case: When no rightsizing is applied to the current headcount.
                           \n \n 
                           Optimistic Case: Maximum Optimization possible in the current Headcount
                           \n \n 
                           Conservative Case: Current headcount is rightsized only to of 50% of the value of the Optimistic case",
                  radioButtons("radio_t2rsss", label = NULL,
                               choiceNames = list("As-is", "Optimistic","Conservative"
                               ),
                               choiceValues = list(
                                 "rs", "r_opt_s","r_crvt_s"
                               ),selected = "rs", inline = TRUE)
                  )
                  ,width = '60%'
                )
              ),
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$div(title="Conservative Case: When Demand is calculated without taking into account Automation and Capability enhancement
                           \n \n 
                           Base Case: When Demand is calculated taking into account Automation and Capability enhancement
                           \n \n 
                           Optimistic Case: When Demand is calculated taking into account
                           maximum Automation potential and Capability enhancement",
                  radioButtons("radio_t2ss", label = NULL,
                               choiceNames = list("Conservative", "Base Case","Optimistic"
                               ),
                               choiceValues = list(
                                 "con", "bc","opt"
                               ),selected = "con", inline = TRUE)
                ),width = '100%')
              ),
              fluidRow(
                tags$style(".nav-tabs {
                           background-color: transparent;
                           }
                           .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                           background-color: #00A8C8  ;
                           border-color: #FFFFFF ;
                           color: #FFFFFF;
                           }
                           .nav-tabs-custom .nav-tabs li.active {
                           border-top-color: transparent ;
                           }"),
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Global Parameters</font></font>')
                  , status = "primary", solidHeader = TRUE,width = '100%',collapsible = TRUE,collapsed=TRUE,
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", width = '100%',# height = "250px",
                    tabPanel("Training",
                             {
                               box(status = "primary", width = '100%',
                                   actionButton("reset_ld","Reset L&D", 
                                                style="color: #fff; background-color: #ED2C67; border-color: #ED2C67; font-size:100%")
                               ,actionButton("save_t2ld","Save L&D"),
                                   # fluidRow(
                                   #   dataTableOutput(outputId = "tld")
                                   # ),
                                   fluidRow(
                                     column(2,br(),br(),h5('Total L&D Spend / FTE')),
                                     column(2,
                                            textInput("t2ld1", h5("Year 1 Value"),
                                                      value = ld$`T=1` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2ld2", h5("Year 2 Value"),
                                                      value = ld$`T=2` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2ld3", h5("Year 3 Value"),
                                                      value = ld$`T=3` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2ld4", h5("Year 4 Value"),
                                                      value = ld$`T=4` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2ld5", h5("Year 5 Value"),
                                                      value = ld$`T=5` #"1250"
                                            ))
                                   )
                                   
                               )
                               
                             }
                    ),
                    tabPanel("Supply",
                             {
                               
                               box(status = "primary",width = 12,
                                   
                                   {fluidRow(
                                     column(1,br(),br(),h5(2018)),
                                     column(2,
                                            sliderInput("MEA2", "MEA:",
                                                        min = -100, max = 100,
                                                        value = nhr$MEA[1]*100,pre = "%", step = 0.01, ticks = FALSE)) ,
                                     column(2,
                                            sliderInput("APAC2", "APAC:",
                                                        min = -100, max = 100,
                                                        value = nhr$APAC[1]*100,pre = "%", step = 0.01, ticks = FALSE)) ,
                                     column(2,
                                            sliderInput("AMR2", "AMR:",
                                                        min = -100, max = 100,
                                                        value = nhr$AMR[1]*100,pre = "%", step = 0.01, ticks = FALSE)) ,
                                     column(2,
                                            sliderInput("EUR2", "EUR:",
                                                        min = -100, max = 100,
                                                        value = nhr$EUR[1]*100,pre = "%", step = 0.01, ticks = FALSE))  
                                     
                                   )},
                                   {fluidRow(
                                     
                                     column(1,br(),br(),h5(2018)),
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("MEA", h5("MEA"),
                                                      
                                                      value = nhr$MEA[1] #"1.1"
                                                      
                                            )) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("APAC", h5("APAC"),
                                                      
                                                      value = nhr$APAC[1] #"1.2"
                                                      
                                            )) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("AMR", h5("AMR"),
                                                      
                                                      value = nhr$AMR[1] #"20"
                                                      
                                            )) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("EUR", h5("EUR"),
                                                      
                                                      value = nhr$EUR[1] #"4"
                                                      
                                            ))  
                                     
                                   )},
                                   {fluidRow(
                                     
                                     column(1,h5(2019)),
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("MEA",NULL,# h5("MEA"),
                                                      
                                                      value = "1.1")) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("APAC",NULL,# h5("APAC"),
                                                      
                                                      value = "1.2")) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("AMR",NULL,# h5("AMR"),
                                                      
                                                      value = "20")) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("EUR",NULL,# h5("EUR"),
                                                      
                                                      value = "4"))  
                                     
                                   )
                                     
                                   },
                                   {fluidRow(
                                     
                                     column(1,h5(2020)),
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("MEA",NULL,# h5("MEA"),
                                                      
                                                      value = "1.1")) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("APAC",NULL,# h5("APAC"),
                                                      
                                                      value = "1.2")) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("AMR",NULL,# h5("AMR"),
                                                      
                                                      value = "20")) ,
                                     
                                     
                                     
                                     column(2,
                                            
                                            textInput("EUR",NULL,# h5("EUR"),
                                                      
                                                      value = "4"))  
                                     
                                   )
                                     
                                   })
                             },
                             #end
                             #knobInput nhr
                             {
                               fluidRow(
                                 box(title=HTML( '<font color=#ffffff> <font size="4">Supply rate adjustments:</font></font>'), solidHeader = TRUE, status="primary",colour="light blue",
                                     width=12,
                                     fluidRow(
                                       actionButton("reset_hrk","Reset NHR"),actionButton("save_hrk","Save NHR"),
                                       tabBox(
                                         # The id lets us use input$tabset1 on the server to find the current tab
                                         id = "tabset1Y", width = '70%', height = "100%",
                                         tabPanel("Year 1",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2km1",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = 'rgba(2, 90, 216, 0.7)',angleOffset = -135,angleArc=270,inputColor = 'rgba(2, 90, 216, 0.7)')),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ka1",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2kap1",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ke1",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                                       ),
                                         tabPanel("Year 2",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2km2",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ka2",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2kap2",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ke2",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 3",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2km3",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ka3",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2kap3",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ke3",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 4",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2km4",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ka4",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = 'rgba(2, 90, 216, 0.7)')),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2kap1",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ke4",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 5",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2km5",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ka5",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2kap5",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ke5",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         )
                                         )
                                       )
                                 ))
                             }
                             #knf
                    ),
                    tabPanel("Growth Project",
                             {
                               
                               box(status = "primary", width = '100%',
                                   
                                   actionButton("reset_ld","Reset L&D"),
                                   
                                   # fluidRow(
                                   
                                   #   dataTableOutput(outputId = "tld")
                                   
                                   # ),
                                   
                                   fluidRow(
                                     column(2,br(),br(),h5('Impact')),
                                     column(2,
                                            textInput("t1", h5("Year 1 Value"),
                                                      value = '0'
                                            )) ,
                                     column(2,
                                            
                                            textInput("t2", h5("Year 2 Value"),
                                                      
                                                      value = '0'
                                                      
                                            )) ,
                                     
                                     column(2,
                                            
                                            textInput("t3", h5("Year 3 Value"),
                                                      
                                                      value = '0'
                                                      
                                            )) ,
                                     
                                     column(2,
                                            
                                            textInput("t4", h5("Year 4 Value"),
                                                      
                                                      value = '0'
                                                      
                                            )) ,
                                     
                                     column(2,
                                            
                                            textInput("t5", h5("Year 5 Value"),
                                                      
                                                      value = '0'
                                                      
                                            ))
                                     
                                   )
                                   
                               )
                               
                             }
                    )
                  )
                ), 
              {
              box(width = 14,status = "primary",
                  column(4,pickerInput("pdiv",label='Org. Structure',choices=unique(db$Division),selected = unique(db$Division), 
                                          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                                                         ), multiple = TRUE)),
                     column(4,pickerInput("prg",label=HTML('<font color=#454545>Region</font>'),choices=unique(db$Region),selected = unique(db$Region),
                                          options = list(
                                            `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),  multiple = TRUE)),
                     column(4,pickerInput("pType",label=HTML('<font color=#454545>Function/BU Type</font>'),choices=unique(db$`CF/SBU/AFF`),
                                          selected=unique(db$`CF/SBU/AFF`),
                                          options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                                                          ),  multiple = TRUE)),
                     column(4,pickerInput("pName",label=HTML('<font color=#454545>Function/Affiliate Name</font>'),choices=unique(db$`CF/SBU/AFF Name`),
                                          selected=unique(db$`CF/SBU/AFF Name`),
                                          options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)),
                     column(4,pickerInput("pFamily",label=HTML('<font color=#454545>Job Family</font>'),choices=unique(db$Family),selected=unique(db$Family),
                                          options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)),
                       
                       column(4,pickerInput("pSub",label=HTML('<font color=#454545>Sub-Family</font>'),choices=unique(db$`Sub Family`),
                                            selected=unique(db$`Sub Family`),
                                            options = list( `actions-box` = TRUE, size = 10,  `selected-text-format` = "count > 3" 
                                                            ),multiple = TRUE))#https://stackoverflow.com/questions/51355878/how-to-text-wrap-choices-from-a-pickerinput-if-the-length-of-the-choices-are-lo
                   )
              },
                uiOutput('variables'),uiOutput('variables2')
                
              ),
              
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Specyfic Parameters</font></font>')
                  , status = "primary", solidHeader = TRUE,width = '100%',
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "250px", width = '100%',
                    tabPanel("Demand",
                             actionButton("reset_t2sdd","Reset L&D"), "First tab content"),
                    tabPanel("Baseline Positioning",
                             actionButton("reset_b","Reset L&D"), "Tab content 2"),
                    tabPanel("Supply",
                             actionButton("reset_s","Reset L&D"), 
                             {
                               fluidRow(
                                 box(title=HTML( '<font color=#ffffff> <font size="4">Supply rate adjustments:</font></font>'), solidHeader = TRUE, status="primary",colour="light blue",
                                     width=12,
                                     fluidRow(
                                       actionButton("reset_shrk","Reset NHR"),actionButton("save_shrk","Save NHR"),
                                       tabBox(
                                         # The id lets us use input$tabset1 on the server to find the current tab
                                         id = "tabset1Y", width = '70%', height = "100%",
                                         tabPanel("Year 1",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skm1",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = 'rgba(2, 90, 216, 0.7)',angleOffset = -135,angleArc=270,inputColor = 'rgba(2, 90, 216, 0.7)')),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ska1",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skap1",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ske1",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 2",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skm2",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ska2",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skap2",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ske2",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 3",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skm3",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ska3",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skap3",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ske3",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 4",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skm4",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ska4",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = 'rgba(2, 90, 216, 0.7)')),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skap1",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ske4",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         ),
                                         tabPanel("Year 5",
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skm5",label = "MEA hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ska5",label = "APAC hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2skap5",label = "AMR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                                  column(3,
                                                         knobInput(
                                                           inputId = "t2ske5",label = "EUR hire percentage:",thickness=0.2, width=100, height=100,
                                                           value = 0,min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                                           fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69"))
                                         )
                                       )
                                     )
                                 ))
                             }
                             ),
                    tabPanel("Training",
                             {
                               box(status = "primary", width = '100%',
                                   actionButton("reset_sld","Reset L&D", 
                                                style="color: #fff; background-color: #ED2C67; border-color: #ED2C67; font-size:100%")
                                   ,actionButton("save_ld","Save L&D"),
                                   # fluidRow(
                                   #   dataTableOutput(outputId = "tld")
                                   # ),
                                   fluidRow(
                                     column(2,br(),br(),h5('Total L&D Spend / FTE')),
                                     column(2,
                                            textInput("t2sld1", h5("Year 1 Value"),
                                                      value = ld$`T=1` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2sld2", h5("Year 2 Value"),
                                                      value = ld$`T=2` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2sld3", h5("Year 3 Value"),
                                                      value = ld$`T=3` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2sld4", h5("Year 4 Value"),
                                                      value = ld$`T=4` #"1250"
                                            )) ,
                                     column(2,
                                            textInput("t2sld5", h5("Year 5 Value"),
                                                      value = ld$`T=5` #"1250"
                                            ))
                                   )
                                   
                               )
                               
                             }
                             )
                  )
                ),
                dataTableOutput('prep_t')
              )
              ),
      ######
      #end tab2
      #tab3
      ######
      tabItem(tabName = "tab3",
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Right-sizing Scenario Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                  radioButtons("radio_t3rs", label = NULL,
                               choiceNames = list("As-is", "Optimistic","Conservative"
                               ),
                               choiceValues = list(
                                 "rs", "r_opt_s","r_crvt_s"
                               ),selected = "rs", inline = TRUE),width = '60%'
                )
              ),
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                  radioButtons("radio_t3ss", label = NULL,
                               choiceNames = list("Conservative", "Base Case","Optimistic"
                               ),
                               choiceValues = list(
                                 "con", "bc","opt"
                               ),selected = "con", inline = TRUE),width = '60%'
                )
              ),
              
              box( status = "primary",collapsible = TRUE,
                   column(4,br(),actionButton("reset_all","Sabic All",width='100%')),
                   column(4,pickerInput("p3div",label='Org. Structure',choices=unique(db$Division),selected = unique(db$Division), 
                                        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                                        ), multiple = TRUE)),
                   column(4,pickerInput("p3rg",label=HTML('<font color=#454545>Region</font>'),choices=unique(db$Region),selected = unique(db$Region),
                                        options = list(
                                          `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),  multiple = TRUE)),
                   column(4,pickerInput("p3Type",label=HTML('<font color=#454545>Function/BU Type</font>'),choices=unique(db$`CF/SBU/AFF`),
                                        selected=unique(db$`CF/SBU/AFF`),
                                        options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                                        ),  multiple = TRUE)),
                   column(4,pickerInput("p3Name",label=HTML('<font color=#454545>Function/Affiliate Name</font>'),choices=unique(db$`CF/SBU/AFF Name`),
                                        selected=unique(db$`CF/SBU/AFF Name`),
                                        options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)),
                   column(4,pickerInput("p3Family",label=HTML('<font color=#454545>Job Family</font>'),choices=unique(db$Family),selected=unique(db$Family),
                                        options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE))
                   ,width = '100%'),
              fluidRow(
                column(3,pickerInput("p3emp",label=HTML('<font color=#454545>Employment</font>'),choices=c('Contractor','Direct Hire','NA'),
                                     selected=c('Contractor','Direct Hire','NA'),multiple = TRUE)),
                column(3,pickerInput("p3seg",label=HTML('<font color=#454545>Segment</font>'),choices=c('Core','Specialist','Support','Critical','Not Map'),
                                     selected=c('Core','Specialist','Support','Critical','Not Map'),multiple = TRUE))
              ),
              
              fluidRow(
                
                box(status = "primary", width = 12, collapsible = TRUE,
                    
                    radioButtons("radio_t3", label = HTML('<font color=#454545>Workforce Journey</font>'),
                                 
                                 choiceNames = list("plot 1","plot 2"
                                                    
                                 ),
                                 
                                 choiceValues = list(
                                   
                                   'plot1',2
                                   
                                 ),selected = "plot1", inline = TRUE),
                    
                    conditionalPanel(
                      
                      condition = "input.radio_t3 == 'plot1'",
                      
                      plotlyOutput("plot31")
                      
                    ),
                    
                    conditionalPanel(
                      
                      condition = "input.radio_t3 == 2",
                      
                      plotlyOutput("plot32")
                      
                    )
                    
                )
                
              ),
              
              fluidRow(
                
                box(status = "primary", width = 12, collapsible = TRUE,
                    
                    label='asd', h4('Scenario Comparison'),
                    
                    plotlyOutput("plot33")
                    
                )
                
              ),
              
              fluidRow(
                
                column(6, box(status = "primary", width = '100%', collapsible = TRUE,
                              
                              label='asd', h4('Gap Analysis'),
                              
                              plotlyOutput("plot34")
                              
                )),
                
                column(6, box(status = "primary", width = '100%', collapsible = TRUE,
                              
                              label='asd', h4('Forecast'),
                              
                              plotlyOutput("plot35")
                              
                ))
                
              )
              
      ),
      
      ######
      #end
      #tab4
      ######
      tabItem(tabName = "tab4",
              tabsetPanel(id = "tabs2",
                          tabPanel(
                            title = "Section Selection",icon = icon("glyphicon glyphicon-saved",lib ="glyphicon"),
                            
                            value = "page1",
                            fluidRow(
                              box(
                                title =HTML( '<font color=#ffffff> <font size="5">Right-sizing Scenario Selection</font></font>')
                                , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                                radioButtons("radio_t4rs", label = NULL,
                                             choiceNames = list("As-is", "Optimistic","Conservative"
                                             ),
                                             choiceValues = list(
                                               "rs", "r_opt_s","r_crvt_s"
                                             ),selected = "rs", inline = TRUE),width = '60%'
                              )
                            ),
                            fluidRow(
                              box(
                                title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection</font></font>')
                                , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                                radioButtons("radio_t4ss", label = NULL,
                                             choiceNames = list("Conservative", "Base Case","Optimistic"
                                             ),
                                             choiceValues = list(
                                               "con", "bc","opt"
                                             ),selected = "con", inline = TRUE),width = '60%'
                              )
                            ),
                            box( status = "primary",collapsible = TRUE,
                                 actionButton("reset_all","Sabic All"),br(),
                                 column(4,pickerInput("p4div",label='Org. Structure',choices=unique(db$Division),selected = unique(db$Division), 
                                                      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                                                      ), multiple = TRUE)),
                                 column(4,pickerInput("p4rg",label=HTML('<font color=#454545>Region</font>'),choices=unique(db$Region),selected = unique(db$Region),
                                                      options = list(
                                                        `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),  multiple = TRUE)),
                                 column(4,pickerInput("p4Type",label=HTML('<font color=#454545>Function/BU Type</font>'),choices=unique(db$`CF/SBU/AFF`),
                                                      selected=unique(db$`CF/SBU/AFF`),
                                                      options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                                                      ),  multiple = TRUE)),
                                 column(4,pickerInput("p4Name",label=HTML('<font color=#454545>Function/Affiliate Name</font>'),choices=unique(db$`CF/SBU/AFF Name`),
                                                      selected=unique(db$`CF/SBU/AFF Name`),
                                                      options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)),
                                 column(4,pickerInput("p4Family",label=HTML('<font color=#454545>Job Family</font>'),choices=unique(db$Family),selected=unique(db$Family),
                                                      options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)),
                                 column(4,pickerInput("p4sFamily",label=HTML('<font color=#454545>Sub Family</font>'),choices=unique(db$`Sub Family`),selected=unique(db$`Sub Family`),
                                                      options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE))
                                 ,width = '100%'),
                            fluidRow(
                              column(3,pickerInput("p4emp",label=HTML('<font color=#454545>Employment</font>'),choices=c('Contractor','Direct Hire','NA'),
                                                   selected=c('Contractor','Direct Hire','NA'),multiple = TRUE)),
                              column(3,pickerInput("p4seg",label=HTML('<font color=#454545>Segment</font>'),choices=c('Core','Specialist','Support','Critical','Not Map'),
                                                   selected=c('Core','Specialist','Support','Critical','Not Map'),multiple = TRUE))
                            )
                          ),
                          tabPanel("Section Summary", icon = icon("line-chart"),
                                   box( status = "primary",width = 12, height=150,
                                        plotlyOutput("plot42",height = '140px')
                                   ),
                                   box( status = "primary",width = 12,
                                        column(6,
                                        plotlyOutput("plot421")),
                                        column(6,
                                               plotlyOutput("plot422"))
                                   )
                          ),
                          tabPanel("Workforce Journey", icon = icon("line-chart"),
                                   fluidRow(
                                     box(status = "primary", width = 12,
                                         radioButtons("radio_t4", label = HTML('<font color=#454545>Workforce Journey</font>'),
                                                      
                                                      choiceNames = list("plot 1","plot 2"
                                                      ),
                                                      choiceValues = list(
                                                        'plot1',2
                                                      ),selected = "plot1", inline = TRUE),
                                         conditionalPanel(
                                           condition = "input.radio_t4 == 'plot1'",
                                           plotlyOutput("plot43")
                                         ),
                                         conditionalPanel(
                                           condition = "input.radio_t4 == 2",
                                           
                                           plotlyOutput("plot432")
                                           
                                         )
                                         
                                     )
                                     
                                   )
                                   
                          ),
                          
                          tabPanel("Workforce Forecast", icon = icon("line-chart"),
                                   box( status = "primary",width = 12,
                                        plotlyOutput("plot44"),
                                        plotlyOutput("plot442")
                                   )
                                   
                                   
                                   
                          )
                          
              )
              
      ),
      
      ######
      #end
      #tab5
      ######
      
      tabItem(tabName = "tab5",
              
              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                               
                               tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                          
                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")),
              
              align = "center",
              
              uiOutput("KKosz")
              
                               )
      
      ######
      #end
                               )
              )
                  )
##########

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #input default
  #####
  observeEvent(input$reset_ld,{
    updateTextInput(session, "t2ld1", value = ld$`T=1`)
    updateTextInput(session, "t2ld2", value = ld$`T=2`)
    updateTextInput(session, "t2ld3", value = ld$`T=3`)
    updateTextInput(session, "t2ld4", value = ld$`T=4`)
    updateTextInput(session, "t2ld5", value = ld$`T=5`)
    
  })
  observeEvent(input$reset_hrk,{
    updateTextInput(session, "t2km1", value = nhr$MEA[1]*100)
    updateTextInput(session, "t2ka1", value = nhr$APAC[1]*100)
    updateTextInput(session, "t2kap1", value = nhr$AMR[1]*100)
    updateTextInput(session, "t2ke1", value = nhr$EUR[1]*100)
    
    updateTextInput(session, "t2km2", value = nhr$MEA[2]*100)
    updateTextInput(session, "t2ka2", value = nhr$APAC[2]*100)
    updateTextInput(session, "t2kap2", value = nhr$AMR[2]*100)
    updateTextInput(session, "t2ke2", value = nhr$EUR[2]*100)
    
  })
  observeEvent(input$reset_sld,{
    updateTextInput(session, "t2sld1", value = ld$`T=1`)
    updateTextInput(session, "t2sld2", value = ld$`T=2`)
    updateTextInput(session, "t2sld3", value = ld$`T=3`)
    updateTextInput(session, "t2sld4", value = ld$`T=4`)
    updateTextInput(session, "t2sld5", value = ld$`T=5`)
    
  })
  #####
  
  #data prep
  #####
  r_prep<-reactive({
    z=0
    if(input$radio_t2rs=="rs"){
      z=rs
    }
    if(input$radio_t2rs=="r_opt_s"){
      z=r_opt_s
    }
    else{
      z=r_crvt_s
    }
    
    if(input$save_t2ld){
      z <- mutate(z, ld1 = as.numeric(input$t2ld1))
      z <- mutate(z, ld2 = as.numeric(input$t2ld2))
      z <- mutate(z, ld3 = as.numeric(input$t2ld3))
      z <- mutate(z, ld4 = as.numeric(input$t2ld4))
      z <- mutate(z, ld5 = as.numeric(input$t2ld5))
      #ld procent cal training
      z <- mutate(z, ldt1 = ldp$`t=1`[3]*z$ld1/2000)
      z <- mutate(z, ldt2 = (ldp$`t=1`[3]+ldp$`t=2`[3])*z$ld2/2000)
      z <- mutate(z, ldt3 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3])*z$ld3/2000)
      z <- mutate(z, ldt4 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3])*z$ld4/2000)
      z <- mutate(z, ldt5 = (ldp$`t=1`[3]+ldp$`t=2`[3]+ldp$`t=3`[3]+ldp$`t=4`[3]+ldp$`t=5`[3])*z$ld5/2000)
      #ld FTE Impact training
      z <- mutate(z, fte1 = (1-z$ldt1)*z$dds19)
      z <- mutate(z, fte2 = (1-z$ldt2)*z$dds20)
      z <- mutate(z, fte3 = (1-z$ldt3)*z$dds21)
      z <- mutate(z, fte4 = (1-z$ldt4)*z$dds22)
      z <- mutate(z, fte5 = (1-z$ldt5)*z$dds23)
    }
    else{
      z
    }
 
    
  })
  
  r_prep_s <- reactive({
    z=r_prep()
    z=z[z$total>1,]
    z <- z[z$Division %in% input$pdiv ,]
    z <- z[z$Region %in% input$prg ,]
    z <- z[z$`CF/SBU/AFF` %in% input$pType ,]
    z <- z[z$`CF/SBU/AFF Name` %in% input$pName ,]
    z <- z[z$Family %in% input$pFamily ,]
    z
  })

  aoc<-reactive({
    z=0
    if(input$radio_t2rsss=="rs"){
      z=rs
    }
    if(input$radio_t2rsss=="r_opt_s"){
      z=r_opt_s
    }
    else{
      z=r_crvt_s
    }
    z <- z[z$Division %in% input$p3div ,]
    z <- z[z$Region %in% input$p3rg ,]
  })
  #####
  
  #filter
  #####
  dataset<-reactive({
    z <- db
    z <- z[z$Family %in%  if(is.null(input$variables2)){unique(db$Family)} else (input$variables2) ,]
  })
  outVar <- reactive({
    vars <- unique(db$Family)
        return(vars)
  })
  
  outVar2 <- reactive({
    vars <- unique(dataset()$`Sub Family`)
    return(vars)
  })
  
  
  
  output$variables = renderUI({
    selectInput('variables2', 'Family', outVar(), multiple = TRUE)
  })
  
  output$variables2 = renderUI({
    selectInput('variables3', 'Sub Family', outVar2(), multiple = TRUE)
  })
  
  
  
  #####
  
  #plot
  #####
  
  output$plot311<- renderPlotly({
    
    x <- c('1.Current<br>headcount', '1.Current<br>headcount', '2.Addition',  '3.Substraction', '4.Automation', '5.Final')
    
    y <- c(300, 410, 660,  590, 400,  340)
    
    base <- c(0, 0, 430,  570, 370,  0)
    
    revenue <- c(320, 110, 260,  0, 0,  0)
    
    costs <- c(0, 0, 0,  120, 200, 0)
    
    profit <- c(0, 0, 0, 0, 0, 370)
    
    text <- c('$320K','$430K', '$260K', '$-120K', '$-200K', '$370K')
    
    data <- data.frame(x, base, revenue, costs, profit, text)
    
    
    
    #The default order will be alphabetized unless specified as below:
    
    
    
    plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'))%>%
      
      add_trace(y = ~revenue, marker = list(color = 'rgba(55, 128, 191, 0.7)',
                                            
                                            line = list(color = 'rgba(55, 128, 191, 0.7)',
                                                        
                                                        width = 2))) %>%
      
      add_trace(y = ~costs, marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                          
                                          line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                      
                                                      width = 2))) %>%
      
      add_trace(y = ~profit, marker = list(color = 'rgba(50, 171, 96, 0.7)',
                                           
                                           line = list(color = 'rgba(50, 171, 96, 1.0)',
                                                       
                                                       width = 2))) %>%
      
      layout(title = 'Headcount',
             
             xaxis = list(title = ""),
             
             yaxis = list(title = ""),
             
             barmode = 'stack',
             
             #  paper_bgcolor = 'rgba(245, 246, 249, 1)',
             
             # plot_bgcolor = 'rgba(245, 246, 249, 1)',
             
             showlegend = FALSE) %>%
      
      add_annotations(text = text,
                      
                      x = x,
                      
                      y = y,
                      
                      xref = "x",
                      
                      yref = "y",
                      
                      font = list(family = 'Arial',
                                  
                                  size = 14,
                                  
                                  color = 'rgba(245, 246, 249, 1)'),
                      
                      showarrow = FALSE)
    
  })
  
  output$plot322<- renderPlotly({
    col=c(rep(c('rgba(282, 157, 13, 1.0)','rgba(2, 90, 216, 1.0)','rgba(2, 69, 186, 1.0)','rgba(232, 137, 13, 1.0)','rgba(112, 127, 13, 1.0)'),8))
    col2=c(rep(c('rgba(282, 157, 13, 1.0)','rgba(2, 90, 216, 1.0)','rgba(2, 69, 186, 1.0)','rgba(232, 137, 13, 1.0)','rgba(112, 127, 13, 1.0)'),8))
    
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),8))
    
    aoc()
    ss=sum(aoc()$Core)+sum(aoc()$Specialist)+sum(aoc()$Support)+sum(aoc()$Critical)
    nm=sum(aoc()$total)-ss
    
    nms =sum(aoc()$subt)-(sum(sign(aoc()$Core)*aoc()$subt) + sum(sign(aoc()$Specialist)*aoc()$subt) + sum(sign(aoc()$Support)*aoc()$subt) + sum(sign(aoc()$Critical)*aoc()$subt))
    
    nma =sum(aoc()$sray23)-(sum(sign(aoc()$Core)*aoc()$sray23) + sum(sign(aoc()$Specialist)*aoc()$sray23) + sum(sign(aoc()$Support)*aoc()$sray23) + sum(sign(aoc()$Critical)*aoc()$sray23))
    
    nmdd =sum(aoc()$dds23)-(sum(sign(aoc()$Core)*aoc()$dds23) + sum(sign(aoc()$Specialist)*aoc()$dds23) + sum(sign(aoc()$Support)*aoc()$dds23) + sum(sign(aoc()$Critical)*aoc()$dds23))
    
    
    x <- c(rep('1.Current<br>headcount',5),rep('2.Upsizing',5),rep('3.Optimization',5), rep('4.Y1 E',5),rep('5.Y2 E',5),rep('6.Y3 E',5),rep('7.Y4 E',5),rep('8.Y5 E',5))
    #y <- c(sum(rs$total), sum(rs$total)+sum(rs$addt),sum(rs$total)+sum(rs$addt)-sum(rs$subt), sum(rs$total)+sum(rs$addt)-sum(rs$subt),sum(rs$total),sum(rs$fte5),7,8)
    base <-    c(rep(0,5),               sum(aoc()$total),rep(0,4),sum(aoc()$total)-sum(aoc()$subt),rep(0,4), rep(0,5), rep(0,5),     rep(0,5),rep(0,5),rep(0,5))
    revenue <- c(sum(aoc()$Core),sum(aoc()$Specialist),sum(aoc()$Support),sum(aoc()$Critical), nm, rep(sum(aoc()$addt),5), rep(0,5),     rep(0,5),  rep(0,5),  rep(0,5), rep(0,5),  rep(0,5))
    costs <-   c(rep(0,5),                 rep(0,5),       sum(sign(aoc()$Core)*aoc()$subt),sum(sign(aoc()$Specialist)*aoc()$subt),sum(sign(aoc()$Support)*aoc()$subt),sum(sign(aoc()$Critical)*aoc()$subt), nms,    rep(0,5),     rep(0,5),   rep(0,5), rep(0,5) ,rep(0,5))
    
    profit <-  c(rep(0,5),                 rep(0,5),                rep(0,5),    sum(aoc()$Core)-sum(sign(aoc()$Core)*aoc()$subt)  ,  sum(aoc()$Specialist)-sum(sign(aoc()$Specialist)*aoc()$subt)  ,  sum(aoc()$Support)-sum(sign(aoc()$Support)*aoc()$subt)  ,  sum(aoc()$Critical)-sum(sign(aoc()$Critical)*aoc()$subt)  ,        nm-nms, 
                 sum(sign(aoc()$Core)*aoc()$sray20), sum(sign(aoc()$Specialist)*aoc()$sray20), sum(sign(aoc()$Support)*aoc()$sray20), sum(sign(aoc()$Critical)*aoc()$sray20), nmdd, 
                 sum(sign(aoc()$Core)*aoc()$sray21), sum(sign(aoc()$Specialist)*aoc()$sray21), sum(sign(aoc()$Support)*aoc()$sray21), sum(sign(aoc()$Critical)*aoc()$sray21), nmdd, 
                 sum(sign(aoc()$Core)*aoc()$sray22), sum(sign(aoc()$Specialist)*aoc()$sray22), sum(sign(aoc()$Support)*aoc()$sray22), sum(sign(aoc()$Critical)*aoc()$sray22), nmdd, 
                 sum(sign(aoc()$Core)*aoc()$sray23), sum(sign(aoc()$Specialist)*aoc()$sray23), sum(sign(aoc()$Support)*aoc()$sray23), sum(sign(aoc()$Critical)*aoc()$sray23), nmdd)
    
    data <- data.frame(x, base, revenue, costs, profit,col,cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', color =~cat, marker = list(color = 'rgba(1,1,1, 0.0)'))%>%
      add_trace(y = ~revenue, color =~cat, colors = ~col,hoverinfo = 'text', text = ~paste(cat, revenue),
                marker = list(color = ~col)) %>%
      add_trace(y = ~costs,color =~cat,hoverinfo = 'text', text = ~paste(cat, costs), 
                marker = list(color = ~col)) %>%
      add_trace(y = ~profit,color =~cat, hoverinfo = 'text', text = ~paste(cat, profit),
                marker = list(color = ~col)
                ) %>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot31<- renderPlotly({
  x <- c(rep('1.Current<br>headcount',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  ss=sum(rs$Core)+sum(rs$Specialist)+sum(rs$Support)+sum(rs$Critical)
  nm=sum(rs$total)-ss
  revenue <- c(sum(rs$Core),sum(rs$Specialist),sum(rs$Support),sum(rs$Critical), nm)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data0 <- data.frame(x, revenue, col,cat)
  
  
  ##opt
  x <- c(rep('2.Upsizing',1))
  col=c('rgba(1,1,1, 0.0)')
  revenue <-c(sum(rs$total))
  cat=c(rep(c('0.Base'),1))
  data1 <- data.frame(x, revenue, col,cat)
  
  ##o21
  x <- c(rep('3.Optimization',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
  revenue <-c(sum(rs$total)-sum(rs$subt), sum(sign(rs$Core)*rs$subt),sum(sign(rs$Specialist)*rs$subt),sum(sign(rs$Support)*rs$subt),sum(sign(rs$Critical)*rs$subt), nms)
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data2 <- data.frame(x, revenue, col,cat)
  
  
  ##1
  x <- c(rep('4.Y1 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
  revenue <-c(sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  ,        nm-nms)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data3 <- data.frame(x, revenue, col,cat)
  
  
  #2
  x <- c(rep('5.Automation Reduction',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nma =sum(rs$sray23)-(sum(sign(rs$Core)*rs$sray23) + sum(sign(rs$Specialist)*rs$sray23) + sum(sign(rs$Support)*rs$sray23) + sum(sign(rs$Critical)*rs$sray23))
  
  nma=sum(rs$sray23)-sum(rs$fte5)
  
  rasa <-c(0,sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  , 0)
  revenue <-c(-sum(rs$sray23), sum(sign(rs$Core)*rs$sray23),sum(sign(rs$Specialist)*rs$sray23),sum(sign(rs$Support)*rs$sray23),sum(sign(rs$Critical)*rs$sray23), 0)
  revenue <-rasa-revenue
  nma=sum(rs$sray23)-sum(rs$fte5)-sum(revenue[2:5])
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data4 <- data.frame(x, revenue, col,cat)
  
  
  
  
  #3Capability Enhancement
  x <- c(rep('6.Capability Enhancement',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmf =sum(rs$fte5)-(sum(sign(rs$Core)*rs$fte5) + sum(sign(rs$Specialist)*rs$fte5) + sum(sign(rs$Support)*rs$fte5) + sum(sign(rs$Critical)*rs$fte5))
  
  rasa <-c(sum(rs$fte5),sum(sign(rs$Core)*rs$sray23),sum(sign(rs$Specialist)*rs$sray23),sum(sign(rs$Support)*rs$sray23),sum(sign(rs$Critical)*rs$sray23),       nma)
  revenue <-c(0, sum(sign(rs$Core)*rs$fte5),sum(sign(rs$Specialist)*rs$fte5),sum(sign(rs$Support)*rs$fte5),sum(sign(rs$Critical)*rs$fte5), nmf)
  revenue <-rasa-revenue
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data5 <- data.frame(x, revenue, col,cat)
  
  #4
  x <- c(rep('7.Business (R&M)',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmd =sum(rs$dds23)-(sum(sign(rs$Core)*rs$dds23) + sum(sign(rs$Specialist)*rs$dds23) + sum(sign(rs$Support)*rs$dds23) + sum(sign(rs$Critical)*rs$dds23))
  
  rasa <-c(sum(rs$fte5),sum(sign(rs$Core)*rs$dds23),sum(sign(rs$Specialist)*rs$dds23),sum(sign(rs$Support)*rs$dds23),sum(sign(rs$Critical)*rs$dds23), nmd)
  revenue <-c(0, sum(sign(rs$Core)*rs$fte5),sum(sign(rs$Specialist)*rs$fte5),sum(sign(rs$Support)*rs$fte5),sum(sign(rs$Critical)*rs$fte5), nmf)
  revenue <-rasa-revenue
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data6 <- data.frame(x, revenue, col,cat)
  
  #5
  x <- c(rep('8.Y5 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmdd =sum(rs$dds23)-(sum(sign(rs$Core)*rs$dds23) + sum(sign(rs$Specialist)*rs$dds23) + sum(sign(rs$Support)*rs$dds23) + sum(sign(rs$Critical)*rs$dds23))
  revenue <-c(sum(sign(rs$Core)*rs$dds23), sum(sign(rs$Specialist)*rs$dds23), sum(sign(rs$Support)*rs$dds23), sum(sign(rs$Critical)*rs$dds23), nmdd)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data7 <- data.frame(x, revenue, col,cat)
  
  data <- union(data0,data1)
  data <- union(data,data2)
  data <- union(data,data3)
  data <- union(data,data4)
  data <- union(data,data5)
  data <- union(data,data6)
  data <- union(data,data7)
  
  plot_ly(data, x = ~x, y = ~revenue, type = 'bar',name='Demand',
          color =~cat, hoverinfo = 'text', text = ~paste(cat, revenue)
          ,colors = c("transparent","darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64"))%>%
    layout(title = 'Headcount', margin = list(b = 100),
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           barmode = 'stack',
           showlegend = FALSE)
})
  
  output$plot312<- renderPlotly({
    col=c(rep(c('rgba(282, 157, 13, 0.7)','rgba(2, 90, 216, 0.7)','rgba(2, 69, 186, 0.7)','rgba(232, 137, 13, 0.7)','rgba(112, 127, 13, 0.7)'),8))
    col2=c(rep(c('rgba(282, 157, 13, 1.0)','rgba(2, 90, 216, 1.0)','rgba(2, 69, 186, 1.0)','rgba(232, 137, 13, 1.0)','rgba(112, 127, 13, 1.0)'),8))
    colb=c(rep(c('rgba(282, 157, 13, 0.0)','rgba(2, 90, 216, 0.0)','rgba(2, 69, 186, 0.0)','rgba(232, 137, 13, 0.0)','rgba(112, 127, 13, 0.0)'),8))
    
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),8))
    
    aoc()
    ss=sum(aoc()$Core)+sum(aoc()$Specialist)+sum(aoc()$Support)+sum(aoc()$Critical)
    nm=sum(aoc()$total)-ss
    
    nms =sum(aoc()$subt)-(sum(sign(aoc()$Core)*aoc()$subt) + sum(sign(aoc()$Specialist)*aoc()$subt) + sum(sign(aoc()$Support)*aoc()$subt) + sum(sign(aoc()$Critical)*aoc()$subt))
    
    nma =sum(aoc()$sray23)-(sum(sign(aoc()$Core)*aoc()$sray23) + sum(sign(aoc()$Specialist)*aoc()$sray23) + sum(sign(aoc()$Support)*aoc()$sray23) + sum(sign(aoc()$Critical)*aoc()$sray23))
    
    nmdd =sum(aoc()$dds23)-(sum(sign(aoc()$Core)*aoc()$dds23) + sum(sign(aoc()$Specialist)*aoc()$dds23) + sum(sign(aoc()$Support)*aoc()$dds23) + sum(sign(aoc()$Critical)*aoc()$dds23))
    
    
    x <- c(rep('1.Current<br>headcount',5),rep('2.Addition',5),rep('3.Substraction',5), rep('4.Y0 Baseline',5),rep('5.Automation',5),rep('6.Training',5),rep('7.Demand',5),rep('8.Y5 FINAL',5))
    #y <- c(sum(rs$total), sum(rs$total)+sum(rs$addt),sum(rs$total)+sum(rs$addt)-sum(rs$subt), sum(rs$total)+sum(rs$addt)-sum(rs$subt),sum(rs$total),sum(rs$fte5),7,8)
    base <-    c(rep(0,5) ,rep(0,4),               sum(aoc()$total),rep(0,4),sum(aoc()$total)-sum(aoc()$subt), rep(0,5), sum(aoc()$sray23),rep(0,4),     sum(aoc()$fte5),rep(0,4), sum(aoc()$fte5),rep(0,4),rep(0,5))
    revenue <- c(sum(aoc()$Core),sum(aoc()$Specialist),sum(aoc()$Support),sum(aoc()$Critical), nm, rep(sum(aoc()$addt),5), rep(0,5),     rep(0,5),  rep(0,5),  rep(0,5), sum(aoc()$dds23)-sum(aoc()$fte5)   ,rep(0,4),  rep(0,5))
    costs <-   c(rep(0,5),                 rep(0,5),       sum(sign(aoc()$Core)*aoc()$subt),sum(sign(aoc()$Specialist)*aoc()$subt),sum(sign(aoc()$Support)*aoc()$subt),sum(sign(aoc()$Critical)*aoc()$subt), nms,    rep(0,5),     rep(0,4), sum(aoc()$total)-sum(aoc()$subt)-sum(aoc()$sray23)   , sum(aoc()$sray23)-sum(aoc()$fte5),  rep(0,4), rep(0,5) ,rep(0,5))
    profit <-  c(rep(0,5),                 rep(0,5),                rep(0,5),    sum(aoc()$Core)-sum(sign(aoc()$Core)*aoc()$subt)  ,  sum(aoc()$Specialist)-sum(sign(aoc()$Specialist)*aoc()$subt)  ,  sum(aoc()$Support)-sum(sign(aoc()$Support)*aoc()$subt)  ,  sum(aoc()$Critical)-sum(sign(aoc()$Critical)*aoc()$subt)  ,        nm-nms, rep(0,5), rep(0,5),rep(0,5),sum(sign(aoc()$Core)*aoc()$dds23), sum(sign(aoc()$Specialist)*aoc()$dds23), sum(sign(aoc()$Support)*aoc()$dds23), sum(sign(aoc()$Critical)*aoc()$dds23), nmdd)
    data <- data.frame(x, base, revenue, costs, profit,col,cat,colb)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', color =~cat, marker = list(color = ~colb))%>%
      add_trace(y = ~revenue, color =~cat, marker = list(color = ~col),hoverinfo = 'text', text = ~paste(cat, revenue)) %>%
      add_trace(y = ~costs,color =~cat,hoverinfo = 'text', text = ~paste(cat, costs), marker = list(color = ~col,
                                                                                                    line = list(color = col2,
                                                                                                                width = 2))) %>%
      add_trace(y = ~profit,color =~cat,hoverinfo = 'text', text = ~paste(cat, profit), marker = list(color = ~col,
                                                                                                      line = list(color = col2,
                                                                                                                  width = 2))) %>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = "", tickangle = 90),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot33<- renderPlotly({
    
    x <- c('Y1 E', 'Y2 E', 'Y3 E', 'Y4 E', 'Y5 E')
    
    As_is <- c(sum(rs$fte1), sum(rs$fte2),sum(rs$fte3),sum(rs$fte4),sum(rs$fte5))
    
    Opt <- c(sum(r_opt_s$fte1), sum(r_opt_s$fte2),sum(r_opt_s$fte3),sum(r_opt_s$fte4),sum(r_opt_s$fte5))
    
    Con <- c(sum(r_crvt_s$fte1), sum(r_crvt_s$fte2),sum(r_crvt_s$fte3),sum(r_crvt_s$fte4),sum(r_crvt_s$fte5))
    
    data <- data.frame(x, Opt, Con, As_is)
    
    plot_ly(data, x = ~x, y = ~As_is, type = 'bar',name='As is', marker = list(color = 'rgba(191, 191, 191, 0.7)'))%>%
      
      add_trace(y = ~Opt,name='Optimistic', marker = list(color = 'rgba(227, 82, 5, 0.7)')) %>%
      
      add_trace(y = ~Con,name='Conservative', marker = list(color = 'rgba(0, 159, 223, 0.7)'))%>%
      
      layout(title = 'Right Sizing Scenario Comparison',
             
             xaxis = list(title = ""),
             
             yaxis = list(title = ""))
    
  })
  
  output$plot34<- renderPlotly({
    
    x <- c(rep(c('Y1 E Demand','Y2 E Demand','Y3 E Demand','Y4 E Demand','Y5 E Demand'),5),rep(c('Y1 E Support','Y2 E Support','Y3 E Support','Y4 E Support','Y5 E Support'),5))
    
    core1 <- c(sum(sign(aoc()$Core)*aoc()$fte1), sum(sign(aoc()$Core)*aoc()$fte2),sum(sign(aoc()$Core)*aoc()$fte3),sum(sign(aoc()$Core)*aoc()$fte4),sum(sign(aoc()$Core)*aoc()$fte5))
    core2 <- c(sum(sign(aoc()$Core)*aoc()$sels), sum(sign(aoc()$Core)*aoc()$sray20),sum(sign(aoc()$Core)*aoc()$sray21),sum(sign(aoc()$Core)*aoc()$sray22),sum(sign(aoc()$Core)*aoc()$sray23))
    
    specialist1 <- c(sum(sign(aoc()$Specialist)*aoc()$fte1), sum(sign(aoc()$Specialist)*aoc()$fte2),sum(sign(aoc()$Specialist)*aoc()$fte3),sum(sign(aoc()$Specialist)*aoc()$fte4),sum(sign(aoc()$Specialist)*aoc()$fte5))
    specialist2 <- c(sum(sign(aoc()$Specialist)*aoc()$sels), sum(sign(aoc()$Specialist)*aoc()$sray20),sum(sign(aoc()$Specialist)*aoc()$sray21),sum(sign(aoc()$Specialist)*aoc()$sray22),sum(sign(aoc()$Specialist)*aoc()$sray23))
    
    support1 <- c(sum(sign(aoc()$Support)*aoc()$fte1), sum(sign(aoc()$Support)*aoc()$fte2),sum(sign(aoc()$Support)*aoc()$fte3),sum(sign(aoc()$Support)*aoc()$fte4),sum(sign(aoc()$Support)*aoc()$fte5))
    support2 <- c(sum(sign(aoc()$Support)*aoc()$sels), sum(sign(aoc()$Support)*aoc()$sray20),sum(sign(aoc()$Support)*aoc()$sray21),sum(sign(aoc()$Support)*aoc()$sray22),sum(sign(aoc()$Support)*aoc()$sray23))
    
    critical1 <- c(sum(sign(aoc()$Critical)*aoc()$fte1), sum(sign(aoc()$Critical)*aoc()$fte2),sum(sign(aoc()$Critical)*aoc()$fte3),sum(sign(aoc()$Critical)*aoc()$fte4),sum(sign(aoc()$Critical)*aoc()$fte5))
    critical2 <- c(sum(sign(aoc()$Critical)*aoc()$sels), sum(sign(aoc()$Critical)*aoc()$sray20),sum(sign(aoc()$Critical)*aoc()$sray21),sum(sign(aoc()$Critical)*aoc()$sray22),sum(sign(aoc()$Critical)*aoc()$sray23))
    
    nm1 <- c(sum(aoc()$sels), sum(aoc()$sray19),sum(aoc()$sray20),sum(aoc()$sray21),sum(aoc()$sray22))-core1-specialist1-support1-critical1
    nm2 <- c(sum(aoc()$fte1), sum(aoc()$fte2),sum(aoc()$fte3),sum(aoc()$fte4),sum(aoc()$fte5))-core2-specialist2-support2-critical2
    
    dema<-c(core1, specialist1, support1,critical1, nm1, core2, specialist2, support2,critical2, nm2)
    cat <-c(rep(c(rep('1.Core',5),rep('2.Specialist',5),rep('3.Support',5),rep('4.Critical',5),rep('5.Not Map',5)),2))
    
    col1<-c(rep('rgba(282, 157, 13, 0.7)',5),rep('rgba(2, 90, 216, 0.7)',5),rep('rgba(200, 90, 216, 0.7)',5),rep('rgba(2, 190, 216, 0.7)',5),rep('rgba(82, 190, 16, 0.7)',5))
    col <- c(col1,col1)
    data <- data.frame(x, dema, cat, col)
    
    plot_ly(data, x = ~x, y = ~dema, type = 'bar',name='Demand',
            color =~cat, hoverinfo = 'text', text = ~paste(cat, dema)
            ,colors = c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
            ) %>%
      layout(title = 'Headcount',barmode = 'stack',margin = list(b = 150),
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
  })
  
  output$plot32<- renderPlotly({
  x <- c(rep('1.Current<br>headcount',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  ss=sum(rs$Core)+sum(rs$Specialist)+sum(rs$Support)+sum(rs$Critical)
  nm=sum(rs$total)-ss
  revenue <- c(sum(rs$Core),sum(rs$Specialist),sum(rs$Support),sum(rs$Critical), nm)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data0 <- data.frame(x, revenue, col,cat)
  
  
  ##opt
  x <- c(rep('2.Upsizing',1))
  col=c('rgba(1,1,1, 0.0)')
  revenue <-c(sum(rs$total))
  cat=c(rep(c('0.Base'),1))
  data1 <- data.frame(x, revenue, col,cat)
  
  ##o21
  x <- c(rep('3.Optimization',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
  revenue <-c(sum(rs$total)-sum(rs$subt), sum(sign(rs$Core)*rs$subt),sum(sign(rs$Specialist)*rs$subt),sum(sign(rs$Support)*rs$subt),sum(sign(rs$Critical)*rs$subt), nms)
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data2 <- data.frame(x, revenue, col,cat)
  
  
  ##1
  x <- c(rep('4.Y1 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
  revenue <-c(sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  ,        nm-nms)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data3 <- data.frame(x, revenue, col,cat)
  
  
  #2
  x <- c(rep('5.Y2 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmdd =sum(rs$sray20)-(sum(sign(rs$Core)*rs$sray20) + sum(sign(rs$Specialist)*rs$sray20) + sum(sign(rs$Support)*rs$sray20) + sum(sign(rs$Critical)*rs$sray20))
  revenue <-c(sum(sign(rs$Core)*rs$sray20), sum(sign(rs$Specialist)*rs$sray20), sum(sign(rs$Support)*rs$sray20), sum(sign(rs$Critical)*rs$sray20), nmdd)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data4 <- data.frame(x, revenue, col,cat)
  
  #3
  x <- c(rep('6.Y3 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmdd =sum(rs$sray21)-(sum(sign(rs$Core)*rs$sray21) + sum(sign(rs$Specialist)*rs$sray21) + sum(sign(rs$Support)*rs$sray21) + sum(sign(rs$Critical)*rs$sray21))
  revenue <-c(sum(sign(rs$Core)*rs$sray21), sum(sign(rs$Specialist)*rs$sray21), sum(sign(rs$Support)*rs$sray21), sum(sign(rs$Critical)*rs$sray21), nmdd)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data5 <- data.frame(x, revenue, col,cat)
  
  #4
  x <- c(rep('7.Y4 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmdd =sum(rs$sray22)-(sum(sign(rs$Core)*rs$sray22) + sum(sign(rs$Specialist)*rs$sray22) + sum(sign(rs$Support)*rs$sray22) + sum(sign(rs$Critical)*rs$sray22))
  revenue <-c(sum(sign(rs$Core)*rs$sray22), sum(sign(rs$Specialist)*rs$sray22), sum(sign(rs$Support)*rs$sray22), sum(sign(rs$Critical)*rs$sray22), nmdd)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data6 <- data.frame(x, revenue, col,cat)
  
  #5
  x <- c(rep('8.Y5 E',5))
  col=c("darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64")
  nmdd =sum(rs$sray23)-(sum(sign(rs$Core)*rs$sray23) + sum(sign(rs$Specialist)*rs$sray23) + sum(sign(rs$Support)*rs$sray23) + sum(sign(rs$Critical)*rs$sray23))
  revenue <-c(sum(sign(rs$Core)*rs$sray23), sum(sign(rs$Specialist)*rs$sray23), sum(sign(rs$Support)*rs$sray23), sum(sign(rs$Critical)*rs$sray23), nmdd)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data7 <- data.frame(x, revenue, col,cat)
  
  data <- union(data0,data1)
  data <- union(data,data2)
  data <- union(data,data3)
  data <- union(data,data4)
  data <- union(data,data5)
  data <- union(data,data6)
  data <- union(data,data7)
  
  plot_ly(data, x = ~x, y = ~revenue, type = 'bar',name='Demand',
          color =~cat, hoverinfo = 'text', text = ~paste(cat, revenue)
          ,colors = c("transparent","darkorange3", "midnightblue", "gold2", "deepskyblue2", "gray64"))%>%
    layout(title = 'Headcount', margin = list(b = 100),
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           barmode = 'stack',
           showlegend = FALSE)
})
  
  output$plot35<- renderPlotly({
    
    x <- c('T0','T1', 'T2', 'T3', 'T4', 'T5')
    
    Opt <- c(sum(r_opt_s$total), sum(r_opt_s$sray19),sum(r_opt_s$sray20),sum(r_opt_s$sray21),sum(r_opt_s$sray22),sum(r_opt_s$sray23))
    
    crvt <- c(sum(r_crvt_s$total), sum(r_crvt_s$sray19),sum(r_crvt_s$sray20),sum(r_crvt_s$sray21),sum(r_crvt_s$sray22),sum(r_crvt_s$sray23))
    
    As_is <- c(sum(rs$total), sum(rs$sray19),sum(rs$sray20),sum(rs$sray21),sum(rs$sray22),sum(rs$sray23))
    
    data <- data.frame(x, Opt,  As_is,crvt)
    
    plot_ly(data, x = ~x, y = ~Opt, type = 'scatter',mode = "lines",name='Optimistic',marker = list(color = 'rgba(55, 128, 191, 0.7)'))%>%
      
      add_trace(y = ~As_is, name='Conservative',  marker = list(color = 'rgba(200, 0, 23, 0.7)',width = 2),
                line = list(color = 'rgba(200, 0, 23, 0.7)'))%>%
      add_trace(y = ~crvt, name='Base Case',  marker = list(color = 'orange',width = 2),
                                                       line = list(color = 'orange'
                                                                   )) %>%
      
      layout(title = 'Forecast',
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
  })
  
  #tab4
  output$plot42<- renderPlotly({
    base <- c('Type','Type')
    cat <- c('Contractor','Direct Hire')
    
    x <- c(sum(rs$Contractor,na.rm = TRUE),sum(rs$`Direct Hire`,na.rm = TRUE))
    text <- c(sum(rs$Contractor,na.rm = TRUE)/2,sum(rs$Contractor,na.rm = TRUE)+(sum(rs$`Direct Hire`,na.rm = TRUE))/2)
    
    data <- data.frame(x,cat, base, text)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h', color = ~cat,  marker = list(color = c('rgba(252, 157, 13, 0.7)','rgba(2, 69, 186, 1.0)')))%>%
      
      layout(title = 'Headcount',height = 140,
             xaxis = list(
               title = "",
               zeroline = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               showgrid = FALSE
             ),
             yaxis = list(
               title = "",
               zeroline = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               showgrid = FALSE
             ),
             barmode = 'stack',
             showlegend = TRUE) %>%
      add_annotations(text = x,
                      x = text,
                      y = base,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(245, 246, 249, 1)'),
                      showarrow = FALSE)
  })
  
  output$plot421<- renderPlotly({
    base <- c('Core', 'Critical', 'Support',  'Specialist')
    x <- c(sum(rs$Core),sum(rs$Critical0),sum(rs$Support),sum(rs$Specialist))
    text <- c('$320K','$430K', '$260K', '$-120K')
    data <- data.frame(x, base, text)
    
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h', marker = list(color = c('rgba(252, 157, 13, 0.7)','rgba(9, 75, 190, 1.0)','rgba(252, 157, 13, 0.7)','rgba(9, 75, 190, 1.0)')))%>%
            layout(title = 'Segmentation',
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot422<- renderPlotly({
    rs <- mutate(rs, age_i = case_when(rs$Age < 25 ~'<25', rs$Age >=25 & rs$Age < 35 ~'25-34', rs$Age >=35 & rs$Age < 45 ~'35-44', rs$Age >=45 & rs$Age < 55 ~'45-54', rs$Age >=55 & rs$Age < 65 ~'55-65', rs$Age >=65  ~'65<'))
    dat <- aggregate(rs$Age, by=list(age=rs$age_i), FUN=sum)
    
    #The default order will be alphabetized unless specified as below:
    plot_ly(dat, x = ~dat$age, y = ~x, type = 'bar', color = ~age)%>%
      layout(title = 'Distribution',
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot43<- renderPlotly({
    col=c(rep(c('rgba(282, 157, 13, 0.7)','rgba(2, 90, 216, 0.7)','rgba(2, 69, 186, 0.7)','rgba(232, 137, 13, 0.7)','rgba(112, 127, 13, 0.7)'),8))
    col2=c(rep(c('rgba(282, 157, 13, 1.0)','rgba(2, 90, 216, 1.0)','rgba(2, 69, 186, 1.0)','rgba(232, 137, 13, 1.0)','rgba(112, 127, 13, 1.0)'),8))
    
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),8))
    
    aoc()
    ss=sum(aoc()$Core)+sum(aoc()$Specialist)+sum(aoc()$Support)+sum(aoc()$Critical)
    nm=sum(aoc()$total)-ss
    
    nms =sum(aoc()$subt)-(sum(sign(aoc()$Core)*aoc()$subt) + sum(sign(aoc()$Specialist)*aoc()$subt) + sum(sign(aoc()$Support)*aoc()$subt) + sum(sign(aoc()$Critical)*aoc()$subt))
    
    nma =sum(aoc()$sray23)-(sum(sign(aoc()$Core)*aoc()$sray23) + sum(sign(aoc()$Specialist)*aoc()$sray23) + sum(sign(aoc()$Support)*aoc()$sray23) + sum(sign(aoc()$Critical)*aoc()$sray23))
    
    nmdd =sum(aoc()$dds23)-(sum(sign(aoc()$Core)*aoc()$dds23) + sum(sign(aoc()$Specialist)*aoc()$dds23) + sum(sign(aoc()$Support)*aoc()$dds23) + sum(sign(aoc()$Critical)*aoc()$dds23))
    
    
    x <- c(rep('1.Current<br>headcount',5),rep('2.Addition',5),rep('3.Substraction',5), rep('4.Y1',5),rep('5.Y2',5),rep('6.Y3',5),rep('7.Y4',5),rep('8.Y5 FINAL',5))
    #y <- c(sum(rs$total), sum(rs$total)+sum(rs$addt),sum(rs$total)+sum(rs$addt)-sum(rs$subt), sum(rs$total)+sum(rs$addt)-sum(rs$subt),sum(rs$total),sum(rs$fte5),7,8)
    base <-    c(rep(0,5),               sum(aoc()$total),rep(0,4),sum(aoc()$total)-sum(aoc()$subt),rep(0,4), rep(0,5), rep(0,5),     rep(0,5),rep(0,5),rep(0,5))
    revenue <- c(sum(aoc()$Core),sum(aoc()$Specialist),sum(aoc()$Support),sum(aoc()$Critical), nm, rep(sum(aoc()$addt),5), rep(0,5),     rep(0,5),  rep(0,5),  rep(0,5), rep(0,5),  rep(0,5))
    costs <-   c(rep(0,5),                 rep(0,5),       sum(sign(aoc()$Core)*aoc()$subt),sum(sign(aoc()$Specialist)*aoc()$subt),sum(sign(aoc()$Support)*aoc()$subt),sum(sign(aoc()$Critical)*aoc()$subt), nms,    rep(0,5),     rep(0,5),   rep(0,5), rep(0,5) ,rep(0,5))
    
    profit <-  c(rep(0,5),                 rep(0,5),                rep(0,5),    sum(aoc()$Core)-sum(sign(aoc()$Core)*aoc()$subt)  ,  sum(aoc()$Specialist)-sum(sign(aoc()$Specialist)*aoc()$subt)  ,  sum(aoc()$Support)-sum(sign(aoc()$Support)*aoc()$subt)  ,  sum(aoc()$Critical)-sum(sign(aoc()$Critical)*aoc()$subt)  ,        nm-nms, 
                 sum(sign(aoc()$Core)*aoc()$sray20), sum(sign(aoc()$Specialist)*aoc()$sray20), sum(sign(aoc()$Support)*aoc()$sray20), sum(sign(aoc()$Critical)*aoc()$sray20), nmdd, 
                 sum(sign(aoc()$Core)*aoc()$sray21), sum(sign(aoc()$Specialist)*aoc()$sray21), sum(sign(aoc()$Support)*aoc()$sray21), sum(sign(aoc()$Critical)*aoc()$sray21), nmdd, 
                 sum(sign(aoc()$Core)*aoc()$sray22), sum(sign(aoc()$Specialist)*aoc()$sray22), sum(sign(aoc()$Support)*aoc()$sray22), sum(sign(aoc()$Critical)*aoc()$sray22), nmdd, 
                 sum(sign(aoc()$Core)*aoc()$sray23), sum(sign(aoc()$Specialist)*aoc()$sray23), sum(sign(aoc()$Support)*aoc()$sray23), sum(sign(aoc()$Critical)*aoc()$sray23), nmdd)
    
    data <- data.frame(x, base, revenue, costs, profit,col,cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'))%>%
      add_trace(y = ~revenue, color =~cat, marker = list(color = ~col),hoverinfo = 'text', text = ~paste(cat, revenue)) %>%
      add_trace(y = ~costs,color =~cat,hoverinfo = 'text', text = ~paste(cat, costs), marker = list(color = ~col,
                                                                                                    line = list(color = col2,
                                                                                                                width = 2))) %>%
      add_trace(y = ~profit,color =~cat,hoverinfo = 'text', text = ~paste(cat, profit), marker = list(color = ~col,
                                                                                                      line = list(color = col2,
                                                                                                                  width = 2))) %>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = "", tickangle = 90),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot4432<- renderPlotly({
    aoc()
    x <- c('1.Current<br>headcount','2.Addition','3.Substraction', '4.Y0 Baseline','5.Automation','6.Training','7.Demand','8.Y5 FINAL')
    #y <- c(sum(rs$total), sum(rs$total)+sum(rs$addt),sum(rs$total)+sum(rs$addt)-sum(rs$subt), sum(rs$total)+sum(rs$addt)-sum(rs$subt),sum(rs$total),sum(rs$fte5),7,8)
    base <-    c(0,                sum(aoc()$total),sum(aoc()$total)-sum(aoc()$subt), 0, sum(aoc()$sray23), sum(aoc()$fte5), sum(aoc()$fte5),0)
    revenue <- c(sum(aoc()$total), sum(aoc()$addt), 0,                                0, 0,  0,sum(aoc()$dds23)-sum(aoc()$fte5),0)
    costs <-   c(0,                0,               sum(aoc()$subt),                  0,sum(aoc()$total)-sum(aoc()$subt)-sum(aoc()$sray23),sum(aoc()$sray23)-sum(aoc()$fte5),0,0)
    profit <-  c(0,                0,               0,                                sum(aoc()$total)-sum(aoc()$subt),0,0,0,sum(aoc()$dds23))
    data <- data.frame(x, base, revenue, costs, profit)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'))%>%
      add_trace(y = ~revenue, marker = list(color = 'rgba(252, 157, 13, 0.7)',
                                            line = list(color = 'rgba(252, 157, 13, 0.7)',
                                                        width = 2))) %>%
      add_trace(y = ~costs, marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                          line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                      width = 2))) %>%
      add_trace(y = ~profit, marker = list(color = 'rgba(2, 69, 186, 0.7)',
                                           line = list(color = 'rgba(2, 69, 186, 1.0)',
                                                       width = 2))) %>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = "", tickangle = 90),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot432<- renderPlotly({
    col=c(rep(c('rgba(282, 157, 13, 0.7)','rgba(2, 90, 216, 0.7)','rgba(2, 69, 186, 0.7)','rgba(232, 137, 13, 0.7)','rgba(112, 127, 13, 0.7)'),8))
    col2=c(rep(c('rgba(282, 157, 13, 1.0)','rgba(2, 90, 216, 1.0)','rgba(2, 69, 186, 1.0)','rgba(232, 137, 13, 1.0)','rgba(112, 127, 13, 1.0)'),8))
    
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),8))
    
    aoc()
    ss=sum(aoc()$Core)+sum(aoc()$Specialist)+sum(aoc()$Support)+sum(aoc()$Critical)
    nm=sum(aoc()$total)-ss
    
    nms =sum(aoc()$subt)-(sum(sign(aoc()$Core)*aoc()$subt) + sum(sign(aoc()$Specialist)*aoc()$subt) + sum(sign(aoc()$Support)*aoc()$subt) + sum(sign(aoc()$Critical)*aoc()$subt))
    
    nma =sum(aoc()$sray23)-(sum(sign(aoc()$Core)*aoc()$sray23) + sum(sign(aoc()$Specialist)*aoc()$sray23) + sum(sign(aoc()$Support)*aoc()$sray23) + sum(sign(aoc()$Critical)*aoc()$sray23))
    
    nmdd =sum(aoc()$dds23)-(sum(sign(aoc()$Core)*aoc()$dds23) + sum(sign(aoc()$Specialist)*aoc()$dds23) + sum(sign(aoc()$Support)*aoc()$dds23) + sum(sign(aoc()$Critical)*aoc()$dds23))
    
    
    x <- c(rep('1.Current<br>headcount',5),rep('2.Addition',5),rep('3.Substraction',5), rep('4.Y0 Baseline',5),rep('5.Automation',5),rep('6.Training',5),rep('7.Demand',5),rep('8.Y5 FINAL',5))
    #y <- c(sum(rs$total), sum(rs$total)+sum(rs$addt),sum(rs$total)+sum(rs$addt)-sum(rs$subt), sum(rs$total)+sum(rs$addt)-sum(rs$subt),sum(rs$total),sum(rs$fte5),7,8)
    base <-    c(rep(0,5),               sum(aoc()$total),rep(0,4),sum(aoc()$total)-sum(aoc()$subt),rep(0,4), rep(0,5), sum(aoc()$sray23),rep(0,4),     sum(aoc()$fte5),rep(0,4), sum(aoc()$fte5),rep(0,4),rep(0,5))
    revenue <- c(sum(aoc()$Core),sum(aoc()$Specialist),sum(aoc()$Support),sum(aoc()$Critical), nm, rep(sum(aoc()$addt),5), rep(0,5),     rep(0,5),  rep(0,5),  rep(0,5), sum(aoc()$dds23)-sum(aoc()$fte5)   ,rep(0,4),  rep(0,5))
    costs <-   c(rep(0,5),                 rep(0,5),       sum(sign(aoc()$Core)*aoc()$subt),sum(sign(aoc()$Specialist)*aoc()$subt),sum(sign(aoc()$Support)*aoc()$subt),sum(sign(aoc()$Critical)*aoc()$subt), nms,    rep(0,5),     rep(0,4), sum(aoc()$total)-sum(aoc()$subt)-sum(aoc()$sray23)   , sum(aoc()$sray23)-sum(aoc()$fte5),  rep(0,4), rep(0,5) ,rep(0,5))
    profit <-  c(rep(0,5),                 rep(0,5),                rep(0,5),    sum(aoc()$Core)-sum(sign(aoc()$Core)*aoc()$subt)  ,  sum(aoc()$Specialist)-sum(sign(aoc()$Specialist)*aoc()$subt)  ,  sum(aoc()$Support)-sum(sign(aoc()$Support)*aoc()$subt)  ,  sum(aoc()$Critical)-sum(sign(aoc()$Critical)*aoc()$subt)  ,        nm-nms, rep(0,5), rep(0,5),rep(0,5),sum(sign(aoc()$Core)*aoc()$dds23), sum(sign(aoc()$Specialist)*aoc()$dds23), sum(sign(aoc()$Support)*aoc()$dds23), sum(sign(aoc()$Critical)*aoc()$dds23), nmdd)
    data <- data.frame(x, base, revenue, costs, profit,col,cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'))%>%
      add_trace(y = ~revenue, color =~cat, marker = list(color = ~col),hoverinfo = 'text', text = ~paste(cat, revenue)) %>%
      add_trace(y = ~costs,color =~cat,hoverinfo = 'text', text = ~paste(cat, costs), marker = list(color = ~col,
                                          line = list(color = col2,
                                                      width = 2))) %>%
      add_trace(y = ~profit,color =~cat,hoverinfo = 'text', text = ~paste(cat, profit), marker = list(color = ~col,
                                           line = list(color = col2,
                                                       width = 2))) %>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = "", tickangle = 90),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })
  
  output$plot44<- renderPlotly({
    
    x <- c('T0','T1', 'T2', 'T3', 'T4', 'T5')
    
    Opt <- c(sum(r_opt_s$total), sum(r_opt_s$sray19),sum(r_opt_s$sray20),sum(r_opt_s$sray21),sum(r_opt_s$sray22),sum(r_opt_s$sray23))
    
    crvt <- c(sum(r_crvt_s$total), sum(r_crvt_s$sray19),sum(r_crvt_s$sray20),sum(r_crvt_s$sray21),sum(r_crvt_s$sray22),sum(r_crvt_s$sray23))
    
    As_is <- c(sum(rs$total), sum(rs$sray19),sum(rs$sray20),sum(rs$sray21),sum(rs$sray22),sum(rs$sray23))
    
    data <- data.frame(x, Opt,  As_is,crvt)
    
    plot_ly(data, x = ~x, y = ~Opt, type = 'scatter',mode = "lines",name='opt',marker = list(color = 'rgba(55, 128, 191, 0.7)'))%>%
      
      add_trace(y = ~As_is, name='asis',  marker = list(color = 'rgba(200, 0, 23, 0.7)',width = 2),
                
                line = list(color = 'rgba(200, 0, 23, 0.7)'))%>%
      
      add_trace(y = ~crvt, name='crvt',  marker = list(color = 'rgba(100, 200, 23, 0.7)',
                                                       
                                                       line = list(color = 'rgba(100, 200, 23, 0.7)',
                                                                   
                                                                   width = 2))) %>%
      
      layout(title = 'Forecast',
             
             xaxis = list(title = ""),
             
             yaxis = list(title = ""))
    
  })
  
  output$plot442<- renderPlotly({
    base <- rep(c('A', 'B', 'C',  'D','E','F','G', 'H','OS', 'UNG'),2)
    yform <- list(categoryorder = "array", title = "",
                  categoryarray = c('UNG','OS','H','G','F','E','D','C','B','A'))
    x1 <- c(sum(rs$A, na.rm = TRUE),sum(rs$B, na.rm = TRUE),sum(rs$C, na.rm = TRUE),sum(rs$D, na.rm = TRUE),sum(rs$E, na.rm = TRUE),sum(rs$F, na.rm = TRUE),sum(rs$G, na.rm = TRUE),sum(rs$H, na.rm = TRUE),sum(rs$OS, na.rm = TRUE),sum(rs$UNG, na.rm = TRUE))/2
    x2 <- -c(sum(rs$A, na.rm = TRUE),sum(rs$B, na.rm = TRUE),sum(rs$C, na.rm = TRUE),sum(rs$D, na.rm = TRUE),sum(rs$E, na.rm = TRUE),sum(rs$F, na.rm = TRUE),sum(rs$G, na.rm = TRUE),sum(rs$H, na.rm = TRUE),sum(rs$OS, na.rm = TRUE),sum(rs$UNG, na.rm = TRUE))/2
    x=c(x1,x2)
    text=x1*2
    data <- data.frame(x, base, text)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h',hoverinfo = 'text', text = ~paste(base, text))%>%
      add_trace()%>% config(displayModeBar = F)%>%
      layout(title = 'Segmentation',
             xaxis = list(
               title = "",
               zeroline = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               showgrid = FALSE
             ),
             yaxis = yform,
             barmode = 'overlay',
             showlegend = FALSE) %>%
      add_annotations(text = text,
                      x = 0,
                      y = base,
                      xref = "0",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(1, 1, 1, 1)'),
                      showarrow = FALSE)
    
  })
  
  #####
  
  #end
  
  #table
  #####
  #nhr
  #####
  
  output$prep_t<- renderDataTable({
    datatable(r_prep_s(),rownames = FALSE,
              options = list(
                #dom = 'Bfrti',
                autoWidth = FALSE,
                deferRender = TRUE,
                scrollX = TRUE,
                scrollY = "51vh",
                scroller = TRUE,
                scollCollapse = TRUE,
                fixedHeader = TRUE,
                columnDefs = list(
                  list(orderable = FALSE, className = 'details-control', targets = 0)
                )
              ) ,escape=F
    )
  })
  
  #####
  
  #end
  
  #ld
  #####
  
  output$tld<- renderDataTable({
    
    datatable(ld,rownames = FALSE,
              
              options = list(
                
                #dom = 'Bfrti',
                
                autoWidth = FALSE,
                
                deferRender = TRUE,
                
                scrollX = TRUE,
                
                scrollY = "51vh",
                
                scroller = TRUE,
                
                scollCollapse = TRUE,
                
                fixedHeader = TRUE,
                
                columnDefs = list(
                  
                  list(orderable = FALSE, className = 'details-control', targets = 0)
                  
                )
                
              ) ,escape=F
              
    )%>%
      
      formatCurrency(c('T=0','T=1','T=2','T=3','T=4','T=5'),currency = "$", interval = 3, mark = ",", digits = 0)
    
  })
  
  #####
  #end
  
  #vals preparation
  ######
  
  vals=reactiveValues()
  
  vals$Data=data.table(db)
  
  
  
  newEntry1 <- observe(vals$Data <- mutate(vals$Data,
                                           
                                           M_NHR= case_when(Region == "MEA" ~ input$MEA,  Region == "APAC" ~ input$APAC,
                                                            
                                                            Region == "AMR" ~ input$AMR, Region == "EUR" ~ input$EUR)
                                           
  )
  
  )
  
  newEntry <- observe({vals$Data <- data.table(db[db$Region %in%  if(is.null(input$rg)){unique(db$Region)} else (input$rg),])
  
  vals$Data <- data.table(db[db$Division %in%  if(is.null(input$div)){unique(db$Division)} else (input$div) ,])
  
  })
  
  ######
  #end vals
  
  #####
  
  output$KKosz=renderUI({
    
    box(width=12,
        
        column(12,dataTableOutput("Main_kosz")),
        
        tags$script(HTML('$(document).on("click", "input", function () {
                         
                         var checkboxes = document.getElementsByName("row_selected");
                         
                         var checkboxesChecked = [];
                         
                         for (var i=0; i<checkboxes.length; i++) {
                         
                         if (checkboxes[i].checked) {
                         
                         checkboxesChecked.push(checkboxes[i].value);
                         
                         }
                         
                         }
                         
                         Shiny.onInputChange("checked_rows",checkboxesChecked);})')),
        
        tags$script("$(document).on('click', '#Main_kosz button', function () {
                    
                    Shiny.onInputChange('lastClickkId',this.id);
                    
                    Shiny.onInputChange('lastClickk', Math.random()) });")
        
        
        
        )
    
    })
  
  
  
  output$Main_kosz=renderDataTable({ DT=vals$Data[]
  
  
  
  datatable(DT[],
            
            options = list(
              
              #dom = 'Bfrti',
              
              autoWidth = FALSE,
              
              deferRender = TRUE,
              
              scrollX = TRUE,
              
              scrollY = "51vh",
              
              scroller = TRUE,
              
              scollCollapse = TRUE,
              
              fixedHeader = TRUE,
              
              columnDefs = list(
                
                list(orderable = FALSE, className = 'details-control', targets = 0)
                
              )
              
            ) ,escape=F)
  
  
  
  })
  
  
  
  #####action showModal
  
  observeEvent(input$lastClickk,
               
               {
                 
                 showModal(modal_modify)
                 
               }
               
  )
  
  #####
  
  ##modalDialog
  #####
  
  modal_modify=modalDialog(
    
    fluidPage(
      
      h3(strong("Row modification"),align="center"),
      
      hr(),
      
      dataTableOutput('row_modif'),
      
      actionButton("save_changes","Save"),
      
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       
                       var list_value=[]
                       
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       
                       {
                       
                       list_value.push($( '.new_input' )[i].value)
                       
                       }
                       
                       Shiny.onInputChange('newValue', list_value)
                       
                       });"

      ))
      
      
      
      
      
      ),
    
    size="m"
    
    )
  
  #####
  
  
  
  ##co modyfikuje
  
  output$row_modif<-renderDataTable({
    
    selected_row=as.numeric(gsub("modify_","",input$lastClickkId))
    
    old_row=vals$Data[selected_row,c('n','Manual')]
    
    row_change=list()
    
    row_change[[1]]=vals$Data[selected_row,c('n')]
    
    row_change[[2]]=paste0('<input class="new_input" type="number" id=new_',2,'><br>')
    
    row_change=as.data.table(row_change)
    
    setnames(row_change,colnames(old_row))
    
    DT=rbind(old_row,row_change)
    
    rownames(DT)<-c("Current values","New values")
    
    DT
    
    
    
  },escape=F,options=list(dom='t',ordering=F),selection="none")
  
  
  
  ##data table update by modification
  
  ######
  
  observeEvent(input$newValue,
               
               {
                 
                 newValue=lapply(input$newValue, function(col) {
                   
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     
                     as.numeric(as.character(col))
                     
                   } else {
                     
                     col
                     
                   }
                   
                 })
                 
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 
                 #dataset()[as.numeric(gsub("modify_","",input$lastClickkId)),'Manual']<-DF
                 
                 
                 
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickkId)),'Manual']<-DF
                 
               }
               
  )
  
  ######
  
  #####
  #end tb
  
  }

# Run the application

shinyApp(ui = ui, server = server)