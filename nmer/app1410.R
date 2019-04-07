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
                
                box(p("This Platform is designed to enable detailed forecasting of workforce requirements throughout SABIC's
                      
                      workforce over a three year time period.",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    h2("Instructions",style = "font-family: 'Source Sans Pro';"),
                    
                    p("First, go to the Model Setup tab. Here you must generate data with the button at the top, before you
                      
                      are able to access the other tabs. Once data is generated, the executive view and deep dive view show both the
                      
                      resulting workforce forecasts, and the details behind them.",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Platform is divided into three sections:",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    h4("1. Model Setup",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Model Setup must be visited first to generate the data, and adjust underlying assumptions if desired.
                      
                      Driving the Platform is data on both the current workforce state and the key levers that influence future demand: driver changes, automation, training and outsourcing,
                      
                      produced from both SABIC's data and market research. These values are preloaded for every individual section based on
                      
                      a combination of research and SABIC's own data, however should you wish to adjust these underlying assumptions in bulk you may do so in this tab.",style = "font-family: 'Source Sans Pro';"),
                    
                    h4("2. Executive View",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Executive View provides a high level overview of SABIC and its largest functions.
                      
                      It displays the workforce forecasts, details the impact of the different workforce levers,
                      
                      and highlights the supply/demand gap.",style = "font-family: 'Source Sans Pro';"),
                    
                    h4("3. Deep Dive",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Deep Dive provides key demographic information at every level of the organisation, and
                      
                      allows for detailed section level planning.",style = "font-family: 'Source Sans Pro';")
                    
                    
                    
                    ,width=12)
                
                    ),
              
              img(src="mercer-logo.png", width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),
              
              p("Platform code and materials are copyrighted works owned exclusively by Mercer and may not be copied,
                
                modified, sold, transformed into any other media, or otherwise transferred in whole or in part
                
                to any party other than SABIC, without prior written consent from Mercer. All rights reserved.",
                
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
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Introduction</font></font>'),
                  status="primary",solidHeader = TRUE, colour="light blue",collapsible = TRUE,width=12,
                  p("This is the Model Initialisation page. Here you are able to adjust the underlying model parameters
                    for the three different scenarios. When you are finished, even if you did not make any
                    changes, click the 'Generate' button just below this box to calculate forecast headcounts and enable the other tabs.",
                    style = "font-family: 'Source Sans Pro';"),
                  p("All values are preloaded with the results of research and SABIC data. An average of these, weighted on
                    baseline headcount, is used to preload the sliders below.",
                    style = "font-family: 'Source Sans Pro';"),
                  p("Default Parameters can be adjusted using Default Parameter Setting tab.",
                    style = "font-family: 'Source Sans Pro';")
                )
              ),
              fluidRow( 
                #loading animation when system busy
                conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                 tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                            <span class="sr-only">Loading...</span>'),id="loadmessage")), #end
                align = "center"),
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Department Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed=TRUE,
                  h6('Here you can adjust parameters for a specific area of SABIC. These will overwrite the underlying  values, and give new value for selected function. '),
                    column(4,h6("1 SABIC All"),actionButton("sall1","SABIC All",width = '100%')),
                    column(4,h6("2 Function/BU Type"), uiOutput('uityp')),column(4,h6("3 Region"),uiOutput('uirg')),column(4, h6('4 Function/Affiliate Name'), uiOutput('uiname')),
                    column(4, h6("5 Org. Structure"),uiOutput('uidiv')),column(4,h6("6 Job Family"),uiOutput('uijob')),column(4,h6("7 Job SubFamily"),uiOutput('uisjob'))
                    #https://stackoverflow.com/questions/51355878/how-to-text-wrap-choices-from-a-pickerinput-if-the-length-of-the-choices-are-lo
                )
              ),#filters
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Right-sizing Scenario Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  #tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                  tags$div(title="As-Is case: When no rightsizing is applied to the current headcount.
                           \n \n 
                           Optimistic Case: Maximum Optimization possible in the current Headcount
                           \n \n 
                           Conservative Case: Current headcount is rightsized only to of 50% of the value of the Optimistic case",
                  radioButtons("radio_t2rsss", label = NULL,
                               choiceNames = list("Conservative","Base Case", "Optimistic"
                               ),
                               choiceValues = list(
                                 "rs","r_crvt_s", "r_opt_s"
                               ),selected = "rs", inline = TRUE)
                  )
                  ,width = 12,
                  box(icon = shiny::icon("line-chart"),
                    title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                    ,status="warning",  solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE, color="orange",
                    # infoBox(title = sum(rs$as_is_s),subtitle = "No change Headcount",  fill = T, color = "red",icon = shiny::icon("balance-scale",lib = "font-awesome")), 
                    # infoBox(title = round(sum(rs$crvt)),subtitle = "Base Headcount",  fill = T, color = "yellow"), 
                    # infoBox(title = round(sum(rs$opt)),subtitle = "Optimistic Headcount",  fill = T, color = "green",icon = shiny::icon("line-chart")), 
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="2"> Conservative Headcount (No Change)</font></font>')
                      , status = "primary", solidHeader = TRUE,width = '100%',
                      h5(paste(sum(rs$as_is_s)))
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="2">Base Headcount</font></font>')
                      , status = "primary", solidHeader = TRUE,width = '100%',
                      h5(round(sum(rs$crvt),0))
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="2">Optimistic Headcount</font></font>')
                      , status = "primary", solidHeader = TRUE,width = '100%',
                      h5(paste(round(sum(rs$opt),0)))
                    ))
                    ),h6('The values of these scarios could be changes suing the Rightsizing Position given under Parameter Setting  below')
                )
              }),#Right-sizing Scenario Selection radio
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection (Automation & L&D Intervention)</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$div(title="Conservative Case: When Demand is calculated without taking into account Automation and Capability enhancement
                           \n \n 
                           Base Case: When Demand is calculated taking into account Automation and Capability enhancement
                           \n \n 
                           Optimistic Case: When Demand is calculated taking into account
                           maximum Automation potential and Capability enhancement",
                  radioButtons("radio_t2ss", label = NULL,
                               choiceNames = list("Conservative Case", "Base Case","Optimistic"
                               ),
                               choiceValues = list(
                                 "con", "bc","opt"
                               ),selected = "con", inline = TRUE)
                ),width=12,
                box(
                  title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                  , status = "warning", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                  fluidRow( 
                  h4('Yearly L&D Investment'),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      sum(0)
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      round(sum(ld[c(-1,-2)])/5)
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      round(sum(ld[c(-1,-2)])/5)
                    ))),h6('The values of these scarios could be changes suing the Rightsizing Position given under Parameter Setting  below'),
                  fluidRow( h4('Automation Impact (5 Years)'),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      0
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      paste(round(sum(rs$ldt5*rs$total)/sum(rs$total)*100,3),'%')
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      paste(round(sum(rs$ldt5*rs$total)/sum(rs$total)*200,3),'%')
                    ))
                  ),h6('The values of these scarios could be changes suing the Rightsizing Position given under Parameter Setting  below')
                )
                )
              }),#Scenario Selection radio
              fluidRow({
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
                           }")
                }),#style tab col
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Default Parameters Settings</font></font>')
                  , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1",  width = '100%',
                    tabPanel("Supply",
                               fluidRow({
                                 box(title=HTML( '<font color=#ffffff> <font size="4">Supply Rate Adjustments:</font></font>'), solidHeader = TRUE, status="primary",colour="light blue",
                                     width=12,
                                       actionButton("reset_shrk","Reset"),actionButton("save_shrk","Save"),
                                     br(),
                                 column(4,
                                        knobInput(
                                          inputId = "t2sk11",label = "Year 1 NHR:",thickness=0.3, width=100, height=100,
                                          value = round(sum(nhr[2,2:5])*25,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                          fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")),
                                 column(4,
                                        knobInput(
                                          inputId = "t2sk21",label = "Year 2 NHR:",thickness=0.3, width=100, height=100,
                                          value = round(sum(nhr[3,2:5])*25,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                          fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")),
                                 column(4,
                                        knobInput(
                                          inputId = "t2sk31",label = "Year 3 NHR:",thickness=0.3, width=100, height=100,
                                          value = round(sum(nhr[4,2:5])*25,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                          fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")),
                                 column(4,
                                        knobInput(
                                          inputId = "t2sk41",label = "Year 4 NHR:",thickness=0.3, width=100, height=100,
                                          value = round(sum(nhr[5,2:5])*25,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                          fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")),
                                 column(4,
                                        knobInput(
                                          inputId = "t2sk51",label = "Year 5 NHR:",thickness=0.3, width=100, height=100,
                                          value = round(sum(nhr[6,2:5])*25,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
                                          fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold"))
                               )
                                 })
                             ),
                    tabPanel("Demand",
                             fluidRow({
                               box(status = "primary", width=12,
                                   actionButton("reset_sdd","Reset")
                                   ,actionButton("save_ld","Save"),
                                   fluidRow(
                                     uiOutput("KKosz")
                                     )
                               )
                               })
                             ),
                    tabPanel("Rightsizing Position",
                             fluidRow({fluidRow(
                               box(
                                 title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                                 , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                                 column(4,box(
                                   title =HTML( '<font color=#ffffff> <font size="2"> Conservative Headcount (No Change)</font></font>')
                                   , status = "primary", solidHeader = TRUE,width = '100%',
                                   h5(paste(sum(rs$as_is_s)))
                                 )),
                                 column(4,box(
                                   title =HTML( '<font color=#ffffff> <font size="2">Base Headcount</font></font>')
                                   , status = "primary", solidHeader = TRUE,width = '100%',
                                   h5(round(sum(rs$crvt),0))
                                 )),
                                 column(4,box(
                                   title =HTML( '<font color=#ffffff> <font size="2">Optimistic Headcount</font></font>')
                                   , status = "primary", solidHeader = TRUE,width = '100%',
                                   h5(paste(round(sum(rs$opt),0)))
                                 ))
                               ),
                               box(status = "primary", width=12,
                                   actionButton("reset_t2sbl","Reset"),
                                   actionButton("save_t2sbl","Save"),
                                   sliderInput("inputst2bl", "Headcount:",
                                               min = 0, max = 100,
                                               value = nhr$MEA[1]*100,pre = "%", step = 0.01, ticks = FALSE)
                               )
                             )
                             })
                             ),
                    tabPanel("L&D Investment",
                             fluidRow({
                               box(status = "primary", width=12,
                                   actionButton("reset_sld","Reset")
                                   ,actionButton("save_sld","Save"),br(),
                                   # fluidRow(
                                   #   dataTableOutput(outputId = "tld")
                                   # ),
                                   fluidRow(
                                     column(6,sliderInput("t2sld11","Year 1 Automation Level:",
                                                          min=0,max=10000,value= ld$`T=1`,post="$")),
                                     column(6,sliderInput("t2sld12","Year 2 Automation Level:",
                                                          min=0,max=10000,value= ld$`T=2`,post="$")),
                                     column(6,sliderInput("t2sld13","Year 3 Automation Level:",
                                                          min=0,max=10000,value= ld$`T=3`,post="$")),
                                     column(6,sliderInput("t2sld14","Year 4 Automation Level:",
                                                          min=0,max=10000,value= ld$`T=4`,post="$")),
                                     column(6,sliderInput("t2sld15","Year 5 Automation Level:",
                                                          min=0,max=10000,value= ld$`T=5`,post="$"))
                                   )
                                   
                               )
                               
                             })
                             ),
                    tabPanel("Automation",
                             fluidRow({
                               box(status = "primary", width=12,
                                   actionButton("reset_sau","Reset")
                                   ,actionButton("save_sau","Save"),
                                   br(),br(),
                                   box(status = "primary", width=12,
                                     fluidRow(
                                     column(6,sliderInput("t2sau1","Year 1 Automation Level:",
                                                          min=0,max=100,value= 0,step = 0.01,post=" %")),
                                     column(6,sliderInput("t2sau2","Year 2 Automation Level:",
                                                          min=0,max=100,value= 0,step = 0.01,post=" %"))
                                     ),
                                   fluidRow(column(6,radioButtons("t2sau1q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),      
                                            column(6,radioButtons("t2sau2q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
                                              )),
                                   box(status = "primary", width=12,
                                     fluidRow(
                                     column(6,sliderInput("t2sau3","Year 3 Automation Level:",
                                                          min=0,max=100,value= 0,step = 0.01,post=" %")),
                                     column(6,sliderInput("t2sau4","Year 4 Automation Level:",
                                                          min=0,max=100,value= 0,step = 0.01,post=" %"))
                                   ),
                                   fluidRow(column(6,radioButtons("t2sau3q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),      
                                            column(6,radioButtons("t2sau4q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
                                   )),
                                   box(status = "primary", width=12,
                                     fluidRow(
                                     column(6,sliderInput("t2sau5","Year 5 Automation Level:",
                                                          min=0,max=100,value= 0,step = 0.01,post=" %"))
                                   ),
                                   fluidRow(column(6,radioButtons("t2sau5q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
                                   ))
                                   )
                               })
                             )
                  )
                )
              )
              ),
      ######
      #end tab2
      #tab3
      ######
      tabItem(tabName = "tab3",
              
              fluidRow( 
                #loading animation when system busy
                conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                 tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                            <span class="sr-only">Loading...</span>'),id="loadmessage")), #end
                align = "center"),
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Department Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed=F,
                  column(4,h6("1 SABIC All"),actionButton("sall2","SABIC All",width = '100%')),
                  column(4,h6("2 Function/BU Type"), uiOutput('uityp3')),column(4,h6("3 Region"),uiOutput('uirg3')),column(4, h6('4 Function/Affiliate Name'), uiOutput('uiname3')),
                  column(4, h6("5 Org. Structure"),uiOutput('uidiv3')),column(4,h6("6 Job Family"),uiOutput('uijob3')),
                  br(),
                  column(4,h6("Employment"),uiOutput('uiemp3')),column(4,h6("Segment"),uiOutput('uiseg3'))
                  #https://stackoverflow.com/questions/51355878/how-to-text-wrap-choices-from-a-pickerinput-if-the-length-of-the-choices-are-lo
                )
              ),#filters
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Right-sizing Scenario Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  #tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                  tags$div(title="As-Is case: When no rightsizing is applied to the current headcount.
                           \n \n 
                           Optimistic Case: Maximum Optimization possible in the current Headcount
                           \n \n 
                           Conservative Case: Current headcount is rightsized only to of 50% of the value of the Optimistic case",
                           radioButtons("radio_t3rsss", label = NULL,
                                        choiceNames = list("Conservative","Base Case", "Optimistic"
                                        ),
                                        choiceValues = list(
                                          "rs","r_crvt_s", "r_opt_s"
                                        ),selected = "rs", inline = TRUE)
                  )
                  ,width=12,
                  box(
                    title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                    ,status="warning",  solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE, color="orange",
                    infoBox(title = sum(rs$as_is_s),subtitle = "No change Headcount",  fill = T, color = "red"), 
                    infoBox(title = round(sum(rs$crvt)),subtitle = "Base Headcount",  fill = T, color = "yellow"), 
                    infoBox(title = round(sum(rs$opt)),subtitle = "Optimistic Headcount",  fill = T, color = "green",icon = shiny::icon("line-chart")), 
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="2">No change Headcount</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      h5(paste(sum(rs$as_is_s),0))
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="2">Base Headcount</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      h5(round(sum(rs$crvt),0))
                    )),
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="2">Optimistic Headcount</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      h5(paste(round(sum(rs$opt),0)))
                    ))
                  )
              )
              }),#Right-sizing Scenario Selection radio
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection (Automation & L&D Intervention)</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$div(title="Conservative Case: When Demand is calculated without taking into account Automation and Capability enhancement
                           \n \n 
                           Base Case: When Demand is calculated taking into account Automation and Capability enhancement
                           \n \n 
                           Optimistic Case: When Demand is calculated taking into account
                           maximum Automation potential and Capability enhancement",
                           radioButtons("radio_t3ss", label = NULL,
                                        choiceNames = list("Conservative Case", "Base Case","Optimistic"
                                        ),
                                        choiceValues = list(
                                          "con", "bc","opt"
                                        ),selected = "con", inline = TRUE)
                  ),width=12,
                  box(
                    title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                    , status = "warning", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                    fluidRow( 
                      h4('Yearly L&D Investment'),
                      column(4,box(
                        title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                        , status = "primary", solidHeader = TRUE,width = '100%',
                        sum(0)
                      )),
                      column(4,box(
                        title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
                        , status = "primary", solidHeader = TRUE,width = '100%',
                        round(sum(ld[c(-1,-2)])/5)
                      )),
                      column(4,box(
                        title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
                        , status = "primary", solidHeader = TRUE,width = '100%',
                        round(sum(ld[c(-1,-2)])/5)
                      ))),
                    fluidRow( h4('Automation Impact (5 Years)'),
                              column(4,box(
                                title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                                , status = "primary", solidHeader = TRUE,width = '100%',
                                0
                              )),
                              column(4,box(
                                title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
                                , status = "primary", solidHeader = TRUE,width = '100%',
                                paste(round(sum(rs$ldt5*rs$total)/sum(rs$total)*100,3),'%')
                              )),
                              column(4,box(
                                title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
                                , status = "primary", solidHeader = TRUE,width = '100%',
                                paste(round(sum(rs$ldt5*rs$total)/sum(rs$total)*200,3),'%')
                              ))
                    )
                  )
              )
              }),#Scenario Selection radio
              
              
              fluidRow(
                box(title =HTML( '<font color=#ffffff> <font size="5">Demand Analysis : Consolidate View (5 Years)</font></font>')
                    , status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12,
                    
                      plotlyOutput("plot31")
                )
              ),
              fluidRow(
                box(title =HTML( '<font color=#ffffff> <font size="5">Demand Analysis</font></font>')
                    , status = "primary", solidHeader = TRUE,collapsible = TRUE, width=12,
                    radioButtons("radio_t3a", label = NULL,
                                 choiceNames = list("Year on Year Demand View (Selected Scenario)","Gap Analysis"
                                 ),
                                 choiceValues = list(
                                   'plot1',2
                                 ),selected = "plot1", inline = TRUE),
                    conditionalPanel(
                      condition = "input.radio_t3a == 'plot1'",
                      column(12,plotlyOutput("plot32"))
                    ),
                    conditionalPanel(
                      condition = "input.radio_t3a == 2",
                      column(12,plotlyOutput("plot34"))
                      
                    )
                    
                )
              ),
              fluidRow(
                box(title =HTML( '<font color=#ffffff> <font size="5">Scenario Comarison</font></font>')
                    , status = "primary", solidHeader = TRUE,collapsible = TRUE, width=12,
                    radioButtons("radio_t3b", label = NULL,
                                 choiceNames = list(HTML("Yearly Demand: Rightsizing<br>Scenario Comparison (Headcount)"),HTML("Yearly Demand: Scenario Comparison<br>(Automation & L&D Intervention)")
                                 ),
                                 choiceValues = list(
                                   'plot1',2
                                 ),selected = "plot1", inline = TRUE),
                    conditionalPanel(
                      condition = "input.radio_t3b == 'plot1'",
                      column(12,plotlyOutput("plot33"))
                    ),
                    conditionalPanel(
                      condition = "input.radio_t3b == 2",
                      column(12,plotlyOutput("plot35"))
                      
                    )
                    
                )
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
                                radioButtons("radio_t4rs", label = NULL,
                                             choiceNames = list("As-is", "Optimistic","Conservative"
                                             ),
                                             choiceValues = list(
                                               "rs", "r_opt_s","r_crvt_s"
                                             ),selected = "rs", inline = TRUE),width=12
                              )
                            ),
                            fluidRow(
                              box(
                                title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection</font></font>')
                                , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                radioButtons("radio_t4ss", label = NULL,
                                             choiceNames = list("Conservative", "Base Case","Optimistic"
                                             ),
                                             choiceValues = list(
                                               "con", "bc","opt"
                                             ),selected = "con", inline = TRUE),width=12
                              )
                            ),
                            box( status = "primary",collapsible = TRUE,
                                 actionButton("reset_all","SABIC All"),br(),
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
                                 ,width=12),
                            fluidRow(
                              column(3,pickerInput("p4emp",label=HTML('<font color=#454545>Employment</font>'),choices=c('Contractor','Direct Hire','NA'),
                                                   selected=c('Contractor','Direct Hire','NA'),multiple = TRUE)),
                              column(3,pickerInput("p4seg",label=HTML('<font color=#454545>Segment</font>'),choices=c('Core','Specialist','Support','Critical','Not Map'),
                                                   selected=c('Core','Specialist','Support','Critical','Not Map'),multiple = TRUE))
                            )
                          ),
                          tabPanel("Section Summary", icon = icon("line-chart"),
                                   box( title =HTML('<font color=#ffffff> <font size="5">Headcount Distribution</font></font>')
                                   , status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12, 
                                        plotlyOutput("plot42",height = '140px')
                                   ),
                                   box( title =HTML('<font color=#ffffff> <font size="5">Headcount Segmrntation</font></font>')
                          , status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 12,
                                        column(6,
                                        plotlyOutput("plot421")),
                                        column(6,
                                               plotlyOutput("plot423"))
                                   ),
                                   box( title =HTML('<font color=#ffffff> <font size="5">Age Distribution</font></font>')
              , status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 12,
                                        column(6,
                                               plotlyOutput("plot424")),
                                        column(6,
                                               plotlyOutput("plot422"))
                                   )
                                   ,
                                   box( title =HTML('<font color=#ffffff> <font size="5">Gender Distribution</font></font>')
      , status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 12,
                                        column(6,
                                               plotlyOutput("plot425")),
                                        column(6,
                                               plotlyOutput("plot426"))
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
                                        plotlyOutput("plot44")
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
              fluidRow(
                dataTableOutput(outputId = "soa")
              )
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
  observeEvent(input$sall1,{
    updatePickerInput(session, "uipType", selected = unique(db$`CF/SBU/AFF`[order(db$`CF/SBU/AFF`)]))
    updatePickerInput(session, "uiprg", selected = unique(db$Region[order(db$Region)]))
    updatePickerInput(session, "uipname", selected = unique(db$`CF/SBU/AFF Name`[order(db$`CF/SBU/AFF Name`)]))
    updatePickerInput(session, "uipdiv", selected = unique(db$Division[order(db$Division)]))
    updatePickerInput(session, "uipjob", selected = unique(db$Family[order(db$Family)]))
    updatePickerInput(session, "uipsjob", selected = unique(db$`Sub Family`[order(db$`Sub Family`)]))
  })
  observeEvent(input$sall2,{
    updatePickerInput(session, "uipType3", selected = unique(db$`CF/SBU/AFF`[order(db$`CF/SBU/AFF`)]))
    updatePickerInput(session, "uiprg3", selected = unique(db$Region[order(db$Region)]))
    updatePickerInput(session, "uipname3", selected = unique(db$`CF/SBU/AFF Name`[order(db$`CF/SBU/AFF Name`)]))
    updatePickerInput(session, "uipdiv3", selected = unique(db$Division[order(db$Division)]))
    updatePickerInput(session, "uipjob3", selected = unique(db$Family[order(db$Family)]))
    updatePickerInput(session, "uipsjob3", selected = unique(db$`Sub Family`[order(db$`Sub Family`)]))
  })
  
  observeEvent(input$reset_sld,{
    updateTextInput(session, "t2sld1", value = ld$`T=1`) 
    updateTextInput(session, "t2sld2", value = ld$`T=2`)
    updateTextInput(session, "t2sld3", value = ld$`T=3`)
    updateTextInput(session, "t2sld4", value = ld$`T=4`)
    updateTextInput(session, "t2sld5", value = ld$`T=5`)
    
    updateSliderInput(session, "t2sld11", value = ld$`T=1`) 
    updateSliderInput(session, "t2sld12", value = ld$`T=2`)
    updateSliderInput(session, "t2sld13", value = ld$`T=3`)
    updateSliderInput(session, "t2sld14", value = ld$`T=4`)
    updateSliderInput(session, "t2sld15", value = ld$`T=5`)
    
  })
  
  observeEvent(input$reset_ld,{
    updateTextInput(session, "t2ld1", value = ld$`T=1`)
    updateTextInput(session, "t2ld2", value = ld$`T=2`)
    updateTextInput(session, "t2ld3", value = ld$`T=3`)
    updateTextInput(session, "t2ld4", value = ld$`T=4`)
    updateTextInput(session, "t2ld5", value = ld$`T=5`)
    
  })
  observeEvent(input$reset_shrk,{
    updateTextInput(session, "t2sk11", value = round(sum(nhr[2,2:5])*25,1))
    updateTextInput(session, "t2sk21", value = round(sum(nhr[3,2:5])*25,1))
    updateTextInput(session, "t2sk31", value = round(sum(nhr[4,2:5])*25,1))
    updateTextInput(session, "t2sk41", value = round(sum(nhr[5,2:5])*25,1))
    updateTextInput(session, "t2sk51", value = round(sum(nhr[6,2:5])*25,1))
    
  })
  observeEvent(input$t2sau1,{
    if(input$t2sau1>input$t2sau2){updateSliderInput(session, "t2sau2", value = input$t2sau1)}
  })
  observeEvent(input$t2sau2,{
    if(input$t2sau2>input$t2sau3){updateSliderInput(session, "t2sau3", value = input$t2sau2)}
  })
  observeEvent(input$t2sau3,{
    if(input$t2sau3>input$t2sau4){updateSliderInput(session, "t2sau4", value = input$t2sau3)}
  })
  observeEvent(input$t2sau4,{
    if(input$t2sau4>input$t2sau5){updateSliderInput(session, "t2sau5", value = input$t2sau4)}
  })
  

  #####
  
  #data prep
  #####
  r_prep<-reactive({
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
    if(input$radio_t3rsss=="rs"){
      z=rs
    }
    if(input$radio_t3rsss=="r_opt_s"){
      z=r_opt_s
    }
    else{
      z=r_crvt_s
    }
    z <- z[z$Division %in% input$uipdiv3 ,]
    z <- z[z$Region %in% input$uiprg3 ,]
  })
  #####
  
  #filter
  #####
  {
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
  }#old sample
  {
    bu<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
    })
    bu2<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
    })
    bu3<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname ,]
    })
    bu4<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname ,]
      z <- z[z$Division %in%  input$uipdiv ,]
    })
    bu5<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname ,]
      z <- z[z$Division %in%  input$uipdiv ,]
      z <- z[z$Family %in%  input$uipjob ,]
    })
    outrg <- reactive({
      vars <- unique(bu()$Region[order(bu()$Region)])
      return(vars)
    })
    outnm <- reactive({
      vars2 <- unique(bu2()$`CF/SBU/AFF Name`[order(bu2()$`CF/SBU/AFF Name`)])
      return(vars2) 
    })
    outdiv <- reactive({
      vars3 <- unique(bu3()$Division[order(bu3()$Division)])
      return(vars3) 
    })
    outjob <- reactive({
      vars4 <- unique(bu4()$Family[order(bu4()$Family)])
      return(vars4) 
    })
    outsjob <- reactive({
      vars5 <- unique(bu5()$`Sub Family`[order(bu5()$`Sub Family`)])
      return(vars5) 
    })
    
    output$uityp = renderUI({
      pickerInput("uipType",label=NULL,choices=unique(db$`CF/SBU/AFF`[order(db$`CF/SBU/AFF`)]),
                  selected=unique(db$`CF/SBU/AFF`),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    output$uirg = renderUI({
      pickerInput("uiprg",label=NULL,choices=outrg(),
                  selected=outrg(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uiname = renderUI({
      pickerInput("uipname",label=NULL,choices=outnm(),
                selected= outnm(),
                options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
      })
    output$uidiv = renderUI({
      pickerInput("uipdiv",label=NULL,choices=outdiv(),
                  selected= outdiv(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    output$uijob = renderUI({
      pickerInput("uipjob",label=NULL,choices=outjob(),
                  selected= outjob(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    output$uisjob = renderUI({
      pickerInput("uipsjob",label=NULL,choices=outsjob(),
                  selected= outsjob(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    
  }#new
  {
    bu13<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
    })
    bu23<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
    })
    bu33<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname3 ,]
    })
    bu43<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname3 ,]
      z <- z[z$Division %in%  input$uipdiv3 ,]
    })
    bu53<-reactive({
      z <- db[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname3 ,]
      z <- z[z$Division %in%  input$uipdiv3 ,]
      z <- z[z$Family %in%  input$uipjob3 ,]
    })
    outrg3 <- reactive({
      vars13 <- unique(bu13()$Region[order(bu13()$Region)])
      return(vars13)
    })
    outnm3 <- reactive({
      vars23 <- unique(bu23()$`CF/SBU/AFF Name`[order(bu23()$`CF/SBU/AFF Name`)])
      return(vars23) 
    })
    outdiv3 <- reactive({
      vars33 <- unique(bu33()$Division[order(bu33()$Division)])
      return(vars33) 
    })
    outjob3 <- reactive({
      vars43 <- unique(bu43()$Family[order(bu43()$Family)])
      return(vars43) 
    })
    outsjob3 <- reactive({
      vars53 <- unique(bu53()$`Sub Family`[order(bu53()$`Sub Family`)])
      return(vars53) 
    })
    
    output$uityp3 = renderUI({
      pickerInput("uipType3",label=NULL,choices=unique(db$`CF/SBU/AFF`[order(db$`CF/SBU/AFF`)]),
                  selected=unique(db$`CF/SBU/AFF`),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    output$uirg3 = renderUI({
      pickerInput("uiprg3",label=NULL,choices=outrg3(),
                  selected=outrg3(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uiname3 = renderUI({
      pickerInput("uipname3",label=NULL,choices=outnm3(),
                  selected= outnm3(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    output$uidiv3 = renderUI({
      pickerInput("uipdiv3",label=NULL,choices=outdiv3(),
                  selected= outdiv3(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    output$uijob3 = renderUI({
      pickerInput("uipjob3",label=NULL,choices=outjob3(),
                  selected= outjob3(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    
    output$uiseg3 = renderUI({
      pickerInput("uipseg3",label=NULL,choices=c('Core', 'Specialist','Support','Critical','Not Map'),
                  selected=c('Core', 'Specialist','Support','Critical','Not Map'),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uiemp3 = renderUI({
      pickerInput("uipemp3",label=NULL,choices=c('Contract', 'Direct Hire','NA'),
                  selected=c('Contract', 'Direct Hire','NA'),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
  }#newt3
  
  #####
  
  #plot
  #####
  
  output$plot31<- renderPlotly({
  x <- c(rep('1.Current<br>headcount',5))
  col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  nm=sum(aoc()$not_map)
  revenue <- c(sum(aoc()$Core),sum(aoc()$Specialist),sum(aoc()$Support),sum(aoc()$Critical), nm)
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data0 <- data.frame(x, revenue, col,cat)
  
  
  ##opt
  x <- c(rep('2.Upsizing',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  revenue <-c(sum(aoc()$total), sum(aoc()$Core*aoc()$addt/aoc()$total),sum(aoc()$Specialist*aoc()$addt/aoc()$total),sum(aoc()$Support*aoc()$addt/aoc()$total),sum(aoc()$Critical*aoc()$addt/aoc()$total), sum(aoc()$not_map*aoc()$addt/aoc()$total))
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data1 <- data.frame(x, revenue, col,cat)
  
  ##o21
  x <- c(rep('3.Optimization',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  revenue <--c(-sum(aoc()$total)-sum(aoc()$addt)-sum(aoc()$subt), sum(aoc()$Core*aoc()$subt/aoc()$total),sum(aoc()$Specialist*aoc()$subt/aoc()$total),sum(aoc()$Support*aoc()$subt/aoc()$total),sum(aoc()$Critical*aoc()$subt/aoc()$total), sum(aoc()$not_map*aoc()$subt/aoc()$total))
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data2 <- data.frame(x, revenue, col,cat)
  
  
  ##1
  x <- c(rep('4.Y1 E',5))
  col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  revenue <-c(sum(aoc()$Core)+sum(aoc()$Core*aoc()$subt/aoc()$total)+sum(aoc()$Core*aoc()$addt/aoc()$total),
              sum(aoc()$Specialist)+sum(aoc()$Specialist*aoc()$subt/aoc()$total)+sum(aoc()$Specialist*aoc()$addt/aoc()$total),
              sum(aoc()$Support)+sum(aoc()$Support*aoc()$subt/aoc()$total)+sum(aoc()$Support*aoc()$addt/aoc()$total),
              sum(aoc()$Critical)+sum(aoc()$Critical*aoc()$subt/aoc()$total)+sum(aoc()$Critical*aoc()$addt/aoc()$total),
              sum(aoc()$not_map)+sum(aoc()$not_map*aoc()$subt/aoc()$total)+sum(aoc()$not_map*aoc()$addt/aoc()$total))
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data3 <- data.frame(x, revenue, col,cat)
  
  
  #4  sum(aoc()$Specialist*aoc()$dds19/aoc()$total)
  x <- c(rep('5.Business (R&M)(+)',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  
  prev <- c(sum(aoc()$Core)+sum(aoc()$Core*aoc()$subt/aoc()$total)+sum(aoc()$Core*aoc()$addt/aoc()$total),
            sum(aoc()$Specialist)+sum(aoc()$Specialist*aoc()$subt/aoc()$total)+sum(aoc()$Specialist*aoc()$addt/aoc()$total),
            sum(aoc()$Support)+sum(aoc()$Support*aoc()$subt/aoc()$total)+sum(aoc()$Support*aoc()$addt/aoc()$total),
            sum(aoc()$Critical)+sum(aoc()$Critical*aoc()$subt/aoc()$total)+sum(aoc()$Critical*aoc()$addt/aoc()$total),
            sum(aoc()$not_map)+sum(aoc()$not_map*aoc()$subt/aoc()$total)+sum(aoc()$not_map*aoc()$addt/aoc()$total))
  
  dds2023 <- c(sum(aoc()$Core*aoc()$dds23/aoc()$total),sum(aoc()$Specialist*aoc()$dds23/aoc()$total),sum(aoc()$Support*aoc()$dds23/aoc()$total),sum(aoc()$Critical*aoc()$dds23/aoc()$total), sum(aoc()$not_map*aoc()$dds23/aoc()$total))
  
  revenue <-c(sum(prev),ifelse((dds2023-prev)<0, 0,(dds2023-prev)))
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data4 <- data.frame(x, revenue, col,cat)
  
  x <- c(rep('5.Business (R&M)(-)',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  
  prev <- c(sum(aoc()$Core)+sum(aoc()$Core*aoc()$subt/aoc()$total)+sum(aoc()$Core*aoc()$addt/aoc()$total),
            sum(aoc()$Specialist)+sum(aoc()$Specialist*aoc()$subt/aoc()$total)+sum(aoc()$Specialist*aoc()$addt/aoc()$total),
            sum(aoc()$Support)+sum(aoc()$Support*aoc()$subt/aoc()$total)+sum(aoc()$Support*aoc()$addt/aoc()$total),
            sum(aoc()$Critical)+sum(aoc()$Critical*aoc()$subt/aoc()$total)+sum(aoc()$Critical*aoc()$addt/aoc()$total),
            sum(aoc()$not_map)+sum(aoc()$not_map*aoc()$subt/aoc()$total)+sum(aoc()$not_map*aoc()$addt/aoc()$total))
  
  dds2023 <- c(sum(aoc()$Core*aoc()$dds23/aoc()$total),sum(aoc()$Specialist*aoc()$dds23/aoc()$total),sum(aoc()$Support*aoc()$dds23/aoc()$total),sum(aoc()$Critical*aoc()$dds23/aoc()$total), sum(aoc()$not_map*aoc()$dds23/aoc()$total))
  
  revenue <-c(sum(data4$revenue)+sum(ifelse((dds2023-prev)>0, 0,(dds2023-prev))),-ifelse((dds2023-prev)>0, 0,(dds2023-prev)))
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data4n <- data.frame(x, revenue, col,cat)
  
  #2
  x <- c(rep('6.Automation<br>Reduction',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  
  
  autom <-c(sum(aoc()$Core*aoc()$dsray23/aoc()$total),sum(aoc()$Specialist*aoc()$dsray23/aoc()$total),sum(aoc()$Support*aoc()$dsray23/aoc()$total),sum(aoc()$Critical*aoc()$dsray23/aoc()$total), sum(aoc()$not_map*aoc()$dsray23/aoc()$total))
  revenue <-c(data4n$revenue[1]-sum(dds2023-autom),dds2023-autom)
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data5 <- data.frame(x, revenue, col,cat)
  
  
  #3Capability Enhancement
  x <- c(rep('7.Capability<br>Enhancement',6))
  col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  
  fte55 <-c(sum(aoc()$Core*aoc()$afte5/aoc()$total),sum(aoc()$Specialist*aoc()$afte5/aoc()$total),sum(aoc()$Support*aoc()$afte5/aoc()$total),sum(aoc()$Critical*aoc()$afte5/aoc()$total), sum(aoc()$not_map*aoc()$afte5/aoc()$total))
  
  revenue <-c(data5$revenue[1]-sum(autom-fte55),autom-fte55)
  cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data6 <- data.frame(x, revenue, col,cat)
  

  #5
  x <- c(rep('8.Y5 E',5))
  col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
  revenue <-fte55
  cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
  data7 <- data.frame(x, revenue, col,cat)
  
  data <- union(data0,data1)
  data <- union(data,data2)
  data <- union(data,data3)
  data <- union(data,data4)
  data <- union(data,data4n)
  data <- union(data,data5)
  data <- union(data,data6)
  data <- union(data,data7)

  
  ############
  p <- plot_ly()
  
  p <-add_trace(p,
                x = c(rep('1.Current<br>headcount',6)),
                y = c(0,data0$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = sum(aoc()$total), bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total)*1.08,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(1, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = data7$revenue[1]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = data7$revenue[1]+data7$revenue[2]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                    x = '1.Current<br>headcount',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('2.Upsizing',6)),
                y = c(data1$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text =round( sum(aoc()$addt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)#%>%
    # add_annotations(text = round(data1$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data1$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data1$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data1$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color ='rgba(255, 255, 255, 1.0)' ),
    #                 showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('3.Optimization',6)),
                y = c(data2$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = round(sum(aoc()$subt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                     showarrow = FALSE)#%>%
    # add_annotations(text = round(data2$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color ='rgba(255, 255, 255, 1.0)' ),
    #                 showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('4.Y1 E',6)),
                y = c(0,data3$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = round(sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '4.Y1 E',
                    y = sum(data3$revenue)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = data7$revenue[1]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = data7$revenue[1]+data7$revenue[2]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                    x = '4.Y1 E',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('5.Business (R&M)(+)',6)),
                y = c(data4$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  ) %>%
    add_annotations(text = round(sum(data4$revenue)-sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '5.Business (R&M)(+)',
                    y = round(sum(aoc()$dds23),0)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('6.Business (R&M)(-)',6)),
                y = c(data4n$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  ) %>%
    add_annotations(text = -round(sum(data4n$revenue[-1]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '6.Business (R&M)(-)',
                    y = round(sum(aoc()$dds23),0)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('7.Automation<br>Reduction',6)),
                y = c(data5$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = round(-sum(data5$revenue[-1]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '7.Automation<br>Reduction',
                    y = sum(aoc()$total)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('8.Capability<br>Enhancement',6)),
                y = c(data6$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = -round(sum(aoc()$dsray23)-sum(aoc()$afte5),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '8.Capability<br>Enhancement',
                    y = round(sum(aoc()$sray23),0)*1.05,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)
  

  p <-add_trace(p,
                x = c(rep('9.Y5 E',6)),
                y = c(0,data7$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  ) %>%
    layout(barmode = 'stack',margin = list(b = 100))%>%
    add_annotations(text = round(sum(fte55),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '9.Y5 E',
                    y = round(sum(fte55)*1.05,0),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = data7$revenue[1]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = data7$revenue[1]+data7$revenue[2]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                    x = '9.Y5 E',
                    y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
  
  
  
  p <-add_lines(p, x=c('1.Current<br>headcount','2.Upsizing') , y= c(sum(data0$revenue), sum(data0$revenue)), name = "",
                line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('2.Upsizing','3.Optimization') , y= c(sum(data1$revenue), sum(data1$revenue)), name = "",
                line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('3.Optimization','4.Y1 E') , y= c(sum(data3$revenue), sum(data3$revenue)), name = "",
                line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('4.Y1 E','5.Business (R&M)(+)') , y= c(sum(data3$revenue), sum(data3$revenue)), name = "",
                line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('5.Business (R&M)(+)','6.Business (R&M)(-)') , y= c(sum(data4$revenue), sum(data4$revenue)), name = "",
                line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('6.Business (R&M)(-)','7.Automation<br>Reduction') , y= c(data4n$revenue[1], data4n$revenue[1]), name = "",
                line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('7.Automation<br>Reduction','8.Capability<br>Enhancement') , y= c(sum(data6$revenue), sum(data6$revenue)), name = "",
                line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('8.Capability<br>Enhancement','9.Y5 E') , y= c(sum(data7$revenue), sum(data7$revenue)), name = "",
                line = list(color= 'black', widthh=0.3, dash="dot"), showlegend=FALSE)
  
  p
  

})#main waterfall
  
  output$plot32<- renderPlotly({
    x <- c(rep('1.Current<br>headcount',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nm=sum(aoc()$not_map)
    revenue <- c(sum(aoc()$Core),sum(aoc()$Specialist),sum(aoc()$Support),sum(aoc()$Critical), nm)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data0 <- data.frame(x, revenue, col,cat)
    
    
    ##opt
    x <- c(rep('2.Upsizing',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$total), sum(aoc()$Core*aoc()$addt/aoc()$total),sum(aoc()$Specialist*aoc()$addt/aoc()$total),sum(aoc()$Support*aoc()$addt/aoc()$total),sum(aoc()$Critical*aoc()$addt/aoc()$total), sum(aoc()$not_map*aoc()$addt/aoc()$total))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data1 <- data.frame(x, revenue, col,cat)
    
    ##o21
    x <- c(rep('3.Optimization',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <--c(-sum(aoc()$total)-sum(aoc()$addt)-sum(aoc()$subt), sum(aoc()$Core*aoc()$subt/aoc()$total),sum(aoc()$Specialist*aoc()$subt/aoc()$total),sum(aoc()$Support*aoc()$subt/aoc()$total),sum(aoc()$Critical*aoc()$subt/aoc()$total), sum(aoc()$not_map*aoc()$subt/aoc()$total))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data2 <- data.frame(x, revenue, col,cat)
    
    
    ##1
    x <- c(rep('4.Y0 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$Core)+sum(aoc()$Core*aoc()$subt/aoc()$total)+sum(aoc()$Core*aoc()$addt/aoc()$total),
                sum(aoc()$Specialist)+sum(aoc()$Specialist*aoc()$subt/aoc()$total)+sum(aoc()$Specialist*aoc()$addt/aoc()$total),
                sum(aoc()$Support)+sum(aoc()$Support*aoc()$subt/aoc()$total)+sum(aoc()$Support*aoc()$addt/aoc()$total),
                sum(aoc()$Critical)+sum(aoc()$Critical*aoc()$subt/aoc()$total)+sum(aoc()$Critical*aoc()$addt/aoc()$total),
                sum(aoc()$not_map)+sum(aoc()$not_map*aoc()$subt/aoc()$total)+sum(aoc()$not_map*aoc()$addt/aoc()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data3 <- data.frame(x, revenue, col,cat)
    
    #2
    x <- c(rep('5.Y1 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$Core*aoc()$afte1/aoc()$total),sum(aoc()$Specialist*aoc()$afte1/aoc()$total),sum(aoc()$Support*aoc()$afte1/aoc()$total),sum(aoc()$Critical*aoc()$afte1/aoc()$total), sum(aoc()$not_map*aoc()$afte1/aoc()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4 <- data.frame(x, revenue, col,cat)#2
    
    x <- c(rep('6.Y2 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$Core*aoc()$afte2/aoc()$total),sum(aoc()$Specialist*aoc()$afte2/aoc()$total),sum(aoc()$Support*aoc()$afte2/aoc()$total),sum(aoc()$Critical*aoc()$afte2/aoc()$total), sum(aoc()$not_map*aoc()$afte2/aoc()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4n <- data.frame(x, revenue, col,cat)
    
    #3
    x <- c(rep('7.Y3 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$Core*aoc()$afte3/aoc()$total),sum(aoc()$Specialist*aoc()$afte3/aoc()$total),sum(aoc()$Support*aoc()$afte3/aoc()$total),sum(aoc()$Critical*aoc()$afte3/aoc()$total), sum(aoc()$not_map*aoc()$afte3/aoc()$total))
    at=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data5 <- data.frame(x, revenue, col,cat)
    
    #4
    x <- c(rep('8.Y4 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$Core*aoc()$afte4/aoc()$total),sum(aoc()$Specialist*aoc()$afte4/aoc()$total),sum(aoc()$Support*aoc()$afte4/aoc()$total),sum(aoc()$Critical*aoc()$afte4/aoc()$total), sum(aoc()$not_map*aoc()$afte4/aoc()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data6 <- data.frame(x, revenue, col,cat)
    
    #5
    x <- c(rep('9.Y5 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(aoc()$Core*aoc()$afte5/aoc()$total),sum(aoc()$Specialist*aoc()$afte5/aoc()$total),sum(aoc()$Support*aoc()$afte5/aoc()$total),sum(aoc()$Critical*aoc()$afte5/aoc()$total), sum(aoc()$not_map*aoc()$afte5/aoc()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data7 <- data.frame(x, revenue, col,cat)
    
    data <- union(data0,data1)
    data <- union(data,data2)
    data <- union(data,data3)
    data <- union(data,data4)
    data <- union(data,data4n)
    data <- union(data,data5)
    data <- union(data,data6)
    data <- union(data,data7)
    
    
    ############
    p <- plot_ly()
    
    p <-add_trace(p,
                  x = c(rep('1.Current<br>headcount',6)),
                  y = c(0,data0$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = sum(aoc()$total), bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total)*1.08,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = data7$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = data7$revenue[1]+data7$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '1.Current<br>headcount',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('2.Upsizing',6)),
                  y = c(data1$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text =round( sum(aoc()$addt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*1.05,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)#%>%
    # add_annotations(text = round(data1$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data1$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data1$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data1$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
    #                 x = '2.Upsizing',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color ='rgba(255, 255, 255, 1.0)' ),
    #                 showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('3.Optimization',6)),
                  y = c(data2$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(aoc()$subt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*1.05,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)#%>%
    # add_annotations(text = round(data2$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color = 'rgba(255, 255, 255, 1.0)'),
    #                 showarrow = FALSE)%>%
    # add_annotations(text = round(data2$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
    #                 x = '3.Optimization',
    #                 y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
    #                 xref = "x",
    #                 font = list(family = 'Arial',
    #                             size = 12,
    #                             color ='rgba(255, 255, 255, 1.0)' ),
    #                 showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('4.Y0 E',6)),
                  y = c(0,data3$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*1.05,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = data7$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = data7$revenue[1]+data7$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '4.Y0 E',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('5.Y1 E',6)),
                  y = c(0,data4$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data4$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '5.Y1 E',
                      y = round(sum(data4$revenue)*1.05,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Y1 E',
                      y = data6$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Y1 E',
                      y = data6$revenue[1]+data6$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Y1 E',
                      y = data6$revenue[1]+data6$revenue[2]+data6$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Y1 E',
                      y = data4n$revenue[1]+data6$revenue[2]+data6$revenue[3]+data6$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '5.Y1 E',
                      y = data4$revenue[1]+data4n$revenue[2]+data4n$revenue[3]+data4n$revenue[4]+data4n$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('6.Y2 E',6)),
                  y = c(0,data4n$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data4n$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '6.Y2 E',
                      y = round(sum(data4n$revenue)*1.05,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Y2 E',
                      y = data6$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Y2 E',
                      y = data6$revenue[1]+data6$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Y2 E',
                      y = data6$revenue[1]+data6$revenue[2]+data6$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Y2 E',
                      y = data4n$revenue[1]+data6$revenue[2]+data6$revenue[3]+data6$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '6.Y2 E',
                      y = data4n$revenue[1]+data4n$revenue[2]+data4n$revenue[3]+data4n$revenue[4]+data4n$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('7.Y3 E',6)),
                  y = c(0,data5$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data5$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '7.Y3 E',
                      y = round(sum(data5$revenue)*1.05,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Y3 E',
                      y = data5$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Y3 E',
                      y = data5$revenue[1]+data5$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Y3 E',
                      y = data5$revenue[1]+data5$revenue[2]+data5$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Y3 E',
                      y = data5$revenue[1]+data5$revenue[2]+data5$revenue[3]+data5$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '7.Y3 E',
                      y = data5$revenue[1]+data5$revenue[2]+data5$revenue[3]+data5$revenue[4]+data5$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('8.Y4 E',6)),
                  y = c(0,data6$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data6$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '8.Y4 E',
                      y = round(sum(data6$revenue)*1.05,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Y4 E',
                      y = data6$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Y4 E',
                      y = data6$revenue[1]+data6$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Y4 E',
                      y = data6$revenue[1]+data6$revenue[2]+data6$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Y4 E',
                      y = data6$revenue[1]+data6$revenue[2]+data6$revenue[3]+data6$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '8.Y4 E',
                      y = data6$revenue[1]+data6$revenue[2]+data6$revenue[3]+data6$revenue[4]+data6$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    
    p <-add_trace(p,
                  x = c(rep('9.Y5 E',6)),
                  y = c(0,data7$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data7$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '9.Y5 E',
                      y = round(sum(data7$revenue)*1.05,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = data7$revenue[1]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = data7$revenue[1]+data7$revenue[2]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '9.Y5 E',
                      y = data7$revenue[1]+data7$revenue[2]+data7$revenue[3]+data7$revenue[4]+data7$revenue[5]/2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    
    
    p <-add_lines(p, x=c('1.Current<br>headcount','2.Upsizing') , y= c(sum(data0$revenue), sum(data0$revenue)), name = "",
                  line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('2.Upsizing','3.Optimization') , y= c(sum(data1$revenue), sum(data1$revenue)), name = "",
                  line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('3.Optimization','4.Y0 E') , y= c(sum(data3$revenue), sum(data3$revenue)), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('4.Y0 E','5.Y1 E') , y= c(sum(data4$revenue), sum(data4$revenue)), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('5.Y1 E','6.Y2 E') , y= c(sum(data4n$revenue), sum(data4n$revenue)), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('6.Y2 E','7.Y3 E') , y= c(sum(data5$revenue), sum(data5$revenue)), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('7.Y3 E','8.Y4 E') , y= c(sum(data6$revenue), sum(data6$revenue)), name = "",
                  line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('8.Y4 E','9.Y5 E') , y= c(sum(data7$revenue), sum(data7$revenue)), name = "",
                  line = list(color= 'black', widthh=0.3, dash="dot"), showlegend=FALSE)
    
    p
    
    
  })#main waterfall
  
  output$plot32b<- renderPlotly({
    x <- c(rep('1.Current<br>headcount',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
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
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
    revenue <-c(sum(rs$total)-sum(rs$subt), sum(sign(rs$Core)*rs$subt),sum(sign(rs$Specialist)*rs$subt),sum(sign(rs$Support)*rs$subt),sum(sign(rs$Critical)*rs$subt), nms)
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data2 <- data.frame(x, revenue, col,cat)
    
    
    ##1
    x <- c(rep('4.Y1 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
    revenue <-c(sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  ,        nm-nms)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data3 <- data.frame(x, revenue, col,cat)
    
    
    #2
    x <- c(rep('5.Y2 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmdd =sum(rs$sray20)-(sum(sign(rs$Core)*rs$sray20) + sum(sign(rs$Specialist)*rs$sray20) + sum(sign(rs$Support)*rs$sray20) + sum(sign(rs$Critical)*rs$sray20))
    revenue <-c(sum(sign(rs$Core)*rs$sray20), sum(sign(rs$Specialist)*rs$sray20), sum(sign(rs$Support)*rs$sray20), sum(sign(rs$Critical)*rs$sray20), nmdd)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4 <- data.frame(x, revenue, col,cat)
    
    #3
    x <- c(rep('6.Y3 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmdd =sum(rs$sray21)-(sum(sign(rs$Core)*rs$sray21) + sum(sign(rs$Specialist)*rs$sray21) + sum(sign(rs$Support)*rs$sray21) + sum(sign(rs$Critical)*rs$sray21))
    revenue <-c(sum(sign(rs$Core)*rs$sray21), sum(sign(rs$Specialist)*rs$sray21), sum(sign(rs$Support)*rs$sray21), sum(sign(rs$Critical)*rs$sray21), nmdd)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data5 <- data.frame(x, revenue, col,cat)
    
    #4
    x <- c(rep('7.Y4 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmdd =sum(rs$sray22)-(sum(sign(rs$Core)*rs$sray22) + sum(sign(rs$Specialist)*rs$sray22) + sum(sign(rs$Support)*rs$sray22) + sum(sign(rs$Critical)*rs$sray22))
    revenue <-c(sum(sign(rs$Core)*rs$sray22), sum(sign(rs$Specialist)*rs$sray22), sum(sign(rs$Support)*rs$sray22), sum(sign(rs$Critical)*rs$sray22), nmdd)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data6 <- data.frame(x, revenue, col,cat)
    
    #5
    x <- c(rep('8.Y5 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
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
            ,colors = c("transparent","darkorange", "midnightblue", "gold", "deepskyblue", "gray"))%>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)%>%
      add_annotations(text = sum(rs$total),
                      x = '1.Current<br>headcount',
                      y = sum(rs$total)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$addt),
                      x = '2.Upsizing',
                      y = sum(rs$total)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$subt),
                      x = '3.Optimization',
                      y = sum(rs$total)-sum(rs$subt)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total)-sum(rs$subt),
                      x = '4.Y1 E',
                      y = sum(rs$total)-sum(rs$subt)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray20),0),
                      x = '5.Y2 E',
                      y = sum(rs$sray20)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray21),0),
                      x = '6.Y3 E',
                      y = round(sum(rs$sray21),0)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray22),0),
                      x = '7.Y4 E',
                      y = round(sum(rs$sray22),0)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray23),0),
                      x = '8.Y5 E',
                      y = round(sum(rs$sray23),0)+2000,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)
  })
  
  output$plot33<- renderPlotly({
    
    x <- c('Y1 E', 'Y2 E', 'Y3 E', 'Y4 E', 'Y5 E')
    As_is <- c(sum(rs$fte1), sum(rs$fte2),sum(rs$fte3),sum(rs$fte4),sum(rs$fte5))
    Opt <- c(sum(r_opt_s$fte1), sum(r_opt_s$fte2),sum(r_opt_s$fte3),sum(r_opt_s$fte4),sum(r_opt_s$fte5))
    
    Con <- c(sum(r_crvt_s$fte1), sum(r_crvt_s$fte2),sum(r_crvt_s$fte3),sum(r_crvt_s$fte4),sum(r_crvt_s$fte5))
    
    data <- data.frame(x, Opt, Con, As_is)
    plot_ly(data, x = ~x, y = ~As_is, type = 'bar',name='Conservative', marker = list(color = 'deepskyblue'))%>%
      add_trace(y = ~Opt,name='Optimistic', marker = list(color = 'darkorange')) %>%
      add_trace(y = ~Con,name='Base Case', marker = list(color = 'midnightblue'))%>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(title = ""))%>%
      add_annotations(text = round(Con,0),xshift=23,xanchor='left',bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='midnightblue',
                      x = x,
                      y = Con*(1+1/40),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(Opt,0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='darkorange',
                      x = x,
                      y = Opt*(1+2/40),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(20, 20, 20, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(As_is,0),xanchor='right',xshift=-23,bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='deepskyblue',
                      x = x,
                      y = As_is*(1+3/40),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)
    
  })
  
  output$plot34<- renderPlotly({
    
    x <- c(rep(c('Y0 E','Y1 E','Y2 E','Y3 E','Y4 E','Y5 E'),5))
    
    core1 <- c(sum(aoc()$Core*aoc()$sels/aoc()$total),sum(aoc()$Core*aoc()$afte1/aoc()$total),sum(aoc()$Core*aoc()$afte2/aoc()$total),sum(aoc()$Core*aoc()$afte3/aoc()$total),sum(aoc()$Core*aoc()$afte4/aoc()$total),sum(aoc()$Core*aoc()$afte5/aoc()$total))
    core2 <- c(sum(aoc()$Core*aoc()$s18/aoc()$total),sum(aoc()$Core*aoc()$s19/aoc()$total),sum(aoc()$Core*aoc()$s20/aoc()$total),sum(aoc()$Core*aoc()$s21/aoc()$total),sum(aoc()$Core*aoc()$s22/aoc()$total),sum(aoc()$Core*aoc()$o23/aoc()$total))
    
    specialist1 <- c(sum(aoc()$Specialist*aoc()$sels/aoc()$total),sum(aoc()$Specialist*aoc()$afte1/aoc()$total),sum(aoc()$Specialist*aoc()$afte2/aoc()$total),sum(aoc()$Specialist*aoc()$afte3/aoc()$total),sum(aoc()$Specialist*aoc()$afte4/aoc()$total),sum(aoc()$Specialist*aoc()$afte5/aoc()$total))
    specialist2 <- c(sum(aoc()$Specialist*aoc()$s18/aoc()$total),sum(aoc()$Specialist*aoc()$s19/aoc()$total),sum(aoc()$Specialist*aoc()$s20/aoc()$total),sum(aoc()$Specialist*aoc()$s21/aoc()$total),sum(aoc()$Specialist*aoc()$s22/aoc()$total),sum(aoc()$Specialist*aoc()$o23/aoc()$total))
    
    support1 <- c(sum(aoc()$Support*aoc()$sels/aoc()$total),sum(aoc()$Support*aoc()$afte1/aoc()$total),sum(aoc()$Support*aoc()$afte2/aoc()$total),sum(aoc()$Support*aoc()$afte3/aoc()$total),sum(aoc()$Support*aoc()$afte4/aoc()$total),sum(aoc()$Support*aoc()$afte5/aoc()$total))
    support2 <- c(sum(aoc()$Support*aoc()$s18/aoc()$total),sum(aoc()$Support*aoc()$s19/aoc()$total),sum(aoc()$Support*aoc()$s20/aoc()$total),sum(aoc()$Support*aoc()$s21/aoc()$total),sum(aoc()$Support*aoc()$s22/aoc()$total),sum(aoc()$Support*aoc()$o23/aoc()$total))
    
    critical1 <- c(sum(aoc()$Critical*aoc()$sels/aoc()$total),sum(aoc()$Critical*aoc()$afte1/aoc()$total),sum(aoc()$Critical*aoc()$afte2/aoc()$total),sum(aoc()$Critical*aoc()$afte3/aoc()$total),sum(aoc()$Critical*aoc()$afte4/aoc()$total),sum(aoc()$Critical*aoc()$afte5/aoc()$total))
    critical2 <- c(sum(aoc()$Critical*aoc()$s18/aoc()$total),sum(aoc()$Critical*aoc()$s19/aoc()$total),sum(aoc()$Critical*aoc()$s20/aoc()$total),sum(aoc()$Critical*aoc()$s21/aoc()$total),sum(aoc()$Critical*aoc()$s22/aoc()$total),sum(aoc()$Critical*aoc()$o23/aoc()$total))
    
    not_map1 <- c(sum(aoc()$not_map*aoc()$sels/aoc()$total),sum(aoc()$not_map*aoc()$afte1/aoc()$total),sum(aoc()$not_map*aoc()$afte2/aoc()$total),sum(aoc()$not_map*aoc()$afte3/aoc()$total),sum(aoc()$not_map*aoc()$afte4/aoc()$total),sum(aoc()$not_map*aoc()$afte5/aoc()$total))
    not_map2 <- c(sum(aoc()$not_map*aoc()$s18/aoc()$total),sum(aoc()$not_map*aoc()$s19/aoc()$total),sum(aoc()$not_map*aoc()$s20/aoc()$total),sum(aoc()$not_map*aoc()$s21/aoc()$total),sum(aoc()$not_map*aoc()$s22/aoc()$total),sum(aoc()$not_map*aoc()$o23/aoc()$total))
    
    dema<-c(core1, specialist1, support1,critical1, not_map1)- c(core2, specialist2, support2,critical2, not_map2)
    cat <-c(c(rep('1.Core',6),rep('2.Specialist',6),rep('3.Support',6),rep('4.Critical',6),rep('5.Not Map',6)))
    
    col1<-c(rep('rgba(282, 157, 13, 1.0)',6),rep('rgba(2, 90, 216, 1.0)',6),rep('rgba(200, 90, 216, 1.0)',6),rep('rgba(2, 190, 216, 1.0)',6),rep('rgba(82, 190, 16, 1.0)',6))
    col <- c(col1)
    data <- data.frame(x, dema, cat, col)
    
    plot_ly(data, x = ~x, y = ~dema, type = 'bar',
            color =~cat, hoverinfo = 'text', text = ~paste(cat, dema)
            ,colors = c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
            ) %>%
      layout(title = 'Headcount',barmode = 'stack',margin = list(b = 150),
             xaxis = list(title = "",showgrid = F),
             yaxis = list(title = "",showgrid = F))
    
  })
  #almost stack and group https://plot.ly/r/bar-charts/
  output$plot341<- renderPlotly({
    
    x <- c(rep(c('Y1 E','Y2 E','Y3 E','Y4 E','Y5 E'),5))
    
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
    
    dema1<-c(core1, specialist1, support1,critical1, nm1)
    dema2<-c(core2, specialist2, support2,critical2, nm2)
    cat <-c(rep('1.Core',5),rep('2.Specialist',5),rep('3.Support',5),rep('4.Critical',5),rep('5.Not Map',5))
    
    col<-c(rep('rgba(282, 157, 13, 0.7)',5),rep('rgba(2, 90, 216, 0.7)',5),rep('rgba(200, 90, 216, 0.7)',5),rep('rgba(2, 190, 216, 0.7)',5),rep('rgba(82, 190, 16, 0.7)',5))
        data <- data.frame(x, dema1,dema2, cat, col)
    
    plot_ly(data, x = ~x, y = ~dema1, type = 'bar',name='1',xaxis= 'x1',
            color =~cat, hoverinfo = 'text', text = ~paste(cat, dema1)
            ,colors = c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    ) %>%
      add_trace(y = ~dema2, type = 'bar',name='2',xaxis= 'x2',
                color =~cat, hoverinfo = 'text', text = ~paste(cat, dema2)
                ,colors = c("darkorange1", "midnightblue2", "gold4", "deepskyblue4", "gray55")) %>%
      layout(title = 'Headcount',barmode = 'stack',margin = list(b = 150), bargap = 0.15, bargroupgap = 0.1,
             xaxis = list(title = "",
               domain= c(0, 0.5),
               anchor= 'x1'),
             xaxis2= list(title = "",
               domain= c(0.5, 1),
               anchor= 'x2') 
             ,
             yaxis = list(title = ""))
    
  })
  
  output$plot35<- renderPlotly({
    
    x <- c('T0','T1', 'T2', 'T3', 'T4', 'T5')
    
    Opt <- c(sum(r_opt_s$sels), sum(r_opt_s$sray19),sum(r_opt_s$sray20),sum(r_opt_s$sray21),sum(r_opt_s$sray22),sum(r_opt_s$sray23))
    
    crvt <- c(sum(r_crvt_s$sels), sum(r_crvt_s$sray19),sum(r_crvt_s$sray20),sum(r_crvt_s$sray21),sum(r_crvt_s$sray22),sum(r_crvt_s$sray23))
    
    As_is <- c(sum(rs$sels), sum(rs$sray19),sum(rs$sray20),sum(rs$sray21),sum(rs$sray22),sum(rs$sray23))
    
    data <- data.frame(x, Opt,  As_is,crvt)
    
    plot_ly(data, x = ~x, y = ~Opt, type = 'scatter',mode = "lines",name='Optimistic',marker = list(color = 'rgba(55, 128, 191, 0.7)'))%>%
      
      add_trace(y = ~As_is, name='Conservative',  marker = list(color = 'rgba(200, 0, 23, 0.7)',width = 2),
                line = list(color = 'rgba(200, 0, 23, 0.7)'))%>%
      add_trace(y = ~crvt, name='Base Case',  marker = list(color = 'orange',width = 2),
                                                       line = list(color = 'orange'
                                                                   )) %>%
      
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(title = "", gridline=F))
    
  })
  
  #tab4
  output$plot42<- renderPlotly({
    base <- c('Type','Type')
    cat <- c('Contractor','Direct Hire')
    
    x <- c(sum(rs$Contractor,na.rm = TRUE),sum(rs$`Direct Hire`,na.rm = TRUE))
    text <- c(sum(rs$Contractor,na.rm = TRUE)/2,sum(rs$Contractor,na.rm = TRUE)+(sum(rs$`Direct Hire`,na.rm = TRUE))/2)
    
    data <- data.frame(x,cat, base, text)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h', color = ~cat,  marker = list(color = c('rgba(227, 82, 5, 1.0)','rgba(4, 30, 66, 1.0)')))%>%
      
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
  }) # direc hire, contract
  
  output$plot421<- renderPlotly({
    base <- c('Core', 'Critical', 'Support',  'Specialist','Non Map')
    x <- c(sum(rs$Core),sum(rs$Critical),sum(rs$Support),sum(rs$Specialist), sum(rs$not_map))#sum(rs$total)-sum(rs$Core)-sum(rs$Critical0)-sum(rs$Support)-sum(rs$Specialist))
    text <- c('$320K','$430K', '$260K', '$-120K', '$-120K')
    data <- data.frame(x, base, text)
    
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h',
            marker = list(color = c('rgba(227, 82, 5, 1.0)','rgba(0, 159, 223, 1.0)','rgba(255, 205, 0, 0.7)','rgba(4, 30, 66, 1.0)','rgba(191, 191, 191, 1.0)'))
            )%>%
            layout(title = 'Segmentation',
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })  # segmentation bar core..
  
  output$plot424<- renderPlotly({
    rs <- mutate(rs, age_i = case_when(rs$Age < 25 ~'<25', rs$Age >=25 & rs$Age < 35 ~'25-34', rs$Age >=35 & rs$Age < 45 ~'35-44', rs$Age >=45 & rs$Age < 55 ~'45-54', rs$Age >=55 & rs$Age < 65 ~'55-65', rs$Age >=65  ~'65<'))
    dat <- aggregate(rs$total, by=list(age=rs$age_i), FUN=sum)
    
    #The default order will be alphabetized unless specified as below:
    plot_ly(dat, x = ~dat$age, y = ~x, type = 'bar', marker = list(color = c('rgba(91, 91, 91, 1.0)','rgba(111, 111, 111, 1.0)','rgba(151, 151, 151, 1.0)','rgba(171, 171, 171, 1.0)','rgba(191, 191, 191, 1.0)'))
            )%>%
    add_lines(x=dat$age , y= c(dat$x[1],sum(dat$x[1:2]),sum(dat$x[1:3]),sum(dat$x[1:4]),sum(dat$x[1:5])), name = "",
                  line = list(color= 'black'), showlegend=FALSE)%>%
      layout(title = 'Age Distribution',
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  })# age distribution
  
  output$plot422<- renderPlotly({
    x <- c('Y1', 'Y2', 'Y3', 'Y4', 'Y5')
    y<-c(sum(rs$`2019`, na.rm = TRUE),sum(rs$`2020`, na.rm = TRUE),sum(rs$`2021`, na.rm = TRUE),sum(rs$`2022`, na.rm = TRUE),sum(rs$`2023`, na.rm = TRUE))
    data <- data.frame(x,y)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~y, type = 'bar', marker = list(color = c('rgba(91, 91, 91, 1.0)','rgba(111, 111, 111, 1.0)','rgba(151, 151, 151, 1.0)','rgba(171, 171, 171, 1.0)','rgba(191, 191, 191, 1.0)'))
    )%>%
      layout(title = 'Retirements',
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)
    
  }) # Retirements
  
  output$plot43<- renderPlotly({
    x <- c(rep('1.Current<br>headcount',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
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
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
    revenue <-c(sum(rs$total)-sum(rs$subt), sum(sign(rs$Core)*rs$subt),sum(sign(rs$Specialist)*rs$subt),sum(sign(rs$Support)*rs$subt),sum(sign(rs$Critical)*rs$subt), nms)
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data2 <- data.frame(x, revenue, col,cat)
    
    
    ##1
    x <- c(rep('4.Y1 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
    revenue <-c(sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  ,        nm-nms)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data3 <- data.frame(x, revenue, col,cat)
    
    
    #2
    x <- c(rep('5.Automation<br>Reduction',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nma =sum(rs$sray23)-(sum(sign(rs$Core)*rs$sray23) + sum(sign(rs$Specialist)*rs$sray23) + sum(sign(rs$Support)*rs$sray23) + sum(sign(rs$Critical)*rs$sray23))
    
    nma=sum(rs$sray23)-sum(rs$fte5)
    
    rasa <-c(0,sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  , 0)
    revenue <-c(-sum(rs$sray23), sum(sign(rs$Core)*rs$sray23),sum(sign(rs$Specialist)*rs$sray23),sum(sign(rs$Support)*rs$sray23),sum(sign(rs$Critical)*rs$sray23), 0)
    revenue <-rasa-revenue
    nma=sum(rs$sray23)-sum(rs$fte5)-sum(revenue[2:5])
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4 <- data.frame(x, revenue, col,cat)
    
    
    
    
    #3Capability Enhancement
    x <- c(rep('6.Capability<br>Enhancement',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmf =sum(rs$fte5)-(sum(sign(rs$Core)*rs$fte5) + sum(sign(rs$Specialist)*rs$fte5) + sum(sign(rs$Support)*rs$fte5) + sum(sign(rs$Critical)*rs$fte5))
    
    rasa <-c(sum(rs$fte5),sum(sign(rs$Core)*rs$sray23),sum(sign(rs$Specialist)*rs$sray23),sum(sign(rs$Support)*rs$sray23),sum(sign(rs$Critical)*rs$sray23),       nma)
    revenue <-c(0, sum(sign(rs$Core)*rs$fte5),sum(sign(rs$Specialist)*rs$fte5),sum(sign(rs$Support)*rs$fte5),sum(sign(rs$Critical)*rs$fte5), nmf)
    revenue <-rasa-revenue
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data5 <- data.frame(x, revenue, col,cat)
    
    #4
    x <- c(rep('7.Business (R&M)',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmd =sum(rs$dds23)-(sum(sign(rs$Core)*rs$dds23) + sum(sign(rs$Specialist)*rs$dds23) + sum(sign(rs$Support)*rs$dds23) + sum(sign(rs$Critical)*rs$dds23))
    
    rasa <-c(sum(rs$fte5),sum(sign(rs$Core)*rs$dds23),sum(sign(rs$Specialist)*rs$dds23),sum(sign(rs$Support)*rs$dds23),sum(sign(rs$Critical)*rs$dds23), nmd)
    revenue <-c(0, sum(sign(rs$Core)*rs$fte5),sum(sign(rs$Specialist)*rs$fte5),sum(sign(rs$Support)*rs$fte5),sum(sign(rs$Critical)*rs$fte5), nmf)
    revenue <-rasa-revenue
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data6 <- data.frame(x, revenue, col,cat)
    
    #5
    x <- c(rep('8.Y5 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
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
            ,colors = c("transparent","darkorange", "midnightblue", "gold", "deepskyblue", "gray"))%>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)%>%
      add_annotations(text = sum(rs$total),
                      x = '1.Current<br>headcount',
                      y = sum(rs$total),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total),
                      x = '2.Upsizing',
                      y = sum(rs$total),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total)-sum(rs$subt),
                      x = '3.Optimization',
                      y = sum(rs$total)-sum(rs$subt),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total)-sum(rs$subt),
                      x = '4.Y1 E',
                      y = sum(rs$total)-sum(rs$subt),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray23),0),
                      x = '5.Automation<br>Reduction',
                      y = sum(rs$total)-sum(rs$subt),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$fte5),0),
                      x = '6.Capability<br>Enhancement',
                      y = round(sum(rs$sray23),0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$dds23),0),
                      x = '7.Business (R&M)',
                      y = round(sum(rs$dds23),0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$dds23),0),
                      x = '8.Y5 E',
                      y = round(sum(rs$dds23),0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)
  })#waterfall
  
  output$plot432<- renderPlotly({
    x <- c(rep('1.Current<br>headcount',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
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
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
    revenue <-c(sum(rs$total)-sum(rs$subt), sum(sign(rs$Core)*rs$subt),sum(sign(rs$Specialist)*rs$subt),sum(sign(rs$Support)*rs$subt),sum(sign(rs$Critical)*rs$subt), nms)
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data2 <- data.frame(x, revenue, col,cat)
    
    
    ##1
    x <- c(rep('4.Y1 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nms =sum(rs$subt)-(sum(sign(rs$Core)*rs$subt) + sum(sign(rs$Specialist)*rs$subt) + sum(sign(rs$Support)*rs$subt) + sum(sign(rs$Critical)*rs$subt))
    revenue <-c(sum(rs$Core)-sum(sign(rs$Core)*rs$subt)  ,  sum(rs$Specialist)-sum(sign(rs$Specialist)*rs$subt)  ,  sum(rs$Support)-sum(sign(rs$Support)*rs$subt)  ,  sum(rs$Critical)-sum(sign(rs$Critical)*rs$subt)  ,        nm-nms)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data3 <- data.frame(x, revenue, col,cat)
    
    
    #2
    x <- c(rep('5.Y2 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmdd =sum(rs$sray20)-(sum(sign(rs$Core)*rs$sray20) + sum(sign(rs$Specialist)*rs$sray20) + sum(sign(rs$Support)*rs$sray20) + sum(sign(rs$Critical)*rs$sray20))
    revenue <-c(sum(sign(rs$Core)*rs$sray20), sum(sign(rs$Specialist)*rs$sray20), sum(sign(rs$Support)*rs$sray20), sum(sign(rs$Critical)*rs$sray20), nmdd)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4 <- data.frame(x, revenue, col,cat)
    
    #3
    x <- c(rep('6.Y3 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmdd =sum(rs$sray21)-(sum(sign(rs$Core)*rs$sray21) + sum(sign(rs$Specialist)*rs$sray21) + sum(sign(rs$Support)*rs$sray21) + sum(sign(rs$Critical)*rs$sray21))
    revenue <-c(sum(sign(rs$Core)*rs$sray21), sum(sign(rs$Specialist)*rs$sray21), sum(sign(rs$Support)*rs$sray21), sum(sign(rs$Critical)*rs$sray21), nmdd)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data5 <- data.frame(x, revenue, col,cat)
    
    #4
    x <- c(rep('7.Y4 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nmdd =sum(rs$sray22)-(sum(sign(rs$Core)*rs$sray22) + sum(sign(rs$Specialist)*rs$sray22) + sum(sign(rs$Support)*rs$sray22) + sum(sign(rs$Critical)*rs$sray22))
    revenue <-c(sum(sign(rs$Core)*rs$sray22), sum(sign(rs$Specialist)*rs$sray22), sum(sign(rs$Support)*rs$sray22), sum(sign(rs$Critical)*rs$sray22), nmdd)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data6 <- data.frame(x, revenue, col,cat)
    
    #5
    x <- c(rep('8.Y5 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
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
            ,colors = c("transparent","darkorange", "midnightblue", "gold", "deepskyblue", "gray"))%>%
      layout(title = 'Headcount', margin = list(b = 100),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             showlegend = FALSE)%>%
      add_annotations(text = sum(rs$total),
                      x = '1.Current<br>headcount',
                      y = sum(rs$total),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total),
                      x = '2.Upsizing',
                      y = sum(rs$total),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total)-sum(rs$subt),
                      x = '3.Optimization',
                      y = sum(rs$total)-sum(rs$subt),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = sum(rs$total)-sum(rs$subt),
                      x = '4.Y1 E',
                      y = sum(rs$total)-sum(rs$subt),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray20),0),
                      x = '5.Y2 E',
                      y = sum(rs$sray20),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray21),0),
                      x = '6.Y3 E',
                      y = round(sum(rs$sray21),0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray22),0),
                      x = '7.Y4 E',
                      y = round(sum(rs$sray22),0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(rs$sray23),0),
                      x = '8.Y5 E',
                      y = round(sum(rs$sray23),0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)
  })#waterfall
  
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
    
  })#waterfall
  
  output$plot44<- renderPlotly({
    
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
    
  })#forecast
  
  output$plot423<- renderPlotly({
    base <- rep(c('Level1', 'Level2', 'Level3',  'Level4','Level5','Level6','Level7', 'Level8','OS', 'UNG'),2)
    yform <- list(categoryorder = "array", title = "",
                  categoryarray = c('UNG','OS','Level8','Level7','Level6','Level5', 'Level4','Level3','Level2','Level1'))
    x1 <- c(sum(rs$A, na.rm = TRUE),sum(rs$B, na.rm = TRUE),sum(rs$C, na.rm = TRUE),sum(rs$D, na.rm = TRUE),sum(rs$E, na.rm = TRUE),sum(rs$F, na.rm = TRUE),sum(rs$G, na.rm = TRUE),sum(rs$H, na.rm = TRUE),sum(rs$OS, na.rm = TRUE),sum(rs$UNG, na.rm = TRUE))/2
    x2 <- -c(sum(rs$A, na.rm = TRUE),sum(rs$B, na.rm = TRUE),sum(rs$C, na.rm = TRUE),sum(rs$D, na.rm = TRUE),sum(rs$E, na.rm = TRUE),sum(rs$F, na.rm = TRUE),sum(rs$G, na.rm = TRUE),sum(rs$H, na.rm = TRUE),sum(rs$OS, na.rm = TRUE),sum(rs$UNG, na.rm = TRUE))/2
    x=c(x1,x2)
    text=x1*2
    data <- data.frame(x, base, text)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h',marker = list(color = 'deepskyblue'),hoverinfo = 'text', text = ~paste(base, text))%>%
      add_trace()%>% config(displayModeBar = F)%>%
      layout(title = 'Career Level Structure',
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
    
  }) #level
  
  output$plot425<- renderPlotly({
    x <- c('Male', 'Female', 'Not Map')
    y<-c(sum(rs$Male, na.rm = TRUE),sum(rs$Female, na.rm = TRUE), sum(rs$total)-sum(rs$Male, na.rm = TRUE)-sum(rs$Female, na.rm = TRUE))
    cat = c("midnightblue", "gold", "deepskyblue")
    data <- data.frame(x,y,cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, labels = ~x, values  = ~y, type = 'pie',marker = list(colors = c("midnightblue", "gold", "deepskyblue")))%>%
      layout(#title = '',
             showlegend = T)
    
  }) # male female pie
  
  output$plot426<- renderPlotly({
    cat <- c(rep('Male',4), rep('Female',4),  rep('Not Map',4))
    x <- rep(c('MEA', 'APAC', 'AMR', 'EUR'),3)
    y<-c(sum(rs[rs$Region == 'MEA','Male'], na.rm = TRUE),sum(rs[rs$Region == 'APAC','Male'], na.rm = TRUE),
         sum(rs[rs$Region == 'AMR','Male'], na.rm = TRUE),sum(rs[rs$Region == 'APAC','Male'], na.rm = TRUE),
         sum(rs[rs$Region == 'MEA','Female'], na.rm = TRUE),sum(rs[rs$Region == 'APAC','Female'], na.rm = TRUE),
         sum(rs[rs$Region == 'AMR','Female'], na.rm = TRUE),sum(rs[rs$Region == 'EUR','Female'], na.rm = TRUE),
         sum(rs[rs$Region == 'MEA','total'], na.rm = TRUE)-sum(rs[rs$Region == 'MEA','Male'], na.rm = TRUE)-sum(rs[rs$Region == 'MEA','Female'], na.rm = TRUE),
         sum(rs[rs$Region == 'APAC','total'], na.rm = TRUE)-sum(rs[rs$Region == 'APAC','Male'], na.rm = TRUE)-sum(rs[rs$Region == 'APAC','Female'], na.rm = TRUE),
         sum(rs[rs$Region == 'AMR','total'], na.rm = TRUE)-sum(rs[rs$Region == 'AMR','Male'], na.rm = TRUE)-sum(rs[rs$Region == 'AMR','Female'], na.rm = TRUE),
         sum(rs[rs$Region == 'APAC','total'], na.rm = TRUE)-sum(rs[rs$Region == 'APAC','Male'], na.rm = TRUE)-sum(rs[rs$Region == 'APAC','Female'], na.rm = TRUE))
    data <- data.frame(x,y, cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~y, type = 'bar',  color = ~cat, colors = c("gold","midnightblue",  "deepskyblue"))%>%
      layout(#title = 'Headcount',
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = list(title = "", showgrid = FALSE),
        barmode = 'stack',
        showlegend = TRUE)
  }) # male female
  
  #####
  
  #end
  
  #table
########
  
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
  
  vals$Data=data.table(rs,
                       rs$Modify<-
                         paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(db),'>Change</button>
           </div>'),
                       rs[["Edit"]]<-
                         paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(db),'><span class="glyphicon glyphicon-edit" aria-hidden="true"></span></button>
           </div>
           '))
  
  #vals$Data=data.table(zxc <- select(db, `CF/SBU/AFF`, Region, `CF/SBU/AFF Name`, Division, Family, `Sub Family` )                       )
  newEntry1 <- observe(vals$Data <- mutate(vals$Data, Modify= paste0('<div class="btn-group" role="group" aria-label="Basic example">
                                                          <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(db),'>Change</button>
                                                          </div>'))
                      )
  
  # newEntry <- observe({vals$Data <- data.table(db[db$Region %in%  if(is.null(input$rg)){unique(db$Region)} else (input$rg),])
  # vals$Data <- data.table(db[db$Division %in%  if(is.null(input$div)){unique(db$Division)} else (input$div) ,])
  # })
  
  aos<-reactive({
    zxc <- select(vals$Data, `CF/SBU/AFF`, Region, `CF/SBU/AFF Name`, Division, Family, `Sub Family`, Modify,
                  `Demand Driver 1`, `DD1 Weightage`, `DD1 2019`, `DD1 2020`, `DD1 2021`, `DD1 2022`, `DD1 2023` )
    
  })
  ######
  #end vals
  
  #aos
  #####
  
  output$soa<- renderDataTable({
    datatable(aos(),rownames = FALSE,
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
  
  
  
  output$Main_kosz=renderDataTable({ DT=aos()
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
            ) ,escape=F)%>%
    formatPercentage('DD1 Weightage', 0)
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
    size="l"
    )
  
  #####
  
  
  
  ##co modyfikuje
  
  output$row_modif<-renderDataTable({
    
    selected_row=as.numeric(gsub("modify_","",input$lastClickkId))
    old_row=vals$Data[selected_row,c('Demand Driver 1','DD1 2019','DD1 2020','DD1 2021','DD1 2022','DD1 2023')]
    row_change=list()
    row_change[[1]]=vals$Data[selected_row,c('Demand Driver 1')]
    row_change[[2]]=paste0('<input class="new_input" type="number" id=new_',2,'><br>')
    row_change[[3]]=paste0('<input class="new_input" type="number" id=new_',3,'><br>')
    row_change[[4]]=paste0('<input class="new_input" type="number" id=new_',4,'><br>')
    row_change[[5]]=paste0('<input class="new_input" type="number" id=new_',5,'><br>')
    row_change[[6]]=paste0('<input class="new_input" type="number" id=new_',6,'><br>')
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=rbind(old_row,row_change)
    rownames(DT)<-c("Current values","New values")
    
    DT
    
  },escape=F,options=list(utoWidth = FALSE,
                          scrollX = TRUE,
                          scrollY = "51vh",
                          scroller = TRUE,
                          scollCollapse = TRUE,
                          fixedHeader = TRUE,dom='t',ordering=F),selection="none")
  
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
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickkId)),c('DD1 2019','DD1 2020','DD1 2021','DD1 2022','DD1 2023')]<-DF
                 
               }
  )
  ######
  
  #####
  #end tb
  
  }

# Run the application

shinyApp(ui = ui, server = server)