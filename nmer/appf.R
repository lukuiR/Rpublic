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
                tags$img(src="sabic.png", height = '51'), target="_blank")


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
      
      menuItem("Deep Dive View", icon = icon("glyphicon glyphicon-stats", lib ='glyphicon'), tabName = "tab4")
      
    )
    
    #####
  ),
  # end
  #dashbody  
  dashboardBody(
    tabItems(
      #tab1
      #####
      tabItem(tabName = "tab1",#includeCSS("styles.css"),
              
              img(src="sabic.png", width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),
              
              h2("Strategic Workforce Interactive Scenario Scoper",align="center",style="color:dark blue"),
              
              fluidRow(
                
                box(p("This Platform is designed to enable detailed forecasting of workforce requirements throughout SABIC's
                      
                      workforce over a five year time period.",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    h2("Instructions",style = "font-family: 'Source Sans Pro';"),
                    
                    p("First, go to the Model Setup tab. Here you must generate data with the button at the top, before you
                      
                      are able to access the other tabs. Once data is generated, the executive view and deep dive view show both the
                      
                      resulting workforce forecasts, and the details behind them.",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Platform is divided into three sections:",
                      
                      style = "font-family: 'Source Sans Pro';"),
                    
                    h4("1. Model Setup",style = "font-family: 'Source Sans Pro';"),
                    
                    p("The Model Setup must be visited first to check the default values, and adjust underlying assumptions if desired. 
Driving the Platform is data on both the current workforce state and the key levers that influence future demand: driver changes, 
automation, and L&D investment produced from both SABIC's data and market research. These values are preloaded for every individual function 
based on a combination of research and SABIC's own data, 
                      however should you wish to adjust these underlying assumptions in bulk you may do so under Default Parameter 
                      Setting section in this tab.",style = "font-family: 'Source Sans Pro';"),
                    
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
                    changes. You can navigate to different tabs",
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
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Department Selection</font></font>')
                  , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed=FALSE,
                  h5('Here you can select the Department/Function of your choice. You can change the default values of the selected filters under Default Parameter Setting. '),
                    column(4,h6("1. SABIC All"),actionButton("sall1","SABIC All",width = '100%')),
                    column(4,h6("2. Function/BU Type"), uiOutput('uityp')),column(4,h6("3. Region"),uiOutput('uirg')),column(4, h6('4. Function/Affiliate Name'), uiOutput('uiname')),
                    column(4, h6("5. Org. Structure"),uiOutput('uidiv')),column(4,h6("6. Job Family"),uiOutput('uijob')),column(4,h6("7. Job SubFamily"),uiOutput('uisjob'))
                    #https://stackoverflow.com/questions/51355878/how-to-text-wrap-choices-from-a-pickerinput-if-the-length-of-the-choices-are-lo
                )
              }),#filters
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
                  radioButtons("radio_t2r1", label = NULL,
                               choiceNames = list("Conservative Case","Base Case", "Optimistic Case"
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
                    
                    column(4,uiOutput('ui2br1')), 
                    column(4,uiOutput('ui2br2')), 
                    column(4,uiOutput('ui2br3'))
                    ),h6('The values of these scarios could be changed using the Rightsizing Position given under Parameter Setting  below.')
                )
              }),#Right-sizing Scenario Selection radio
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection: Automation & L&D Intervention</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$div(title="Conservative Case: When Demand is calculated without taking into account Automation and Capability enhancement
                           \n \n 
                           Base Case: When Demand is calculated taking into account Automation and Capability enhancement
                           \n \n 
                           Optimistic Case: When Demand is calculated taking into account
                           maximum Automation potential and Capability enhancement",
                  radioButtons("radio_t2r2", label = NULL,
                               choiceNames = list("Conservative Case", "Base Case","Optimistic Case"
                               ),
                               choiceValues = list(
                                 "con", "bc","opt"
                               ),selected = "con", inline = TRUE)
                ),width=12,
                box(
                  title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                  , status = "warning", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                  h5('Yearly L&D Investment'),
                  fluidRow( 
                  
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      sum(0)
                    )),
                    column(4,uiOutput('ui2iss2')),
                    column(4,uiOutput('ui2iss3'))),h6('The values of these scarios could be changed using the Rightsizing Position given under Parameter Setting  below'),
                  br(),
                  h5('Automation Impact (5 Years)'),
                  fluidRow( 
                    column(4,box(
                      title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                      , status = "primary", solidHeader = TRUE,width=12,
                      0
                    )),
                    column(4,uiOutput('ui2iai2')),
                    column(4,uiOutput('ui2iai3'))
                  ),h6('The values of these scarios could be changed using the Rightsizing Position given under Parameter Setting  below')
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
                    tabPanel("Rightsizing Position",
                             fluidRow({
                               box(
                                 title =HTML( '<font color=#ffffff> <font size="4">Right-sizing Adjustments</font></font>')
                                 , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,
                                 
                                 box(
                                   title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                                   , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                                   column(4,uiOutput('ui2trighto1')
                                   ),
                                   column(4,uiOutput('ui2trighto2')
                                   ),
                                   column(4,uiOutput('ui2trighto'))
                                 ),
                                 actionButton("save_t2sbl","Save"),
                                 uiOutput('ui2trightsl')
                               )
                             })
                    ),
                    tabPanel("Supply",
                               fluidRow({
                                 box(title=HTML( '<font color=#ffffff> <font size="4">Supply Rate Adjustments:</font></font>'), solidHeader = TRUE, status="primary",colour="light blue",
                                     width=12,
                                       actionButton("reset_shrk","Reset"),actionButton("save_nhr","Save"),
                                     br(),
                                     column(4,uiOutput("ui2sknhr1")),
                                     column(4,uiOutput("ui2sknhr2")),
                                     column(4,uiOutput("ui2sknhr3")),
                                     column(4,uiOutput("ui2sknhr4")),
                                     column(4,uiOutput("ui2sknhr5"))
                               )
                                 })
                             ),
                    tabPanel("Demand",
                             fluidRow({
                               box(status = "primary", width=12,
                                   fluidRow(
                                     uiOutput("KKosz")
                                     )
                               )
                               })
                             ),
                    tabPanel("L&D Investment",
                             fluidRow({
                               box(
                                 title =HTML( '<font color=#ffffff> <font size="4">YoY L&D Investment</font></font>')
                                 , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,
                                   actionButton("reset_sld","Reset")
                                   ,actionButton("save_sld","Save"),br(),
                                   # fluidRow(
                                   #   dataTableOutput(outputId = "tld")
                                   # ),
                                   fluidRow(
                                     column(6, uiOutput("ui2lid1")),
                                     column(6, uiOutput("ui2lid2")),
                                     column(6, uiOutput("ui2lid3")),
                                     column(6, uiOutput("ui2lid4")),
                                     column(6, uiOutput("ui2lid5"))
                                   )
                                   
                               )
                               
                             })
                             ),
                    tabPanel("Automation Impact",
                             fluidRow({
                               box(title =HTML( '<font color=#ffffff> <font size="4">YoY Automation Investment Impact</font></font>')
                                   , status = "primary", solidHeader = TRUE,width=12,collapsible = TRUE,
                                   actionButton("reset_sau","Reset")
                                   ,actionButton("save_sau","Save"),
                                   br(),br(),
                                   box(status = "primary", width=12,
                                     fluidRow(
                                     column(6,
                                            uiOutput('ui2sai1')),
                                     column(6,
                                            uiOutput('ui2sai2'))
                                     ),
                                   fluidRow(column(6,radioButtons("t2sau1q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),      
                                            column(6,radioButtons("t2sau2q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
                                              )),
                                   box(status = "primary", width=12,
                                       fluidRow(
                                         column(6,
                                                uiOutput('ui2sai3')),
                                         column(6,
                                                uiOutput('ui2sai4'))
                                       ),
                                   fluidRow(column(6,radioButtons("t2sau3q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE)),      
                                            column(6,radioButtons("t2sau4q","Quarter it will take effect from:",choices=c("Q1","Q2","Q3","Q4"),selected = "Q1",inline = TRUE))
                                   )),
                                   box(status = "primary", width=12,
                                     fluidRow(
                                       column(6,
                                              uiOutput('ui2sai5'))
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
                  column(4,h6("1. SABIC All"),actionButton("sall2","SABIC All",width = '100%')),
                  column(4,h6("2. Function/BU Type"), uiOutput('uityp3')),column(4,h6("3. Region"),uiOutput('uirg3')),column(4, h6('4. Function/Affiliate Name'), uiOutput('uiname3')),
                  column(4, h6("5. Org. Structure"),uiOutput('uidiv3')),column(4,h6("6. Job Family"),uiOutput('uijob3')),column(4,h6("7. Job SubFamily"),uiOutput('uisjob3'))
                ),
                box(status = "primary", solidHeader = TRUE,width=12,
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
                           radioButtons("radio_t3r1", label = NULL, 
                                        choiceNames = list("Conservative Case","Base Case", "Optimistic Case"
                                        ),
                                        choiceValues = list(
                                          "rs","r_crvt_s", "r_opt_s"
                                        ),selected = "rs", inline = TRUE)
                  )
                  ,width = 12,
                  box(icon = shiny::icon("line-chart"),
                      title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                      ,status="warning",  solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE, color="orange",
                      column(4,uiOutput('ui3br1')), 
                      column(4,uiOutput('ui3br2')), 
                      column(4,uiOutput('ui3br3'))
                  ),h6('The values of these scarios could be changed using the Rightsizing Position given under Parameter Setting  below.')
              )
              }),#Right-sizing Scenario Selection radio
              
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection: Automation & L&D Intervention</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  tags$div(title="Conservative Case: When Demand is calculated without taking into account Automation and Capability enhancement
                           \n \n 
                           Base Case: When Demand is calculated taking into account Automation and Capability enhancement
                           \n \n 
                           Optimistic Case: When Demand is calculated taking into account
                           maximum Automation potential and Capability enhancement",
                           radioButtons("radio_t3r2", label = NULL,
                                        choiceNames = list("Conservative Case", "Base Case","Optimistic Case"
                                        ),
                                        choiceValues = list(
                                          "con", "bc","opt"
                                        ),selected = "con", inline = TRUE)
                  ),width=12,
                  box(
                    title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                    , status = "warning", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                    h5('Yearly L&D Investment'),
                    fluidRow( 
                      
                      column(4,box(
                        title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                        , status = "primary", solidHeader = TRUE,width=12,
                        sum(0)
                      )),
                      column(4,uiOutput('ui3iss2')),
                      column(4,uiOutput('ui3iss3'))),
                    br(),
                    h5('Automation Impact (5 Years)'),
                    fluidRow( 
                      column(4,box(
                        title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                        , status = "primary", solidHeader = TRUE,width=12,
                        0
                      )),
                      column(4,uiOutput('ui3iai2')),
                      column(4,uiOutput('ui3iai3'))
                    )
                  )
              )
              }),#Scenario Selection radio
              
              fluidRow(dataTableOutput('lluk2')),
              
              fluidRow(
                box(title =HTML( '<font color=#ffffff> <font size="5">Demand Analysis : Consolidated View</font></font>')
                    , status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12,collapsed=TRUE,
                    
                      plotlyOutput("plot31")
                )
              ),
              fluidRow(
                box(title =HTML( '<font color=#ffffff> <font size="5">Demand Analysis: YoY View</font></font>')
                    , status = "primary", solidHeader = TRUE,collapsible = TRUE, width=12,collapsed=TRUE,
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
                box(title =HTML( '<font color=#ffffff> <font size="5">Scenario Comparison</font></font>')
                    , status = "primary", solidHeader = TRUE,collapsible = TRUE, width=12,collapsed=TRUE,
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
              
              fluidRow({
                box(
                  title =HTML( '<font color=#ffffff> <font size="3">Introduction</font></font>')
                  , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                  width=12,
                  h6('The Deep Dive section provide detailed demographic infromation and projections for any area of SABIC down to the Job Sub Family level.
                     First, select the area of organisation you wish to see below.')
              )
              }),
              box( status = "primary", solidHeader = TRUE,width=12,
                   column(6,h6("Function/BU Type:"),h6(textOutput('tab4ch1'))),column(6,h6("Region:"),h6(textOutput('tab4ch2')))
              ),
              
              tabsetPanel(id = "tabs2",
                          tabPanel(
                            title = "Section Selection",icon = icon("glyphicon glyphicon-saved",lib ="glyphicon"),
                            value = "page1",
                            
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
                                column(4,h6("1. SABIC All"),actionButton("sall4","SABIC All",width = '100%')),
                                column(4,h6("2. Function/BU Type"), uiOutput('uityp34')),column(4,h6("3. Region"),uiOutput('uirg34')),column(4, h6('4. Function/Affiliate Name'), uiOutput('uiname34')),
                                column(4, h6("5. Org. Structure"),uiOutput('uidiv34')),column(4,h6("6. Job Family"),uiOutput('uijob34')),column(4,h6("7. Job SubFamily"),uiOutput('uisjob34')),
                                br()
                                #https://stackoverflow.com/questions/51355878/how-to-text-wrap-choices-from-a-pickerinput-if-the-length-of-the-choices-are-lo
                              ),box( status = "primary", solidHeader = TRUE,width=12,
                                column(4,h6("Employment"),uiOutput('uiemp34')),column(4,h6("Segment"),uiOutput('uiseg34'))
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
                                         radioButtons("radio_t4r1", label = NULL,
                                                      choiceNames = list("Conservative Case","Base Case", "Optimistic Case"
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
                                    
                                    column(4,uiOutput('ui4br1')), 
                                    column(4,uiOutput('ui4br2')), 
                                    column(4,uiOutput('ui4br3'))
                                ),h6('The values of these scarios could be changed using the Rightsizing Position given under Parameter Setting  below.')
                            )
                            }),#Right-sizing Scenario Selection radio
                            
                            fluidRow({
                              box(
                                title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection: Automation & L&D Intervention</font></font>')
                                , status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                tags$div(title="Conservative Case: When Demand is calculated without taking into account Automation and Capability enhancement
                                         \n \n 
                                         Base Case: When Demand is calculated taking into account Automation and Capability enhancement
                                         \n \n 
                                         Optimistic Case: When Demand is calculated taking into account
                                         maximum Automation potential and Capability enhancement",
                                         radioButtons("radio_t4r2", label = NULL,
                                                      choiceNames = list("Conservative Case", "Base Case","Optimistic Case"
                                                      ),
                                                      choiceValues = list(
                                                        "con", "bc","opt"
                                                      ),selected = "con", inline = TRUE)
                                ),width=12,
                                box(
                                  title =HTML( '<font color=#ffffff> <font size="4">Default Values</font></font>')
                                  , status = "warning", solidHeader = TRUE,width=12,collapsible = TRUE,collapsed = TRUE,
                                  h5('Yearly L&D Investment'),
                                  fluidRow( 
                                    
                                    column(4,box(
                                      title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                                      , status = "primary", solidHeader = TRUE,width=12,
                                      sum(0)
                                    )),
                                    column(4,uiOutput('ui4iss2')),
                                    column(4,uiOutput('ui4iss3'))),
                                  br(),
                                  h5('Automation Impact (5 Years)'),
                                  fluidRow( 
                                    column(4,box(
                                      title =HTML( '<font color=#ffffff> <font size="3">Conservative Case</font></font>')
                                      , status = "primary", solidHeader = TRUE,width=12,
                                      0
                                    )),
                                    column(4,uiOutput('ui4iai2')),
                                    column(4,uiOutput('ui4iai3'))
                                  )
                                )
                            )
                            })#Scenario Selection radio
                          ),
                          tabPanel("Section Summary", icon = icon("line-chart"),
                                   fluidRow(
                                   box( title =HTML('<font color=#ffffff> <font size="5">Headcount Distribution</font></font>')
                                   , status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12, 
                                        plotlyOutput("plot42",height = '140px')
                                   ),
                                   box( title =HTML('<font color=#ffffff> <font size="5">Headcount Segmentation</font></font>')
                          , status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 12,
                                        box(status = "primary", solidHeader = TRUE,width=6,
                                        plotlyOutput("plot421")),
                          box(status = "primary", solidHeader = TRUE,width=6,
                                               plotlyOutput("plot423"))
                                   ),
                                   box( title =HTML('<font color=#ffffff> <font size="5">Age Distribution</font></font>')
              , status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 12,
              box(status = "primary", solidHeader = TRUE,width=6,
                                               plotlyOutput("plot424")),
              box(status = "primary", solidHeader = TRUE,width=6,
                                               plotlyOutput("plot422"))
                                   )
                                   ,
                                   box( title =HTML('<font color=#ffffff> <font size="5">Gender Distribution</font></font>')
      , status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 12,
      box(status = "primary", solidHeader = TRUE,width=6,
                                               plotlyOutput("plot425")),
      box(status = "primary", solidHeader = TRUE,width=6,
                                               plotlyOutput("plot426"))
                                   )
                          )),
                          tabPanel("Workforce Journey", icon = icon("line-chart"),
                                   fluidRow(
                                     box(status = "primary", width = 12,
                                         radioButtons("radio_t4", label = HTML('<font color=#454545>Workforce Journey</font>'),
                                                      
                                                      choiceNames = list("Demand Analysis : Consolidated View","Demand Analysis : YoY View"
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
                                   
                          )
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
  
  luk<-reactive({
    
    z=0
    if(input$radio_t2r1=="r_crvt_s"){
      z=luk_crvt$Data
    }
    else if(input$radio_t2r1=="r_opt_s"){
      z=luk_opt$Data
    }
    else if(input$radio_t2r1=="rs"){
      z=luk_as$Data
    }
    
    es1=segem[0,]
    es2=segem[0,]
    es3=segem[0,]
    es4=segem[0,]
    es5=segem[0,]
    es6=segem[0,]
    es7=segem[0,]
    es8=segem[0,]
    
    if("Core" %in% input$uipseg3){
      es1=filter(segem, Core>0)
    }
    if("Critical" %in% input$uipseg3){
      es2=filter(segem, Critical>0)
    }
    if("Specialist" %in% input$uipseg3){
      es3=filter(segem, Specialist>0)
    }
    if("Support" %in% input$uipseg3){
      es4=filter(segem, Support>0)
    }
    if("not_map" %in% input$uipseg3){
      es5=filter(segem, not_map>0)
    }
    if("Contractor" %in% input$uipemp3){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp3){
      es7=filter(segem, not_job>0)
    }
    if("Direct Hire" %in% input$uipemp3){
      es8=filter(segem, `Direct Hire`>0)
    }
    esa = rbind(es1,es2,es3,es4,es5)
    esb = rbind(es6,es7,es8)
    es <- intersect(esa,esb)
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType 
               & Region %in%  input$uiprg 
               & `CF/SBU/AFF Name` %in%  input$uipname 
               & Division %in%  input$uipdiv
               & Family %in%  input$uipjob 
               & `Sub Family` %in%  input$uipsjob
               & id %in% es$id
    )
    
  })
  
  
  observeEvent(input$sall1,{
    updatePickerInput(session, "uipType", selected = unique(luk1$`CF/SBU/AFF`[order(luk1$`CF/SBU/AFF`)]))
    updatePickerInput(session, "uiprg", selected = unique(luk1$Region[order(luk1$Region)]))
    updatePickerInput(session, "uipname", selected = unique(luk1$`CF/SBU/AFF Name`[order(luk1$`CF/SBU/AFF Name`)]))
    updatePickerInput(session, "uipdiv", selected = unique(luk1$Division[order(luk1$Division)]))
    updatePickerInput(session, "uipjob", selected = unique(luk1$Family[order(luk1$Family)]))
    updatePickerInput(session, "uipsjob", selected = unique(luk1$`Sub Family`[order(luk1$`Sub Family`)]))
  })
  observeEvent(input$sall2,{
    updatePickerInput(session, "uipType3", selected = unique(luk1$`CF/SBU/AFF`[order(luk1$`CF/SBU/AFF`)]))
    updatePickerInput(session, "uiprg3", selected = unique(luk1$Region[order(luk1$Region)]))
    updatePickerInput(session, "uipname3", selected = unique(luk1$`CF/SBU/AFF Name`[order(luk1$`CF/SBU/AFF Name`)]))
    updatePickerInput(session, "uipdiv3", selected = unique(luk1$Division[order(luk1$Division)]))
    updatePickerInput(session, "uipjob3", selected = unique(luk1$Family[order(luk1$Family)]))
    updatePickerInput(session, "uipsjob3", selected = unique(luk1$`Sub Family`[order(luk1$`Sub Family`)]))
  })
  
  observeEvent(input$radio_t2r1,{
    updateRadioButtons(session,'radio_t3r1', selected = input$radio_t2r1)
  })
  observeEvent(input$radio_t2r2,{
    updateRadioButtons(session,'radio_t3r2', selected = input$radio_t2r2)
  })
  
  observeEvent(input$radio_t2r1,{
    updateRadioButtons(session,'radio_t4r1', selected = input$radio_t2r1)
  })
  observeEvent(input$radio_t2r2,{
    updateRadioButtons(session,'radio_t4r2', selected = input$radio_t2r2)
  })
  
  observeEvent(input$sall4,{
    updatePickerInput(session, "uipType34", selected = unique(luk1$`CF/SBU/AFF`[order(luk1$`CF/SBU/AFF`)]))
    updatePickerInput(session, "uiprg34", selected = unique(luk1$Region[order(luk1$Region)]))
    updatePickerInput(session, "uipname34", selected = unique(luk1$`CF/SBU/AFF Name`[order(luk1$`CF/SBU/AFF Name`)]))
    updatePickerInput(session, "uipdiv34", selected = unique(luk1$Division[order(luk1$Division)]))
    updatePickerInput(session, "uipjob34", selected = unique(luk1$Family[order(luk1$Family)]))
    updatePickerInput(session, "uipsjob34", selected = unique(luk1$`Sub Family`[order(luk1$`Sub Family`)]))
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
    updateTextInput(session, "t2sk11", value = round(sum(luk3()$NHR19*luk3()$total)/sum(luk3()$total)*100,1))
    updateTextInput(session, "t2sk21", value = round(sum(luk3()$NHR20*luk3()$total)/sum(luk3()$total)*100,1))
    updateTextInput(session, "t2sk31", value = round(sum(luk3()$NHR21*luk3()$total)/sum(luk3()$total)*100,1))
    updateTextInput(session, "t2sk41", value = round(sum(luk3()$NHR22*luk3()$total)/sum(luk3()$total)*100,1))
    updateTextInput(session, "t2sk51", value = round(sum(luk3()$NHR23*luk3()$total)/sum(luk3()$total)*100,1))
    
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
  #ai
  observeEvent(input$reset_sau,{
    if(input$t2sau1q=="Q1"){
    updateTextInput(session, "t2sau1", value = round(sum(luk3()$yai19q1*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau1q=="Q2"){
      updateTextInput(session, "t2sau1", value = round(sum(luk3()$yai19q2*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau1q=="Q3"){
      updateTextInput(session, "t2sau1", value = round(sum(luk3()$yai19q3*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau1q=="Q4"){
      updateTextInput(session, "t2sau1", value = round(sum(luk3()$yai19q4*luk3()$total)/sum(luk3()$total)*100,3))
    }
    
    if(input$t2sau2q=="Q1"){
      updateTextInput(session, "t2sau2", value = round(sum(luk3()$yai20q1*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau2q=="Q2"){
      updateTextInput(session, "t2sau2", value = round(sum(luk3()$yai20q2*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau2q=="Q3"){
      updateTextInput(session, "t2sau2", value = round(sum(luk3()$yai20q3*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau2q=="Q4"){
      updateTextInput(session, "t2sau2", value = round(sum(luk3()$yai20q4*luk3()$total)/sum(luk3()$total)*100,3))
    }
    
    if(input$t2sau3q=="Q1"){
      updateTextInput(session, "t2sau3", value = round(sum(luk3()$yai21q1*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau3q=="Q2"){
      updateTextInput(session, "t2sau3", value = round(sum(luk3()$yai21q2*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau3q=="Q3"){
      updateTextInput(session, "t2sau3", value = round(sum(luk3()$yai21q3*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau3q=="Q4"){
      updateTextInput(session, "t2sau3", value = round(sum(luk3()$yai21q4*luk3()$total)/sum(luk3()$total)*100,3))
    }
    
    if(input$t2sau4q=="Q1"){
      updateTextInput(session, "t2sau4", value = round(sum(luk3()$yai22q1*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau4q=="Q2"){
      updateTextInput(session, "t2sau4", value = round(sum(luk3()$yai22q2*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau4q=="Q3"){
      updateTextInput(session, "t2sau4", value = round(sum(luk3()$yai22q3*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau4q=="Q4"){
      updateTextInput(session, "t2sau4", value = round(sum(luk3()$yai22q4*luk3()$total)/sum(luk3()$total)*100,3))
    }
    
    if(input$t2sau5q=="Q1"){
      updateTextInput(session, "t2sau5", value = round(sum(luk3()$yai23q1*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau5q=="Q2"){
      updateTextInput(session, "t2sau5", value = round(sum(luk3()$yai23q2*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau5q=="Q3"){
      updateTextInput(session, "t2sau5", value = round(sum(luk3()$yai23q3*luk3()$total)/sum(luk3()$total)*100,3))
    }
    else if(input$t2sau5q=="Q4"){
      updateTextInput(session, "t2sau5", value = round(sum(luk3()$yai23q4*luk3()$total)/sum(luk3()$total)*100,3))
    }
    
  })


  #####
  
  #data prep
  #####

  aoc<-reactive({
    z=0
    if(input$radio_t3r1=="r_crvt_s"){
      z=luk_crvt$Data
    }
    else if(input$radio_t3r1=="r_opt_s"){
      z=luk_opt$Data
    }
    else if(input$radio_t3r1=="rs"){
      z=luk_as$Data
    }
    #z <- z[z$Division %in% input$uipdiv3 ,]
    #z <- z[z$Region %in% input$uiprg3 ,]
    
    es1=segem[0,]
    es2=segem[0,]
    es3=segem[0,]
    es4=segem[0,]
    es5=segem[0,]
    es6=segem[0,]
    es7=segem[0,]
    es8=segem[0,]
    
    if("Core" %in% input$uipseg3){
      es1=filter(segem, Core>0)
    }
    if("Critical" %in% input$uipseg3){
      es2=filter(segem, Critical>0)
    }
    if("Specialist" %in% input$uipseg3){
      es3=filter(segem, Specialist>0)
    }
    if("Support" %in% input$uipseg3){
      es4=filter(segem, Support>0)
    }
    if("not_map" %in% input$uipseg3){
      es5=filter(segem, not_map>0)
    }
    if("Contractor" %in% input$uipemp3){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp3){
      es7=filter(segem, not_job>0)
    }
    if("Direct Hire" %in% input$uipemp3){
      es8=filter(segem, `Direct Hire`>0)
    }
    esa = rbind(es1,es2,es3,es4,es5)
    esb = rbind(es6,es7,es8)
    es <- intersect(esa,esb)
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType3 
               & Region %in%  input$uiprg3 
               & `CF/SBU/AFF Name` %in%  input$uipname3 
               & Division %in%  input$uipdiv3
               & Family %in%  input$uipjob3 
               & `Sub Family` %in%  input$uipsjob3 
               & id %in% es$id
    )
    if("Contractor" %in% input$uipemp3){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp3){
      es7=filter(segem, not_job>0)
    }
    # if(("Direct Hire" %in% input$uipemp3)==FALSE){ #######3???
    #   es8=filter(segem, `Direct Hire`>0)
    # }
    # 
  })
  
  as_dat<-reactive({
    
    z=luk_as$Data
    
    es1=segem[0,]
    es2=segem[0,]
    es3=segem[0,]
    es4=segem[0,]
    es5=segem[0,]
    es6=segem[0,]
    es7=segem[0,]
    es8=segem[0,]
    
    if("Core" %in% input$uipseg3){
      es1=filter(segem, Core>0)
    }
    if("Critical" %in% input$uipseg3){
      es2=filter(segem, Critical>0)
    }
    if("Specialist" %in% input$uipseg3){
      es3=filter(segem, Specialist>0)
    }
    if("Support" %in% input$uipseg3){
      es4=filter(segem, Support>0)
    }
    if("not_map" %in% input$uipseg3){
      es5=filter(segem, not_map>0)
    }
    if("Contractor" %in% input$uipemp3){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp3){
      es7=filter(segem, not_job>0)
    }
    if("Direct Hire" %in% input$uipemp3){
      es8=filter(segem, `Direct Hire`>0)
    }
    esa = rbind(es1,es2,es3,es4,es5)
    esb = rbind(es6,es7,es8)
    es <- intersect(esa,esb)
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType3 
               & Region %in%  input$uiprg3 
               & `CF/SBU/AFF Name` %in%  input$uipname3 
               & Division %in%  input$uipdiv3
               & Family %in%  input$uipjob3 
               & `Sub Family` %in%  input$uipsjo3
               & id %in% es$id
    )
    
  })
  opt_dat<-reactive({
    
    z=luk_opt$Data
    
    es1=segem[0,]
    es2=segem[0,]
    es3=segem[0,]
    es4=segem[0,]
    es5=segem[0,]
    es6=segem[0,]
    es7=segem[0,]
    es8=segem[0,]
    
    if("Core" %in% input$uipseg3){
      es1=filter(segem, Core>0)
    }
    if("Critical" %in% input$uipseg3){
      es2=filter(segem, Critical>0)
    }
    if("Specialist" %in% input$uipseg3){
      es3=filter(segem, Specialist>0)
    }
    if("Support" %in% input$uipseg3){
      es4=filter(segem, Support>0)
    }
    if("not_map" %in% input$uipseg3){
      es5=filter(segem, not_map>0)
    }
    if("Contractor" %in% input$uipemp3){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp3){
      es7=filter(segem, not_job>0)
    }
    if("Direct Hire" %in% input$uipemp3){
      es8=filter(segem, `Direct Hire`>0)
    }
    esa = rbind(es1,es2,es3,es4,es5)
    esb = rbind(es6,es7,es8)
    es <- intersect(esa,esb)
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType3 
               & Region %in%  input$uiprg3 
               & `CF/SBU/AFF Name` %in%  input$uipname3 
               & Division %in%  input$uipdiv3
               & Family %in%  input$uipjob3 
               & `Sub Family` %in%  input$uipsjob3
               & id %in% es$id
    )
    
  })
  crvt_dat<-reactive({
    
    z=luk_crvt$Data
    
    es1=segem[0,]
    es2=segem[0,]
    es3=segem[0,]
    es4=segem[0,]
    es5=segem[0,]
    es6=segem[0,]
    es7=segem[0,]
    es8=segem[0,]
    
    if("Core" %in% input$uipseg3){
      es1=filter(segem, Core>0)
    }
    if("Critical" %in% input$uipseg3){
      es2=filter(segem, Critical>0)
    }
    if("Specialist" %in% input$uipseg3){
      es3=filter(segem, Specialist>0)
    }
    if("Support" %in% input$uipseg3){
      es4=filter(segem, Support>0)
    }
    if("not_map" %in% input$uipseg3){
      es5=filter(segem, not_map>0)
    }
    if("Contractor" %in% input$uipemp3){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp3){
      es7=filter(segem, not_job>0)
    }
    if("Direct Hire" %in% input$uipemp3){
      es8=filter(segem, `Direct Hire`>0)
    }
    esa = rbind(es1,es2,es3,es4,es5)
    esb = rbind(es6,es7,es8)
    es <- intersect(esa,esb)
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType3 
               & Region %in%  input$uiprg3 
               & `CF/SBU/AFF Name` %in%  input$uipname3 
               & Division %in%  input$uipdiv3
               & Family %in%  input$uipjob3 
               & `Sub Family` %in%  input$uipsjob3
               & id %in% es$id
    )
    
  })
  
  coa4<-reactive({
    z=0
    if(input$radio_t4r1=="r_crvt_s"){
      z=luk_crvt$Data
    }
    else if(input$radio_t4r1=="r_opt_s"){
      z=luk_opt$Data
    }
    else if(input$radio_t4r1=="rs"){
      z=luk_as$Data
    }
    #z <- z[z$Division %in% input$uipdiv3 ,]
    #z <- z[z$Region %in% input$uiprg3 ,]
    
    es1=segem[0,]
    es2=segem[0,]
    es3=segem[0,]
    es4=segem[0,]
    es5=segem[0,]
    es6=segem[0,]
    es7=segem[0,]
    es8=segem[0,]
    
    if("Core" %in% input$uipseg34){
      es1=filter(segem, Core>0)
    }
    if("Critical" %in% input$uipseg34){
      es2=filter(segem, Critical>0)
    }
    if("Specialist" %in% input$uipseg34){
      es3=filter(segem, Specialist>0)
    }
    if("Support" %in% input$uipseg34){
      es4=filter(segem, Support>0)
    }
    if("not_map" %in% input$uipseg34){
      es5=filter(segem, not_map>0)
    }
    if("Contractor" %in% input$uipemp34){
      es6=filter(segem, Contractor>0)
    }
    if("NA" %in% input$uipemp34){
      es7=filter(segem, not_job>0)
    }
    if("Direct Hire" %in% input$uipemp34){
      es8=filter(segem, `Direct Hire`>0)
    }
    esa = rbind(es1,es2,es3,es4,es5)
    esb = rbind(es6,es7,es8)
    es <- intersect(esa,esb)
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType34
               & Region %in%  input$uiprg34
               & `CF/SBU/AFF Name` %in%  input$uipname34
               & Division %in%  input$uipdiv34
               & Family %in%  input$uipjob34
               & `Sub Family` %in%  input$uipsjob34
               & id %in% es$id
    )
    
  })
  
  luk2<-reactiveValues()
  luk2$Data=data.table(luk1)
  newEntry1 <- observe(luk2$Data 
  )
  
  luk3<-reactive({
    
    
    zxc <- filter(luk2$Data, `CF/SBU/AFF` %in%  input$uipType 
                  & Region %in%  input$uiprg 
                  & `CF/SBU/AFF Name` %in%  input$uipname 
                  & Division %in%  input$uipdiv
                  & Family %in%  input$uipjob 
                  & `Sub Family` %in%  input$uipsjob
    )
    
  })
  
  luk4<-reactive({
    
    z=0
    if(input$radio_t2r1=="r_crvt_s"){
      z=luk_crvt$Data
    }
    else if(input$radio_t2r1=="r_opt_s"){
      z=luk_opt$Data
    }
    else if(input$radio_t2r1=="rs"){
      z=luk_as$Data
    }
    
    z<- filter(z, `CF/SBU/AFF` %in%  input$uipType 
               & Region %in%  input$uiprg 
               & `CF/SBU/AFF Name` %in%  input$uipname 
               & Division %in%  input$uipdiv
               & Family %in%  input$uipjob 
               & `Sub Family` %in%  input$uipsjob
    )
    
  })
  
  observeEvent(input$save_sld,
               {
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('ld1')]<-input$t2sld11
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('ld2')]<-input$t2sld12
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('ld3')]<-input$t2sld13
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('ld4')]<-input$t2sld14
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('ld5')]<-input$t2sld15
                 
               }
  )
  
  observeEvent(input$save_nhr,
               {
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,'NHR19']<-input$t2sk11/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('NHR20')]<-input$t2sk21/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('NHR21')]<-input$t2sk31/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('NHR22')]<-input$t2sk41/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('NHR23')]<-input$t2sk51/100
                 
                 # 
                 # updateTextInput(session, "t2sk11", value = round(mean(luk3()$NHR19)*100,1))
                 # updateTextInput(session, "t2sk21", value = round(sum(nhr[3,2:5])*25,1))
                 # updateTextInput(session, "t2sk31", value = round(sum(nhr[4,2:5])*25,1))
                 # updateTextInput(session, "t2sk41", value = round(sum(nhr[5,2:5])*25,1))
                 # updateTextInput(session, "t2sk51", value = round(sum(nhr[6,2:5])*25,1))
                 
               }
  )
  
  observeEvent(input$save_t2sbl,
               {
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('opt')]<-luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                                                                                   & Region %in%  input$uiprg 
                                                                                   & `CF/SBU/AFF Name` %in%  input$uipname 
                                                                                   & Division %in%  input$uipdiv
                                                                                   & Family %in%  input$uipjob 
                                                                                   & `Sub Family` %in%  input$uipsjob,c('as_is_s')]*(1-input$inputst2bl/100)
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('crvt')]<-(luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                                                                                    & Region %in%  input$uiprg 
                                                                                    & `CF/SBU/AFF Name` %in%  input$uipname 
                                                                                    & Division %in%  input$uipdiv
                                                                                    & Family %in%  input$uipjob 
                                                                                    & `Sub Family` %in%  input$uipsjob,c('as_is_s')]+luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                                                                                                                                               & Region %in%  input$uiprg 
                                                                                                                                               & `CF/SBU/AFF Name` %in%  input$uipname 
                                                                                                                                               & Division %in%  input$uipdiv
                                                                                                                                               & Family %in%  input$uipjob 
                                                                                                                                               & `Sub Family` %in%  input$uipsjob,c('opt')])/2

                 
               }
  )
  
  observeEvent(input$save_sau,
               {
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('yai19q1','yai19q2','yai19q3','yai19q4')]<-input$t2sau1/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('yai20q1','yai20q2','yai20q3','yai20q4')]<-input$t2sau2/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('yai21q1','yai21q2','yai21q3','yai21q4')]<-input$t2sau3/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('yai22q1','yai22q2','yai22q3','yai22q4')]<-input$t2sau4/100
                 
                 luk2$Data[`CF/SBU/AFF` %in%  input$uipType 
                           & Region %in%  input$uiprg 
                           & `CF/SBU/AFF Name` %in%  input$uipname 
                           & Division %in%  input$uipdiv
                           & Family %in%  input$uipjob 
                           & `Sub Family` %in%  input$uipsjob,c('yai23q1','yai23q2','yai23q3','yai23q4')]<-input$t2sau5/100
                 
               }
  )
  
  #####
  
  #filter
  #####
  {
  dataset<-reactive({
    z <- luk1
    z <- z[z$Family %in%  if(is.null(input$variables2)){unique(luk1$Family)} else (input$variables2) ,]
  })
  outVar <- reactive({
    vars <- unique(luk1$Family)
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
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
    })
    bu2<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
    })
    bu3<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname ,]
    })
    bu4<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType ,]
      z <- z[z$Region %in%  input$uiprg ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname ,]
      z <- z[z$Division %in%  input$uipdiv ,]
    })
    bu5<-reactive({
      z <- luk1[,2:8]
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
      pickerInput("uipType",label=NULL,choices=unique(luk1$`CF/SBU/AFF`[order(luk1$`CF/SBU/AFF`)]),
                  selected=unique(luk1$`CF/SBU/AFF`),
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
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
    })
    bu23<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
    })
    bu33<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname3 ,]
    })
    bu43<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType3 ,]
      z <- z[z$Region %in%  input$uiprg3 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname3 ,]
      z <- z[z$Division %in%  input$uipdiv3 ,]
    })
    bu53<-reactive({
      z <- luk1[,2:8]
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
      pickerInput("uipType3",label=NULL,choices=unique(luk1$`CF/SBU/AFF`[order(luk1$`CF/SBU/AFF`)]),
                  selected=unique(luk1$`CF/SBU/AFF`),
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
      pickerInput("uipemp3",label=NULL,choices=c('Contractor', 'Direct Hire','NA'),
                  selected=c('Contractor', 'Direct Hire','NA'),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uisjob3 = renderUI({
      pickerInput("uipsjob3",label=NULL,choices=outsjob3(),
                  selected= outsjob3(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    
  }#newt3
  {
    bu134<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType34 ,]
    })
    bu234<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType34 ,]
      z <- z[z$Region %in%  input$uiprg34 ,]
    })
    bu334<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType34 ,]
      z <- z[z$Region %in%  input$uiprg34 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname34 ,]
    })
    bu434<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType34 ,]
      z <- z[z$Region %in%  input$uiprg34 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname34 ,]
      z <- z[z$Division %in%  input$uipdiv34 ,]
    })
    bu534<-reactive({
      z <- luk1[,2:8]
      z <- z[z$`CF/SBU/AFF` %in%  input$uipType34 ,]
      z <- z[z$Region %in%  input$uiprg34 ,]
      z <- z[z$`CF/SBU/AFF Name` %in%  input$uipname34 ,]
      z <- z[z$Division %in%  input$uipdiv34 ,]
      z <- z[z$Family %in%  input$uipjob34 ,]
    })
    outrg34 <- reactive({
      vars134 <- unique(bu134()$Region[order(bu134()$Region)])
      return(vars134)
    })
    outnm34 <- reactive({
      vars234 <- unique(bu234()$`CF/SBU/AFF Name`[order(bu234()$`CF/SBU/AFF Name`)])
      return(vars234) 
    })
    outdiv34 <- reactive({
      vars334 <- unique(bu334()$Division[order(bu334()$Division)])
      return(vars334) 
    })
    outjob34 <- reactive({
      vars434 <- unique(bu434()$Family[order(bu434()$Family)])
      return(vars434) 
    })
    outsjob34 <- reactive({
      vars534 <- unique(bu534()$`Sub Family`[order(bu534()$`Sub Family`)])
      return(vars534) 
    })
    
    output$uityp34 = renderUI({
      pickerInput("uipType34",label=NULL,choices=unique(luk1$`CF/SBU/AFF`[order(luk1$`CF/SBU/AFF`)]),
                  selected=unique(luk1$`CF/SBU/AFF`),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    output$uirg34 = renderUI({
      pickerInput("uiprg34",label=NULL,choices=outrg34(),
                  selected=outrg34(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uiname34 = renderUI({
      pickerInput("uipname34",label=NULL,choices=outnm34(),
                  selected= outnm34(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    output$uidiv34 = renderUI({
      pickerInput("uipdiv34",label=NULL,choices=outdiv34(),
                  selected= outdiv34(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    output$uijob34 = renderUI({
      pickerInput("uipjob34",label=NULL,choices=outjob34(),
                  selected= outjob34(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    
    output$uiseg34 = renderUI({
      pickerInput("uipseg34",label=NULL,choices=c('Core', 'Specialist','Support','Critical','Not Map'),
                  selected=c('Core', 'Specialist','Support','Critical','Not Map'),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uiemp34 = renderUI({
      pickerInput("uipemp34",label=NULL,choices=c('Contract', 'Direct Hire','NA'),
                  selected=c('Contract', 'Direct Hire','NA'),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
    })
    
    output$uisjob34 = renderUI({
      pickerInput("uipsjob34",label=NULL,choices=outsjob34(),
                  selected= outsjob34(),
                  options = list( `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3" ),  multiple = TRUE)
    })
    
  }#newt4
  #####
  #text
  ######
  output$tab4ch1<-renderText(paste(shQuote(input$uipType34), collapse=", ")   )
  output$tab4ch2<-renderText(paste(shQuote(input$uiprg34), collapse=", ")   )
  output$tab2pright<-renderText( round(sum(rs$opt)*(1-input$inputst2bl/100+sum(nhr[1,-1])/4))  )
  
  output$ui2trighto1 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Conservative (No Change)</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(luk3()$as_is_s) ))
  )
  })
  
  output$ui2trighto2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(luk3()$opt) ))
  )
  })
  
  output$ui2trighto = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Change Optimistic Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round(sum(luk3()$as_is_s)-input$inputst2bl*sum(luk3()$as_is_s)/100))
  )
  })
  
  output$ui2trightsl = renderUI({
    sliderInput("inputst2bl", "Headcount:",
                min = 0, max = 100,
                value = (sum(luk3()$as_is_s)-sum(luk3()$opt))/sum(luk3()$as_is_s)*100,pre = "%", step = 0.001, ticks = FALSE)
  })
  
  output$ui2br1 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Conservative (No Change)</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(luk3()$as_is_s) ))
  )
  })
  output$ui2br2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(luk3()$crvt) ))
  )
  })
  output$ui2br3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(luk3()$opt) ))
  )
  })
  
  
  output$ui2iss2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    round((mean(luk3()[,c('ld1')])+mean(luk3()[,c('ld2')])+mean(luk3()[,c('ld3')])+mean(luk3()[,c('ld4')])+mean(luk3()[,c('ld5')]))/5)
  )
  })
  output$ui2iss3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    round((mean(luk3()[,c('ld1')])+mean(luk3()[,c('ld2')])+mean(luk3()[,c('ld3')])+mean(luk3()[,c('ld4')])+mean(luk3()[,c('ld5')]))/5)
  )
  })
  # automation
  output$ui2iai2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    paste(round(sum(luk4()$yai23q4*luk4()$sels)/sum(luk4()$sels)*100,3),'%')
  )
  })
  output$ui2iai3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    paste(round(sum(luk4()$yai23q4*luk4()$sels)/sum(luk4()$sels)*200,3),'%')
  )
  })
  #l&d
  output$ui2lid1 = renderUI({sliderInput("t2sld11","Year 1 L&D Investment:",
                                         min=0,max=10000,value= mean(luk3()$ld1),post="$")
  })
  output$ui2lid2 = renderUI({sliderInput("t2sld12","Year 2 L&D Investment:",
                                         min=0,max=10000,value= mean(luk3()$ld2),post="$")
  })
  output$ui2lid3 = renderUI({sliderInput("t2sld13","Year 3 L&D Investment:",
                                         min=0,max=10000,value= mean(luk3()$ld3),post="$")
  })
  output$ui2lid4 = renderUI({sliderInput("t2sld14","Year 4 L&D Investment:",
                                         min=0,max=10000,value= mean(luk3()$ld4),post="$")
  })
  output$ui2lid5 = renderUI({sliderInput("t2sld15","Year 5 L&D Investment:",
                                         min=0,max=10000,value= mean(luk3()$ld5),post="$")
  })
  #aut par
  output$ui2sai1 = renderUI({
  sliderInput("t2sau1","Year 1 Automation Level:",
              min=0,max=100,value= 
                case_when(input$t2sau1q=="Q1" ~ round(sum(luk3()$yai19q1*luk3()$total)/sum(luk3()$total)*100,4),
                          input$t2sau1q=="Q2" ~ round(sum(luk3()$yai19q2*luk3()$total)/sum(luk3()$total)*100,4),
                          input$t2sau1q=="Q3" ~ round(sum(luk3()$yai19q3*luk3()$total)/sum(luk3()$total)*100,4),
                          input$t2sau1q=="Q4" ~ round(sum(luk3()$yai19q4*luk3()$total)/sum(luk3()$total)*100,4)
                          )
              ,step = 0.001,post=" %")
  })
  output$ui2sai2 = renderUI({
    sliderInput("t2sau2","Year 2 Automation Level:",
                min=0,max=100,value= 
                  case_when(input$t2sau2q=="Q1" ~ round(sum(luk3()$yai20q1*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau2q=="Q2" ~ round(sum(luk3()$yai20q2*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau2q=="Q3" ~ round(sum(luk3()$yai20q3*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau2q=="Q4" ~ round(sum(luk3()$yai20q4*luk3()$total)/sum(luk3()$total)*100,4)
                  )
                ,step = 0.001,post=" %")
  })
  output$ui2sai3 = renderUI({
    sliderInput("t2sau3","Year 3 Automation Level:",
                min=0,max=100,value= 
                  case_when(input$t2sau3q=="Q1" ~ round(sum(luk3()$yai21q1*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau3q=="Q2" ~ round(sum(luk3()$yai21q2*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau3q=="Q3" ~ round(sum(luk3()$yai21q3*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau3q=="Q4" ~ round(sum(luk3()$yai21q4*luk3()$total)/sum(luk3()$total)*100,4)
                  )
                ,step = 0.001,post=" %")
  })
  output$ui2sai4 = renderUI({
    sliderInput("t2sau4","Year 4 Automation Level:",
                min=0,max=100,value= 
                  case_when(input$t2sau4q=="Q1" ~ round(sum(luk3()$yai22q1*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau4q=="Q2" ~ round(sum(luk3()$yai22q2*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau4q=="Q3" ~ round(sum(luk3()$yai22q3*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau4q=="Q4" ~ round(sum(luk3()$yai22q4*luk3()$total)/sum(luk3()$total)*100,4)
                  )
                ,step = 0.001,post=" %")
  })
  output$ui2sai5 = renderUI({
    sliderInput("t2sau5","Year 5 Automation Level:",
                min=0,max=100,value= 
                  case_when(input$t2sau5q=="Q1" ~ round(sum(luk3()$yai23q1*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau5q=="Q2" ~ round(sum(luk3()$yai23q2*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau5q=="Q3" ~ round(sum(luk3()$yai23q3*luk3()$total)/sum(luk3()$total)*100,4),
                            input$t2sau5q=="Q4" ~ round(sum(luk3()$yai23q4*luk3()$total)/sum(luk3()$total)*100,4)
                  )
                ,step = 0.001,post=" %")
  })
  #nhr
  output$ui2sknhr1 = renderUI({knobInput(
    inputId = "t2sk11",label = "Year 1 NHR:",thickness=0.3, width=100, height=100,
    value = round(sum(luk3()$NHR19*luk3()$total)/sum(luk3()$total)*100,1),min = -100, max = 100, step = 0.1, #displayPrevious = TRUE,lineCap = "round",
    fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")
  })
  output$ui2sknhr2 = renderUI({knobInput(
    inputId = "t2sk21",label = "Year 2 NHR:",thickness=0.3, width=100, height=100,
    value = round(sum(luk3()$NHR20*luk3()$total)/sum(luk3()$total)*100,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
    fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")
  })
  output$ui2sknhr3 = renderUI({knobInput(
    inputId = "t2sk31",label = "Year 3 NHR:",thickness=0.3, width=100, height=100,
    value = round(sum(luk3()$NHR21*luk3()$total)/sum(luk3()$total)*100,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
    fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")
  })
  output$ui2sknhr4 = renderUI({knobInput(
    inputId = "t2sk41",label = "Year 4 NHR:",thickness=0.3, width=100, height=100,
    value = round(sum(luk3()$NHR22*luk3()$total)/sum(luk3()$total)*100,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
    fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")
  })
  output$ui2sknhr5 = renderUI({knobInput(
    inputId = "t2sk51",label = "Year 5 NHR:",thickness=0.3, width=100, height=100,
    value = round(sum(luk3()$NHR23*luk3()$total)/sum(luk3()$total)*100,1),min = -100, max = 100, step = 0.1, displayPrevious = TRUE,lineCap = "round",
    fgColor = "gold",angleOffset = -135,angleArc=270,inputColor = "gold")
  })
  
  #tab3
  output$ui3br1 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Conservative (No Change)</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(aoc()$as_is_s) ))
  )
  })
  output$ui3br2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(aoc()$crvt) ))
  )
  })
  output$ui3br3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(aoc()$opt) ))
  )
  })
  
  output$ui3iss2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    round((mean(aoc()[,c('ld1')])+mean(aoc()[,c('ld2')])+mean(aoc()[,c('ld3')])+mean(aoc()[,c('ld4')])+mean(aoc()[,c('ld5')]))/5)
  )
  })
  output$ui3iss3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    round((mean(aoc()[,c('ld1')])+mean(aoc()[,c('ld2')])+mean(aoc()[,c('ld3')])+mean(aoc()[,c('ld4')])+mean(aoc()[,c('ld5')]))/5)
  )
  })
  # automation
  output$ui3iai2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    paste(round(sum(aoc()$yai23q4*aoc()$sels)/sum(aoc()$sels)*100,3),'%')
  )
  })
  output$ui3iai3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    paste(round(sum(aoc()$yai23q4*aoc()$sels)/sum(aoc()$sels)*200,3),'%')
  )
  })
  
  #tab4
  output$ui4br1 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Conservative (No Change)</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(coa4()$as_is_s) ))
  )
  })
  output$ui4br2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(coa4()$crvt) ))
  )
  })
  output$ui4br3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Headcount</font></font>')
    , status = "primary", solidHeader = TRUE,width = '100%',
    h5(round( sum(coa4()$opt) ))
  )
  })
  
  output$ui4iss2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    round((mean(coa4()[,c('ld1')])+mean(coa4()[,c('ld2')])+mean(coa4()[,c('ld3')])+mean(coa4()[,c('ld4')])+mean(coa4()[,c('ld5')]))/5)
  )
  })
  output$ui4iss3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    round((mean(coa4()[,c('ld1')])+mean(coa4()[,c('ld2')])+mean(coa4()[,c('ld3')])+mean(coa4()[,c('ld4')])+mean(coa4()[,c('ld5')]))/5)
  )
  })
  # automation
  output$ui4iai2 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Base Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    paste(round(sum(coa4()$yai23q4*coa4()$sels)/sum(coa4()$sels)*100,3),'%')
  )
  })
  output$ui4iai3 = renderUI({box(
    title =HTML( '<font color=#ffffff> <font size="3">Optimistic Case</font></font>')
    , status = "primary", solidHeader = TRUE,width=12,
    paste(round(sum(coa4()$yai23q4*coa4()$sels)/sum(coa4()$sels)*200,3),'%')
  )
  })

  ######
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
  x <- c(rep('5.Business Demand(+)',6))
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
  
  x <- c(rep('5.Business Demand(-)',6))
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
    add_annotations(text = round(sum(aoc()$total)), bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(1, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data0$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                    x = '1.Current<br>headcount',
                    y = sum(aoc()$total),
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
                    y = sum(data1$revenue)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('3.Optimization',6)),
                y = c(data2$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = round(sum(aoc()$subt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[3],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('4.Y1 E',6)),
                y = c(0,data3$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  )%>%
    add_annotations(text = round(sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '4.Y1 E',
                    y = sum(data3$revenue)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = sum(data3$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = sum(data3$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = sum(data3$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '4.Y1 E',
                    y = sum(data3$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data3$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                    x = '4.Y1 E',
                    y = sum(data3$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('5.Business Demand(+)',6)),
                y = c(data4$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  ) %>%
    add_annotations(text = round(sum(data4$revenue)-sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '5.Business Demand(+)',
                    y = sum(data4$revenue)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '5.Business Demand(+)',
                    y = sum(data4$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '5.Business Demand(+)',
                    y = sum(data4$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '5.Business Demand(+)',
                    y = sum(data4$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '5.Business Demand(+)',
                    y = sum(data4$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '5.Business Demand(+)',
                    y = sum(data4$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)
  
  p <-add_trace(p,
                x = c(rep('6.Business Demand(-)',6)),
                y = c(data4n$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=FALSE
  ) %>%
    add_annotations(text = -round(sum(data4n$revenue[-1]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '6.Business Demand(-)',
                    y = round(sum(aoc()$dds23),0)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4n$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '6.Business Demand(-)',
                    y = sum(data4$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4n$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '6.Business Demand(-)',
                    y = sum(data4$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4n$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '6.Business Demand(-)',
                    y = sum(data4$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4n$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '6.Business Demand(-)',
                    y = sum(data4$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data4n$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '6.Business Demand(-)',
                    y = sum(data4$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
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
                    y = sum(data5$revenue)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data5$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '7.Automation<br>Reduction',
                    y = sum(data4$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data5$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '7.Automation<br>Reduction',
                    y = sum(data4$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data5$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '7.Automation<br>Reduction',
                    y = sum(data4$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data5$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '7.Automation<br>Reduction',
                    y = sum(data4$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data5$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '7.Automation<br>Reduction',
                    y = sum(data4$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
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
                    y = round(sum(data6$revenue),0)*1.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data6$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '8.Capability<br>Enhancement',
                    y = sum(data6$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data6$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '8.Capability<br>Enhancement',
                    y = sum(data6$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data6$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '8.Capability<br>Enhancement',
                    y = sum(data6$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data6$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '8.Capability<br>Enhancement',
                    y = sum(data6$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data6$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '8.Capability<br>Enhancement',
                    y = sum(data6$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)
  

  p <-add_trace(p,
                x = c(rep('9.Y5 E',6)),
                y = c(0,data7$revenue),
                colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                type='bar', showlegend=T
  ) %>%
    layout(barmode = 'stack',margin = list(b = 150),legend = list(y= 40000,orientation = 'h', traceorder='normal'), yaxis=list(title='Headcount'))%>%
    add_annotations(text = round(sum(fte55),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                    x = '9.Y5 E',
                    y = round(sum(fte55)*1.2,0),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(50, 1, 1, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = round(sum(fte55)*0.2,0),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = sum(fte55)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = sum(fte55)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '9.Y5 E',
                    y = sum(fte55)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data7$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                    x = '9.Y5 E',
                    y = sum(fte55),
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
  p <-add_lines(p, x=c('4.Y1 E','5.Business Demand(+)') , y= c(sum(data3$revenue), sum(data3$revenue)), name = "",
                line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('5.Business Demand(+)','6.Business Demand(-)') , y= c(sum(data4$revenue), sum(data4$revenue)), name = "",
                line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
  p <-add_lines(p, x=c('6.Business Demand(-)','7.Automation<br>Reduction') , y= c(data4n$revenue[1], data4n$revenue[1]), name = "",
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
      add_annotations(text = round(sum(aoc()$total)), bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '1.Current<br>headcount',
                      y = sum(aoc()$total),
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
                      y = sum(data1$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
    add_annotations(text = round(data1$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '2.Upsizing',
                    y = sum(data1$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('3.Optimization',6)),
                  y = c(data2$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(aoc()$subt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.2,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[3],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.4,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.6,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue)*0.8,
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color = 'rgba(255, 255, 255, 1.0)'),
                    showarrow = FALSE)%>%
    add_annotations(text = round(data2$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                    x = '3.Optimization',
                    y = sum(data1$revenue),
                    xref = "x",
                    font = list(family = 'Arial',
                                size = 12,
                                color ='rgba(255, 255, 255, 1.0)' ),
                    showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('4.Y0 E',6)),
                  y = c(0,data3$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '4.Y0 E',
                      y = sum(data3$revenue),
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
                  type='bar', showlegend=T
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100),legend = list(y= 40000,orientation = 'h', traceorder='normal'), yaxis=list(title='Headcount'))%>%
      add_annotations(text = round(sum(data4$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '5.Y1 E',
                      y = round(sum(data4$revenue)*1.2,0),
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
                  type='bar', showlegend=F
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data4n$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '6.Y2 E',
                      y = round(sum(data4n$revenue)*1.2,0),
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
                      y = round(sum(data5$revenue)*1.2,0),
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
                      y = round(sum(data6$revenue)*1.2,0),
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
                      y = round(sum(data7$revenue)*1.2,0),
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
  
  output$plot33<- renderPlotly({
    
    x <- c('Y1 E', 'Y2 E', 'Y3 E', 'Y4 E', 'Y5 E')
    As_is <- c(sum(as_dat()$fte1), sum(as_dat()$fte2),sum(as_dat()$fte3),sum(as_dat()$fte4),sum(as_dat()$fte5))
    Opt <- c(sum(opt_dat()$fte1), sum(opt_dat()$fte2),sum(opt_dat()$fte3),sum(opt_dat()$fte4),sum(opt_dat()$fte5))
    
    Con <- c(sum(crvt_dat()$fte1), sum(crvt_dat()$fte2),sum(crvt_dat()$fte3),sum(crvt_dat()$fte4),sum(crvt_dat()$fte5))
    
    data <- data.frame(x, Opt, Con, As_is)
    plot_ly(data, x = ~x, y = ~As_is, type = 'bar',name='Conservative', marker = list(color = 'deepskyblue'))%>%
      add_trace(y = ~Opt,name='Optimistic', marker = list(color = 'darkorange')) %>%
      add_trace(y = ~Con,name='Base Case', marker = list(color = 'midnightblue'))%>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(title = "Headcount"))%>%
      add_annotations(text = round(Con,0),xshift=23,xanchor='left',bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='midnightblue',
                      x = x,
                      y = Con*(1+2/40),
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
                      y = As_is*(1+2/40),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)
    
  })# comparasion scenario
  
  output$plot34<- renderPlotly({
    
    x <- c(rep(c('Y0 E','Y1 E','Y2 E','Y3 E','Y4 E','Y5 E'),5))
    
    core1 <- c(sum(aoc()$Core*aoc()$sels/aoc()$total),sum(aoc()$Core*aoc()$afte1/aoc()$total),sum(aoc()$Core*aoc()$afte2/aoc()$total),sum(aoc()$Core*aoc()$afte3/aoc()$total),sum(aoc()$Core*aoc()$afte4/aoc()$total),sum(aoc()$Core*aoc()$afte5/aoc()$total,na.rm = TRUE))
    core2 <- c(sum(aoc()$Core*aoc()$as_is_s/aoc()$total),sum(aoc()$Core*aoc()$s19/aoc()$total),sum(aoc()$Core*aoc()$s20/aoc()$total),sum(aoc()$Core*aoc()$s21/aoc()$total),sum(aoc()$Core*aoc()$s22/aoc()$total),sum(aoc()$Core*aoc()$o23/aoc()$total,na.rm = TRUE))
    
    specialist1 <- c(sum(aoc()$Specialist*aoc()$sels/aoc()$total),sum(aoc()$Specialist*aoc()$afte1/aoc()$total),sum(aoc()$Specialist*aoc()$afte2/aoc()$total),sum(aoc()$Specialist*aoc()$afte3/aoc()$total),sum(aoc()$Specialist*aoc()$afte4/aoc()$total),sum(aoc()$Specialist*aoc()$afte5/aoc()$total,na.rm = TRUE))
    specialist2 <- c(sum(aoc()$Specialist*aoc()$as_is_s/aoc()$total),sum(aoc()$Specialist*aoc()$s19/aoc()$total),sum(aoc()$Specialist*aoc()$s20/aoc()$total),sum(aoc()$Specialist*aoc()$s21/aoc()$total),sum(aoc()$Specialist*aoc()$s22/aoc()$total),sum(aoc()$Specialist*aoc()$o23/aoc()$total,na.rm = TRUE))
    
    support1 <- c(sum(aoc()$Support*aoc()$sels/aoc()$total),sum(aoc()$Support*aoc()$afte1/aoc()$total),sum(aoc()$Support*aoc()$afte2/aoc()$total),sum(aoc()$Support*aoc()$afte3/aoc()$total),sum(aoc()$Support*aoc()$afte4/aoc()$total),sum(aoc()$Support*aoc()$afte5/aoc()$total,na.rm = TRUE))
    support2 <- c(sum(aoc()$Support*aoc()$as_is_s/aoc()$total),sum(aoc()$Support*aoc()$s19/aoc()$total),sum(aoc()$Support*aoc()$s20/aoc()$total),sum(aoc()$Support*aoc()$s21/aoc()$total),sum(aoc()$Support*aoc()$s22/aoc()$total),sum(aoc()$Support*aoc()$o23/aoc()$total,na.rm = TRUE))
    
    critical1 <- c(sum(aoc()$Critical*aoc()$sels/aoc()$total),sum(aoc()$Critical*aoc()$afte1/aoc()$total),sum(aoc()$Critical*aoc()$afte2/aoc()$total),sum(aoc()$Critical*aoc()$afte3/aoc()$total),sum(aoc()$Critical*aoc()$afte4/aoc()$total),sum(aoc()$Critical*aoc()$afte5/aoc()$total,na.rm = TRUE))
    critical2 <- c(sum(aoc()$Critical*aoc()$as_is_s/aoc()$total),sum(aoc()$Critical*aoc()$s19/aoc()$total),sum(aoc()$Critical*aoc()$s20/aoc()$total),sum(aoc()$Critical*aoc()$s21/aoc()$total),sum(aoc()$Critical*aoc()$s22/aoc()$total),sum(aoc()$Critical*aoc()$o23/aoc()$total,na.rm = TRUE))
    
    not_map1 <- c(sum(aoc()$not_map*aoc()$sels/aoc()$total),sum(aoc()$not_map*aoc()$afte1/aoc()$total),sum(aoc()$not_map*aoc()$afte2/aoc()$total),sum(aoc()$not_map*aoc()$afte3/aoc()$total),sum(aoc()$not_map*aoc()$afte4/aoc()$total),sum(aoc()$not_map*aoc()$afte5/aoc()$total,na.rm = TRUE))
    not_map2 <- c(sum(aoc()$not_map*aoc()$as_is_s/aoc()$total),sum(aoc()$not_map*aoc()$s19/aoc()$total),sum(aoc()$not_map*aoc()$s20/aoc()$total),sum(aoc()$not_map*aoc()$s21/aoc()$total),sum(aoc()$not_map*aoc()$s22/aoc()$total),sum(aoc()$not_map*aoc()$o23/aoc()$total,na.rm = TRUE))
    
    dema<- -c(core1, specialist1, support1,critical1, not_map1) + c(core2, specialist2, support2,critical2, not_map2)
    cat <-c(c(rep('1.Core',6),rep('2.Specialist',6),rep('3.Support',6),rep('4.Critical',6),rep('5.Not Map',6)))
    
    col1<-c(rep('rgba(282, 157, 13, 1.0)',6),rep('rgba(2, 90, 216, 1.0)',6),rep('rgba(200, 90, 216, 1.0)',6),rep('rgba(2, 190, 216, 1.0)',6),rep('rgba(82, 190, 16, 1.0)',6))
    col <- c(col1)
    data <- data.frame(x, dema, cat, col)
    
    plot_ly(data, x = ~x, y = ~dema, type = 'bar',
            color =~cat, hoverinfo = 'text', text = ~paste(cat, dema)
            ,colors = c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
            ) %>%
      layout(title = '',barmode = 'stack',margin = list(b = 150),
             xaxis = list(title = "",showgrid = F),
             yaxis = list(title = "Headcount",showgrid = F))%>%
      add_annotations(text = round(sum(core1[1]-core2[1]+specialist1[1]-specialist2[1]+support1[1]-support2[1]+critical1[1]-critical2[1]+not_map1[1]-not_map2[1]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = 'Y0 E',
                      y = sum(core1[1]-core2[1]+specialist1[1]-specialist2[1]+support1[1]-support2[1]+critical1[1]-critical2[1]+not_map1[1]-not_map2[1])*(2),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(core1[2]-core2[2]+specialist1[2]-specialist2[2]+support1[2]-support2[2]+critical1[2]-critical2[2]+not_map1[2]-not_map2[2]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = 'Y1 E',
                      y = sum(core1[2]-core2[2]+specialist1[2]-specialist2[2]+support1[2]-support2[2]+critical1[2]-critical2[2]+not_map1[2]-not_map2[2])*(2),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(core1[3]-core2[3]+specialist1[3]-specialist2[3]+support1[3]-support2[3]+critical1[3]-critical2[3]+not_map1[3]-not_map2[3]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = 'Y2 E',
                      y = sum(core1[3]-core2[3]+specialist1[3]-specialist2[3]+support1[3]-support2[3]+critical1[3]-critical2[3]+not_map1[3]-not_map2[3])*(1+2/4),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(core1[4]-core2[4]+specialist1[4]-specialist2[4]+support1[4]-support2[4]+critical1[4]-critical2[4]+not_map1[4]-not_map2[4]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = 'Y3 E',
                      y = sum(core1[4]-core2[4]+specialist1[4]-specialist2[4]+support1[4]-support2[4]+critical1[4]-critical2[4]+not_map1[4]-not_map2[4])*(1+1/4),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(core1[5]-core2[5]+specialist1[5]-specialist2[5]+support1[5]-support2[5]+critical1[5]-critical2[5]+not_map1[5]-not_map2[5]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = 'Y4 E',
                      y = sum(core1[5]-core2[5]+specialist1[5]-specialist2[5]+support1[5]-support2[5]+critical1[5]-critical2[5]+not_map1[5]-not_map2[5])*(1+1/4),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(sum(core1[6]-core2[6]+specialist1[6]-specialist2[6]+support1[6]-support2[6]+critical1[6]-critical2[6]+not_map1[6]-not_map2[6]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = 'Y5 E',
                      y = sum(core1[6]-core2[6]+specialist1[1]-specialist2[6]+support1[6]-support2[6]+critical1[6]-critical2[6]+not_map1[6]-not_map2[6])*(1+1/40),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 10, 50, 1.0)'),
                      showarrow = FALSE)
    
  })
  #almost stack and group https://plot.ly/r/bar-charts/
  output$plot35<- renderPlotly({
    
    x <- c('T0','T1', 'T2', 'T3', 'T4', 'T5')
    
    Opt <- c(sum(opt_dat()$sels), sum(opt_dat()$dsray19),sum(opt_dat()$dsray20),sum(opt_dat()$dsray21),sum(opt_dat()$dsray22),sum(opt_dat()$dsray23))
    
    crvt <- c(sum(crvt_dat()$sels), sum(crvt_dat()$dsray19),sum(crvt_dat()$dsray20),sum(crvt_dat()$dsray21),sum(crvt_dat()$dsray22),sum(crvt_dat()$dsray23))
    
    As_is <- c(sum(as_dat()$sels), sum(as_dat()$dsray19),sum(as_dat()$dsray20),sum(as_dat()$dsray21),sum(as_dat()$dsray22),sum(as_dat()$dsray23))
    
    data <- data.frame(x, Opt,  As_is,crvt)
    
    plot_ly(data, x = ~x, y = ~Opt, type = 'scatter',mode = "lines",name='Optimistic Case',marker = list(color = 'rgba(55, 128, 191, 0.7)'))%>%
      
      add_trace(y = ~As_is, name='Conservative Case',  marker = list(color = 'rgba(200, 0, 23, 0.7)',width = 2),
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
    
    x <- c(round(sum(coa4()$Contractor,na.rm = TRUE)), round(sum(coa4()$`Direct Hire`,na.rm = TRUE)))
    text <- c(sum(coa4()$Contractor,na.rm = TRUE)/2,sum(coa4()$Contractor,na.rm = TRUE)+(sum(coa4()$`Direct Hire`,na.rm = TRUE))/2)
    
    data <- data.frame(x,cat, base, text)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h', color = ~cat,  marker = list(color = c('rgba(227, 82, 5, 1.0)','rgba(4, 30, 66, 1.0)')))%>%
      
      layout(title = 'Headcount',height = 140,legend=list(orientation='h'),
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
    yform <- list(categoryorder = "array", title = "",
                  categoryarray = c('Core', 'Critical', 'Support',  'Specialist','Non Map')[5:1])
    x <- c(sum(coa4()$Core),sum(coa4()$Critical),sum(coa4()$Support),sum(coa4()$Specialist), sum(coa4()$not_map))#sum(coa4()$total)-sum(coa4()$Core)-sum(coa4()$Critical0)-sum(coa4()$Support)-sum(coa4()$Specialist))
    text <- c('$320K','$430K', '$260K', '$-120K', '$-120K')
    data <- data.frame(x, base, text)
    
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h',
            marker = list(color = c('rgba(227, 82, 5, 1.0)','rgba(0, 159, 223, 1.0)','rgba(255, 205, 0, 0.7)','rgba(4, 30, 66, 1.0)','rgba(191, 191, 191, 1.0)'))
            )%>%
            layout(title = 'Segmentation',
             xaxis = list(title = "Headcount"),
             yaxis = yform,
             barmode = 'stack',
             showlegend = FALSE)
    
  })  # segmentation bar core..
  
  output$plot424<- renderPlotly({
    zz <- mutate(coa4(), age_i = case_when(coa4()$Age < 25 ~'<25', coa4()$Age >=25 & coa4()$Age < 35 ~'25-34', coa4()$Age >=35 & coa4()$Age < 45 ~'35-44', coa4()$Age >=45 & coa4()$Age < 55 ~'45-54', coa4()$Age >=55 & coa4()$Age < 65 ~'55-65', coa4()$Age >=65  ~'65<'))
    dat <- aggregate(zz$total, by=list(age=zz$age_i), FUN=sum)
    
    #The default order will be alphabetized unless specified as below:
    plot_ly(dat, x = ~dat$age, y = ~x, type = 'bar', marker = list(color = c('rgba(91, 91, 91, 1.0)','rgba(111, 111, 111, 1.0)','rgba(151, 151, 151, 1.0)','rgba(171, 171, 171, 1.0)','rgba(191, 191, 191, 1.0)'))
            )%>%
    add_lines(x=dat$age , y= c(dat$x[1],sum(dat$x[1:2]),sum(dat$x[1:3]),sum(dat$x[1:4]),sum(dat$x[1:5])), name = "",
                  line = list(color= 'black'), showlegend=FALSE)%>%
      layout(title = 'Age Distribution',
             xaxis = list(title = ""),
             yaxis = list(title = "Headcount"
                          ),
             barmode = 'stack',
             showlegend = FALSE)%>%
      add_annotations(text = paste0(floor(sum(dat$x[1:4])/sum(dat$x[1:5])*1000)/10,'%'),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='grey',
                      x = '55-65',
                      y = sum(dat$x[1:4])*1.05,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(1, 1, 1, 1)'),
                      showarrow = FALSE)
    
  })# age distribution
  
  output$plot422<- renderPlotly({
    x <- c('Y1', 'Y2', 'Y3', 'Y4', 'Y5')
    y<-c(sum(coa4()$`2019`, na.rm = TRUE),sum(coa4()$`2020`, na.rm = TRUE),sum(coa4()$`2021`, na.rm = TRUE),sum(coa4()$`2022`, na.rm = TRUE),sum(coa4()$`2023`, na.rm = TRUE))
    data <- data.frame(x,y)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~y, type = 'bar', marker = list(color = c('rgba(91, 91, 91, 1.0)','rgba(111, 111, 111, 1.0)','rgba(151, 151, 151, 1.0)','rgba(171, 171, 171, 1.0)','rgba(191, 191, 191, 1.0)'))
    )%>%
      layout(title = 'Retirements',
             xaxis = list(title = ""),
             yaxis = list(title = "Headcount"),
             barmode = 'stack',
             showlegend = FALSE)%>%
      add_annotations(text = round(y,0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='grey',
                      x = x,
                      y = y*1.05,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(1, 1, 1, 1)'),
                      showarrow = FALSE)
    
  }) # Retirements
  
  output$plot423<- renderPlotly({
    base <- rep(c('Level1', 'Level2', 'Level3',  'Level4','Level5','Level6','Level7', 'Level8','OS', 'UNG'),2)
    yform <- list(categoryorder = "array", title = "",
                  categoryarray = c('UNG','OS','Level8','Level7','Level6','Level5', 'Level4','Level3','Level2','Level1'))
    x1 <- c(sum(coa4()$A, na.rm = TRUE),sum(coa4()$B, na.rm = TRUE),sum(coa4()$C, na.rm = TRUE),sum(coa4()$D, na.rm = TRUE),sum(coa4()$E, na.rm = TRUE),sum(coa4()$F, na.rm = TRUE),sum(coa4()$G, na.rm = TRUE),sum(coa4()$H, na.rm = TRUE),sum(coa4()$OS, na.rm = TRUE),sum(coa4()$UNG, na.rm = TRUE))/2
    x2 <- -c(sum(coa4()$A, na.rm = TRUE),sum(coa4()$B, na.rm = TRUE),sum(coa4()$C, na.rm = TRUE),sum(coa4()$D, na.rm = TRUE),sum(coa4()$E, na.rm = TRUE),sum(coa4()$F, na.rm = TRUE),sum(coa4()$G, na.rm = TRUE),sum(coa4()$H, na.rm = TRUE),sum(coa4()$OS, na.rm = TRUE),sum(coa4()$UNG, na.rm = TRUE))/2
    x=c(x1,x2)
    text=x1*2
    data <- data.frame(x, base, text)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~base, type = 'bar', orientation = 'h',marker = list(color = 'brown'),hoverinfo = 'text', text = ~paste(base, text))%>%
      add_trace()%>% config(displayModeBar = F)%>%
      layout(title = 'Career Level Structure',
             xaxis = list(
               title = "Headcount",
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
    y<-c(sum(coa4()$Male, na.rm = TRUE),sum(coa4()$Female, na.rm = TRUE), sum(coa4()$total)-sum(coa4()$Male, na.rm = TRUE)-sum(coa4()$Female, na.rm = TRUE))
    cat = c("midnightblue", "gold", "deepskyblue")
    data <- data.frame(x,y,cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, labels = ~x, values  = ~y, type = 'pie',marker = list(colors = c("midnightblue", "gold", "deepskyblue")))%>%
      layout(title = 'Gender Ratio',
             showlegend = T)
    
  }) # male female pie
  
  output$plot426<- renderPlotly({
    cat <- c(rep('Male',4), rep('Female',4),  rep('Not Map',4))
    x <- rep(c('MEA', 'APAC', 'AMR', 'EUR'),3)
    y<-c(sum(coa4()[coa4()$Region == 'MEA','Male'], na.rm = TRUE),sum(coa4()[coa4()$Region == 'APAC','Male'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'AMR','Male'], na.rm = TRUE),sum(coa4()[coa4()$Region == 'APAC','Male'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'MEA','Female'], na.rm = TRUE),sum(coa4()[coa4()$Region == 'APAC','Female'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'AMR','Female'], na.rm = TRUE),sum(coa4()[coa4()$Region == 'EUR','Female'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'MEA','total'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'MEA','Male'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'MEA','Female'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'APAC','total'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'APAC','Male'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'APAC','Female'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'AMR','total'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'AMR','Male'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'AMR','Female'], na.rm = TRUE),
         sum(coa4()[coa4()$Region == 'APAC','total'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'APAC','Male'], na.rm = TRUE)-sum(coa4()[coa4()$Region == 'APAC','Female'], na.rm = TRUE))
    data <- data.frame(x,y, cat)
    #The default order will be alphabetized unless specified as below:
    plot_ly(data, x = ~x, y = ~y, type = 'bar',  color = ~cat, colors = c("gold","midnightblue",  "deepskyblue"))%>%
      layout(title = 'Gender Ratio peer Region',
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "Headcount", showgrid = FALSE),
             barmode = 'stack',
             showlegend = TRUE)
  }) # male female
  
  output$plot43<- renderPlotly({
    x <- c(rep('1.Current<br>headcount',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nm=sum(coa4()$not_map)
    revenue <- c(sum(coa4()$Core),sum(coa4()$Specialist),sum(coa4()$Support),sum(coa4()$Critical), nm)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data0 <- data.frame(x, revenue, col,cat)
    
    
    ##opt
    x <- c(rep('2.Upsizing',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$total), sum(coa4()$Core*coa4()$addt/coa4()$total),sum(coa4()$Specialist*coa4()$addt/coa4()$total),sum(coa4()$Support*coa4()$addt/coa4()$total),sum(coa4()$Critical*coa4()$addt/coa4()$total), sum(coa4()$not_map*coa4()$addt/coa4()$total))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data1 <- data.frame(x, revenue, col,cat)
    
    ##o21
    x <- c(rep('3.Optimization',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <--c(-sum(coa4()$total)-sum(coa4()$addt)-sum(coa4()$subt), sum(coa4()$Core*coa4()$subt/coa4()$total),sum(coa4()$Specialist*coa4()$subt/coa4()$total),sum(coa4()$Support*coa4()$subt/coa4()$total),sum(coa4()$Critical*coa4()$subt/coa4()$total), sum(coa4()$not_map*coa4()$subt/coa4()$total))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data2 <- data.frame(x, revenue, col,cat)
    
    
    ##1
    x <- c(rep('4.Y1 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core)+sum(coa4()$Core*coa4()$subt/coa4()$total)+sum(coa4()$Core*coa4()$addt/coa4()$total),
                sum(coa4()$Specialist)+sum(coa4()$Specialist*coa4()$subt/coa4()$total)+sum(coa4()$Specialist*coa4()$addt/coa4()$total),
                sum(coa4()$Support)+sum(coa4()$Support*coa4()$subt/coa4()$total)+sum(coa4()$Support*coa4()$addt/coa4()$total),
                sum(coa4()$Critical)+sum(coa4()$Critical*coa4()$subt/coa4()$total)+sum(coa4()$Critical*coa4()$addt/coa4()$total),
                sum(coa4()$not_map)+sum(coa4()$not_map*coa4()$subt/coa4()$total)+sum(coa4()$not_map*coa4()$addt/coa4()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data3 <- data.frame(x, revenue, col,cat)
    
    
    #4  sum(coa4()$Specialist*coa4()$dds19/coa4()$total)
    x <- c(rep('5.Business Demand(+)',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    
    prev <- c(sum(coa4()$Core)+sum(coa4()$Core*coa4()$subt/coa4()$total)+sum(coa4()$Core*coa4()$addt/coa4()$total),
              sum(coa4()$Specialist)+sum(coa4()$Specialist*coa4()$subt/coa4()$total)+sum(coa4()$Specialist*coa4()$addt/coa4()$total),
              sum(coa4()$Support)+sum(coa4()$Support*coa4()$subt/coa4()$total)+sum(coa4()$Support*coa4()$addt/coa4()$total),
              sum(coa4()$Critical)+sum(coa4()$Critical*coa4()$subt/coa4()$total)+sum(coa4()$Critical*coa4()$addt/coa4()$total),
              sum(coa4()$not_map)+sum(coa4()$not_map*coa4()$subt/coa4()$total)+sum(coa4()$not_map*coa4()$addt/coa4()$total))
    
    dds2023 <- c(sum(coa4()$Core*coa4()$dds23/coa4()$total),sum(coa4()$Specialist*coa4()$dds23/coa4()$total),sum(coa4()$Support*coa4()$dds23/coa4()$total),sum(coa4()$Critical*coa4()$dds23/coa4()$total), sum(coa4()$not_map*coa4()$dds23/coa4()$total))
    
    revenue <-c(sum(prev),ifelse((dds2023-prev)<0, 0,(dds2023-prev)))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4 <- data.frame(x, revenue, col,cat)
    
    x <- c(rep('5.Business Demand(-)',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    
    prev <- c(sum(coa4()$Core)+sum(coa4()$Core*coa4()$subt/coa4()$total)+sum(coa4()$Core*coa4()$addt/coa4()$total),
              sum(coa4()$Specialist)+sum(coa4()$Specialist*coa4()$subt/coa4()$total)+sum(coa4()$Specialist*coa4()$addt/coa4()$total),
              sum(coa4()$Support)+sum(coa4()$Support*coa4()$subt/coa4()$total)+sum(coa4()$Support*coa4()$addt/coa4()$total),
              sum(coa4()$Critical)+sum(coa4()$Critical*coa4()$subt/coa4()$total)+sum(coa4()$Critical*coa4()$addt/coa4()$total),
              sum(coa4()$not_map)+sum(coa4()$not_map*coa4()$subt/coa4()$total)+sum(coa4()$not_map*coa4()$addt/coa4()$total))
    
    dds2023 <- c(sum(coa4()$Core*coa4()$dds23/coa4()$total),sum(coa4()$Specialist*coa4()$dds23/coa4()$total),sum(coa4()$Support*coa4()$dds23/coa4()$total),sum(coa4()$Critical*coa4()$dds23/coa4()$total), sum(coa4()$not_map*coa4()$dds23/coa4()$total))
    
    revenue <-c(sum(data4$revenue)+sum(ifelse((dds2023-prev)>0, 0,(dds2023-prev))),-ifelse((dds2023-prev)>0, 0,(dds2023-prev)))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4n <- data.frame(x, revenue, col,cat)
    
    #2
    x <- c(rep('6.Automation<br>Reduction',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    
    
    autom <-c(sum(coa4()$Core*coa4()$dsray23/coa4()$total),sum(coa4()$Specialist*coa4()$dsray23/coa4()$total),sum(coa4()$Support*coa4()$dsray23/coa4()$total),sum(coa4()$Critical*coa4()$dsray23/coa4()$total), sum(coa4()$not_map*coa4()$dsray23/coa4()$total))
    revenue <-c(data4n$revenue[1]-sum(dds2023-autom),dds2023-autom)
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data5 <- data.frame(x, revenue, col,cat)
    
    
    #3Capability Enhancement
    x <- c(rep('7.Capability<br>Enhancement',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    
    fte55 <-c(sum(coa4()$Core*coa4()$afte5/coa4()$total),sum(coa4()$Specialist*coa4()$afte5/coa4()$total),sum(coa4()$Support*coa4()$afte5/coa4()$total),sum(coa4()$Critical*coa4()$afte5/coa4()$total), sum(coa4()$not_map*coa4()$afte5/coa4()$total))
    
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
      add_annotations(text = round(sum(coa4()$total)), bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total),
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
      add_annotations(text =round( sum(coa4()$addt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('3.Optimization',6)),
                  y = c(data2$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(coa4()$subt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[3],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('4.Y1 E',6)),
                  y = c(0,data3$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '4.Y1 E',
                      y = sum(data3$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y1 E',
                      y = sum(data3$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y1 E',
                      y = sum(data3$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y1 E',
                      y = sum(data3$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y1 E',
                      y = sum(data3$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '4.Y1 E',
                      y = sum(data3$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('5.Business Demand(+)',6)),
                  y = c(data4$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    ) %>%
      add_annotations(text = round(sum(data4$revenue)-sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '5.Business Demand(+)',
                      y = sum(data4$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Business Demand(+)',
                      y = sum(data4$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Business Demand(+)',
                      y = sum(data4$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Business Demand(+)',
                      y = sum(data4$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Business Demand(+)',
                      y = sum(data4$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '5.Business Demand(+)',
                      y = sum(data4$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('6.Business Demand(-)',6)),
                  y = c(data4n$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    ) %>%
      add_annotations(text = -round(sum(data4n$revenue[-1]),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '6.Business Demand(-)',
                      y = round(sum(coa4()$dds23),0)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Business Demand(-)',
                      y = sum(data4$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Business Demand(-)',
                      y = sum(data4$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Business Demand(-)',
                      y = sum(data4$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Business Demand(-)',
                      y = sum(data4$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data4n$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '6.Business Demand(-)',
                      y = sum(data4$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
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
                      y = sum(data5$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Automation<br>Reduction',
                      y = sum(data4$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Automation<br>Reduction',
                      y = sum(data4$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Automation<br>Reduction',
                      y = sum(data4$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Automation<br>Reduction',
                      y = sum(data4$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data5$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '7.Automation<br>Reduction',
                      y = sum(data4$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('8.Capability<br>Enhancement',6)),
                  y = c(data6$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = -round(sum(coa4()$dsray23)-sum(coa4()$afte5),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '8.Capability<br>Enhancement',
                      y = round(sum(data6$revenue),0)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Capability<br>Enhancement',
                      y = sum(data6$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Capability<br>Enhancement',
                      y = sum(data6$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Capability<br>Enhancement',
                      y = sum(data6$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Capability<br>Enhancement',
                      y = sum(data6$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data6$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '8.Capability<br>Enhancement',
                      y = sum(data6$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)
    
    
    p <-add_trace(p,
                  x = c(rep('9.Y5 E',6)),
                  y = c(0,data7$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=T
    ) %>%
      layout(barmode = 'stack',margin = list(b = 150),legend = list(y= 40000,orientation = 'h', traceorder='normal'), yaxis=list(title='Headcount'))%>%
      add_annotations(text = round(sum(fte55),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '9.Y5 E',
                      y = round(sum(fte55)*1.2,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = round(sum(fte55)*0.2,0),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = sum(fte55)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = sum(fte55)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '9.Y5 E',
                      y = sum(fte55)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data7$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '9.Y5 E',
                      y = sum(fte55),
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
    p <-add_lines(p, x=c('4.Y1 E','5.Business Demand(+)') , y= c(sum(data3$revenue), sum(data3$revenue)), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('5.Business Demand(+)','6.Business Demand(-)') , y= c(sum(data4$revenue), sum(data4$revenue)), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('6.Business Demand(-)','7.Automation<br>Reduction') , y= c(data4n$revenue[1], data4n$revenue[1]), name = "",
                  line = list(color= 'black', widthh=0.7, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('7.Automation<br>Reduction','8.Capability<br>Enhancement') , y= c(sum(data6$revenue), sum(data6$revenue)), name = "",
                  line = list(color= 'black', widthh=0.1, dash="dot"), showlegend=FALSE)
    p <-add_lines(p, x=c('8.Capability<br>Enhancement','9.Y5 E') , y= c(sum(data7$revenue), sum(data7$revenue)), name = "",
                  line = list(color= 'black', widthh=0.3, dash="dot"), showlegend=FALSE)
    
    p
    
    
  })#main waterfall
  
  output$plot432<- renderPlotly({
    x <- c(rep('1.Current<br>headcount',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    nm=sum(coa4()$not_map)
    revenue <- c(sum(coa4()$Core),sum(coa4()$Specialist),sum(coa4()$Support),sum(coa4()$Critical), nm)
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data0 <- data.frame(x, revenue, col,cat)
    
    
    ##opt
    x <- c(rep('2.Upsizing',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$total), sum(coa4()$Core*coa4()$addt/coa4()$total),sum(coa4()$Specialist*coa4()$addt/coa4()$total),sum(coa4()$Support*coa4()$addt/coa4()$total),sum(coa4()$Critical*coa4()$addt/coa4()$total), sum(coa4()$not_map*coa4()$addt/coa4()$total))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data1 <- data.frame(x, revenue, col,cat)
    
    ##o21
    x <- c(rep('3.Optimization',6))
    col=c('rgba(1,1,1, 0.0)',"darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <--c(-sum(coa4()$total)-sum(coa4()$addt)-sum(coa4()$subt), sum(coa4()$Core*coa4()$subt/coa4()$total),sum(coa4()$Specialist*coa4()$subt/coa4()$total),sum(coa4()$Support*coa4()$subt/coa4()$total),sum(coa4()$Critical*coa4()$subt/coa4()$total), sum(coa4()$not_map*coa4()$subt/coa4()$total))
    cat=c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data2 <- data.frame(x, revenue, col,cat)
    
    
    ##1
    x <- c(rep('4.Y0 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core)+sum(coa4()$Core*coa4()$subt/coa4()$total)+sum(coa4()$Core*coa4()$addt/coa4()$total),
                sum(coa4()$Specialist)+sum(coa4()$Specialist*coa4()$subt/coa4()$total)+sum(coa4()$Specialist*coa4()$addt/coa4()$total),
                sum(coa4()$Support)+sum(coa4()$Support*coa4()$subt/coa4()$total)+sum(coa4()$Support*coa4()$addt/coa4()$total),
                sum(coa4()$Critical)+sum(coa4()$Critical*coa4()$subt/coa4()$total)+sum(coa4()$Critical*coa4()$addt/coa4()$total),
                sum(coa4()$not_map)+sum(coa4()$not_map*coa4()$subt/coa4()$total)+sum(coa4()$not_map*coa4()$addt/coa4()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data3 <- data.frame(x, revenue, col,cat)
    
    #2
    x <- c(rep('5.Y1 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core*coa4()$afte1/coa4()$total),sum(coa4()$Specialist*coa4()$afte1/coa4()$total),sum(coa4()$Support*coa4()$afte1/coa4()$total),sum(coa4()$Critical*coa4()$afte1/coa4()$total), sum(coa4()$not_map*coa4()$afte1/coa4()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4 <- data.frame(x, revenue, col,cat)#2
    
    x <- c(rep('6.Y2 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core*coa4()$afte2/coa4()$total),sum(coa4()$Specialist*coa4()$afte2/coa4()$total),sum(coa4()$Support*coa4()$afte2/coa4()$total),sum(coa4()$Critical*coa4()$afte2/coa4()$total), sum(coa4()$not_map*coa4()$afte2/coa4()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data4n <- data.frame(x, revenue, col,cat)
    
    #3
    x <- c(rep('7.Y3 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core*coa4()$afte3/coa4()$total),sum(coa4()$Specialist*coa4()$afte3/coa4()$total),sum(coa4()$Support*coa4()$afte3/coa4()$total),sum(coa4()$Critical*coa4()$afte3/coa4()$total), sum(coa4()$not_map*coa4()$afte3/coa4()$total))
    at=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data5 <- data.frame(x, revenue, col,cat)
    
    #4
    x <- c(rep('8.Y4 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core*coa4()$afte4/coa4()$total),sum(coa4()$Specialist*coa4()$afte4/coa4()$total),sum(coa4()$Support*coa4()$afte4/coa4()$total),sum(coa4()$Critical*coa4()$afte4/coa4()$total), sum(coa4()$not_map*coa4()$afte4/coa4()$total))
    cat=c(rep(c('1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1))
    data6 <- data.frame(x, revenue, col,cat)
    
    #5
    x <- c(rep('9.Y5 E',5))
    col=c("darkorange", "midnightblue", "gold", "deepskyblue", "gray")
    revenue <-c(sum(coa4()$Core*coa4()$afte5/coa4()$total),sum(coa4()$Specialist*coa4()$afte5/coa4()$total),sum(coa4()$Support*coa4()$afte5/coa4()$total),sum(coa4()$Critical*coa4()$afte5/coa4()$total), sum(coa4()$not_map*coa4()$afte5/coa4()$total))
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
      add_annotations(text = round(sum(coa4()$total)), bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(1, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data0$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '1.Current<br>headcount',
                      y = sum(coa4()$total),
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
      add_annotations(text =round( sum(coa4()$addt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[3],0), bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data1$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '2.Upsizing',
                      y = sum(data1$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('3.Optimization',6)),
                  y = c(data2$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(coa4()$subt),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[2],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[3],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[4],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[5],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data2$revenue[6],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '3.Optimization',
                      y = sum(data1$revenue),
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color ='rgba(255, 255, 255, 1.0)' ),
                      showarrow = FALSE)
    
    p <-add_trace(p,
                  x = c(rep('4.Y0 E',6)),
                  y = c(0,data3$revenue),
                  colors=c('transparent',"darkorange", "midnightblue", "gold", "deepskyblue", "gray"),
                  color =c(rep(c('0.Base','1.Core','2.Specialist','3.Support','4.Critical','5.Not Map'),1)),
                  type='bar', showlegend=FALSE
    )%>%
      add_annotations(text = round(sum(data3$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*1.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(50, 1, 1, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[1],0), bgcolor="darkorange",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.2,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[2],0),bgcolor="midnightblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.4,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[3],0), bgcolor="gold",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.6,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[4],0), bgcolor="deepskyblue",bordercolor='rgba(255, 255, 255, 1.0)',
                      x = '4.Y0 E',
                      y = sum(data3$revenue)*0.8,
                      xref = "x",
                      font = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgba(255, 255, 255, 1.0)'),
                      showarrow = FALSE)%>%
      add_annotations(text = round(data3$revenue[5],0), bgcolor="gray",bordercolor='rgba(255, 255, 255, 1.0)', 
                      x = '4.Y0 E',
                      y = sum(data3$revenue),
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
                  type='bar', showlegend=T
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100),legend = list(y= 40000,orientation = 'h', traceorder='normal'), yaxis=list(title='Headcount'))%>%
      add_annotations(text = round(sum(data4$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '5.Y1 E',
                      y = round(sum(data4$revenue)*1.2,0),
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
                  type='bar', showlegend=F
    ) %>%
      layout(barmode = 'stack',margin = list(b = 100))%>%
      add_annotations(text = round(sum(data4n$revenue),0),bgcolor='rgba(255, 255, 255, 1.0)',bordercolor='rgba(1, 1, 1, 1.0)',
                      x = '6.Y2 E',
                      y = round(sum(data4n$revenue)*1.2,0),
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
                      y = round(sum(data5$revenue)*1.2,0),
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
                      y = round(sum(data6$revenue)*1.2,0),
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
                      y = round(sum(data7$revenue)*1.2,0),
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
  
  #####
  #end
  
  #data proc
  #####
  
  luk_as=reactiveValues()
  luk_opt=reactiveValues()
  luk_crvt=reactiveValues()
  # 
  # luk_as$Data<-luk2$Data
  # luk_opt$Data<-luk2$Data
  # luk_crvt$Data<-luk2$Data
  # 
  
  
  newluk1a <- observe(luk_as$Data <- mutate(luk2$Data, sels = luk2$Data$as_is_s))
  newluk2a <- observe(luk_as$Data <- mutate(luk_as$Data, addt = ifelse((luk_as$Data$as_is_s - luk_as$Data$sels)>0, 0,-(luk_as$Data$as_is_s - luk_as$Data$sels)),
                                            subt= ifelse((luk_as$Data$as_is_s - luk_as$Data$sels)<0, 0,-(luk_as$Data$as_is_s - luk_as$Data$sels)),
                                            o18 = luk_as$Data$as_is_s - ifelse(is.na(luk_as$Data$`2018`),0,luk_as$Data$`2018`)))
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,s18 = luk_as$Data$o18*(1+luk_as$Data$NHR19)))
  
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,o19 = luk_as$Data$s18 - ifelse(is.na(luk_as$Data$`2019`),0,luk_as$Data$`2019`)))
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,s19 = luk_as$Data$o19*(1+luk_as$Data$NHR20)))
  
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,o20 = luk_as$Data$s19 - ifelse(is.na(luk_as$Data$`2020`),0,luk_as$Data$`2020`)))
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,s20 = luk_as$Data$o20*(1+luk_as$Data$NHR21)))
  
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,o21 = luk_as$Data$s20 - ifelse(is.na(luk_as$Data$`2021`),0,luk_as$Data$`2021`)))
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,s21 = luk_as$Data$o21*(1+luk_as$Data$NHR22)))
  
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,o22 = luk_as$Data$s21 - ifelse(is.na(luk_as$Data$`2022`),0,luk_as$Data$`2022`)))
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,s22 = luk_as$Data$o22*(1+luk_as$Data$NHR23),
                                            o23 = luk_as$Data$as_is_s - ifelse(is.na(luk_as$Data$`2023`),0,luk_as$Data$`2023`),
                      `DD1 Weightage` = case_when(is.na( luk_as$Data$`DD1 Weightage`) ~ 1, TRUE ~ luk_as$Data$`DD1 Weightage`),
                      dd_test = rowSums(luk_as$Data[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE),
                      `DD1 2018` = case_when(is.na( luk_as$Data$`DD1 2018`) ~ 1, TRUE ~ luk_as$Data$`DD1 2018`),
                      `DD1 2019` = case_when(is.na( luk_as$Data$`DD1 2019`) ~ 1, TRUE ~ luk_as$Data$`DD1 2019`),
                      `DD1 2020` = case_when(is.na( luk_as$Data$`DD1 2020`) ~ 1, TRUE ~ luk_as$Data$`DD1 2020`),
                      `DD1 2021` = case_when(is.na( luk_as$Data$`DD1 2021`) ~ 1, TRUE ~ luk_as$Data$`DD1 2021`),
                      `DD1 2022` = case_when(is.na( luk_as$Data$`DD1 2022`) ~ 1, TRUE ~ luk_as$Data$`DD1 2022`),
                      `DD1 2023` = case_when(is.na( luk_as$Data$`DD1 2023`) ~ 1, TRUE ~ luk_as$Data$`DD1 2023`)))
  newluk3a <- observe(luk_as$Data <- mutate(luk_as$Data,dd_c = `DD1 Weightage` + 1 - luk_as$Data$dd_test))
  newluk4a <- observe(luk_as$Data <- mutate(luk_as$Data,dds19 = luk_as$Data$sels * (luk_as$Data$dd_c *ifelse(is.na(luk_as$Data$`DD1 2019` / luk_as$Data$`DD1 2018`),0,luk_as$Data$`DD1 2019` / luk_as$Data$`DD1 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD2 2019` / luk_as$Data$`DD2 2018`),0,luk_as$Data$`DD2 Weightage`*luk_as$Data$`DD2 2019` / luk_as$Data$`DD2 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD3 2019` / luk_as$Data$`DD3 2018`),0,luk_as$Data$`DD3 Weightage`*luk_as$Data$`DD3 2019` / luk_as$Data$`DD3 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD4 2019` / luk_as$Data$`DD4 2018`),0,luk_as$Data$`DD4 Weightage`*luk_as$Data$`DD4 2019` / luk_as$Data$`DD4 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD5 2019` / luk_as$Data$`DD5 2018`),0,luk_as$Data$`DD5 Weightage`*luk_as$Data$`DD5 2019` / luk_as$Data$`DD5 2018`)),
                                            dds20 = luk_as$Data$sels * (luk_as$Data$dd_c *ifelse(is.na(luk_as$Data$`DD1 2020` / luk_as$Data$`DD1 2018`),0,luk_as$Data$`DD1 2020` / luk_as$Data$`DD1 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD2 2020` / luk_as$Data$`DD2 2018`),0,luk_as$Data$`DD2 Weightage`*luk_as$Data$`DD2 2020` / luk_as$Data$`DD2 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD3 2020` / luk_as$Data$`DD3 2018`),0,luk_as$Data$`DD3 Weightage`*luk_as$Data$`DD3 2020` / luk_as$Data$`DD3 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD4 2020` / luk_as$Data$`DD4 2018`),0,luk_as$Data$`DD4 Weightage`*luk_as$Data$`DD4 2020` / luk_as$Data$`DD4 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD5 2020` / luk_as$Data$`DD5 2018`),0,luk_as$Data$`DD5 Weightage`*luk_as$Data$`DD5 2020` / luk_as$Data$`DD5 2018`)),
                                            dds21 = luk_as$Data$sels * (luk_as$Data$dd_c *ifelse(is.na(luk_as$Data$`DD1 2021` / luk_as$Data$`DD1 2018`),0,luk_as$Data$`DD1 2021` / luk_as$Data$`DD1 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD2 2021` / luk_as$Data$`DD2 2018`),0,luk_as$Data$`DD2 Weightage`*luk_as$Data$`DD2 2021` / luk_as$Data$`DD2 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD3 2021` / luk_as$Data$`DD3 2018`),0,luk_as$Data$`DD3 Weightage`*luk_as$Data$`DD3 2021` / luk_as$Data$`DD3 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD4 2021` / luk_as$Data$`DD4 2018`),0,luk_as$Data$`DD4 Weightage`*luk_as$Data$`DD4 2021` / luk_as$Data$`DD4 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD5 2021` / luk_as$Data$`DD5 2018`),0,luk_as$Data$`DD5 Weightage`*luk_as$Data$`DD5 2021` / luk_as$Data$`DD5 2018`)),
                                            dds22 = luk_as$Data$sels * (luk_as$Data$dd_c *ifelse(is.na(luk_as$Data$`DD1 2022` / luk_as$Data$`DD1 2018`),0,luk_as$Data$`DD1 2022` / luk_as$Data$`DD1 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD2 2022` / luk_as$Data$`DD2 2018`),0,luk_as$Data$`DD2 Weightage`*luk_as$Data$`DD2 2022` / luk_as$Data$`DD2 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD3 2022` / luk_as$Data$`DD3 2018`),0,luk_as$Data$`DD3 Weightage`*luk_as$Data$`DD3 2022` / luk_as$Data$`DD3 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD4 2022` / luk_as$Data$`DD4 2018`),0,luk_as$Data$`DD4 Weightage`*luk_as$Data$`DD4 2022` / luk_as$Data$`DD4 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD5 2022` / luk_as$Data$`DD5 2018`),0,luk_as$Data$`DD5 Weightage`*luk_as$Data$`DD5 2022` / luk_as$Data$`DD5 2018`)),
                                            dds23 = luk_as$Data$sels * (luk_as$Data$dd_c *ifelse(is.na(luk_as$Data$`DD1 2023` / luk_as$Data$`DD1 2018`),0,luk_as$Data$`DD1 2023` / luk_as$Data$`DD1 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD2 2023` / luk_as$Data$`DD2 2018`),0,luk_as$Data$`DD2 Weightage`*luk_as$Data$`DD2 2023` / luk_as$Data$`DD2 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD3 2023` / luk_as$Data$`DD3 2018`),0,luk_as$Data$`DD3 Weightage`*luk_as$Data$`DD3 2023` / luk_as$Data$`DD3 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD4 2023` / luk_as$Data$`DD4 2018`),0,luk_as$Data$`DD4 Weightage`*luk_as$Data$`DD4 2023` / luk_as$Data$`DD4 2018`)
                                                                        + ifelse(is.na(luk_as$Data$`DD5 2023` / luk_as$Data$`DD5 2018`),0,luk_as$Data$`DD5 Weightage`*luk_as$Data$`DD5 2023` / luk_as$Data$`DD5 2018`))
                                            ))
  
  newluk4a <- observe(luk_as$Data <- mutate(luk_as$Data, fte1 = (1-luk_as$Data$ldt1)*luk_as$Data$dds19,
                                            fte2 = (1-luk_as$Data$ldt2)*luk_as$Data$dds20,
                                            fte3 = (1-luk_as$Data$ldt3)*luk_as$Data$dds21,
                                            fte4 = (1-luk_as$Data$ldt4)*luk_as$Data$dds22,
                                            fte5 = (1-luk_as$Data$ldt5)*luk_as$Data$dds23,
                                            dsray19 = (1-luk_as$Data$yai19q4)*luk_as$Data$dds19,
                                            dsray20 = (1-luk_as$Data$yai20q4)*luk_as$Data$dds20,
                                            dsray21 = (1-luk_as$Data$yai21q4)*luk_as$Data$dds21,
                                            dsray22 = (1-luk_as$Data$yai22q4)*luk_as$Data$dds22,
                                            dsray23 = (1-luk_as$Data$yai23q4)*luk_as$Data$dds23,
                                            afte1 = (1-luk_as$Data$yai19q4)*(1-luk_as$Data$ldt1)*luk_as$Data$dds19,
                                            afte2 = (1-luk_as$Data$yai20q4)*(1-luk_as$Data$ldt2)*luk_as$Data$dds20,
                                            afte3 = (1-luk_as$Data$yai21q4)*(1-luk_as$Data$ldt3)*luk_as$Data$dds21,
                                            afte4 = (1-luk_as$Data$yai22q4)*(1-luk_as$Data$ldt4)*luk_as$Data$dds22,
                                            afte5 = (1-luk_as$Data$yai23q4)*(1-luk_as$Data$ldt5)*luk_as$Data$dds23
                                            ))
  
  
  newluko1a <- observe(luk_opt$Data <- mutate(luk2$Data, sels = luk2$Data$opt))
  newluko2a <- observe(luk_opt$Data <- mutate(luk_opt$Data, addt = ifelse((luk_opt$Data$as_is_s - luk_opt$Data$sels)>0, 0,-(luk_opt$Data$as_is_s - luk_opt$Data$sels)),
                                              subt= ifelse((luk_opt$Data$as_is_s - luk_opt$Data$sels)<0, 0,-(luk_opt$Data$as_is_s - luk_opt$Data$sels)),
                                              o18 = luk_opt$Data$as_is_s - ifelse(is.na(luk_opt$Data$`2018`),0,luk_opt$Data$`2018`)))
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,s18 = luk_opt$Data$o18*(1+luk_opt$Data$NHR19)))
  
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,o19 = luk_opt$Data$s18 - ifelse(is.na(luk_opt$Data$`2019`),0,luk_opt$Data$`2019`)))
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,s19 = luk_opt$Data$o19*(1+luk_opt$Data$NHR20)))
  
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,o20 = luk_opt$Data$s19 - ifelse(is.na(luk_opt$Data$`2020`),0,luk_opt$Data$`2020`)))
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,s20 = luk_opt$Data$o20*(1+luk_opt$Data$NHR21)))
  
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,o21 = luk_opt$Data$s20 - ifelse(is.na(luk_opt$Data$`2021`),0,luk_opt$Data$`2021`)))
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,s21 = luk_opt$Data$o21*(1+luk_opt$Data$NHR22)))
  
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,o22 = luk_opt$Data$s21 - ifelse(is.na(luk_opt$Data$`2022`),0,luk_opt$Data$`2022`)))
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,s22 = luk_opt$Data$o22*(1+luk_opt$Data$NHR23),
                                              o23 = luk_opt$Data$as_is_s - ifelse(is.na(luk_opt$Data$`2023`),0,luk_opt$Data$`2023`),
                                              `DD1 Weightage` = case_when(is.na( luk_opt$Data$`DD1 Weightage`) ~ 1, TRUE ~ luk_opt$Data$`DD1 Weightage`),
                                              dd_test = rowSums(luk_opt$Data[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE),
                                              `DD1 2018` = case_when(is.na( luk_opt$Data$`DD1 2018`) ~ 1, TRUE ~ luk_opt$Data$`DD1 2018`),
                                              `DD1 2019` = case_when(is.na( luk_opt$Data$`DD1 2019`) ~ 1, TRUE ~ luk_opt$Data$`DD1 2019`),
                                              `DD1 2020` = case_when(is.na( luk_opt$Data$`DD1 2020`) ~ 1, TRUE ~ luk_opt$Data$`DD1 2020`),
                                              `DD1 2021` = case_when(is.na( luk_opt$Data$`DD1 2021`) ~ 1, TRUE ~ luk_opt$Data$`DD1 2021`),
                                              `DD1 2022` = case_when(is.na( luk_opt$Data$`DD1 2022`) ~ 1, TRUE ~ luk_opt$Data$`DD1 2022`),
                                              `DD1 2023` = case_when(is.na( luk_opt$Data$`DD1 2023`) ~ 1, TRUE ~ luk_opt$Data$`DD1 2023`)))
  newluko3a <- observe(luk_opt$Data <- mutate(luk_opt$Data,dd_c = `DD1 Weightage` + 1 - luk_opt$Data$dd_test))
  newluko4a <- observe(luk_opt$Data <- mutate(luk_opt$Data,dds19 = luk_opt$Data$sels * (luk_opt$Data$dd_c *ifelse(is.na(luk_opt$Data$`DD1 2019` / luk_opt$Data$`DD1 2018`),0,luk_opt$Data$`DD1 2019` / luk_opt$Data$`DD1 2018`)
                                                                                        + ifelse(is.na(luk_opt$Data$`DD2 2019` / luk_opt$Data$`DD2 2018`),0,luk_opt$Data$`DD2 Weightage`*luk_opt$Data$`DD2 2019` / luk_opt$Data$`DD2 2018`)
                                                                                        + ifelse(is.na(luk_opt$Data$`DD3 2019` / luk_opt$Data$`DD3 2018`),0,luk_opt$Data$`DD3 Weightage`*luk_opt$Data$`DD3 2019` / luk_opt$Data$`DD3 2018`)
                                                                                        + ifelse(is.na(luk_opt$Data$`DD4 2019` / luk_opt$Data$`DD4 2018`),0,luk_opt$Data$`DD4 Weightage`*luk_opt$Data$`DD4 2019` / luk_opt$Data$`DD4 2018`)
                                                                                        + ifelse(is.na(luk_opt$Data$`DD5 2019` / luk_opt$Data$`DD5 2018`),0,luk_opt$Data$`DD5 Weightage`*luk_opt$Data$`DD5 2019` / luk_opt$Data$`DD5 2018`)),
                                              dds20 = luk_opt$Data$sels * (luk_opt$Data$dd_c *ifelse(is.na(luk_opt$Data$`DD1 2020` / luk_opt$Data$`DD1 2018`),0,luk_opt$Data$`DD1 2020` / luk_opt$Data$`DD1 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD2 2020` / luk_opt$Data$`DD2 2018`),0,luk_opt$Data$`DD2 Weightage`*luk_opt$Data$`DD2 2020` / luk_opt$Data$`DD2 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD3 2020` / luk_opt$Data$`DD3 2018`),0,luk_opt$Data$`DD3 Weightage`*luk_opt$Data$`DD3 2020` / luk_opt$Data$`DD3 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD4 2020` / luk_opt$Data$`DD4 2018`),0,luk_opt$Data$`DD4 Weightage`*luk_opt$Data$`DD4 2020` / luk_opt$Data$`DD4 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD5 2020` / luk_opt$Data$`DD5 2018`),0,luk_opt$Data$`DD5 Weightage`*luk_opt$Data$`DD5 2020` / luk_opt$Data$`DD5 2018`)),
                                              dds21 = luk_opt$Data$sels * (luk_opt$Data$dd_c *ifelse(is.na(luk_opt$Data$`DD1 2021` / luk_opt$Data$`DD1 2018`),0,luk_opt$Data$`DD1 2021` / luk_opt$Data$`DD1 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD2 2021` / luk_opt$Data$`DD2 2018`),0,luk_opt$Data$`DD2 Weightage`*luk_opt$Data$`DD2 2021` / luk_opt$Data$`DD2 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD3 2021` / luk_opt$Data$`DD3 2018`),0,luk_opt$Data$`DD3 Weightage`*luk_opt$Data$`DD3 2021` / luk_opt$Data$`DD3 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD4 2021` / luk_opt$Data$`DD4 2018`),0,luk_opt$Data$`DD4 Weightage`*luk_opt$Data$`DD4 2021` / luk_opt$Data$`DD4 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD5 2021` / luk_opt$Data$`DD5 2018`),0,luk_opt$Data$`DD5 Weightage`*luk_opt$Data$`DD5 2021` / luk_opt$Data$`DD5 2018`)),
                                              dds22 = luk_opt$Data$sels * (luk_opt$Data$dd_c *ifelse(is.na(luk_opt$Data$`DD1 2022` / luk_opt$Data$`DD1 2018`),0,luk_opt$Data$`DD1 2022` / luk_opt$Data$`DD1 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD2 2022` / luk_opt$Data$`DD2 2018`),0,luk_opt$Data$`DD2 Weightage`*luk_opt$Data$`DD2 2022` / luk_opt$Data$`DD2 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD3 2022` / luk_opt$Data$`DD3 2018`),0,luk_opt$Data$`DD3 Weightage`*luk_opt$Data$`DD3 2022` / luk_opt$Data$`DD3 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD4 2022` / luk_opt$Data$`DD4 2018`),0,luk_opt$Data$`DD4 Weightage`*luk_opt$Data$`DD4 2022` / luk_opt$Data$`DD4 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD5 2022` / luk_opt$Data$`DD5 2018`),0,luk_opt$Data$`DD5 Weightage`*luk_opt$Data$`DD5 2022` / luk_opt$Data$`DD5 2018`)),
                                              dds23 = luk_opt$Data$sels * (luk_opt$Data$dd_c *ifelse(is.na(luk_opt$Data$`DD1 2023` / luk_opt$Data$`DD1 2018`),0,luk_opt$Data$`DD1 2023` / luk_opt$Data$`DD1 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD2 2023` / luk_opt$Data$`DD2 2018`),0,luk_opt$Data$`DD2 Weightage`*luk_opt$Data$`DD2 2023` / luk_opt$Data$`DD2 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD3 2023` / luk_opt$Data$`DD3 2018`),0,luk_opt$Data$`DD3 Weightage`*luk_opt$Data$`DD3 2023` / luk_opt$Data$`DD3 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD4 2023` / luk_opt$Data$`DD4 2018`),0,luk_opt$Data$`DD4 Weightage`*luk_opt$Data$`DD4 2023` / luk_opt$Data$`DD4 2018`)
                                                                           + ifelse(is.na(luk_opt$Data$`DD5 2023` / luk_opt$Data$`DD5 2018`),0,luk_opt$Data$`DD5 Weightage`*luk_opt$Data$`DD5 2023` / luk_opt$Data$`DD5 2018`))
  ))
  
  newluko4a <- observe(luk_opt$Data <- mutate(luk_opt$Data, fte1 = (1-luk_opt$Data$ldt1)*luk_opt$Data$dds19,
                                              fte2 = (1-luk_opt$Data$ldt2)*luk_opt$Data$dds20,
                                              fte3 = (1-luk_opt$Data$ldt3)*luk_opt$Data$dds21,
                                              fte4 = (1-luk_opt$Data$ldt4)*luk_opt$Data$dds22,
                                              fte5 = (1-luk_opt$Data$ldt5)*luk_opt$Data$dds23,
                                              dsray19 = (1-luk_opt$Data$yai19q4)*luk_opt$Data$dds19,
                                              dsray20 = (1-luk_opt$Data$yai20q4)*luk_opt$Data$dds20,
                                              dsray21 = (1-luk_opt$Data$yai21q4)*luk_opt$Data$dds21,
                                              dsray22 = (1-luk_opt$Data$yai22q4)*luk_opt$Data$dds22,
                                              dsray23 = (1-luk_opt$Data$yai23q4)*luk_opt$Data$dds23,
                                              afte1 = (1-luk_opt$Data$yai19q4)*(1-luk_opt$Data$ldt1)*luk_opt$Data$dds19,
                                              afte2 = (1-luk_opt$Data$yai20q4)*(1-luk_opt$Data$ldt2)*luk_opt$Data$dds20,
                                              afte3 = (1-luk_opt$Data$yai21q4)*(1-luk_opt$Data$ldt3)*luk_opt$Data$dds21,
                                              afte4 = (1-luk_opt$Data$yai22q4)*(1-luk_opt$Data$ldt4)*luk_opt$Data$dds22,
                                              afte5 = (1-luk_opt$Data$yai23q4)*(1-luk_opt$Data$ldt5)*luk_opt$Data$dds23
  ))
  
  newlukc1a <- observe(luk_crvt$Data <- mutate(luk2$Data, sels = luk2$Data$crvt))
  newlukc2a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data, addt = ifelse((luk_crvt$Data$as_is_s - luk_crvt$Data$sels)>0, 0,-(luk_crvt$Data$as_is_s - luk_crvt$Data$sels)),
                                               subt= ifelse((luk_crvt$Data$as_is_s - luk_crvt$Data$sels)<0, 0,-(luk_crvt$Data$as_is_s - luk_crvt$Data$sels)),
                                               o18 = luk_crvt$Data$as_is_s - ifelse(is.na(luk_crvt$Data$`2018`),0,luk_crvt$Data$`2018`)))
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,s18 = luk_crvt$Data$o18*(1+luk_crvt$Data$NHR19)))
  
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,o19 = luk_crvt$Data$s18 - ifelse(is.na(luk_crvt$Data$`2019`),0,luk_crvt$Data$`2019`)))
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,s19 = luk_crvt$Data$o19*(1+luk_crvt$Data$NHR20)))
  
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,o20 = luk_crvt$Data$s19 - ifelse(is.na(luk_crvt$Data$`2020`),0,luk_crvt$Data$`2020`)))
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,s20 = luk_crvt$Data$o20*(1+luk_crvt$Data$NHR21)))
  
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,o21 = luk_crvt$Data$s20 - ifelse(is.na(luk_crvt$Data$`2021`),0,luk_crvt$Data$`2021`)))
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,s21 = luk_crvt$Data$o21*(1+luk_crvt$Data$NHR22)))
  
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,o22 = luk_crvt$Data$s21 - ifelse(is.na(luk_crvt$Data$`2022`),0,luk_crvt$Data$`2022`)))
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,s22 = luk_crvt$Data$o22*(1+luk_crvt$Data$NHR23),
                                               o23 = luk_crvt$Data$as_is_s - ifelse(is.na(luk_crvt$Data$`2023`),0,luk_crvt$Data$`2023`),
                                               `DD1 Weightage` = case_when(is.na( luk_crvt$Data$`DD1 Weightage`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 Weightage`),
                                               dd_test = rowSums(luk_crvt$Data[,c("DD1 Weightage","DD2 Weightage","DD3 Weightage","DD4 Weightage","DD5 Weightage")], na.rm=TRUE),
                                               `DD1 2018` = case_when(is.na( luk_crvt$Data$`DD1 2018`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 2018`),
                                               `DD1 2019` = case_when(is.na( luk_crvt$Data$`DD1 2019`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 2019`),
                                               `DD1 2020` = case_when(is.na( luk_crvt$Data$`DD1 2020`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 2020`),
                                               `DD1 2021` = case_when(is.na( luk_crvt$Data$`DD1 2021`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 2021`),
                                               `DD1 2022` = case_when(is.na( luk_crvt$Data$`DD1 2022`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 2022`),
                                               `DD1 2023` = case_when(is.na( luk_crvt$Data$`DD1 2023`) ~ 1, TRUE ~ luk_crvt$Data$`DD1 2023`)))
  newlukc3a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,dd_c = `DD1 Weightage` + 1 - luk_crvt$Data$dd_test))
  newlukc4a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data,dds19 = luk_crvt$Data$sels * (luk_crvt$Data$dd_c *ifelse(is.na(luk_crvt$Data$`DD1 2019` / luk_crvt$Data$`DD1 2018`),0,luk_crvt$Data$`DD1 2019` / luk_crvt$Data$`DD1 2018`)
                                                                                           + ifelse(is.na(luk_crvt$Data$`DD2 2019` / luk_crvt$Data$`DD2 2018`),0,luk_crvt$Data$`DD2 Weightage`*luk_crvt$Data$`DD2 2019` / luk_crvt$Data$`DD2 2018`)
                                                                                           + ifelse(is.na(luk_crvt$Data$`DD3 2019` / luk_crvt$Data$`DD3 2018`),0,luk_crvt$Data$`DD3 Weightage`*luk_crvt$Data$`DD3 2019` / luk_crvt$Data$`DD3 2018`)
                                                                                           + ifelse(is.na(luk_crvt$Data$`DD4 2019` / luk_crvt$Data$`DD4 2018`),0,luk_crvt$Data$`DD4 Weightage`*luk_crvt$Data$`DD4 2019` / luk_crvt$Data$`DD4 2018`)
                                                                                           + ifelse(is.na(luk_crvt$Data$`DD5 2019` / luk_crvt$Data$`DD5 2018`),0,luk_crvt$Data$`DD5 Weightage`*luk_crvt$Data$`DD5 2019` / luk_crvt$Data$`DD5 2018`)),
                                               dds20 = luk_crvt$Data$sels * (luk_crvt$Data$dd_c *ifelse(is.na(luk_crvt$Data$`DD1 2020` / luk_crvt$Data$`DD1 2018`),0,luk_crvt$Data$`DD1 2020` / luk_crvt$Data$`DD1 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD2 2020` / luk_crvt$Data$`DD2 2018`),0,luk_crvt$Data$`DD2 Weightage`*luk_crvt$Data$`DD2 2020` / luk_crvt$Data$`DD2 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD3 2020` / luk_crvt$Data$`DD3 2018`),0,luk_crvt$Data$`DD3 Weightage`*luk_crvt$Data$`DD3 2020` / luk_crvt$Data$`DD3 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD4 2020` / luk_crvt$Data$`DD4 2018`),0,luk_crvt$Data$`DD4 Weightage`*luk_crvt$Data$`DD4 2020` / luk_crvt$Data$`DD4 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD5 2020` / luk_crvt$Data$`DD5 2018`),0,luk_crvt$Data$`DD5 Weightage`*luk_crvt$Data$`DD5 2020` / luk_crvt$Data$`DD5 2018`)),
                                               dds21 = luk_crvt$Data$sels * (luk_crvt$Data$dd_c *ifelse(is.na(luk_crvt$Data$`DD1 2021` / luk_crvt$Data$`DD1 2018`),0,luk_crvt$Data$`DD1 2021` / luk_crvt$Data$`DD1 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD2 2021` / luk_crvt$Data$`DD2 2018`),0,luk_crvt$Data$`DD2 Weightage`*luk_crvt$Data$`DD2 2021` / luk_crvt$Data$`DD2 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD3 2021` / luk_crvt$Data$`DD3 2018`),0,luk_crvt$Data$`DD3 Weightage`*luk_crvt$Data$`DD3 2021` / luk_crvt$Data$`DD3 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD4 2021` / luk_crvt$Data$`DD4 2018`),0,luk_crvt$Data$`DD4 Weightage`*luk_crvt$Data$`DD4 2021` / luk_crvt$Data$`DD4 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD5 2021` / luk_crvt$Data$`DD5 2018`),0,luk_crvt$Data$`DD5 Weightage`*luk_crvt$Data$`DD5 2021` / luk_crvt$Data$`DD5 2018`)),
                                               dds22 = luk_crvt$Data$sels * (luk_crvt$Data$dd_c *ifelse(is.na(luk_crvt$Data$`DD1 2022` / luk_crvt$Data$`DD1 2018`),0,luk_crvt$Data$`DD1 2022` / luk_crvt$Data$`DD1 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD2 2022` / luk_crvt$Data$`DD2 2018`),0,luk_crvt$Data$`DD2 Weightage`*luk_crvt$Data$`DD2 2022` / luk_crvt$Data$`DD2 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD3 2022` / luk_crvt$Data$`DD3 2018`),0,luk_crvt$Data$`DD3 Weightage`*luk_crvt$Data$`DD3 2022` / luk_crvt$Data$`DD3 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD4 2022` / luk_crvt$Data$`DD4 2018`),0,luk_crvt$Data$`DD4 Weightage`*luk_crvt$Data$`DD4 2022` / luk_crvt$Data$`DD4 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD5 2022` / luk_crvt$Data$`DD5 2018`),0,luk_crvt$Data$`DD5 Weightage`*luk_crvt$Data$`DD5 2022` / luk_crvt$Data$`DD5 2018`)),
                                               dds23 = luk_crvt$Data$sels * (luk_crvt$Data$dd_c *ifelse(is.na(luk_crvt$Data$`DD1 2023` / luk_crvt$Data$`DD1 2018`),0,luk_crvt$Data$`DD1 2023` / luk_crvt$Data$`DD1 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD2 2023` / luk_crvt$Data$`DD2 2018`),0,luk_crvt$Data$`DD2 Weightage`*luk_crvt$Data$`DD2 2023` / luk_crvt$Data$`DD2 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD3 2023` / luk_crvt$Data$`DD3 2018`),0,luk_crvt$Data$`DD3 Weightage`*luk_crvt$Data$`DD3 2023` / luk_crvt$Data$`DD3 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD4 2023` / luk_crvt$Data$`DD4 2018`),0,luk_crvt$Data$`DD4 Weightage`*luk_crvt$Data$`DD4 2023` / luk_crvt$Data$`DD4 2018`)
                                                                             + ifelse(is.na(luk_crvt$Data$`DD5 2023` / luk_crvt$Data$`DD5 2018`),0,luk_crvt$Data$`DD5 Weightage`*luk_crvt$Data$`DD5 2023` / luk_crvt$Data$`DD5 2018`))
  ))
  
  newlukc4a <- observe(luk_crvt$Data <- mutate(luk_crvt$Data, fte1 = (1-luk_crvt$Data$ldt1)*luk_crvt$Data$dds19,
                                               fte2 = (1-luk_crvt$Data$ldt2)*luk_crvt$Data$dds20,
                                               fte3 = (1-luk_crvt$Data$ldt3)*luk_crvt$Data$dds21,
                                               fte4 = (1-luk_crvt$Data$ldt4)*luk_crvt$Data$dds22,
                                               fte5 = (1-luk_crvt$Data$ldt5)*luk_crvt$Data$dds23,
                                               dsray19 = (1-luk_crvt$Data$yai19q4)*luk_crvt$Data$dds19,
                                               dsray20 = (1-luk_crvt$Data$yai20q4)*luk_crvt$Data$dds20,
                                               dsray21 = (1-luk_crvt$Data$yai21q4)*luk_crvt$Data$dds21,
                                               dsray22 = (1-luk_crvt$Data$yai22q4)*luk_crvt$Data$dds22,
                                               dsray23 = (1-luk_crvt$Data$yai23q4)*luk_crvt$Data$dds23,
                                               afte1 = (1-luk_crvt$Data$yai19q4)*(1-luk_crvt$Data$ldt1)*luk_crvt$Data$dds19,
                                               afte2 = (1-luk_crvt$Data$yai20q4)*(1-luk_crvt$Data$ldt2)*luk_crvt$Data$dds20,
                                               afte3 = (1-luk_crvt$Data$yai21q4)*(1-luk_crvt$Data$ldt3)*luk_crvt$Data$dds21,
                                               afte4 = (1-luk_crvt$Data$yai22q4)*(1-luk_crvt$Data$ldt4)*luk_crvt$Data$dds22,
                                               afte5 = (1-luk_crvt$Data$yai23q4)*(1-luk_crvt$Data$ldt5)*luk_crvt$Data$dds23
  ))
  
  # vals$Data=data.table(luk2$Data,
  #                      luk2$Data <- mutate(luk2$Data, sels = luk2$Data$as_is_s)
  #                      )
  
  #####
  
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
  
  vals$Data=data.table(luk1,
                       luk1$Modify<-
                         paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(luk1),'>Change</button>
           </div>'),
                       luk1[["Edit"]]<-
                         paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(luk1),'><span class="glyphicon glyphicon-edit" aria-hidden="true"></span></button>
           </div>
           '))
  
  #vals$Data=data.table(zxc <- select(luk1, `CF/SBU/AFF`, Region, `CF/SBU/AFF Name`, Division, Family, `Sub Family` )                       )
  newEntry1 <- observe(vals$Data <- mutate(vals$Data, Modify= paste0('<div class="btn-group" role="group" aria-label="Basic example">
                                                          <button type="button" class="btn btn-secondary delete" id=modify_',1:nrow(luk1),'>Change</button>
                                                          </div>'))
                      )
  
  # newEntry <- observe({vals$Data <- data.table(luk1[luk1$Region %in%  if(is.null(input$rg)){unique(luk1$Region)} else (input$rg),])
  # vals$Data <- data.table(luk1[luk1$Division %in%  if(is.null(input$div)){unique(luk1$Division)} else (input$div) ,])
  # })
  
  aos<-reactive({
    zxc <- select(luk2$Data, `CF/SBU/AFF`, Region, `CF/SBU/AFF Name`, Division, Family, `Sub Family`, Modify,
                  `Demand Driver 1`, `DD1 Weightage`, `DD1 2019`, `DD1 2020`, `DD1 2021`, `DD1 2022`, `DD1 2023` , id)
    zxc <- filter(zxc, `CF/SBU/AFF` %in%  input$uipType 
                  & Region %in%  input$uiprg 
                  & `CF/SBU/AFF Name` %in%  input$uipname 
                  & Division %in%  input$uipdiv
                  & Family %in%  input$uipjob 
                  & `Sub Family` %in%  input$uipsjob)
    
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
    old_row=luk2$Data[selected_row,c('Demand Driver 1','DD1 2019','DD1 2020','DD1 2021','DD1 2022','DD1 2023')]
    row_change=list()
    row_change[[1]]=luk2$Data[selected_row,c('Demand Driver 1')]
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
                     -0.03#col
                   }
                 })
                 
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 #dataset()[as.numeric(gsub("modify_","",input$lastClickkId)),'Manual']<-DF
                 if(DF[1]!=-0.03){
                   luk2$Data[as.numeric(gsub("modify_","",input$lastClickkId)),c('DD1 2019')]<-DF[1]
                 }
                 if(DF[2]!=-0.03){
                   luk2$Data[as.numeric(gsub("modify_","",input$lastClickkId)),c('DD1 2020')]<-DF[2]
                 }
                 if(DF[3]!=-0.03){
                   luk2$Data[as.numeric(gsub("modify_","",input$lastClickkId)),c('DD1 2021')]<-DF[3]
                 }
                 if(DF[4]!=-0.03){
                   luk2$Data[as.numeric(gsub("modify_","",input$lastClickkId)),c('DD1 2022')]<-DF[4]
                 }
                 if(DF[5]!=-0.03){
                   luk2$Data[as.numeric(gsub("modify_","",input$lastClickkId)),c('DD1 2023')]<-DF[5]
                 }
               }
  )
  ######

  
  output$lluk2<- renderDataTable({
    datatable(luk4(),rownames = FALSE,
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
  #end tb
  
  }

# Run the application

shinyApp(ui = ui, server = server)